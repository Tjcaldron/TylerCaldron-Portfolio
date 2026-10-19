"""Browser UI.

A thin layer over the same pieces the CLI uses: the manifest for state,
pipeline.run_all for processing, and mixdown for building backing tracks.

Two tabs:
  Process - add songs, see the queue, run the pipeline with a live log
  Remix   - pick a separated track, change stems/gains/loudness, hear it
            immediately, and save it as the track's backing output

Remix previews are built in memory and never touch disk until you press
Save, so moving a slider and re-previewing takes a second or two rather than
re-running anything.
"""

from __future__ import annotations

import dataclasses
import io
import shutil
import threading
import time
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

import gradio as gr
import numpy as np

from . import mixdown, pipeline
from .audio import read, sum_stems
from .config import Config, load_config
from .ingest import discover, title_from_path
from .manifest import Manifest, Track

QUEUE_HEADERS = ["Title", "Stage", "BPM", "Key", "LUFS", "Error"]


class _LogBuffer(io.TextIOBase):
    """Thread-safe stand-in for stdout so the pipeline's prints reach the page."""

    def __init__(self) -> None:
        self._parts: list[str] = []
        self._lock = threading.Lock()

    def write(self, s: str) -> int:
        with self._lock:
            self._parts.append(s)
        return len(s)

    def getvalue(self) -> str:
        with self._lock:
            return "".join(self._parts)


def build(cfg: Config, manifest: Manifest, config_path: str | Path) -> gr.Blocks:
    run_lock = threading.Lock()
    # Stems for the track currently open in Remix. Only one track is kept,
    # since four full-length float stems are a few hundred MB.
    stem_cache: dict[str, dict[str, np.ndarray]] = {}
    stem_names = list(cfg.stems_for_mode)
    upload_dir = cfg.workspace / "00_uploads"

    # -- helpers ------------------------------------------------------------
    def queue_rows() -> list[list]:
        rows = []
        for t in manifest.tracks.values():
            stage = "done" if t.done else f"next: {t.next_stage()}"
            rows.append([
                t.title,
                "error" if t.error else stage,
                f"{t.bpm:g}" if t.bpm else "",
                t.key or "",
                f"{t.output_lufs:.1f}" if t.output_lufs is not None else "",
                t.error or "",
            ])
        return rows

    def remixable() -> list[tuple[str, str]]:
        return [(t.title, t.id) for t in manifest.tracks.values() if t.separated]

    def load_stems(track: Track) -> tuple[dict[str, np.ndarray], int]:
        rate = cfg.sample_rate
        if track.id not in stem_cache:
            stem_cache.clear()
            loaded = {}
            for name, path in track.stem_paths.items():
                loaded[name], rate = read(Path(path))
            stem_cache[track.id] = loaded
        return stem_cache[track.id], rate

    def saved_output(track: Track) -> str | None:
        # mp3 first: every browser plays it, which isn't true of flac.
        for fmt in ("mp3", "wav", "flac"):
            path = track.outputs.get(fmt)
            if path and Path(path).exists():
                return path
        return None

    def track_info(track: Track) -> str:
        bits = []
        if track.bpm:
            bits.append(f"**{track.bpm:g} BPM**")
        if track.key:
            conf = f" ({track.key_confidence:.0%} conf)" if track.key_confidence else ""
            bits.append(f"**{track.key}**{conf}")
        if track.output_lufs is not None:
            bits.append(f"saved mix at {track.output_lufs:.1f} LUFS")
        return " · ".join(bits) or "_Not analyzed yet._"

    def remix_cfg(stems: list[str], target: float, gains: tuple[float, ...]) -> Config:
        return dataclasses.replace(
            cfg,
            backing_stems=list(stems),
            stem_gains_db=dict(zip(stem_names, gains)),
            target_lufs=float(target),
        )

    # -- Process tab --------------------------------------------------------
    def add_uploads(files: list[str] | None):
        if not files:
            return "No files selected.", queue_rows()
        upload_dir.mkdir(parents=True, exist_ok=True)
        added = 0
        for f in files:
            src = Path(f)
            # Copy out of Gradio's temp dir: it gets cleaned up, and a stable
            # path keeps the track id stable if the same song is added twice.
            dest = upload_dir / src.name
            if not dest.exists():
                shutil.copy2(src, dest)
            manifest.add(str(dest.resolve()), title_from_path(dest))
            added += 1
        return f"Added {added} track(s).", queue_rows()

    def add_folder(folder: str):
        path = Path(folder.strip().strip('"')).expanduser()
        if not folder.strip():
            return "Enter a folder or file path.", queue_rows()
        if path.is_dir():
            files = discover(path)
        elif path.exists():
            files = [path]
        else:
            return f"Not found: {path}", queue_rows()
        if not files:
            return f"No audio files found in {path}", queue_rows()
        for f in files:
            manifest.add(str(f.resolve()), title_from_path(f))
        return f"Added {len(files)} track(s) from {path}.", queue_rows()

    def run_queue():
        if not run_lock.acquire(blocking=False):
            yield "Already running.", queue_rows()
            return
        try:
            # Re-read config.yaml so edits made while the UI is open apply.
            try:
                run_cfg = load_config(config_path)
            except Exception as exc:
                yield f"Config error: {exc}", queue_rows()
                return

            buf = _LogBuffer()

            def work() -> None:
                try:
                    with redirect_stdout(buf), redirect_stderr(buf):
                        result = pipeline.run_all(run_cfg, manifest)
                        print(f"Done. {result['processed']} processed, "
                              f"{result['failed']} failed.")
                except Exception as exc:
                    buf.write(f"\n! {exc}\n")

            worker = threading.Thread(target=work, daemon=True)
            worker.start()
            while worker.is_alive():
                yield buf.getvalue(), queue_rows()
                time.sleep(1)
            yield buf.getvalue(), queue_rows()
        finally:
            run_lock.release()

    # -- Remix tab ----------------------------------------------------------
    def select_track(tid: str | None):
        track = manifest.get(tid) if tid else None
        if track is None:
            return "", None, None, ""
        return track_info(track), saved_output(track), None, ""

    def preview(tid, stems, target, *gains):
        track = manifest.get(tid) if tid else None
        if track is None:
            raise gr.Error("Pick a track first.")
        stems = [s for s in stems if s in track.stem_paths]
        if not stems:
            raise gr.Error("Tick at least one stem.")

        loaded, rate = load_stems(track)
        mix = sum_stems({s: loaded[s] for s in stems}, dict(zip(stem_names, gains)))
        mix, achieved = mixdown.normalize_loudness(
            mix, rate, float(target), cfg.peak_ceiling_db
        )
        pcm = (np.clip(mix, -1.0, 1.0) * 32767).astype(np.int16)

        if achieved is None:
            note = "Preview ready (loudness couldn't be measured)."
        elif abs(achieved - target) > 0.5:
            note = (f"Preview at {achieved:.1f} LUFS - held below {target:.0f} "
                    f"to stay under the {cfg.peak_ceiling_db:g} dBFS peak ceiling.")
        else:
            note = f"Preview at {achieved:.1f} LUFS."
        return (rate, pcm), note

    def save_mix(tid, stems, target, *gains):
        track = manifest.get(tid) if tid else None
        if track is None:
            raise gr.Error("Pick a track first.")
        if not stems:
            raise gr.Error("Tick at least one stem.")
        mixdown.run(track, remix_cfg(stems, target, gains), verbose=False)
        manifest.save()
        files = ", ".join(Path(p).name for p in track.outputs.values())
        return track_info(track), saved_output(track), f"Saved: {files}", queue_rows()

    # -- layout -------------------------------------------------------------
    with gr.Blocks(title="covertools") as demo:
        gr.Markdown("# covertools\nTurn songs into backing tracks you can sing over.")

        with gr.Tab("Process"):
            with gr.Row():
                with gr.Column():
                    uploads = gr.File(
                        label="Drop songs here", file_count="multiple", type="filepath"
                    )
                    add_btn = gr.Button("Add uploaded songs")
                with gr.Column():
                    folder = gr.Textbox(
                        label="...or add a folder or file already on this computer",
                        placeholder=r"C:\Users\you\Music\covers",
                    )
                    folder_btn = gr.Button("Add from path")
            add_status = gr.Markdown()

            queue = gr.Dataframe(
                value=queue_rows, headers=QUEUE_HEADERS,
                interactive=False, wrap=True, label="Tracks",
            )
            run_btn = gr.Button("Process pending tracks", variant="primary")
            log = gr.Textbox(
                label="Log", lines=14, max_lines=30, autoscroll=True,
                placeholder="Separation is the slow part - a few minutes per song "
                            "on GPU, much longer on CPU. Demucs's own progress "
                            "bar appears in the terminal you started this from.",
            )

        with gr.Tab("Remix"):
            gr.Markdown(
                "Rebuild a track's backing mix from its saved stems. "
                "**Preview** is instant and changes nothing; **Save** writes the "
                "files to `03_backing`."
            )
            with gr.Row():
                track_pick = gr.Dropdown(
                    choices=remixable(), label="Track", scale=3,
                )
                refresh_btn = gr.Button("Refresh list", scale=1)
            info = gr.Markdown()

            with gr.Row():
                with gr.Column():
                    stem_pick = gr.CheckboxGroup(
                        choices=stem_names, value=cfg.backing_stems,
                        label="Stems in the mix",
                    )
                    gain_sliders = [
                        gr.Slider(
                            -24, 6, step=0.5, label=f"{name} gain (dB)",
                            value=cfg.stem_gains_db.get(name, 0.0),
                        )
                        for name in stem_names
                    ]
                    target = gr.Slider(
                        -24, -9, step=0.5, value=cfg.target_lufs,
                        label="Target loudness (LUFS)",
                        info="-16 to -18 leaves room for your vocal.",
                    )
                with gr.Column():
                    preview_btn = gr.Button("Preview", variant="primary")
                    preview_audio = gr.Audio(label="Preview", interactive=False)
                    preview_note = gr.Markdown()
                    save_btn = gr.Button("Save as this track's backing")
                    saved_audio = gr.Audio(
                        label="Currently saved", interactive=False, type="filepath",
                    )

        # -- wiring ---------------------------------------------------------
        add_btn.click(add_uploads, uploads, [add_status, queue])
        folder_btn.click(add_folder, folder, [add_status, queue])
        folder.submit(add_folder, folder, [add_status, queue])
        run_btn.click(run_queue, None, [log, queue])

        refresh_btn.click(lambda: gr.Dropdown(choices=remixable()), None, track_pick)
        track_pick.change(
            select_track, track_pick, [info, saved_audio, preview_audio, preview_note]
        )
        mix_inputs = [track_pick, stem_pick, target, *gain_sliders]
        preview_btn.click(preview, mix_inputs, [preview_audio, preview_note])
        save_btn.click(save_mix, mix_inputs, [info, saved_audio, preview_note, queue])

        # Pick up tracks processed from the CLI since the page was opened.
        demo.load(lambda: (queue_rows(), gr.Dropdown(choices=remixable())),
                  None, [queue, track_pick])

    return demo


def launch(cfg: Config, manifest: Manifest, config_path: str | Path, port: int) -> None:
    demo = build(cfg, manifest, config_path)
    demo.launch(
        server_port=port,
        inbrowser=True,
        allowed_paths=[str(cfg.workspace.resolve())],
    )
