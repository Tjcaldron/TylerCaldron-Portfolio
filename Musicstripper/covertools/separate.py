"""Stage 2: separation.

This is the part that does the actual magic, and it's the part you write the
least code for. Demucs is a neural network that takes a stereo mix and outputs
four signals which sum back to it.

The intuition: the model was trained on a large set of songs where the isolated
tracks were available, so it learned what a drum kit looks like versus what a
voice looks like, in both the waveform and the spectrogram. At inference it
predicts a mask for each source. It is not filtering by frequency - a bass
guitar and a kick drum occupy the same frequencies, and the model separates
them anyway, because it learned what each one *behaves* like over time.

Practical consequences worth knowing:

  - Artifacts cluster where sources overlap most. A loud belted chorus leaves
    more residue in the instrumental than a sparse verse. For covers this is
    lucky: your new vocal sits on top of exactly the spots that are messiest.

  - Reverb tails belong to whoever generated them. Vocal reverb usually follows
    the vocal out of the mix, which is why a stripped instrumental can sound
    drier and smaller than the record. Adding a little reverb back to the
    backing track in your DAW closes most of that gap.

  - Live recordings separate worse than studio ones, because mic bleed means
    the sources were never really independent to begin with.
"""

from __future__ import annotations

import shutil
import subprocess
import sys
from pathlib import Path

from .config import Config
from .manifest import Track


class SeparationError(RuntimeError):
    pass


def detect_device(preference: str = "auto") -> str:
    if preference != "auto":
        return preference
    try:
        import torch
        if torch.cuda.is_available():
            return "cuda"
        if getattr(torch.backends, "mps", None) and torch.backends.mps.is_available():
            return "mps"  # Apple Silicon
    except ImportError:
        pass
    return "cpu"


def build_command(src: Path, out_dir: Path, cfg: Config, device: str) -> list[str]:
    cmd = [
        sys.executable, "-m", "demucs.separate",
        "-n", cfg.model,
        "-o", str(out_dir),
        "--filename", "{stem}.{ext}",
        "-d", device,
        "--segment", str(cfg.segment_seconds),
    ]
    if cfg.shifts:
        cmd += ["--shifts", str(cfg.shifts)]
    if cfg.stem_mode == "two":
        cmd += ["--two-stems", "vocals"]
    cmd.append(str(src))
    return cmd


def run(track: Track, cfg: Config, verbose: bool = True) -> Track:
    src = Path(track.source_wav)
    if not src.exists():
        raise SeparationError(f"source wav missing: {src}")

    track_dir = cfg.dir_stems / track.id
    expected = {s: track_dir / f"{s}.wav" for s in cfg.stems_for_mode}

    if all(p.exists() for p in expected.values()):
        track.stem_paths = {k: str(v) for k, v in expected.items()}
        track.separated = True
        return track

    device = detect_device(cfg.device)
    staging = cfg.workspace / ".demucs" / track.id
    staging.mkdir(parents=True, exist_ok=True)

    cmd = build_command(src, staging, cfg, device)
    if verbose:
        print(f"    device={device} model={cfg.model}")

    proc = subprocess.run(cmd, capture_output=not verbose, text=True)
    if proc.returncode != 0:
        detail = (proc.stderr or "")[-800:] if proc.stderr else "(see output above)"
        raise SeparationError(f"demucs exited {proc.returncode}\n{detail}")

    # Demucs writes to <out>/<model_name>/<...>. Flatten it into our layout.
    produced = list(staging.rglob("*.wav"))
    if not produced:
        raise SeparationError("demucs produced no stems")

    track_dir.mkdir(parents=True, exist_ok=True)
    found: dict[str, str] = {}
    for path in produced:
        name = path.stem
        target = track_dir / f"{name}.wav"
        shutil.move(str(path), target)
        found[name] = str(target)

    shutil.rmtree(staging, ignore_errors=True)

    missing = set(cfg.stems_for_mode) - set(found)
    if missing:
        raise SeparationError(f"missing expected stems: {sorted(missing)}")

    track.stem_paths = found
    track.separated = True
    return track
