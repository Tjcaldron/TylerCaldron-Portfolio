"""Pipeline orchestration.

Runs each track through ingest -> separate -> analyze -> mixdown, saving state
after every stage. Any track that errors is recorded and skipped; the rest of
the batch continues. Re-running picks up exactly where it left off.
"""

from __future__ import annotations

import time
import traceback
from typing import Callable, Iterable

from . import ingest, separate, analyze, mixdown
from .config import Config
from .manifest import Manifest, Track, STAGES

StageFn = Callable[..., Track]

STAGE_FNS: dict[str, StageFn] = {
    "ingested": ingest.run,
    "separated": separate.run,
    "analyzed": analyze.run,
    "mixed": mixdown.run,
}

# Stages that accept a verbose flag.
VERBOSE_STAGES = {"separated", "mixed"}


def run_track(track: Track, cfg: Config, manifest: Manifest, verbose: bool = True) -> bool:
    """Advance one track through every remaining stage. Returns success."""
    for stage in STAGES:
        if getattr(track, stage):
            continue

        fn = STAGE_FNS[stage]
        started = time.time()
        if verbose:
            print(f"  [{stage}] {track.title}")

        try:
            if stage in VERBOSE_STAGES:
                fn(track, cfg, verbose=verbose)
            else:
                fn(track, cfg)
        except Exception as exc:
            track.error = f"{stage}: {exc}"
            manifest.save()
            print(f"  ! failed at {stage}: {exc}")
            if verbose:
                traceback.print_exc(limit=2)
            return False

        track.error = None
        manifest.save()
        if verbose:
            print(f"    ok ({time.time() - started:.1f}s)")

    return True


def run_all(
    cfg: Config,
    manifest: Manifest,
    only: Iterable[str] | None = None,
    verbose: bool = True,
) -> dict[str, int]:
    cfg.ensure_dirs()

    if only:
        wanted = set(only)
        tracks = [t for t in manifest.tracks.values() if t.id in wanted]
    else:
        tracks = list(manifest.pending())

    if not tracks:
        print("Nothing to do - everything is already processed.")
        return {"processed": 0, "failed": 0}

    print(f"Processing {len(tracks)} track(s)\n")
    processed = failed = 0

    for i, track in enumerate(tracks, 1):
        print(f"[{i}/{len(tracks)}] {track.title}")
        if run_track(track, cfg, manifest, verbose=verbose):
            processed += 1
            _report(track)
        else:
            failed += 1
        print()

    return {"processed": processed, "failed": failed}


def _report(track: Track) -> None:
    bits = []
    if track.bpm:
        bits.append(f"{track.bpm} BPM")
    if track.key:
        conf = f" ({track.key_confidence:.0%} conf)" if track.key_confidence else ""
        bits.append(f"key {track.key}{conf}")
    if track.output_lufs is not None:
        bits.append(f"{track.output_lufs:.1f} LUFS")
    if bits:
        print("  " + " | ".join(bits))
    if track.outputs:
        primary = track.outputs.get("flac") or track.outputs.get("wav")
        print(f"  -> {primary}")
