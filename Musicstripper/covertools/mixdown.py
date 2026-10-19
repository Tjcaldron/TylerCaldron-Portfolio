"""Stage 4: mixdown.

Sums the stems you want into a backing track, then normalizes it so every
track you produce sits at the same perceived loudness.

Why normalize at all: if track A comes out at -9 LUFS and track B at -20, you
end up re-riding your monitor level and your vocal chain settings for every
song. Pinning everything to one target means your recording setup stays put.

Why -16 rather than -14: -14 LUFS is roughly what streaming services normalize
to, so a commercial master already sits about there. But a full commercial
master already has a vocal in it. Yours doesn't yet. Leaving the backing track
a couple dB quieter gives your voice somewhere to sit without you having to
fight the instrumental in the mix.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np

from .audio import (
    encode_mp3, read, write, sum_stems, peak_dbfs, db_to_linear,
    linear_to_db, null_test,
)
from .config import Config
from .manifest import Track


def normalize_loudness(
    data: np.ndarray,
    rate: int,
    target_lufs: float,
    peak_ceiling_db: float,
) -> tuple[np.ndarray, float | None]:
    """Gain the audio to hit target LUFS, backing off if it would clip.

    This is gain only - no compression, no limiting. If a track can't reach
    the target without exceeding the peak ceiling, we take the quieter result
    rather than squashing the dynamics. You'd rather have a slightly quiet
    backing track than a crushed one.
    """
    try:
        import pyloudnorm as pyln
    except ImportError:
        return data, None

    if data.shape[0] < rate * 0.5:
        return data, None

    meter = pyln.Meter(rate)
    current = meter.integrated_loudness(data)
    if not np.isfinite(current):
        return data, None

    gain_db = target_lufs - current
    gained = data * db_to_linear(gain_db)

    # Peak check. Sample peak isn't the same as true peak (the analog waveform
    # between samples can overshoot), which is why the default ceiling is -1
    # dBFS rather than 0 - that dB is the safety margin.
    ceiling = db_to_linear(peak_ceiling_db)
    peak = float(np.max(np.abs(gained))) if gained.size else 0.0
    if peak > ceiling:
        gained *= ceiling / peak

    achieved = meter.integrated_loudness(gained)
    return gained.astype(np.float32), float(achieved) if np.isfinite(achieved) else None


def run(track: Track, cfg: Config, verbose: bool = True) -> Track:
    if not track.stem_paths:
        raise RuntimeError("no stems available to mix")

    rate = cfg.sample_rate
    loaded: dict[str, np.ndarray] = {}
    all_loaded: dict[str, np.ndarray] = {}

    for name, path in track.stem_paths.items():
        data, r = read(Path(path))
        rate = r
        all_loaded[name] = data
        if name in cfg.backing_stems:
            loaded[name] = data

    if not loaded:
        raise RuntimeError(
            f"none of {cfg.backing_stems} found in stems {sorted(track.stem_paths)}"
        )

    # Sanity check: do the stems actually reconstruct the original?
    if verbose and track.source_wav and len(all_loaded) > 1:
        try:
            source, _ = read(Path(track.source_wav))
            recon = sum_stems(all_loaded, {})
            residual = null_test(source, recon)
            flag = "" if residual < -40 else "   <- higher than expected"
            print(f"    null test residual: {residual:.1f} dBFS{flag}")
        except Exception:
            pass

    mix = sum_stems(loaded, cfg.stem_gains_db)
    if verbose:
        print(f"    summed {len(loaded)} stems, peak {peak_dbfs(mix):.1f} dBFS")

    mix, achieved = normalize_loudness(
        mix, rate, cfg.target_lufs, cfg.peak_ceiling_db
    )
    track.output_lufs = round(achieved, 2) if achieved is not None else None

    stem_tag = "instrumental" if "vocals" not in cfg.backing_stems else "custom"
    base = cfg.dir_output / f"{track.id}-{stem_tag}"

    outputs: dict[str, str] = {}
    wav_path = write(base.with_suffix(".wav"), mix, rate, subtype="PCM_24")
    outputs["wav"] = str(wav_path)

    if cfg.write_flac:
        try:
            outputs["flac"] = str(
                write(base.with_suffix(".flac"), mix, rate, subtype="PCM_24")
            )
        except Exception as exc:
            print(f"    ! flac write skipped: {exc}")

    if cfg.write_mp3_preview:
        try:
            outputs["mp3"] = str(
                encode_mp3(wav_path, base.with_suffix(".mp3"), cfg.mp3_bitrate)
            )
        except Exception as exc:
            print(f"    ! mp3 preview skipped: {exc}")

    track.outputs = outputs
    track.mixed = True
    return track
