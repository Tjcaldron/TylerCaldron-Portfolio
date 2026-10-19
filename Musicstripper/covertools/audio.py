"""Low-level audio helpers.

A quick primer on the numbers you'll see everywhere in this project:

  Sample rate (44100 Hz)  How many times per second the waveform is measured.
                          CD standard. Everything in the pipeline is resampled
                          to one rate so stems always line up sample-for-sample.

  Bit depth / float32     How precisely each measurement is stored. We work in
                          32-bit float internally because it can go above 0 dBFS
                          without clipping, which matters when you sum stems.

  dBFS                    Decibels relative to full scale. 0 dBFS is the loudest
                          a sample can be before it clips. Everything is negative.

  LUFS                    Perceived loudness, averaged over the whole track and
                          weighted for how human hearing works. Two songs at the
                          same peak level can differ hugely in LUFS. This is what
                          you normalize to, not peak.
"""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

import numpy as np
import soundfile as sf


class AudioError(RuntimeError):
    pass


def require_ffmpeg() -> str:
    exe = shutil.which("ffmpeg")
    if not exe:
        raise AudioError(
            "ffmpeg not found on PATH. Install it - it's the universal "
            "decoder that lets this handle mp3/m4a/flac/wav without caring."
        )
    return exe


def to_wav(src: Path, dst: Path, sample_rate: int = 44100, channels: int = 2) -> Path:
    """Decode anything into a clean, uniform WAV.

    Why bother: Demucs needs a consistent input, and lossy formats decode with
    slightly different lengths depending on the encoder's padding. Normalizing
    to WAV once up front means your stems are guaranteed to be sample-aligned
    with the source, which is what makes a null test possible later.
    """
    require_ffmpeg()
    dst.parent.mkdir(parents=True, exist_ok=True)
    cmd = [
        "ffmpeg", "-nostdin", "-y", "-loglevel", "error",
        "-i", str(src),
        "-ar", str(sample_rate),
        "-ac", str(channels),
        "-c:a", "pcm_s24le",   # 24-bit PCM: plenty of headroom, no compression
        str(dst),
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise AudioError(f"ffmpeg failed on {src.name}:\n{proc.stderr.strip()}")
    return dst


def encode_mp3(src: Path, dst: Path, bitrate: str = "192k") -> Path:
    require_ffmpeg()
    dst.parent.mkdir(parents=True, exist_ok=True)
    cmd = [
        "ffmpeg", "-nostdin", "-y", "-loglevel", "error",
        "-i", str(src), "-c:a", "libmp3lame", "-b:a", bitrate, str(dst),
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise AudioError(f"mp3 encode failed:\n{proc.stderr.strip()}")
    return dst


def read(path: Path) -> tuple[np.ndarray, int]:
    """Read a file as float32, always shaped (samples, channels)."""
    data, rate = sf.read(str(path), dtype="float32", always_2d=True)
    return data, rate


def write(path: Path, data: np.ndarray, rate: int, subtype: str = "PCM_24") -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    sf.write(str(path), data, rate, subtype=subtype)
    return path


def db_to_linear(db: float) -> float:
    """-6 dB -> 0.501. Decibels are logarithmic; gain math is multiplicative."""
    return float(10.0 ** (db / 20.0))


def linear_to_db(x: float) -> float:
    return float(20.0 * np.log10(max(abs(x), 1e-12)))


def peak_dbfs(data: np.ndarray) -> float:
    return linear_to_db(float(np.max(np.abs(data))) if data.size else 0.0)


def sum_stems(stems: dict[str, np.ndarray], gains_db: dict[str, float]) -> np.ndarray:
    """Add stems back together, applying per-stem gain.

    Stems from Demucs sum back to (approximately) the original mix, so a plain
    addition is correct - no averaging, no division. Summing can push peaks
    above 1.0, which is exactly why we work in float and normalize afterwards
    instead of clipping here.
    """
    if not stems:
        raise AudioError("no stems to sum")

    length = max(s.shape[0] for s in stems.values())
    chans = max(s.shape[1] for s in stems.values())
    mix = np.zeros((length, chans), dtype=np.float32)

    for name, data in stems.items():
        gain = db_to_linear(gains_db.get(name, 0.0))
        # Pad any stem that came back a few samples short.
        if data.shape[0] < length:
            pad = np.zeros((length - data.shape[0], data.shape[1]), dtype=np.float32)
            data = np.vstack([data, pad])
        if data.shape[1] < chans:
            data = np.repeat(data, chans, axis=1)
        mix += data * gain

    return mix


def null_test(a: np.ndarray, b: np.ndarray) -> float:
    """Subtract one signal from another and report the residual in dBFS.

    Sum all four stems back together and null them against the source: if the
    result is very quiet (roughly -60 dBFS or lower), separation was lossless
    in the sense that nothing went missing - it only got redistributed. A loud
    residual means a stem failed to write or lengths drifted.
    """
    n = min(a.shape[0], b.shape[0])
    c = min(a.shape[1], b.shape[1])
    return peak_dbfs(a[:n, :c] - b[:n, :c])
