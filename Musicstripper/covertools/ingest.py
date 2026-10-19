"""Stage 1: ingest.

Takes whatever you point it at and produces one canonical WAV in
workspace/01_source/. Everything downstream reads from there.
"""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

from .audio import to_wav, AudioError
from .config import Config
from .manifest import Track

AUDIO_EXTS = {".wav", ".mp3", ".flac", ".m4a", ".aac", ".ogg", ".opus", ".wma", ".aiff"}


def is_url(source: str) -> bool:
    return source.startswith(("http://", "https://"))


def discover(folder: Path) -> list[Path]:
    """Find every audio file in a folder, for batch adds."""
    return sorted(
        p for p in Path(folder).rglob("*")
        if p.is_file() and p.suffix.lower() in AUDIO_EXTS
    )


def title_from_path(path: Path) -> str:
    return path.stem.replace("_", " ").strip()


def _fetch_url(url: str, dest_dir: Path) -> Path:
    """Optional path: pull audio from a URL with yt-dlp.

    yt-dlp is not a dependency of this project and isn't installed by
    requirements.txt. Only use this for material you have the right to
    download - your own uploads, Creative Commons tracks, karaoke channels
    that permit it. For commercial releases, buy the file and point the
    pipeline at the local copy instead.
    """
    if not shutil.which("yt-dlp"):
        raise AudioError(
            "yt-dlp is not installed. Either install it yourself or download "
            "the audio manually and add the local file."
        )
    dest_dir.mkdir(parents=True, exist_ok=True)
    template = str(dest_dir / "%(title)s.%(ext)s")
    cmd = ["yt-dlp", "-x", "--audio-format", "wav", "-o", template, url]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise AudioError(f"yt-dlp failed:\n{proc.stderr.strip()[:500]}")
    candidates = sorted(dest_dir.glob("*.wav"), key=lambda p: p.stat().st_mtime)
    if not candidates:
        raise AudioError("yt-dlp reported success but produced no file")
    return candidates[-1]


def run(track: Track, cfg: Config) -> Track:
    dest = cfg.dir_source / f"{track.id}.wav"

    if dest.exists():
        track.source_wav = str(dest)
        track.ingested = True
        return track

    if is_url(track.source):
        tmp_dir = cfg.workspace / ".download"
        raw = _fetch_url(track.source, tmp_dir)
    else:
        raw = Path(track.source).expanduser()
        if not raw.exists():
            raise AudioError(f"source file not found: {raw}")

    to_wav(raw, dest, sample_rate=cfg.sample_rate, channels=cfg.channels)

    track.source_wav = str(dest)
    track.ingested = True
    return track
