"""Track state.

A single JSON file records every track and which pipeline stages it has
finished. This is what makes the pipeline resumable: if separation dies
halfway through a batch of 40 songs, re-running skips everything already done.
"""

from __future__ import annotations

import hashlib
import json
import re
import tempfile
import time
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Any, Iterator

STAGES = ("ingested", "separated", "analyzed", "mixed")


def slugify(text: str) -> str:
    text = re.sub(r"[^\w\s-]", "", text).strip().lower()
    text = re.sub(r"[\s_-]+", "-", text)
    return text[:60] or "untitled"


def track_id(source: str, title: str) -> str:
    """Stable ID from the source path/URL, so re-adding the same file is a no-op."""
    digest = hashlib.sha1(source.encode("utf-8")).hexdigest()[:8]
    return f"{slugify(title)}-{digest}"


@dataclass
class Track:
    id: str
    title: str
    source: str
    added_at: float = field(default_factory=time.time)

    # stage completion flags
    ingested: bool = False
    separated: bool = False
    analyzed: bool = False
    mixed: bool = False

    # artifacts
    source_wav: str | None = None
    stem_paths: dict[str, str] = field(default_factory=dict)
    outputs: dict[str, str] = field(default_factory=dict)

    # analysis results
    duration_sec: float | None = None
    bpm: float | None = None
    key: str | None = None
    key_confidence: float | None = None
    source_lufs: float | None = None
    output_lufs: float | None = None

    error: str | None = None

    @property
    def done(self) -> bool:
        return all(getattr(self, s) for s in STAGES)

    def next_stage(self) -> str | None:
        for s in STAGES:
            if not getattr(self, s):
                return s
        return None


class Manifest:
    def __init__(self, path: Path):
        self.path = Path(path)
        self.tracks: dict[str, Track] = {}
        self.load()

    def load(self) -> None:
        if not self.path.exists():
            return
        try:
            raw = json.loads(self.path.read_text())
        except json.JSONDecodeError:
            backup = self.path.with_suffix(".json.corrupt")
            self.path.rename(backup)
            print(f"  ! manifest was unreadable, moved to {backup}")
            return
        for tid, data in raw.get("tracks", {}).items():
            known = {f for f in Track.__dataclass_fields__}
            self.tracks[tid] = Track(**{k: v for k, v in data.items() if k in known})

    def save(self) -> None:
        """Atomic write - a crash mid-save can't leave a truncated manifest."""
        self.path.parent.mkdir(parents=True, exist_ok=True)
        payload = {
            "version": 1,
            "tracks": {tid: asdict(t) for tid, t in self.tracks.items()},
        }
        fd, tmp = tempfile.mkstemp(dir=self.path.parent, suffix=".tmp")
        try:
            with open(fd, "w") as fh:
                json.dump(payload, fh, indent=2)
            Path(tmp).replace(self.path)
        except Exception:
            Path(tmp).unlink(missing_ok=True)
            raise

    def add(self, source: str, title: str) -> Track:
        tid = track_id(source, title)
        if tid in self.tracks:
            return self.tracks[tid]
        track = Track(id=tid, title=title, source=source)
        self.tracks[tid] = track
        self.save()
        return track

    def get(self, tid: str) -> Track | None:
        return self.tracks.get(tid)

    def pending(self) -> Iterator[Track]:
        for t in self.tracks.values():
            if not t.done:
                yield t

    def reset_stage(self, tid: str, stage: str) -> None:
        """Force a stage (and everything after it) to re-run."""
        track = self.tracks[tid]
        idx = STAGES.index(stage)
        for s in STAGES[idx:]:
            setattr(track, s, False)
        track.error = None
        self.save()

    def summary(self) -> dict[str, Any]:
        counts = {s: 0 for s in STAGES}
        for t in self.tracks.values():
            for s in STAGES:
                counts[s] += int(getattr(t, s))
        return {
            "total": len(self.tracks),
            "complete": sum(1 for t in self.tracks.values() if t.done),
            "errored": sum(1 for t in self.tracks.values() if t.error),
            "stages": counts,
        }
