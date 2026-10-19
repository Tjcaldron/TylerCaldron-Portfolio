"""Configuration loading.

Everything tunable lives in config.yaml so you can change models and
loudness targets without touching code.
"""

from __future__ import annotations

from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Any

try:
    import yaml
except ImportError:  # pragma: no cover - yaml is in requirements
    yaml = None


# The four stems Demucs produces in its default 4-stem mode.
ALL_STEMS = ("vocals", "drums", "bass", "other")


@dataclass
class Config:
    # --- Where things live -------------------------------------------------
    workspace: Path = Path("./workspace")

    # --- Separation --------------------------------------------------------
    # htdemucs_ft is the fine-tuned Hybrid Transformer Demucs model. It runs
    # each of the four sub-models separately, so it's ~4x slower than plain
    # htdemucs but noticeably cleaner. Use "htdemucs" if you're impatient.
    model: str = "htdemucs_ft"

    # "four" gives you vocals/drums/bass/other and lets you rebuild the
    # backing track however you like. "two" is faster but gives you only
    # vocals + everything-else with no further control.
    stem_mode: str = "four"

    # Splitting the file into overlapping chunks keeps VRAM use bounded.
    # Lower this if you hit out-of-memory errors on your GPU.
    segment_seconds: int = 10

    # Shift trick: run separation N times with small time offsets and average
    # the results. Meaningfully better output, N times slower. 0 disables it.
    shifts: int = 0

    device: str = "auto"  # "auto" | "cuda" | "cpu"

    # --- Mixdown -----------------------------------------------------------
    # Which stems get summed into the backing track. Drop "vocals" for a
    # standard instrumental; keep it at low gain if you want a guide vocal.
    backing_stems: list[str] = field(
        default_factory=lambda: ["drums", "bass", "other"]
    )

    # Per-stem gain in dB applied before summing. Handy for pulling down a
    # "other" stem that's full of vocal bleed.
    stem_gains_db: dict[str, float] = field(default_factory=dict)

    # Integrated loudness target for the finished backing track, in LUFS.
    # Streaming platforms normalize to about -14. Backing tracks want to sit
    # lower than that so your vocal has somewhere to live: -16 to -18.
    target_lufs: float = -16.0

    # Hard ceiling for sample peaks after normalization, in dBFS.
    peak_ceiling_db: float = -1.0

    # --- Output ------------------------------------------------------------
    # Lossless master for the DAW, plus a small mp3 for phone reference.
    write_flac: bool = True
    write_mp3_preview: bool = True
    mp3_bitrate: str = "192k"

    # --- Working format ----------------------------------------------------
    sample_rate: int = 44100
    channels: int = 2

    @property
    def stems_for_mode(self) -> tuple[str, ...]:
        return ALL_STEMS if self.stem_mode == "four" else ("vocals", "no_vocals")

    # -- directories --------------------------------------------------------
    @property
    def dir_source(self) -> Path:
        return self.workspace / "01_source"

    @property
    def dir_stems(self) -> Path:
        return self.workspace / "02_stems"

    @property
    def dir_output(self) -> Path:
        return self.workspace / "03_backing"

    @property
    def manifest_path(self) -> Path:
        return self.workspace / "manifest.json"

    def ensure_dirs(self) -> None:
        for d in (self.dir_source, self.dir_stems, self.dir_output):
            d.mkdir(parents=True, exist_ok=True)

    def to_dict(self) -> dict[str, Any]:
        d = asdict(self)
        d["workspace"] = str(self.workspace)
        return d


def load_config(path: str | Path = "config.yaml") -> Config:
    """Load config.yaml, falling back to defaults for anything absent."""
    path = Path(path)
    if not path.exists():
        return Config()
    if yaml is None:
        raise RuntimeError("pyyaml is required to read config.yaml")

    raw = yaml.safe_load(path.read_text()) or {}
    known = {f for f in Config.__dataclass_fields__}
    unknown = set(raw) - known
    if unknown:
        raise ValueError(f"Unknown config keys: {sorted(unknown)}")

    if "workspace" in raw:
        raw["workspace"] = Path(raw["workspace"]).expanduser()

    cfg = Config(**raw)
    _validate(cfg)
    return cfg


def _validate(cfg: Config) -> None:
    if cfg.stem_mode not in ("two", "four"):
        raise ValueError("stem_mode must be 'two' or 'four'")
    if cfg.stem_mode == "two" and set(cfg.backing_stems) - {"no_vocals", "vocals"}:
        raise ValueError(
            "In two-stem mode, backing_stems can only contain 'no_vocals'/'vocals'"
        )
    if cfg.stem_mode == "four":
        bad = set(cfg.backing_stems) - set(ALL_STEMS)
        if bad:
            raise ValueError(f"Unknown stems in backing_stems: {sorted(bad)}")
    if not cfg.backing_stems:
        raise ValueError("backing_stems is empty - nothing to mix")
    if cfg.target_lufs > -6:
        raise ValueError("target_lufs looks wrong; expected something like -16")
