"""Stage 3: analysis.

Three numbers you want before you ever open the DAW:

  BPM   Beats per minute. Set your project tempo to this and your click,
        punch-ins, and any MIDI you layer will line up with the backing track.

  Key   Which set of notes the song lives in. If it sits outside your range,
        you now know how many semitones to shift. Pitch-shifting the backing
        track by more than about three semitones starts to sound obviously
        processed, so it's better to know before you commit.

  LUFS  How loud the source actually is, perceptually. Used as a sanity
        reference for the normalized output.

None of these are ground truth. Tempo detection commonly reports half or double
the real value on sparse material, and key detection gets confused by songs
that modulate. Treat them as a strong first guess, not gospel.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np

from .audio import read
from .config import Config
from .manifest import Track

NOTES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

# Krumhansl-Schmuckler key profiles: empirically derived ratings of how well
# each of the 12 pitch classes "fits" a major or minor key. Correlating a
# song's average pitch content against all 24 rotations of these gives you a
# key estimate. This is the classic approach and it's still competitive.
MAJOR_PROFILE = np.array(
    [6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88]
)
MINOR_PROFILE = np.array(
    [6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17]
)


def measure_lufs(path: Path) -> float | None:
    """Integrated loudness per ITU-R BS.1770 / EBU R128."""
    try:
        import pyloudnorm as pyln
    except ImportError:
        return None
    data, rate = read(path)
    meter = pyln.Meter(rate)
    # Files shorter than the 400 ms measurement block can't be measured.
    if data.shape[0] < rate * 0.5:
        return None
    value = meter.integrated_loudness(data)
    return None if not np.isfinite(value) else float(value)


def estimate_key(y: np.ndarray, sr: int) -> tuple[str, float]:
    """Return e.g. ('F# minor', 0.82) where the second value is confidence."""
    import librosa

    # Constant-Q chroma: energy folded into 12 pitch classes, log-spaced in
    # frequency so it matches how musical pitch actually works.
    chroma = librosa.feature.chroma_cqt(y=y, sr=sr)
    profile = chroma.mean(axis=1)
    if profile.sum() <= 0:
        return "unknown", 0.0
    profile = profile / profile.sum()

    scores: list[tuple[float, str]] = []
    for i in range(12):
        rotated = np.roll(profile, -i)
        for name, template in (("major", MAJOR_PROFILE), ("minor", MINOR_PROFILE)):
            corr = float(np.corrcoef(rotated, template)[0, 1])
            if np.isfinite(corr):
                scores.append((corr, f"{NOTES[i]} {name}"))

    if not scores:
        return "unknown", 0.0
    scores.sort(reverse=True)
    best_score, best_key = scores[0]
    runner_up = scores[1][0] if len(scores) > 1 else 0.0
    # Confidence = how far ahead the winner is. A near-tie means the song is
    # ambiguous (often a relative major/minor pair, which share all 7 notes).
    confidence = max(0.0, min(1.0, (best_score - runner_up) * 3))
    return best_key, round(confidence, 2)


def estimate_tempo(y: np.ndarray, sr: int) -> float:
    import librosa

    onset_env = librosa.onset.onset_strength(y=y, sr=sr)

    # tempo() moved from librosa.beat to librosa.feature.rhythm in 0.10.
    # Import the submodule explicitly - it isn't always exposed as an attribute.
    try:
        from librosa.feature.rhythm import tempo as tempo_fn
    except ImportError:
        tempo_fn = librosa.beat.tempo

    tempo = tempo_fn(onset_envelope=onset_env, sr=sr, aggregate=np.median)
    return round(float(np.atleast_1d(tempo)[0]), 1)


def run(track: Track, cfg: Config) -> Track:
    src = Path(track.source_wav)

    try:
        import librosa
    except ImportError:
        track.analyzed = True  # analysis is optional; don't block the pipeline
        return track

    # Analysing the drum stem gives a much cleaner tempo read than the full
    # mix, and the harmonic stems give a cleaner key read. Use them if we have
    # them; fall back to the full mix otherwise.
    tempo_src = Path(track.stem_paths.get("drums", src))
    harmonic_src = Path(track.stem_paths.get("other", src))

    y_t, sr = librosa.load(str(tempo_src), sr=22050, mono=True)
    track.bpm = estimate_tempo(y_t, sr)
    track.duration_sec = round(float(librosa.get_duration(path=str(src))), 2)

    y_h, sr_h = librosa.load(str(harmonic_src), sr=22050, mono=True)
    key, conf = estimate_key(y_h, sr_h)
    track.key = key
    track.key_confidence = conf

    track.source_lufs = measure_lufs(src)
    track.analyzed = True
    return track
