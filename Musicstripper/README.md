# covertools

Turns a song into a normalized backing track you can sing over, plus the BPM
and key you need before you open your DAW.

```
song.mp3  ->  [ingest]  ->  [separate]  ->  [analyze]  ->  [mixdown]  ->  backing.flac
              uniform       4 stems        bpm/key       summed +        + .mp3 preview
              44.1k wav                                  normalized      + bpm/key in manifest
```

---

## Quick start

```bash
pip install -r requirements.txt

python run.py add ./my-songs/           # queue a folder (or a single file)
python run.py run                       # process everything pending
python run.py status                    # where things stand
python run.py list                      # per-track state, bpm, key
```

Output lands in `workspace/03_backing/`. Stems stay in `workspace/02_stems/`
so you can remix without re-running separation, which is the slow part.

You also need **ffmpeg** on your PATH — it's the universal decoder that lets
this accept mp3/m4a/flac/wav without caring which one you gave it.

---

## Audio concepts, briefly

If you haven't worked with audio before, these are the only ideas you need to
follow what the code is doing.

**Sample rate (44100 Hz)** — how many times per second the waveform is
measured. CD standard. Everything is resampled to one rate up front so stems
line up sample-for-sample with each other and with the source.

**dBFS (decibels full scale)** — 0 dBFS is the loudest a sample can be before
it clips. All real values are negative. Decibels are logarithmic: -6 dB is
half the amplitude, not six units quieter.

**LUFS** — perceived loudness averaged over a whole track and weighted for how
human hearing works. This is the number that matters. Two songs can have
identical peaks and still sound wildly different in volume; LUFS captures that
difference and peak doesn't. Streaming services normalize to about -14 LUFS.

**Clipping** — when a sample exceeds full scale and gets flattened at the
ceiling. Audible as harsh distortion. Summing stems can easily push past full
scale, which is why the pipeline works in 32-bit float internally (float can
exceed 1.0 harmlessly) and only commits to a fixed-point format after
normalizing.

**Stems** — the isolated component tracks of a mix. Demucs gives you four:
vocals, drums, bass, and "other" (everything else — guitars, keys, synths,
strings). They sum back to the original.

**Null test** — subtract the summed stems from the original source. If the
result is near-silent, nothing was lost, only redistributed. The pipeline runs
this automatically and prints the residual. On a healthy run you'll see
something below -40 dBFS. A loud residual means a stem failed to write or the
lengths drifted.

---

## How separation actually works

Demucs is a neural network trained on songs where the isolated tracks were
available, so it learned what a drum kit looks like versus what a voice looks
like — in both the waveform and the spectrogram simultaneously.

The important thing to understand: **it is not filtering by frequency.** A kick
drum and a bass guitar occupy the same frequency range, and the model separates
them anyway, because it learned what each one *behaves* like over time.

Three consequences that show up in practice:

1. **Artifacts cluster where sources overlap most.** A big belted chorus leaves
   more residue in the instrumental than a sparse verse. For covers this works
   in your favour — your new vocal sits directly on top of the messiest spots
   and masks them. A stem that sounds watery soloed often sounds fine under a
   performance.

2. **Reverb follows whoever generated it.** Vocal reverb usually leaves the mix
   with the vocal, so a stripped instrumental can sound drier and smaller than
   the record. Adding a touch of reverb back in your DAW closes most of that gap.

3. **Live recordings separate worse than studio ones**, because mic bleed means
   the sources were never truly independent to begin with.

---

## Configuration

Everything lives in `config.yaml`. The knobs worth knowing:

| Setting | What it does |
|---|---|
| `model` | `htdemucs_ft` is best quality (~4x slower — it runs four specialised sub-models). `htdemucs` is the sensible default. |
| `stem_mode` | `four` gives vocals/drums/bass/other and lets you rebuild the backing track however you like. `two` is faster but one-shot. |
| `shifts` | Runs separation N times at small time offsets and averages. Real quality gain, N times the cost. `0` off, `2` for a track you care about. |
| `segment_seconds` | Lower this if your GPU runs out of memory. |
| `backing_stems` | Which stems get summed. Drop `vocals` for an instrumental. |
| `stem_gains_db` | Per-stem trim before summing. Vocal bleed tends to land in `other`, so a 1–2 dB cut there sometimes helps. |
| `target_lufs` | Loudness target for the output. |

### Why -16 LUFS and not -14

-14 is streaming standard, so a commercial master already sits about there.
But a commercial master already *has a vocal in it*. Yours doesn't yet. Leaving
the backing track a couple dB quieter means your voice has somewhere to sit
without fighting the instrumental.

The normalizer is gain only — no compression, no limiting. If a track can't
reach the target without exceeding the peak ceiling, it takes the quieter
result rather than squashing the dynamics. You'll occasionally see a track land
at -17.7 instead of -16 for exactly this reason. That's the safety behaving
correctly, not a bug.

---

## Recipes

**Guide vocal instead of a clean instrumental** — useful for learning a melody:

```yaml
backing_stems: [drums, bass, other, vocals]
stem_gains_db:
  vocals: -12.0
```

**Drums-only practice track:**

```yaml
backing_stems: [drums]
```

**Faster turnaround for a batch you're auditioning** — pair this with 2-stem
mode when you just want to hear whether a song works before committing:

```yaml
model: htdemucs
stem_mode: two
backing_stems: [no_vocals]
```

**Re-mix without re-separating.** Change `backing_stems` or `stem_gains_db`,
then:

```bash
python run.py reset <track-id> mixed
python run.py run
```

Separation results are cached on disk, so this takes seconds.

---

## Layout

```
covertools/
├── config.yaml          all tunables
├── run.py               entrypoint
├── requirements.txt
└── covertools/
    ├── config.py        typed config + validation
    ├── manifest.py      per-track state, atomic saves, resume
    ├── audio.py         ffmpeg wrappers, dB math, stem summing, null test
    ├── ingest.py        stage 1 - anything in, uniform wav out
    ├── separate.py      stage 2 - demucs subprocess wrapper
    ├── analyze.py       stage 3 - bpm, key, loudness
    ├── mixdown.py       stage 4 - sum, normalize, encode
    ├── pipeline.py      orchestration + resume
    └── cli.py           add / run / status / list / reset
```

Every stage is idempotent and state is saved after each one, so a crash 30
tracks into a batch of 40 costs you one track, not thirty.

---

## Notes on accuracy

Tempo detection commonly reports half or double the real value on sparse
material. Key detection gets confused by songs that modulate, and relative
major/minor pairs share all seven notes so they're genuinely ambiguous — that's
what the confidence score is telling you when it comes back low. Treat both as
a strong first guess to verify by ear, not as ground truth.

## Sourcing audio

The pipeline takes local files. There's an optional URL path that shells out to
`yt-dlp`, which is deliberately not in `requirements.txt` — install it yourself
only if you need it, and only for material you have the right to download.
For commercial releases, buying the file and pointing the pipeline at a local
copy gives you better source quality anyway, since you skip a lossy
re-encode before separation even starts.
