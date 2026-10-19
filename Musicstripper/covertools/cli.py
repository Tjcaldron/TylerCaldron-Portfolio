"""Command line interface."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from . import pipeline
from .config import load_config, Config
from .ingest import discover, is_url, title_from_path
from .manifest import Manifest, STAGES


def cmd_add(args, cfg: Config, manifest: Manifest) -> int:
    added = 0
    for item in args.sources:
        if is_url(item):
            title = args.title or item.rsplit("/", 1)[-1]
            manifest.add(item, title)
            added += 1
            continue

        path = Path(item).expanduser()
        if path.is_dir():
            files = discover(path)
            if not files:
                print(f"No audio files found in {path}")
                continue
            for f in files:
                manifest.add(str(f.resolve()), title_from_path(f))
                added += 1
        elif path.exists():
            manifest.add(str(path.resolve()), args.title or title_from_path(path))
            added += 1
        else:
            print(f"Not found: {item}")

    print(f"Added {added} track(s). Run `covertools run` to process.")
    return 0


def cmd_run(args, cfg: Config, manifest: Manifest) -> int:
    result = pipeline.run_all(
        cfg, manifest, only=args.id, verbose=not args.quiet
    )
    print(f"Done. {result['processed']} processed, {result['failed']} failed.")
    return 1 if result["failed"] else 0


def cmd_status(args, cfg: Config, manifest: Manifest) -> int:
    s = manifest.summary()
    print(f"Workspace: {cfg.workspace}")
    print(f"Model:     {cfg.model} ({cfg.stem_mode}-stem)")
    print(f"Target:    {cfg.target_lufs} LUFS, peak ceiling {cfg.peak_ceiling_db} dBFS")
    print(f"Backing:   {' + '.join(cfg.backing_stems)}")
    print()
    print(f"Tracks: {s['total']} total, {s['complete']} complete, {s['errored']} errored")
    for stage in STAGES:
        print(f"  {stage:<12} {s['stages'][stage]}/{s['total']}")
    return 0


def cmd_list(args, cfg: Config, manifest: Manifest) -> int:
    if not manifest.tracks:
        print("No tracks yet.")
        return 0
    if args.json:
        print(json.dumps(
            {t.id: t.__dict__ for t in manifest.tracks.values()}, indent=2, default=str
        ))
        return 0

    for t in manifest.tracks.values():
        state = "done" if t.done else (t.next_stage() or "?")
        mark = "!" if t.error else " "
        meta = []
        if t.bpm:
            meta.append(f"{t.bpm:g}bpm")
        if t.key:
            meta.append(t.key)
        suffix = f"  [{', '.join(meta)}]" if meta else ""
        print(f"{mark} {t.id:<48} {state:<10}{suffix}")
        if t.error and args.verbose:
            print(f"    {t.error}")
    return 0


def cmd_reset(args, cfg: Config, manifest: Manifest) -> int:
    if args.id not in manifest.tracks:
        print(f"Unknown track id: {args.id}")
        return 1
    manifest.reset_stage(args.id, args.stage)
    print(f"Reset {args.id} to re-run from '{args.stage}'.")
    return 0


def cmd_ui(args, cfg: Config, manifest: Manifest) -> int:
    try:
        from .ui import launch
    except ImportError as exc:
        print(f"The UI needs gradio: pip install gradio ({exc})", file=sys.stderr)
        return 1
    launch(cfg, manifest, args.config, args.port)
    return 0


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="covertools",
        description="Turn songs into normalized backing tracks for covers.",
    )
    p.add_argument("-c", "--config", default="config.yaml", help="path to config.yaml")
    sub = p.add_subparsers(dest="command", required=True)

    a = sub.add_parser("add", help="queue a file, folder, or URL")
    a.add_argument("sources", nargs="+")
    a.add_argument("--title", help="override the derived title (single source only)")
    a.set_defaults(func=cmd_add)

    r = sub.add_parser("run", help="process everything pending")
    r.add_argument("--id", nargs="*", help="only process these track ids")
    r.add_argument("-q", "--quiet", action="store_true")
    r.set_defaults(func=cmd_run)

    s = sub.add_parser("status", help="show workspace summary")
    s.set_defaults(func=cmd_status)

    l = sub.add_parser("list", help="list tracks")
    l.add_argument("--json", action="store_true")
    l.add_argument("-v", "--verbose", action="store_true")
    l.set_defaults(func=cmd_list)

    rs = sub.add_parser("reset", help="force a track to re-run from a stage")
    rs.add_argument("id")
    rs.add_argument("stage", choices=STAGES)
    rs.set_defaults(func=cmd_reset)

    u = sub.add_parser("ui", help="open the browser interface")
    u.add_argument("--port", type=int, default=7860)
    u.set_defaults(func=cmd_ui)

    return p


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    try:
        cfg = load_config(args.config)
    except Exception as exc:
        print(f"Config error: {exc}", file=sys.stderr)
        return 1

    cfg.ensure_dirs()
    manifest = Manifest(cfg.manifest_path)
    return args.func(args, cfg, manifest)


if __name__ == "__main__":
    raise SystemExit(main())
