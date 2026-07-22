#!/usr/bin/env python3
"""Generate or verify deterministic cross-platform SHA-256 inventories.

Text files are hashed after canonical newline normalization (CRLF/CR -> LF),
while binary files are hashed byte-for-byte. This makes inventories stable
between Windows development worktrees and Linux GitHub Actions checkouts.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SHA_PATH = ROOT / "SHA256SUMS.txt"
CSV_PATH = ROOT / "release_inventory.csv"
EXCLUDED_FILES = {SHA_PATH, CSV_PATH}
LOCAL_ONLY_DIRS = {".git", ".Rproj.user", "__pycache__"}
RENV_LOCAL_DIRS = {
    "library", "local", "cellar", "lock", "python", "sandbox", "staging"
}

# Explicitly binary formats. A NUL-byte fallback below catches other binaries.
BINARY_SUFFIXES = {
    ".pdf", ".png", ".jpg", ".jpeg", ".gif", ".webp", ".ico",
    ".zip", ".gz", ".bz2", ".xz", ".7z", ".tar",
    ".rds", ".rdata", ".rda", ".so", ".dll", ".exe",
    ".woff", ".woff2", ".ttf", ".otf",
}


def is_local_only(rel: Path) -> bool:
    if any(part in LOCAL_ONLY_DIRS for part in rel.parts):
        return True
    return (
        len(rel.parts) >= 2
        and rel.parts[0] == "renv"
        and rel.parts[1] in RENV_LOCAL_DIRS
    )


def repository_candidate_files() -> list[Path]:
    """Return tracked plus untracked/non-ignored files, or archive fallback."""
    try:
        proc = subprocess.run(
            [
                "git", "-C", str(ROOT), "ls-files",
                "--cached", "--others", "--exclude-standard", "-z",
            ],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
        if proc.returncode == 0:
            files: list[Path] = []
            for raw in proc.stdout.split(b"\0"):
                if not raw:
                    continue
                rel = Path(raw.decode("utf-8", errors="surrogateescape"))
                path = ROOT / rel
                if path.is_file() and path not in EXCLUDED_FILES:
                    files.append(path)
            return sorted(set(files), key=lambda p: p.relative_to(ROOT).as_posix())
    except (OSError, ValueError):
        pass

    # Fallback for a downloaded source archive without .git metadata.
    files: list[Path] = []
    for path in ROOT.rglob("*"):
        if not path.is_file() or path in EXCLUDED_FILES:
            continue
        rel = path.relative_to(ROOT)
        if is_local_only(rel):
            continue
        files.append(path)
    return sorted(files, key=lambda p: p.relative_to(ROOT).as_posix())


def canonical_bytes(path: Path) -> bytes:
    """Return stable bytes for hashing across Windows and Linux checkouts."""
    data = path.read_bytes()
    if path.suffix.lower() in BINARY_SUFFIXES or b"\x00" in data:
        return data
    # Git stores normal text blobs with LF. Normalize a Windows worktree to the
    # same representation before hashing so CI and local checks agree.
    return data.replace(b"\r\n", b"\n").replace(b"\r", b"\n")


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def render() -> tuple[str, str, int]:
    files = repository_candidate_files()
    rows: list[tuple[str, int, str]] = []
    for path in files:
        rel = path.relative_to(ROOT).as_posix()
        data = canonical_bytes(path)
        rows.append((rel, len(data), digest(data)))

    sha_text = "".join(f"{sha}  {rel}\n" for rel, _, sha in rows)

    buffer = io.StringIO(newline="")
    writer = csv.writer(buffer, lineterminator="\n")
    writer.writerow(["path", "size_bytes", "sha256"])
    writer.writerows(rows)
    return sha_text, buffer.getvalue(), len(rows)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail if the committed inventory files are not current",
    )
    args = parser.parse_args()

    sha_text, csv_text, count = render()
    if args.check:
        errors: list[str] = []
        if not SHA_PATH.is_file() or SHA_PATH.read_text(encoding="utf-8") != sha_text:
            errors.append("SHA256SUMS.txt is missing or stale")
        if not CSV_PATH.is_file() or CSV_PATH.read_text(encoding="utf-8-sig") != csv_text:
            errors.append("release_inventory.csv is missing or stale")
        if errors:
            print("FAIL")
            for error in errors:
                print("-", error)
            print("Run: python scripts/generate-release-inventory.py")
            return 1
        print(
            f"PASS: release inventories are current for {count} "
            "repository-candidate files (canonical LF hashing for text files)."
        )
        return 0

    SHA_PATH.write_text(sha_text, encoding="utf-8", newline="\n")
    CSV_PATH.write_text(csv_text, encoding="utf-8", newline="\n")
    print(
        f"WROTE: {SHA_PATH.name}, {CSV_PATH.name} "
        f"({count} repository-candidate files; canonical LF hashing for text files)"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
