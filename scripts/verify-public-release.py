#!/usr/bin/env python3
from pathlib import Path
import csv
import hashlib
import re
import sys

root = Path(__file__).resolve().parents[1]
errors = []

required = [
    "README.md",
    "REPRODUCIBILITY.md",
    "DATA_AVAILABILITY.md",
    "CITATION.cff",
    "LICENSE",
    "paper/Beyond_Average_Accuracy_Preprint_v0.10.5.pdf",
    "results/release/public_release_manifest.csv",
    "results/release/ssrn_release_validation_summary.csv",
]

for rel in required:
    if not (root / rel).is_file():
        errors.append(f"missing required file: {rel}")

# The generic Windows-drive pattern is skipped only for renv's generated
# bootstrap file. renv/activate.R itself contains a regular-expression literal
# matching Windows paths (for example, [A-Za-z]:\\\\), which is not a leaked
# local path. All other privacy patterns are still applied to that file.
privacy_patterns = [
    ("windows_absolute_path", re.compile(r"[A-Za-z]:\\\\")),
    ("macos_user_path", re.compile(r"/Users/")),
    ("linux_home_path", re.compile(r"/home/[^/]+/")),
    ("onedrive_path", re.compile(r"OneDrive", re.I)),
    ("employer_name", re.compile("흥국자산운용")),
    ("internal_project_marker", re.compile(r"## SK_Main")),
]

binary_suffixes = {".pdf", ".png", ".jpg", ".jpeg", ".zip"}
renv_activate = Path("renv/activate.R")

for p in root.rglob("*"):
    if (
        not p.is_file()
        or ".git" in p.parts
        or p == Path(__file__).resolve()
        or p.suffix.lower() in binary_suffixes
    ):
        continue

    try:
        text = p.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        continue

    rel = p.relative_to(root)
    for name, pattern in privacy_patterns:
        if rel == renv_activate and name == "windows_absolute_path":
            continue
        if pattern.search(text):
            errors.append(
                f"forbidden local/private path token in {rel}: {pattern.pattern}"
            )

for p in root.rglob("*.csv"):
    try:
        with p.open(encoding="utf-8-sig", newline="") as f:
            list(csv.reader(f))
    except Exception as exc:
        errors.append(f"invalid CSV {p.relative_to(root)}: {exc}")

if (root / "data/current.csv").exists():
    md5 = hashlib.md5((root / "data/current.csv").read_bytes()).hexdigest()
    if md5 != "8591dd9f169f7aeb45b7c91782fbd947":
        errors.append(f"data/current.csv MD5 mismatch: {md5}")

if errors:
    print("FAIL")
    for error in errors:
        print("-", error)
    sys.exit(1)

print("PASS: public release structure, CSV readability, and privacy-path checks.")
