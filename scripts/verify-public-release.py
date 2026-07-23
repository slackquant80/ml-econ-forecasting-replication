#!/usr/bin/env python3
from pathlib import Path
import csv
import hashlib
import re
import subprocess
import sys

root = Path(__file__).resolve().parents[1]
errors = []

required = [
    "README.md",
    "REPRODUCIBILITY.md",
    "DATA_AVAILABILITY.md",
    "CITATION.cff",
    "LICENSE",
    "VERSION",
    "renv.lock",
    "renv/activate.R",
    "scripts/validate-r-environment.R",
    "scripts/validate-frozen-release.py",
    "scripts/generate-release-inventory.py",
    "SHA256SUMS.txt",
    "release_inventory.csv",
    "paper/Beyond_Average_Accuracy_Preprint_v1.0.pdf",
    "results/release/public_release_manifest.csv",
    "results/release/ssrn_release_validation_summary.csv",
]

for rel in required:
    if not (root / rel).is_file():
        errors.append(f"missing required file: {rel}")

# Validate only files that belong to the repository candidate. In a Git working
# tree this means tracked files plus untracked, non-ignored files. This avoids
# false positives from local-only directories such as .Rproj.user/ and
# renv/library/, while still catching an accidentally added file before commit.
LOCAL_ONLY_DIRS = {".git", ".Rproj.user", "__pycache__"}
RENV_LOCAL_DIRS = {
    "library", "local", "cellar", "lock", "python", "sandbox", "staging"
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
    try:
        proc = subprocess.run(
            [
                "git", "-C", str(root), "ls-files",
                "--cached", "--others", "--exclude-standard", "-z",
            ],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
        if proc.returncode == 0:
            paths = []
            for raw in proc.stdout.split(b"\0"):
                if not raw:
                    continue
                rel = Path(raw.decode("utf-8", errors="surrogateescape"))
                path = root / rel
                if path.is_file():
                    paths.append(path)
            return sorted(set(paths), key=lambda p: p.relative_to(root).as_posix())
    except (OSError, ValueError):
        pass

    # Fallback for a source archive without .git metadata.
    paths = []
    for path in root.rglob("*"):
        if not path.is_file():
            continue
        rel = path.relative_to(root)
        if is_local_only(rel):
            continue
        paths.append(path)
    return sorted(paths, key=lambda p: p.relative_to(root).as_posix())


candidate_files = repository_candidate_files()
candidate_rel = {p.relative_to(root) for p in candidate_files}

# renv/activate.R contains a regex literal that matches Windows paths. Skip only
# the generic drive-pattern check for this generated bootstrap file; retain all
# other privacy checks.
privacy_patterns = [
    ("windows_absolute_path", re.compile(r"[A-Za-z]:\\\\")),
    ("windows_user_path", re.compile(r"[A-Za-z]:[\\/]+Users[\\/]+[^\\/\s]+", re.I)),
    ("macos_user_path", re.compile(r"/Users/[^/\s]+/")),
    ("linux_home_path", re.compile(r"/home/[^/\s]+/")),
    ("onedrive_path", re.compile(r"OneDrive", re.I)),
    ("employer_name_ko", re.compile("흥국자산운용")),
    ("employer_name_en", re.compile("Heungkuk", re.I)),
    ("internal_project_marker", re.compile(r"## SK_Main")),
]
secret_patterns = [
    ("github_token", re.compile(r"github_pat_[A-Za-z0-9_]{20,}|gh[pousr]_[A-Za-z0-9]{30,}")),
    ("aws_key", re.compile(r"AKIA[0-9A-Z]{16}")),
    ("private_key", re.compile(r"-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----")),
]

binary_suffixes = {".pdf", ".png", ".jpg", ".jpeg", ".zip"}
renv_activate = Path("renv/activate.R")
scanner_files = {Path("scripts/verify-public-release.py")}

for p in candidate_files:
    if p.suffix.lower() in binary_suffixes:
        continue
    try:
        text = p.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        continue

    rel = p.relative_to(root)
    for name, pattern in privacy_patterns:
        if rel in scanner_files:
            continue
        if rel == renv_activate and name in {"windows_absolute_path", "windows_user_path"}:
            continue
        if pattern.search(text):
            errors.append(f"forbidden local/private path token in {rel}: {name}")
    for name, pattern in secret_patterns:
        if rel in scanner_files:
            continue
        if pattern.search(text):
            errors.append(f"possible credential in {rel}: {name}")

for p in candidate_files:
    if p.suffix.lower() != ".csv":
        continue
    try:
        with p.open(encoding="utf-8-sig", newline="") as f:
            list(csv.reader(f))
    except Exception as exc:
        errors.append(f"invalid CSV {p.relative_to(root)}: {exc}")

if (root / "data/current.csv").exists():
    md5 = hashlib.md5((root / "data/current.csv").read_bytes()).hexdigest()
    if md5 != "8591dd9f169f7aeb45b7c91782fbd947":
        errors.append(f"data/current.csv MD5 mismatch: {md5}")

for forbidden in ["rsconnect", ".Rproj.user"]:
    if any(forbidden in rel.parts for rel in candidate_rel):
        errors.append(f"forbidden development directory present in repository candidate: {forbidden}")

for suffix in [".rds", ".rdata", ".rhistory", ".log"]:
    matches = [
        rel for rel in candidate_rel
        if rel.suffix.lower() == suffix
    ]
    if matches:
        errors.append(f"forbidden binary/development files present ({suffix}): {matches[:3]}")

version = (root / "VERSION").read_text(encoding="utf-8").strip() if (root / "VERSION").is_file() else ""
cff = (root / "CITATION.cff").read_text(encoding="utf-8") if (root / "CITATION.cff").is_file() else ""
cff_match = re.search(r"(?m)^version:\s*[\"']?([^\"'\s]+)", cff)
if cff_match and cff_match.group(1) != version:
    errors.append(f"VERSION ({version}) and CITATION.cff version ({cff_match.group(1)}) differ")

if errors:
    print("FAIL")
    for error in errors:
        print("-", error)
    sys.exit(1)

print(
    "PASS: public release structure, CSV readability, privacy, credential, "
    f"and file-policy checks ({len(candidate_files)} repository-candidate files)."
)
