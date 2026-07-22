
#!/usr/bin/env python3
from pathlib import Path
import csv, hashlib, re, sys
root = Path(__file__).resolve().parents[1]
errors=[]
required=["README.md","REPRODUCIBILITY.md","DATA_AVAILABILITY.md","CITATION.cff","LICENSE",
          "paper/Beyond_Average_Accuracy_Preprint_v0.10.5.pdf",
          "results/release/public_release_manifest.csv","results/release/ssrn_release_validation_summary.csv"]
for rel in required:
    if not (root/rel).is_file(): errors.append(f"missing required file: {rel}")
forbidden=[re.compile(r"[A-Za-z]:\\\\"),re.compile(r"/Users/"),re.compile(r"/home/[^/]+/"),
           re.compile(r"OneDrive",re.I),re.compile("흥국자산운용"),re.compile(r"## SK_Main")]
for p in root.rglob("*"):
    if not p.is_file() or ".git" in p.parts or p == Path(__file__).resolve() or p.suffix.lower() in {".pdf",".png",".jpg",".jpeg",".zip"}: continue
    try: t=p.read_text(encoding="utf-8",errors="ignore")
    except Exception: continue
    for pat in forbidden:
        if pat.search(t): errors.append(f"forbidden local/private path token in {p.relative_to(root)}: {pat.pattern}")
for p in root.rglob("*.csv"):
    try:
        with p.open(encoding="utf-8-sig",newline="") as f: list(csv.reader(f))
    except Exception as e: errors.append(f"invalid CSV {p.relative_to(root)}: {e}")
if (root/"data/current.csv").exists():
    md5=hashlib.md5((root/"data/current.csv").read_bytes()).hexdigest()
    if md5!="8591dd9f169f7aeb45b7c91782fbd947": errors.append(f"data/current.csv MD5 mismatch: {md5}")
if errors:
    print("FAIL")
    for e in errors: print("-",e)
    sys.exit(1)
print("PASS: public release structure, CSV readability, and privacy-path checks.")
