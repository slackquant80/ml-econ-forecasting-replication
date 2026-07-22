#!/usr/bin/env python3
"""Validate the archived CSV release without requiring private RDS objects."""
from __future__ import annotations

import csv
import json
import math
import statistics
import sys
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
EXPECTED_TARGETS = {"CPIAUCSL", "PCEPI", "INDPRO", "UNRATE"}
EXPECTED_HORIZONS = {1, 3, 6, 12}
EXPECTED_MODELS = {
    "RW", "AR(4)", "Ridge", "LASSO", "ElasticNet", "Factor",
    "RandomForest", "XGBoost", "BorutaRF", "Ensemble_Mean",
    "Ensemble_Median", "Ensemble_InvRMSE",
}
EXPECTED_MD5 = "8591dd9f169f7aeb45b7c91782fbd947"
EXPECTED_EVALUATION = {"CPIAUCSL": 88, "PCEPI": 90, "INDPRO": 90, "UNRATE": 88}

errors: list[str] = []


def read_csv(rel: str) -> list[dict[str, str]]:
    path = ROOT / rel
    if not path.is_file():
        errors.append(f"missing CSV: {rel}")
        return []
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def require(condition: bool, message: str) -> None:
    if not condition:
        errors.append(message)


def close(actual: float, expected: float, tol: float = 1e-12) -> bool:
    return math.isclose(actual, expected, rel_tol=tol, abs_tol=tol)


release_rows = read_csv("results/release/ssrn_release_manifest.csv")
require(len(release_rows) == 4, f"release manifest rows: {len(release_rows)} (expected 4)")
require({r.get("target_code") for r in release_rows} == EXPECTED_TARGETS, "release target set mismatch")
require(all(r.get("validation_status") == "passed" for r in release_rows), "release manifest has non-passed target")
require(all(r.get("data_md5") == EXPECTED_MD5 for r in release_rows), "release manifest data MD5 mismatch")
require(all(r.get("horizons") == "1,3,6,12" for r in release_rows), "release horizon specification mismatch")
require(all(r.get("execution_profile") == "full" for r in release_rows), "non-FULL release entry")

for release in release_rows:
    target = release.get("target_code", "")
    result_dir = ROOT / release.get("result_path", "")
    require(result_dir.is_dir(), f"missing frozen result directory for {target}: {result_dir}")
    if not result_dir.is_dir():
        continue

    forecasts = read_csv(str((result_dir / "forecast_results.csv").relative_to(ROOT)))
    require(len(forecasts) == 4320, f"{target}: forecast rows {len(forecasts)} (expected 4320)")
    require({r.get("target_code") for r in forecasts} == {target}, f"{target}: target_code mismatch")
    require({r.get("model") for r in forecasts} == EXPECTED_MODELS, f"{target}: model set mismatch")
    require({int(r["horizon"]) for r in forecasts} == EXPECTED_HORIZONS, f"{target}: horizon set mismatch")
    require(all(r.get("status") == "ok" for r in forecasts), f"{target}: non-ok forecast status")

    raw_counts: Counter[tuple[str, int]] = Counter()
    eval_counts: Counter[tuple[str, int]] = Counter()
    for row in forecasts:
        key = (row["model"], int(row["horizon"]))
        raw_counts[key] += 1
        if row.get("evaluation_included", "").upper() == "TRUE":
            eval_counts[key] += 1
    require(set(raw_counts.values()) == {90}, f"{target}: raw model-horizon panel is not 90 observations per cell")
    require(
        set(eval_counts.values()) == {EXPECTED_EVALUATION[target]},
        f"{target}: evaluation count mismatch; observed {sorted(set(eval_counts.values()))}",
    )
    require(len(eval_counts) == 48, f"{target}: incomplete evaluation model-horizon coverage")

    dm = read_csv(str((result_dir / "dm_test_results.csv").relative_to(ROOT)))
    primary_dm_target = [r for r in dm if r.get("track") == "monthly_transformed"]
    require(len(dm) == 176, f"{target}: all-track DM rows {len(dm)} (expected 176)")
    require(len(primary_dm_target) == 88, f"{target}: primary-track DM rows {len(primary_dm_target)} (expected 88)")
    require({r.get("track") for r in dm} == {"monthly_transformed", "cumulative_level"}, f"{target}: DM track coverage mismatch")
    require({r.get("loss") for r in primary_dm_target} == {"SE", "AE"}, f"{target}: primary DM loss coverage mismatch")
    require({int(r["horizon"]) for r in primary_dm_target} == EXPECTED_HORIZONS, f"{target}: primary DM horizon coverage mismatch")

    mcs = read_csv(str((result_dir / "model_confidence_set_summary.csv").relative_to(ROOT)))
    primary_mcs_target = [r for r in mcs if r.get("track") == "monthly_transformed"]
    require(len(mcs) == 16, f"{target}: all-track MCS summary rows {len(mcs)} (expected 16)")
    require(len(primary_mcs_target) == 8, f"{target}: primary-track MCS rows {len(primary_mcs_target)} (expected 8)")
    require({r.get("track") for r in mcs} == {"monthly_transformed", "cumulative_level"}, f"{target}: MCS track coverage mismatch")
    require(all(r.get("status") == "ok" for r in mcs), f"{target}: non-ok MCS status")
    require(all(r.get("audit_status") == "pass" for r in mcs), f"{target}: MCS audit failure")
    require(all(r.get("bootstrap_samples") == "5000" for r in mcs), f"{target}: MCS bootstrap mismatch")

validation = read_csv("results/release/ssrn_release_validation_sanitized.csv")
summary = read_csv("results/release/ssrn_release_validation_summary.csv")
require(len(validation) == 139, f"sanitized validation rows {len(validation)} (expected 139)")
require(all(r.get("passed", "").upper() == "TRUE" for r in validation), "sanitized validation contains a failure")
require(len(summary) == 1, "release validation summary must contain one row")
if summary:
    s = summary[0]
    require(s.get("current_data_md5") == EXPECTED_MD5, "validation summary data MD5 mismatch")
    require(s.get("n_checks") == "139", "validation summary check count mismatch")
    require(s.get("n_failed") == "0", "validation summary failure count is not zero")
    require(s.get("release_status") == "PASS", "validation summary status is not PASS")

primary_dm = read_csv("results/paper_exports/table_primary_dm.csv")
require(len(primary_dm) == 352, f"paper DM rows {len(primary_dm)} (expected 352)")
require(Counter(r.get("loss") for r in primary_dm) == Counter({"SE": 176, "AE": 176}), "paper DM loss counts mismatch")
sig = Counter((r.get("loss"), r.get("significant_better", "").upper()) for r in primary_dm)
require(sig[("SE", "TRUE")] == 0, "squared-error Holm significant-better count is not zero")
require(sig[("AE", "TRUE")] == 5, "absolute-error Holm significant-better count is not five")
require(all(r.get("p_adjust_method") == "holm" for r in primary_dm), "paper DM adjustment is not uniformly Holm")

mcs_summary = read_csv("results/paper_exports/table_primary_mcs_summary.csv")
require(len(mcs_summary) == 32, f"paper MCS panels {len(mcs_summary)} (expected 32)")
survivors = Counter(r.get("n_survivors") for r in mcs_summary)
require(survivors == Counter({"12": 25, "11": 5, "10": 2}), f"MCS survivor distribution mismatch: {dict(survivors)}")
require(all(r.get("audit_status") == "pass" for r in mcs_summary), "paper MCS audit failure")

turnover = read_csv("results/paper_exports/table_winner_turnover.csv")
require(len(turnover) == 16, f"winner-turnover rows {len(turnover)} (expected 16)")
turn_rates = [int(r["winner_set_switches"]) / (int(r["n_windows"]) - 1) for r in turnover]

concentration = read_csv("results/paper_exports/table_loss_concentration.csv")
se_positive = [
    float(r["top_months_share_of_positive_reduction"])
    for r in concentration
    if r.get("loss") == "SE" and r.get("top_months_share_of_positive_reduction")
]
se_deterioration = [
    float(r["top_months_share_of_deterioration"])
    for r in concentration
    if r.get("loss") == "SE" and r.get("top_months_share_of_deterioration")
]
require(len(se_positive) == 176 and len(se_deterioration) == 176, "SE concentration cell count mismatch")

try:
    audit = json.loads((ROOT / "PACKAGING_AUDIT.json").read_text(encoding="utf-8"))
    require(audit.get("total_forecast_rows") == 17280, "audit total forecast rows mismatch")
    require(audit.get("primary_dm_rows") == 352, "audit DM row count mismatch")
    require(audit.get("mcs_panels") == 32, "audit MCS panel count mismatch")
    require(close(audit.get("mean_12m_winner_switch_rate"), statistics.mean(turn_rates)), "audit mean winner switch rate mismatch")
    require(close(audit.get("mean_top12_positive_share_SE"), statistics.mean(se_positive)), "audit positive concentration mean mismatch")
    require(close(audit.get("mean_top12_deterioration_share_SE"), statistics.mean(se_deterioration)), "audit deterioration concentration mean mismatch")
except Exception as exc:
    errors.append(f"invalid PACKAGING_AUDIT.json: {exc}")

if errors:
    print("FAIL")
    for error in errors:
        print("-", error)
    sys.exit(1)

print("PASS: frozen FULL outputs, paper exports, and sanitized release validation are internally consistent.")
