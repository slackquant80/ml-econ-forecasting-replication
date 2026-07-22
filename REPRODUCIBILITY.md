# Reproducibility protocol

## Fixed research design

- Data: June 2026 FRED-MD monthly vintage, frozen in July 2026
- Data MD5: `8591dd9f169f7aeb45b7c91782fbd947`
- Targets: `CPIAUCSL`, `PCEPI`, `INDPRO`, `UNRATE`
- Horizons: 1, 3, 6, 12 months
- Rolling estimation window: 360 months
- Maximum OOS target months: 90 (December 2018–May 2026)
- Base seed: `20260716`
- Primary inference: DM and MCS
- MCS: `Tmax`, 90% confidence, 5,000 block-bootstrap replications

## Three verification routes

### Route A — inspect and validate the archived derived outputs

The `results/frozen_runs/` directories contain human-readable CSV outputs from each FULL experiment. `results/paper_exports/` contains the manuscript tables and figure-data exports. Run:

```bash
python scripts/verify-public-release.py
python scripts/validate-frozen-release.py
python scripts/generate-release-inventory.py --check
```

These checks cover repository structure, CSV readability, privacy/file policy, the four archived forecast panels, DM and MCS coverage, the sanitized 139-check release validation, key manuscript aggregates, and checksum-inventory freshness.

### Route B — restore the software environment from a clean clone

The repository includes `.Rprofile`, `renv/activate.R`, `renv/settings.json`, and `renv.lock`. From a clean clone:

```r
install.packages("renv", repos = "https://cloud.r-project.org")
renv::restore()
renv::status()
source("scripts/validate-r-environment.R")
```

A clean-clone test completed successfully on Windows 11 with R 4.6.0 and `renv` 1.2.3. The packages were restored into the clone-specific project library, all seven direct package versions matched, the project state was consistent, and all project functions loaded successfully.

### Route C — rerun the complete experiment

Run the commands in `README.md`. The workflow creates new timestamped run IDs. Numerical results should agree up to platform- and package-dependent floating-point differences. Stochastic learners use prespecified origin-specific seeds, but exact bitwise identity across operating systems is not guaranteed.

An end-to-end Route C validation was completed from a clean GitHub clone on 2026-07-23. It verified the frozen data checksum, completed all four target FULL experiments, passed the SSRN research-protocol validator, and completed the paper-table export. The validation-only run directories were not committed.

## Reference environment

The frozen runs and release-machine checks use:

- R 4.6.0 (Windows 11 x64)
- `glmnet` 5.0
- `randomForest` 4.7-1.2
- `xgboost` 3.2.1.1
- `Boruta` 10.0.0
- `forecast` 9.0.2
- `sandwich` 3.1-2
- `MCS` 0.2.0
- `renv` 1.2.3 for dependency restoration

Per-run `session_info.txt` files are archived with the frozen CSV outputs.

## Completed validation

The following checks have passed:

- R version exactly 4.6.0;
- all seven direct package versions matched the recorded versions;
- `renv::snapshot(prompt = FALSE)` reported the lockfile up to date;
- `renv::status()` reported a consistent project state;
- a newly cloned repository restored into its own project-local library;
- `config.R` and `functions/source-all.R` loaded successfully;
- GitHub Actions passed the public file-policy/privacy, frozen-output, and inventory checks;
- the archived CSV release passed the independent frozen-output validator;
- the frozen FRED-MD MD5 was verified in the clean clone;
- the complete four-target FULL workflow completed from the clean clone;
- the SSRN research-protocol validator returned `PASS`; and
- paper tables and figure data were exported successfully from the rerun.

## Interpretation of the full-rerun gate

The public repository intentionally excludes private development RDS objects and operational logs. Consequently, the release claim is not one of cross-platform, byte-for-byte identity of every stochastic object. The evidence is instead layered:

1. the paper's frozen human-readable CSV release is independently validated;
2. the exact R and direct-package environment is restorable from `renv.lock`;
3. all public project functions load successfully; and
4. the full four-target workflow completes from a clean clone and passes the same research-protocol validation.

This scope is appropriate for reproducibility while avoiding redistribution of private development artifacts.

## Expected validation status

The internal release validation summary records 139 checks, zero failures, and status `PASS`. The public detailed validation file is sanitized; the internal path-bearing original remains unchanged in the private development project.

## Scope and limitations

The public package was assembled separately from the development project. File selection, privacy-path scanning, checksums, and CSV parsing were performed on the public copy. The archived outputs are independently checkable without the excluded private RDS objects. A complete rerun requires downloading the frozen FRED-MD vintage and substantial computation time. Exact bitwise identity across operating systems is not guaranteed for stochastic learners, parallel numerical libraries, or bootstrap procedures.
