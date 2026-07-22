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

## Two verification routes

### Route A — inspect the archived derived outputs

The `results/frozen_runs/` directories contain human-readable CSV outputs from each FULL experiment. `results/paper_exports/` contains the manuscript tables and figure-data exports. Run `python scripts/verify-public-release.py` to check repository structure, CSV readability, and the absence of known local paths.

### Route B — rerun the complete experiment

Run the commands in `README.md`. The workflow creates new timestamped run IDs. Numerical results should agree up to platform- and package-dependent floating-point differences. Stochastic learners use prespecified origin-specific seeds, but exact bitwise identity across operating systems is not guaranteed.

## Reference environment

The frozen runs were produced under:

- R 4.6.0 (Windows 11 x64)
- `glmnet` 5.0
- `randomForest` 4.7-1.2
- `xgboost` 3.2.1.1
- `Boruta` 10.0.0
- `forecast` 9.0.2
- `sandwich` 3.1-2
- `MCS` 0.2.0

Per-run `session_info.txt` files are archived with the frozen CSV outputs.

## Why no `renv.lock` is supplied yet

The internal project did not contain an `renv.lock`. This public package preserves the recorded session and exact direct-package versions rather than presenting an automatically reconstructed lockfile as verified. A lockfile should be generated and tested on the release machine before the GitHub/Zenodo v1.0.0 tag.

## Expected validation status

The internal release validation summary records 139 checks, zero failures, and status `PASS`. The public copy of the detailed validation file is sanitized; the internal original remains unchanged in the private project.

## Packaging limitation

The public package was assembled in an environment without an R runtime. File selection, privacy-path scanning, checksums, and CSV parsing were executed, but the R pipeline was not rerun during packaging. The frozen outputs themselves were produced and validated in the reference R environment above.
