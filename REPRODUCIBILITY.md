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

The `results/frozen_runs/` directories contain human-readable CSV outputs from each FULL experiment. `results/paper_exports/` contains the manuscript tables and figure-data exports. Run:

```bash
python scripts/verify-public-release.py
```

This checks repository structure, CSV readability, and the absence of known local/private paths.

### Route B — rerun the complete experiment

Restore the dependency environment from `renv.lock`, validate it, and then run the commands in `README.md`. The workflow creates new timestamped run IDs. Numerical results should agree up to platform- and package-dependent floating-point differences. Stochastic learners use prespecified origin-specific seeds, but exact bitwise identity across operating systems is not guaranteed.

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

Per-run `session_info.txt` files are archived with the frozen CSV outputs.

## Dependency restoration with `renv`

The repository includes `.Rprofile`, `renv/activate.R`, `renv/settings.json`, and `renv.lock`. From a clean clone:

```r
install.packages("renv", repos = "https://cloud.r-project.org")
renv::restore()
renv::status()
```

On Windows, Rtools45 may be required if a package must be compiled from source. The lockfile records direct and transitive dependencies. It should be treated as part of the tagged research release.

## Release-machine validation completed

On the reference Windows machine, the following checks passed:

- R version exactly 4.6.0
- all seven direct package versions matched the recorded reference versions
- `renv::snapshot(prompt = FALSE)` reported the lockfile already up to date
- `renv::status()` reported a consistent project state
- `config.R` loaded successfully
- `functions/source-all.R` loaded all project functions successfully

The same checks can be rerun with:

```bash
Rscript scripts/validate-r-environment.R
```

## Validation still required before `v1.0.0`

The current consistency check was performed in the environment used to create the lockfile. Before the public `v1.0.0` tag, the repository must also pass:

1. `renv::restore()` in a clean clone or clean project library;
2. the local R environment/function-loading validation script;
3. the final frozen-output/release validation workflow; and
4. regeneration of the release checksum inventory after all tracked files are final.

## Expected validation status

The internal release validation summary records 139 checks, zero failures, and status `PASS`. The public copy of the detailed validation file is sanitized; the internal original remains unchanged in the private project.

## Packaging and rerun scope

The initial public package was assembled in an environment without an R runtime. File selection, privacy-path scanning, checksums, and CSV parsing were executed there. The dependency environment and R source-loading path have since been validated on the reference Windows machine. The complete four-target FULL pipeline has not yet been rerun from a clean clone; that remains a release gate rather than a completed claim.
