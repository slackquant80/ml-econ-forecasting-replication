# ML Economic Forecasting — replication materials

This repository contains the code, frozen derived outputs, validation records, and manuscript associated with **“Beyond Average Accuracy: Statistical Distinguishability and Temporal Concentration in Data-Rich Macroeconomic Forecasting.”**

The study compares nine individual forecasting models and three forecast combinations for CPI inflation, PCE inflation, industrial production growth, and changes in unemployment at 1-, 3-, 6-, and 12-month horizons. It separates average accuracy from statistical distinguishability and temporal durability.

## Release status

This is a **public-release candidate (v0.1.1)** prepared from the validated internal research release. The internal validation recorded 139 checks, zero failures, and status `PASS`. Local absolute paths and development-only artifacts have been removed from this public copy.

The reference dependency environment is now recorded in `renv.lock`. A release-machine validation confirmed R 4.6.0, the seven direct package versions reported in the paper, a consistent `renv` project state, and successful loading of all project functions. A clean-clone `renv::restore()` test and the final end-to-end release run remain pre-release gates.

## Repository contents

- `functions/`, `main.R`, `config.R`: forecasting and statistical-validation engine
- `scripts/`: environment validation, data preparation, FULL experiment workflow, release validation, and paper exports
- `results/frozen_runs/`: CSV outputs from the four paper-core FULL runs (RDS objects and logs excluded)
- `results/paper_exports/`: tables and figure-data exports used by the manuscript
- `results/release/`: sanitized release manifest and validation records
- `paper/`: working-paper PDF
- `data/`: instructions and checksum only; source data are not redistributed

## Quick verification

Repository structure, CSV readability, and privacy-path checks:

```bash
python scripts/verify-public-release.py
```

Reference R environment and function-loading check:

```bash
Rscript scripts/validate-r-environment.R
```

## Restore the R environment

Use R 4.6.0. On Windows, install Rtools45 when source compilation is required. From the repository root:

```r
install.packages("renv", repos = "https://cloud.r-project.org")
renv::restore()
renv::status()
```

The lockfile records direct and transitive package dependencies. See `REPRODUCIBILITY.md` for the reference environment and validation scope.

## Full reproduction

1. Restore and validate the dependency environment:
   ```bash
   Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"
   Rscript -e "renv::restore()"
   Rscript scripts/validate-r-environment.R
   ```
2. Download and checksum the frozen FRED-MD vintage:
   ```bash
   Rscript scripts/prepare-fred-md-data.R
   ```
3. Run the four FULL experiments, validate, publish pointers, and freeze:
   ```bash
   Rscript scripts/run-ssrn-core-experiments.R --publish=true --freeze=true --seed=20260716
   ```
4. Export paper tables and figure data:
   ```bash
   Rscript scripts/export-ssrn-paper-tables.R
   ```

FULL reproduction is computationally intensive. XGBoost uses one thread per fit; MCS uses 5,000 bootstrap replications per target–horizon–loss panel. See `REPRODUCIBILITY.md`.

## Data

The study used the dated June 2026 FRED-MD monthly vintage, with MD5:

`8591dd9f169f7aeb45b7c91782fbd947`

The source CSV is not redistributed. See `DATA_AVAILABILITY.md` and `data/README.md`.

## Interactive dashboard

https://slackquant.shinyapps.io/ml_econ_forecasting/

## License and citation

Code is released under the MIT License. The manuscript and third-party source data are excluded from that license. Citation metadata are provided in `CITATION.cff`.
