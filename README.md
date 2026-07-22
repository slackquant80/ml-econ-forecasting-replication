# ML Economic Forecasting — replication materials

This repository contains the code, frozen derived outputs, validation records, and manuscript associated with **“Beyond Average Accuracy: Statistical Distinguishability and Temporal Concentration in Data-Rich Macroeconomic Forecasting.”** The study compares nine individual forecasting models and three forecast combinations for CPI inflation, PCE inflation, industrial production growth, and changes in unemployment at 1-, 3-, 6-, and 12-month horizons. It separates average accuracy from statistical distinguishability and temporal durability.

Repository: https://github.com/slackquant80/ml-econ-forecasting-replication

## Release status

This branch contains the finalized source tree for **v1.0.0**. The internal research release recorded 139 checks, zero failures, and status `PASS`. Local absolute paths and development-only artifacts have been removed from the public package.

The dependency environment is recorded in `renv.lock`. Validation on a clean GitHub clone confirmed R 4.6.0, restoration into a project-local `renv` library, the seven direct package versions reported in the paper, a consistent `renv` state, and successful loading of all project functions. The archived CSV release and paper exports are checked independently by Python.

An end-to-end four-target FULL rerun was also completed from the clean clone on the reference Windows environment. The frozen FRED-MD checksum was verified, the complete workflow finished, the SSRN research-protocol validation returned `PASS`, and the paper-table export completed. The rerun directories were validation-only and are not committed; the repository retains the frozen paper release as its archival result set. See `results/release/FULL_RERUN_VALIDATION.md` and `REPRODUCIBILITY.md` for scope and limitations.

## Repository contents

- `functions/`, `main.R`, `config.R`: forecasting and statistical-validation engine
- `scripts/`: environment validation, data preparation, FULL experiment workflow, archived-output validation, release validation, and paper exports
- `results/frozen_runs/`: CSV outputs from the four paper-core FULL runs (RDS objects and logs excluded)
- `results/paper_exports/`: tables and figure-data exports used by the manuscript
- `results/release/`: sanitized release manifest and validation records
- `paper/`: working-paper PDF
- `data/`: instructions and checksum only; source data are not redistributed

The interactive dashboard is linked below, but its Shiny UI and deployment source are **not** included in this replication repository.

## Quick verification

Repository structure, CSV readability, privacy/file-policy checks, archived-output consistency, and checksum-inventory freshness:

```bash
python scripts/verify-public-release.py
python scripts/validate-frozen-release.py
python scripts/generate-release-inventory.py --check
```

Reference R environment and function-loading check:

```bash
Rscript scripts/validate-r-environment.R
```

## Restore the R environment

Use R 4.6.0. On Windows, install Rtools45 when source compilation is required. From a clean clone:

```r
install.packages("renv", repos = "https://cloud.r-project.org")
renv::restore()
renv::status()
source("scripts/validate-r-environment.R")
```

The lockfile records direct and transitive package dependencies. See `REPRODUCIBILITY.md` for the reference environment and validation scope.

## Full reproduction

1. Restore and validate the dependency environment:

   ```bash
   Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"
   Rscript -e "renv::restore(prompt=FALSE)"
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

5. Regenerate and verify the repository inventories after the tracked release files are final:

   ```bash
   python scripts/generate-release-inventory.py
   python scripts/generate-release-inventory.py --check
   ```

FULL reproduction is computationally intensive. XGBoost uses one thread per fit; MCS uses 5,000 bootstrap replications per target–horizon–loss panel. See `REPRODUCIBILITY.md`.

## Data

The study used the dated June 2026 FRED-MD monthly vintage, frozen in July 2026, with MD5:

`8591dd9f169f7aeb45b7c91782fbd947`

The source CSV is not redistributed. See `DATA_AVAILABILITY.md` and `data/README.md`.

## Interactive dashboard

https://slackquant.shinyapps.io/ml_econ_forecasting/

## License and citation

Code is released under the MIT License. The manuscript and third-party source data are excluded from that license. Citation metadata are provided in `CITATION.cff`.

The permanent Zenodo DOI will be added to `CITATION.cff`, this README, and the manuscript after the GitHub `v1.0.0` release is archived.
