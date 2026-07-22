# ML Economic Forecasting — replication materials

This repository contains the code, frozen derived outputs, validation records, and manuscript associated with **“Beyond Average Accuracy: Statistical Distinguishability and Temporal Concentration in Data-Rich Macroeconomic Forecasting.”**

The study compares nine individual forecasting models and three forecast combinations for CPI inflation, PCE inflation, industrial production growth, and changes in unemployment at 1-, 3-, 6-, and 12-month horizons. It separates average accuracy from statistical distinguishability and temporal durability.

## Release status

This is a **public-release candidate (v0.1.0)** prepared from the validated internal research release. The internal validation recorded 139 checks, zero failures, and status `PASS`. Local absolute paths and development-only artifacts have been removed from this public copy.

## Repository contents

- `functions/`, `main.R`, `config.R`: forecasting and statistical-validation engine
- `scripts/`: data preparation, FULL experiment workflow, release validation, and paper exports
- `results/frozen_runs/`: CSV outputs from the four paper-core FULL runs (RDS objects and logs excluded)
- `results/paper_exports/`: tables and figure-data exports used by the manuscript
- `results/release/`: sanitized release manifest and validation records
- `paper/`: working-paper PDF
- `data/`: instructions and checksum only; source data are not redistributed

## Quick verification

```bash
python scripts/verify-public-release.py
```

## Full reproduction

1. Install R 4.6.0 or a compatible recent R release.
2. Install packages:
   ```bash
   Rscript scripts/install-replication-packages.R
   ```
3. Download and checksum the frozen FRED-MD vintage:
   ```bash
   Rscript scripts/prepare-fred-md-data.R
   ```
4. Run the four FULL experiments, validate, publish pointers, and freeze:
   ```bash
   Rscript scripts/run-ssrn-core-experiments.R --publish=true --freeze=true --seed=20260716
   ```
5. Export paper tables and figure data:
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
