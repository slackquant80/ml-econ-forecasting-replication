# Packaging audit

## Frozen research outputs

- Frozen FULL runs: 4
- Frozen forecast rows: 17,280
- Primary DM comparisons: 352 (176 AE; 176 SE)
- Holm-adjusted significant-better counts: 5 AE; 0 SE
- MCS panels: 32
- MCS survivor distribution: 10 models in 2 panels; 11 in 5; 12 in 25
- Mean 12-month winner-set switch rate: 29.137%
- Mean top-12 positive SE contribution share: 85.600%
- Mean top-12 SE deterioration share: 89.124%

## Public-package checks completed

- required-file and CSV-readability checks passed;
- known local/private paths, credential patterns, source data, RDS objects, logs, and deployment metadata were screened;
- the source-data checksum rule was verified;
- the sanitized release validation contains 139 checks, zero failures, and status `PASS`;
- the archived forecast, DM, MCS, turnover, and loss-concentration exports passed the independent Python validator;
- R 4.6.0 and all seven direct package versions matched the paper;
- `renv` restoration succeeded in a clean GitHub clone using a project-local library;
- `renv::status()` was consistent and all project functions loaded; and
- GitHub Actions passed after the clean-clone validation updates.

## Remaining release gate

The complete four-target FULL forecasting pipeline has not yet been rerun from the clean clone. That computational rerun, comparison with the frozen outputs, final inventory regeneration, public visibility change, GitHub `v1.0.0` release, and Zenodo archiving remain pending.
