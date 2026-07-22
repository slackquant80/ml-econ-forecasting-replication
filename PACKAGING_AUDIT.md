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
- `renv::status()` was consistent and all project functions loaded;
- GitHub Actions passed the public-file, frozen-output, and inventory checks; and
- the repository inventory is generated with canonical LF hashing for text files so that Windows and Linux checks agree.

## End-to-end clean-clone rerun

The complete four-target FULL forecasting workflow was run from a clean GitHub clone on 2026-07-23 using Windows 11 x64, R 4.6.0, and the restored `renv` environment. The frozen FRED-MD checksum was verified, all four experiments completed, the SSRN research-protocol validator returned `PASS`, and the paper-table export completed.

The rerun directories were used only for release validation and were not committed. The public archival result set remains the frozen human-readable CSV release. No claim is made that stochastic outputs are byte-for-byte identical across operating systems.

## Release state

The source tree is finalized for version `1.0.0`. Remaining publication steps are Zenodo enablement, GitHub release publication, DOI capture, and post-DOI updates to citation metadata and the manuscript.
