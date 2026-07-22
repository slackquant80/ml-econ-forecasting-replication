# Packaging audit

- Frozen FULL runs: 4
- Frozen forecast rows: 17,280
- Primary DM comparisons: 352 ({'AE': 176, 'SE': 176})
- Holm-adjusted significant-better counts: {'AE': 5, 'SE': 0}
- MCS panels: 32
- MCS survivor distribution: {'10': 2, '11': 5, '12': 25}
- Mean 12-month winner-set switch rate: 29.137%
- Mean top-12 positive SE contribution share: 85.600%
- Mean top-12 SE deterioration share: 89.124%

Static package validation passed: required files, CSV readability, data checksum rule, and known local/private path patterns were checked.

The R forecasting pipeline was not rerun in the packaging environment because R was unavailable. See `REPRODUCIBILITY.md` for the reference runtime and release-machine checks still required.
