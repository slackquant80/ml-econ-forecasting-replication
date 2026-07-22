# Source data

The source CSV is intentionally not redistributed in this repository because FRED-MD combines series from multiple providers whose reuse conditions can differ.

The paper used the **June 2026 FRED-MD monthly vintage**, downloaded from the official Federal Reserve Bank of St. Louis FRED-MD page and frozen in July 2026.

- Official index: https://www.stlouisfed.org/research/economists/mccracken/fred-databases
- Direct vintage file used by the study: https://www.stlouisfed.org/-/media/project/frbstl/stlouisfed/research/fred-md/monthly/2026-06-md.csv
- Required local filename: `data/current.csv`
- Expected MD5: `8591dd9f169f7aeb45b7c91782fbd947`

Run:

```r
Rscript scripts/prepare-fred-md-data.R
```

The script downloads the dated vintage, verifies the checksum, and refuses to continue if the file differs.
