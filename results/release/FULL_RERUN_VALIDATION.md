# Clean-clone end-to-end FULL rerun validation

- Validation date: 2026-07-23
- Platform: Windows 11 x64
- R: 4.6.0
- Dependency restoration: project-local `renv` library
- Data vintage: June 2026 FRED-MD, frozen in July 2026
- Expected MD5: `8591dd9f169f7aeb45b7c91782fbd947`
- Targets: `CPIAUCSL`, `PCEPI`, `INDPRO`, `UNRATE`
- Base seed: `20260716`

## Commands used

```bash
Rscript scripts/prepare-fred-md-data.R
Rscript scripts/run-ssrn-core-experiments.R --publish=true --freeze=true --seed=20260716
Rscript scripts/validate-ssrn-release.R
Rscript scripts/export-ssrn-paper-tables.R
```

## Observed status

- Frozen source-data checksum verified: `PASS`
- Complete four-target FULL workflow: completed
- SSRN research-protocol validation: `PASS`
- Paper tables and figure-data export: completed

## Scope

The generated run directories were validation-only and were intentionally not committed. The repository retains the paper's frozen human-readable CSV release as the archival result set. The public package excludes private development RDS objects and operational logs, so this record does not assert byte-for-byte identity of every stochastic object across platforms. Reproducibility is supported by the frozen-output validator, the locked R environment, successful clean-clone restoration, and successful completion of the full research workflow under the reference environment.
