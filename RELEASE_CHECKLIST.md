# Public release checklist

## Completed

- [x] Development backup separated from public package
- [x] PREVIEW/QUICK runs excluded
- [x] `.RData`, `.Rhistory`, logs, deployment metadata, and raw source data excluded
- [x] Dashboard UI and deployment source excluded
- [x] Internal path-bearing validation original left untouched
- [x] Sanitized public validation copy created
- [x] Four FULL-run CSV output sets included
- [x] Paper table and figure-data exports included
- [x] Manuscript PDF included
- [x] Known local paths, employer strings, and credential patterns scanned
- [x] CSV files parsed successfully
- [x] R version, direct-package, syntax, and function-loading checks passed
- [x] `renv.lock` generated and project consistency confirmed
- [x] GitHub repository created and made public
- [x] GitHub Actions passed
- [x] `renv::restore()` passed in a clean GitHub clone and project-local library
- [x] Independent Python validation of frozen outputs and paper exports passed
- [x] Complete four-target FULL workflow run from the clean clone
- [x] Frozen FRED-MD checksum verified in the clean clone
- [x] SSRN research-protocol validation returned `PASS` after the rerun
- [x] Paper tables and figure data exported after the rerun
- [x] `VERSION` and `CITATION.cff` promoted to `1.0.0`

## Remaining for permanent archiving and SSRN

- [ ] Regenerate and verify `SHA256SUMS.txt` and `release_inventory.csv` after this final metadata update
- [ ] Confirm the final GitHub Actions run is green
- [ ] Enable the public repository in Zenodo before publishing the GitHub release
- [ ] Create GitHub release and tag `v1.0.0`
- [ ] Confirm Zenodo archive and record the version DOI and concept DOI
- [ ] Add the DOI to `CITATION.cff`, README, and the manuscript
- [ ] Produce final paper PDF v1.0 with permanent repository wording
- [ ] Submit the final PDF and metadata to SSRN
