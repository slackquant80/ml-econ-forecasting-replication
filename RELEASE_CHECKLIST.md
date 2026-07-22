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
- [x] Private GitHub repository created and candidate pushed
- [x] GitHub Actions passed
- [x] `renv::restore()` passed in a clean GitHub clone and project-local library
- [x] Independent Python validation of frozen outputs and paper exports passed

## Remaining before public `v1.0.0`

- [ ] Run the complete four-target FULL workflow from the clean clone
- [ ] Compare regenerated results with the archived release
- [ ] Update the paper PDF with the permanent repository/DOI wording
- [ ] Change `VERSION` and `CITATION.cff` from `1.0.0-rc.1` to `1.0.0`
- [ ] Regenerate and verify `SHA256SUMS.txt` and `release_inventory.csv`
- [ ] Remove candidate-only operational documents if they are not intended for public readers (`RELEASE_CHECKLIST.md`, `GITHUB_AND_ZENODO_SETUP.md`)
- [ ] Review repository rendering and links one final time
- [ ] Make repository public
- [ ] Enable the repository in Zenodo
- [ ] Create GitHub `v1.0.0` release
- [ ] Confirm Zenodo archive and record the version DOI
- [ ] Add the DOI to `CITATION.cff` and the manuscript
- [ ] Produce final paper PDF v1.0 and submit to SSRN
