# GitHub and Zenodo release steps (candidate-stage operations)

The repository currently exists as a private GitHub repository:

`https://github.com/slackquant80/ml-econ-forecasting-replication`

## Before changing visibility

1. Complete the four-target FULL rerun from the validated clean clone.
2. Compare the regenerated outputs with the archived release.
3. Update the manuscript's Data and Code Availability section.
4. Change `VERSION` and `CITATION.cff` to `1.0.0`.
5. Run:
   ```bash
   python scripts/verify-public-release.py
   python scripts/validate-frozen-release.py
   python scripts/generate-release-inventory.py
   python scripts/generate-release-inventory.py --check
   ```
6. Commit and push the final candidate; confirm GitHub Actions is green.
7. Remove this candidate-stage operations file and `RELEASE_CHECKLIST.md` if they are not intended for public readers, then regenerate the inventories once more.

## Public release and Zenodo

1. Change repository visibility to public in GitHub Settings.
2. Sign in to Zenodo and enable the GitHub repository in the Zenodo integration.
3. Create an annotated GitHub tag and release named `v1.0.0`.
4. Wait for Zenodo to archive the release and mint the version DOI.
5. Confirm the Zenodo record metadata and archived files.
6. Add the DOI to `CITATION.cff` and the final paper PDF.
7. If the DOI must appear in the PDF before the GitHub release, use Zenodo's DOI reservation workflow instead of publishing the record prematurely.

Keep `CITATION.cff` as the single repository metadata source unless a deliberate `.zenodo.json` workflow is adopted. If both are present, Zenodo gives `.zenodo.json` precedence for GitHub release archiving.
