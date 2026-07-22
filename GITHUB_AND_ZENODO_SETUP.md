# GitHub release and Zenodo archiving procedure

## Current state

- Repository: public
- Source-tree version: `1.0.0`
- Clean-clone environment validation: `PASS`
- Clean-clone four-target FULL rerun: completed
- GitHub release: not yet published
- Zenodo DOI: not yet minted

## Release order

1. Commit the final `1.0.0` metadata and regenerate the repository inventories.
2. Confirm all GitHub Actions checks are green.
3. Sign in to Zenodo with the GitHub account that owns the repository.
4. Open the Zenodo GitHub integration, synchronize repositories, and enable `slackquant80/ml-econ-forecasting-replication`.
5. Only after the repository is enabled in Zenodo, publish the GitHub release with tag `v1.0.0`.
6. Wait for Zenodo to archive the release and mint the version DOI and concept DOI.
7. Record the DOI in `CITATION.cff`, README, and the final manuscript. Do not move or rewrite the published `v1.0.0` tag.

## GitHub release settings

- Tag: `v1.0.0`
- Target: `main`
- Title: `ML Economic Forecasting Replication Package v1.0.0`
- Pre-release: no
- Latest release: yes

Use the prepared release notes supplied with the final promotion patch.

## Post-DOI update

After Zenodo provides the DOI, update the default branch with the DOI and the final paper PDF. The already published `v1.0.0` tag remains immutable and continues to identify the exact software/archive version used to mint the DOI.
