# GitHub and Zenodo publication steps

## Recommended repository

- Name: `ml-econ-forecasting-replication`
- Visibility during final checks: private
- Final visibility: public
- Description: `Replication code and frozen outputs for Beyond Average Accuracy.`

## Publish with GitHub Desktop

1. Extract the release ZIP.
2. In GitHub Desktop, choose **File → Add local repository** and select the extracted folder.
3. Choose **Publish repository**.
4. Use the repository name above and keep it private for the first review.
5. After verifying the rendered README, Actions check, and absence of private files, change visibility to public.

## Publish with GitHub CLI

From the repository directory:

```bash
git init
git add .
git commit -m "Prepare public replication release v0.1.0"
git branch -M main
gh repo create ml-econ-forecasting-replication --private --source=. --remote=origin --push
```

After final review, make the repository public in GitHub settings.

## Release tag

Do not tag `v1.0.0` until the repository URL, final manuscript, and environment lockfile are fixed. The present package is a candidate release:

```bash
git tag -a v0.1.0 -m "Public replication candidate"
git push origin v0.1.0
```

For the permanent paper release, use `v1.0.0` and create a GitHub Release from that tag.

## Zenodo

1. Sign in to Zenodo with GitHub or link the GitHub account.
2. Open the Zenodo GitHub integration, sync repositories, and enable this repository.
3. Create the final GitHub Release (`v1.0.0`).
4. Wait for Zenodo to archive the release and mint a version DOI.
5. Add the version DOI and GitHub URL to the paper's Data and Code Availability section and to `CITATION.cff`.

A DOI may also be reserved in a Zenodo draft before publication when the DOI must appear inside the final PDF. Do not publish the final SSRN PDF until the chosen DOI workflow is complete.
