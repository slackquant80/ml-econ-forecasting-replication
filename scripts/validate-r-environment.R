#!/usr/bin/env Rscript

# Validate the reference R environment without downloading data or fitting models.
# Run from the repository root:
#   Rscript scripts/validate-r-environment.R

options(stringsAsFactors = FALSE)

fail <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

root_markers <- c("README.md", "config.R", "functions/source-all.R", "renv.lock")
missing_markers <- root_markers[!file.exists(root_markers)]
if (length(missing_markers) > 0L) {
  fail(
    "Run this script from the repository root. Missing: %s",
    paste(missing_markers, collapse = ", ")
  )
}

reference_r <- package_version("4.6.0")
current_r <- getRversion()
cat(sprintf("R version: %s\n", current_r))
if (current_r != reference_r) {
  fail("Reference R version is %s; detected %s.", reference_r, current_r)
}

required_versions <- c(
  glmnet = "5.0",
  randomForest = "4.7-1.2",
  xgboost = "3.2.1.1",
  Boruta = "10.0.0",
  forecast = "9.0.2",
  sandwich = "3.1-2",
  MCS = "0.2.0"
)

for (pkg in names(required_versions)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    fail("Required package is not installed: %s", pkg)
  }

  # packageVersion() canonicalizes hyphens as dots (for example,
  # 4.7-1.2 becomes 4.7.1.2). Read the DESCRIPTION field so the
  # displayed and checked version matches the package's published version.
  detected <- unname(utils::packageDescription(pkg, fields = "Version"))
  expected <- required_versions[[pkg]]

  if (is.na(detected) || !identical(detected, expected)) {
    fail("Package %s: expected %s, detected %s.", pkg, expected, detected)
  }
  cat(sprintf("PASS package: %-14s %s\n", pkg, detected))
}

if (!requireNamespace("renv", quietly = TRUE)) {
  fail("Package 'renv' is required. Install it with install.packages('renv').")
}

cat("Checking renv project status...\n")
renv::status()

cat("Loading project configuration and functions...\n")
source("config.R", local = .GlobalEnv)
source(file.path("functions", "source-all.R"), local = .GlobalEnv)

cat(sprintf("Locale: %s\n", Sys.getlocale()))
cat(sprintf("Time zone: %s\n", Sys.timezone()))
cat("PASS: reference R version, direct package versions, renv status, and project function loading.\n")
