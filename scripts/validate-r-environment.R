#!/usr/bin/env Rscript

# Validate the reference R environment without downloading data or fitting models.
# Run from the repository root:
#   Rscript scripts/validate-r-environment.R
# or in RStudio:
#   source("scripts/validate-r-environment.R")

options(stringsAsFactors = FALSE)

fail <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

root_markers <- c("README.md", "config.R", "functions/source-all.R", "renv.lock")
missing_markers <- root_markers[!file.exists(file.path(project_root, root_markers))]
if (length(missing_markers) > 0L) {
  fail(
    "Run this script from the repository root. Missing: %s",
    paste(missing_markers, collapse = ", ")
  )
}

if (!requireNamespace("renv", quietly = TRUE)) {
  fail("Package 'renv' is required. Install it with install.packages('renv').")
}

# Ensure the clean clone is actually using its project-specific renv library.
# This prevents globally installed packages from producing a false PASS.
renv::activate(project = project_root)
active_project <- normalizePath(
  renv::project(),
  winslash = "/",
  mustWork = TRUE
)
if (!identical(tolower(active_project), tolower(project_root))) {
  fail(
    "The active renv project is %s, but the repository root is %s.",
    active_project,
    project_root
  )
}

project_library <- normalizePath(
  renv::paths$library(project = project_root),
  winslash = "/",
  mustWork = FALSE
)
active_library <- normalizePath(
  .libPaths()[1L],
  winslash = "/",
  mustWork = FALSE
)
if (!identical(tolower(active_library), tolower(project_library))) {
  fail(
    paste0(
      "The renv project library is not first in .libPaths(). ",
      "Run source('renv/activate.R') and renv::restore(prompt = FALSE). ",
      "Expected %s; detected %s."
    ),
    project_library,
    active_library
  )
}

reference_r <- package_version("4.6.0")
current_r <- getRversion()
cat(sprintf("R version: %s\n", current_r))
cat(sprintf("Active renv project: %s\n", active_project))
cat(sprintf("Project library: %s\n", project_library))
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
  pkg_path <- find.package(
    pkg,
    lib.loc = project_library,
    quiet = TRUE
  )
  if (!nzchar(pkg_path)) {
    fail(
      paste0(
        "Required package is not installed in the project library: %s. ",
        "Run renv::restore(prompt = FALSE)."
      ),
      pkg
    )
  }

  # packageVersion() canonicalizes hyphens as dots (for example,
  # 4.7-1.2 becomes 4.7.1.2). Read the DESCRIPTION field so the
  # displayed and checked version matches the package's published version.
  detected <- unname(utils::packageDescription(
    pkg,
    lib.loc = project_library,
    fields = "Version"
  ))
  expected <- required_versions[[pkg]]

  if (is.na(detected) || !identical(detected, expected)) {
    fail("Package %s: expected %s, detected %s.", pkg, expected, detected)
  }
  cat(sprintf("PASS package: %-14s %s\n", pkg, detected))
}

cat("Checking renv project status...\n")
renv::status(project = project_root)

cat("Loading project configuration and functions...\n")
assign("project_root", project_root, envir = .GlobalEnv)
source(file.path(project_root, "config.R"), local = .GlobalEnv)
source(file.path(project_root, "functions", "source-all.R"), local = .GlobalEnv)

cat(sprintf("Locale: %s\n", Sys.getlocale()))
cat(sprintf("Time zone: %s\n", Sys.timezone()))
cat("PASS: clean renv project library, reference R and package versions, renv status, and project function loading.\n")
