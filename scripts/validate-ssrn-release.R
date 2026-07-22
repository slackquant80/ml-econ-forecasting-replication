###############################################################################
### Validate the Published FULL Results for the SSRN Working Paper
###############################################################################

source("scripts/experiment-script-utils.R")
project_root <- resolve_experiment_project_root()
source(file.path(project_root, "functions", "core", "func-utils.R"))
source(file.path(project_root, "functions", "core", "func-target-processing.R"))
source(file.path(project_root, "functions", "registry", "target-registry.R"))
source(file.path(project_root, "functions", "experiments", "func-run-manifest.R"))
source(file.path(project_root, "functions", "experiments", "func-ssrn-release.R"))

validation <- validate_ssrn_release(project_root = project_root)
print(validation$summary, row.names = FALSE)

if (!isTRUE(validation$passed)) {
  failed <- validation$validation[!validation$validation$passed, , drop = FALSE]
  print(failed, row.names = FALSE)
  stop("SSRN release validation failed.")
}

cat("PASS: Published FULL results satisfy the SSRN research protocol.\n")
