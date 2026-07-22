###############################################################################
### Freeze the Validated SSRN Working-Paper Research Release
###############################################################################

source("scripts/experiment-script-utils.R")
project_root <- resolve_experiment_project_root()
source(file.path(project_root, "functions", "core", "func-utils.R"))
source(file.path(project_root, "functions", "core", "func-target-processing.R"))
source(file.path(project_root, "functions", "registry", "target-registry.R"))
source(file.path(project_root, "functions", "experiments", "func-run-manifest.R"))
source(file.path(project_root, "functions", "experiments", "func-ssrn-release.R"))

frozen <- freeze_ssrn_release(project_root = project_root)
print(frozen$freeze_summary, row.names = FALSE)
cat("PASS: SSRN research release and FRED-MD data vintage are frozen.\n")
