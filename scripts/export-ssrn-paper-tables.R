###############################################################################
### Export Paper-Ready Tables and Figure Data from the Frozen SSRN Release
###############################################################################

source("scripts/experiment-script-utils.R")
project_root <- resolve_experiment_project_root()
source(file.path(project_root, "functions", "core", "func-utils.R"))
source(file.path(project_root, "functions", "core", "func-target-processing.R"))
source(file.path(project_root, "functions", "registry", "target-registry.R"))
source(file.path(project_root, "functions", "experiments", "func-run-manifest.R"))
source(file.path(project_root, "functions", "experiments", "func-ssrn-release.R"))
source(file.path(project_root, "functions", "experiments", "func-ssrn-analysis.R"))

args <- parse_experiment_cli_args()
rolling_months <- suppressWarnings(as.integer(cli_value(args, "rolling-months", "12")))
if (is.na(rolling_months) || rolling_months < 2L) {
  stop("--rolling-months must be an integer of at least 2.")
}

exported <- export_ssrn_analysis_tables(
  project_root = project_root,
  rolling_months = rolling_months
)
print(exported$inventory, row.names = FALSE)
cat("PASS: SSRN paper tables and figure data were exported.\n")
