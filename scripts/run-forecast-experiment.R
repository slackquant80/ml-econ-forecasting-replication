###############################################################################
### Run One Multi-Target Forecast Experiment
###############################################################################

resolve_this_script_directory <- function() {
  command_files <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )
  if (length(command_files) > 0L) {
    return(dirname(normalizePath(
      sub("^--file=", "", command_files[1L]),
      mustWork = FALSE
    )))
  }

  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
    },
    FUN.VALUE = character(1)
  )
  source_files <- source_files[!is.na(source_files) & nzchar(source_files)]
  if (length(source_files) > 0L) {
    return(dirname(normalizePath(tail(source_files, 1L), mustWork = FALSE)))
  }

  candidate <- file.path(getwd(), "scripts")
  if (file.exists(file.path(candidate, "experiment-script-utils.R"))) {
    return(normalizePath(candidate, mustWork = TRUE))
  }

  normalizePath(getwd(), mustWork = FALSE)
}

script_directory <- resolve_this_script_directory()
source(file.path(script_directory, "experiment-script-utils.R"))

project_root <- resolve_experiment_project_root()
source(file.path(project_root, "functions", "registry", "target-registry.R"))
source(file.path(project_root, "functions", "experiments", "func-run-manifest.R"))
source(file.path(project_root, "functions", "experiments", "func-experiment-runner.R"))

args <- parse_experiment_cli_args()
target_code <- cli_value(args, "target", "CPIAUCSL")
execution_profile <- tolower(cli_value(args, "profile", "quick"))
publish <- cli_flag(args, "publish", FALSE)
force <- cli_flag(args, "force", FALSE)
run_id <- cli_value(args, "run-id", NULL)
base_seed <- suppressWarnings(as.integer(cli_value(args, "seed", "20260716")))
statistics <- cli_flag(args, "statistics", TRUE)

if (is.na(base_seed) || base_seed < 1L) {
  stop("--seed는 1 이상의 정수여야 합니다.")
}

cat("Starting forecast experiment...\n")
cat("Project root: ", project_root, "\n", sep = "")
cat("Target: ", target_code, "\n", sep = "")
cat("Profile: ", execution_profile, "\n", sep = "")
cat("Publish after validation: ", publish, "\n", sep = "")

result <- run_forecast_experiment(
  project_root = project_root,
  target_code = target_code,
  execution_profile = execution_profile,
  publish = publish,
  run_id = run_id,
  base_seed = base_seed,
  enable_statistical_validation = statistics,
  force = force
)

cat("\nPASS: Forecast experiment completed and validated.\n")
cat("Run ID: ", result$manifest$run_id, "\n", sep = "")
cat("Result directory: ", result$result_directory, "\n", sep = "")
cat("Publication status: ", result$manifest$publication_status, "\n", sep = "")
