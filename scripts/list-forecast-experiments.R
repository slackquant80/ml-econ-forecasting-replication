###############################################################################
### List Forecast Experiment Runs
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
source(file.path(project_root, "functions", "experiments", "func-run-manifest.R"))

index <- refresh_experiment_index(project_root)
if (nrow(index) == 0L) {
  cat("No experiment runs are registered.\n")
} else {
  print(index[
    , c(
      "run_id", "target_code", "execution_profile", "status",
      "validation_status", "publication_status", "created_at"
    ),
    drop = FALSE
  ], row.names = FALSE)
}
