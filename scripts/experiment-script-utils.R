###############################################################################
### Command-Line Utilities for Forecast Experiment Scripts
###############################################################################

resolve_experiment_project_root <- function() {
  configured <- getOption("ml_forecast.project_root", NULL)
  if (
    is.character(configured) && length(configured) == 1L &&
      !is.na(configured) && nzchar(configured)
  ) {
    return(normalizePath(configured, mustWork = TRUE))
  }

  command_args <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command_args, value = TRUE)
  if (length(file_argument) > 0L) {
    script_path <- normalizePath(
      sub("^--file=", "", file_argument[1L]),
      mustWork = FALSE
    )
    candidate <- dirname(dirname(script_path))
    if (file.exists(file.path(candidate, "main.R"))) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }

  current <- normalizePath(getwd(), mustWork = FALSE)
  for (i in seq_len(8L)) {
    if (
      file.exists(file.path(current, "main.R")) &&
        file.exists(file.path(current, "config.R"))
    ) {
      return(normalizePath(current, mustWork = TRUE))
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  stop("프로젝트 최상위 폴더를 찾을 수 없습니다.")
}

parse_experiment_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  output <- list()
  for (arg in args) {
    if (!startsWith(arg, "--")) next
    token <- substring(arg, 3L)
    parts <- strsplit(token, "=", fixed = TRUE)[[1L]]
    key <- parts[1L]
    value <- if (length(parts) >= 2L) {
      paste(parts[-1L], collapse = "=")
    } else {
      "true"
    }
    output[[key]] <- value
  }
  output
}

cli_value <- function(args, name, default = NULL) {
  if (name %in% names(args)) args[[name]] else default
}

cli_flag <- function(args, name, default = FALSE) {
  value <- cli_value(args, name, if (isTRUE(default)) "true" else "false")
  value <- tolower(trimws(as.character(value)))
  if (value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n")) return(FALSE)
  stop("--", name, " 값은 true 또는 false여야 합니다.")
}
