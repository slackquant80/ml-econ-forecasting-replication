###############################################################################
### Compare a Candidate Experiment with the Published Baseline
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

args <- parse_experiment_cli_args()
reference_run_id <- cli_value(args, "reference-run", NULL)
candidate_run_id <- cli_value(args, "candidate-run", NULL)
tolerance <- suppressWarnings(as.numeric(cli_value(args, "tolerance", "1e-8")))

if (!is.finite(tolerance) || tolerance < 0) {
  stop("--tolerance는 0 이상의 유한한 수여야 합니다.")
}

index <- refresh_experiment_index(project_root)
if (nrow(index) < 2L) {
  stop("비교하려면 baseline과 candidate를 포함하여 최소 2개 run이 필요합니다.")
}

published <- read_published_run_pointer(project_root)
if (is.null(reference_run_id)) {
  if (is.null(published)) {
    stop("published baseline이 없습니다. initialize-published-baseline.R을 먼저 실행하십시오.")
  }
  reference_run_id <- published$run_id
}

reference_row <- index[index$run_id == reference_run_id, , drop = FALSE]
if (nrow(reference_row) != 1L) {
  stop("reference run을 하나로 찾을 수 없습니다: ", reference_run_id)
}

if (is.null(candidate_run_id)) {
  candidate_pool <- index[
    index$run_id != reference_run_id &
      index$target_code == reference_row$target_code[[1L]] &
      index$execution_profile == reference_row$execution_profile[[1L]] &
      index$status == "completed" &
      index$validation_status == "passed",
    ,
    drop = FALSE
  ]
  if (nrow(candidate_pool) < 1L) {
    stop("동일 target/profile의 완료된 candidate run을 찾을 수 없습니다.")
  }
  candidate_run_id <- candidate_pool$run_id[[1L]]
}

candidate_row <- index[index$run_id == candidate_run_id, , drop = FALSE]
if (nrow(candidate_row) != 1L) {
  stop("candidate run을 하나로 찾을 수 없습니다: ", candidate_run_id)
}

reference_manifest <- read_experiment_manifest(reference_row$run_directory[[1L]])
candidate_manifest <- read_experiment_manifest(candidate_row$run_directory[[1L]])
reference_project <- readRDS(reference_manifest$result_rds)
candidate_project <- readRDS(candidate_manifest$result_rds)

compare_table <- function(
    table_name,
    reference,
    candidate,
    key_columns,
    numeric_columns,
    exact_columns = character(0),
    tolerance = 1e-8
) {
  if (!is.data.frame(reference) || !is.data.frame(candidate)) {
    return(data.frame(
      table = table_name,
      passed = FALSE,
      key_match = FALSE,
      row_count_reference = if (is.data.frame(reference)) nrow(reference) else NA_integer_,
      row_count_candidate = if (is.data.frame(candidate)) nrow(candidate) else NA_integer_,
      max_absolute_difference = Inf,
      message = "One or both objects are not data.frames.",
      stringsAsFactors = FALSE
    ))
  }

  missing_reference <- setdiff(c(key_columns, numeric_columns, exact_columns), names(reference))
  missing_candidate <- setdiff(c(key_columns, numeric_columns, exact_columns), names(candidate))
  if (length(missing_reference) > 0L || length(missing_candidate) > 0L) {
    return(data.frame(
      table = table_name,
      passed = FALSE,
      key_match = FALSE,
      row_count_reference = nrow(reference),
      row_count_candidate = nrow(candidate),
      max_absolute_difference = Inf,
      message = paste0(
        "Missing columns. reference=", paste(missing_reference, collapse = "|"),
        "; candidate=", paste(missing_candidate, collapse = "|")
      ),
      stringsAsFactors = FALSE
    ))
  }

  order_reference <- do.call(order, reference[key_columns])
  order_candidate <- do.call(order, candidate[key_columns])
  reference <- reference[order_reference, , drop = FALSE]
  candidate <- candidate[order_candidate, , drop = FALSE]
  rownames(reference) <- NULL
  rownames(candidate) <- NULL

  same_rows <- nrow(reference) == nrow(candidate)
  key_match <- same_rows && identical(
    lapply(reference[key_columns], as.character),
    lapply(candidate[key_columns], as.character)
  )

  max_difference <- Inf
  numeric_match <- FALSE
  if (key_match) {
    differences <- vapply(
      numeric_columns,
      function(column) {
        x <- as.numeric(reference[[column]])
        y <- as.numeric(candidate[[column]])
        both_na <- is.na(x) & is.na(y)
        mismatch_na <- xor(is.na(x), is.na(y))
        if (any(mismatch_na)) return(Inf)
        delta <- abs(x - y)
        delta[both_na] <- 0
        if (length(delta) == 0L) 0 else max(delta, na.rm = TRUE)
      },
      FUN.VALUE = numeric(1)
    )
    max_difference <- if (length(differences) == 0L) 0 else max(differences)
    numeric_match <- is.finite(max_difference) && max_difference <= tolerance
  }

  exact_match <- key_match
  if (key_match && length(exact_columns) > 0L) {
    exact_match <- all(vapply(
      exact_columns,
      function(column) {
        identical(as.character(reference[[column]]), as.character(candidate[[column]]))
      },
      FUN.VALUE = logical(1)
    ))
  }

  passed <- key_match && numeric_match && exact_match
  data.frame(
    table = table_name,
    passed = passed,
    key_match = key_match,
    row_count_reference = nrow(reference),
    row_count_candidate = nrow(candidate),
    max_absolute_difference = max_difference,
    message = if (passed) {
      "Keys, selected numeric values, and exact-status columns match."
    } else {
      "Run comparison failed for this table."
    },
    stringsAsFactors = FALSE
  )
}

comparisons <- list(
  compare_table(
    "monthly_forecasts",
    reference_project$forecasts,
    candidate_project$forecasts,
    key_columns = c("model", "horizon", "forecast_number"),
    numeric_columns = c("actual", "prediction", "error", "squared_error", "absolute_error"),
    exact_columns = c("status", "evaluation_included"),
    tolerance = tolerance
  ),
  compare_table(
    "monthly_accuracy",
    reference_project$accuracy,
    candidate_project$accuracy,
    key_columns = c("model", "horizon"),
    numeric_columns = c("n_evaluation", "RMSE", "MAE", "bias", "correlation"),
    tolerance = tolerance
  ),
  compare_table(
    "cumulative_forecasts",
    reference_project$cumulative_backtest_results,
    candidate_project$cumulative_backtest_results,
    key_columns = c("model", "horizon", "forecast_number"),
    numeric_columns = c(
      "actual", "prediction", "actual_level", "forecast_level",
      "level_error", "level_squared_error", "level_absolute_error"
    ),
    exact_columns = c("status", "evaluation_included"),
    tolerance = tolerance
  ),
  compare_table(
    "cumulative_accuracy",
    reference_project$cumulative_accuracy,
    candidate_project$cumulative_accuracy,
    key_columns = c("model", "horizon"),
    numeric_columns = c(
      "n_evaluation", "cumulative_log_RMSE", "cumulative_log_MAE",
      "level_RMSE", "level_MAE", "level_bias"
    ),
    tolerance = tolerance
  ),
  compare_table(
    "forward_forecasts",
    reference_project$forward_forecasts,
    candidate_project$forward_forecasts,
    key_columns = c("model", "horizon"),
    numeric_columns = c(
      "cumulative_log_forecast", "cumulative_percent_forecast",
      "annualized_percent_forecast", "raw_level_forecast"
    ),
    exact_columns = c("status"),
    tolerance = tolerance
  ),
  compare_table(
    "dm_tests",
    reference_project$dm_test_results,
    candidate_project$dm_test_results,
    key_columns = c("track", "horizon", "loss", "model"),
    numeric_columns = c(
      "n_evaluation", "mean_model_loss", "mean_benchmark_loss",
      "mean_loss_difference", "dm_statistic", "dm_p_value",
      "dm_p_value_adjusted"
    ),
    exact_columns = c("dm_test_status", "significant_better", "significant_worse"),
    tolerance = tolerance
  ),
  compare_table(
    "gw_tests",
    reference_project$gw_test_results,
    candidate_project$gw_test_results,
    key_columns = c("track", "horizon", "model"),
    numeric_columns = c(
      "n_evaluation", "mean_model_mae", "mean_benchmark_mae",
      "mean_loss_difference", "gw_statistic", "gw_standard_error",
      "gw_p_value", "gw_p_value_adjusted"
    ),
    exact_columns = c("gw_test_status", "significant_worse_than_best"),
    tolerance = tolerance
  ),
  compare_table(
    "model_confidence_set",
    reference_project$model_confidence_set,
    candidate_project$model_confidence_set,
    key_columns = c("track", "horizon", "loss", "model"),
    numeric_columns = c(
      "n_evaluation", "mean_loss", "loss_rank", "mcs_stage_p_value",
      "mcs_p_value", "bootstrap_samples", "block_length"
    ),
    exact_columns = c("in_mcs", "mcs_status"),
    tolerance = tolerance
  )
)

comparison_summary <- do.call(rbind, comparisons)
comparison_summary$reference_run_id <- reference_run_id
comparison_summary$candidate_run_id <- candidate_run_id
comparison_summary$tolerance <- tolerance

output_path <- file.path(
  candidate_row$run_directory[[1L]],
  "baseline_reproducibility_comparison.csv"
)
utils::write.csv(comparison_summary, output_path, row.names = FALSE, na = "")

print(comparison_summary, row.names = FALSE)
if (any(!comparison_summary$passed)) {
  stop("Candidate run does not reproduce the published baseline within tolerance.")
}

cat("\nPASS: Candidate run reproduces the published baseline.\n")
cat("Reference: ", reference_run_id, "\n", sep = "")
cat("Candidate: ", candidate_run_id, "\n", sep = "")
cat("Comparison file: ", output_path, "\n", sep = "")
