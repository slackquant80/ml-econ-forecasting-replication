###############################################################################
### Rebuild Statistical Results from the Existing Forecast Project
###############################################################################

find_project_root <- function(start_path = getwd()) {
  current <- normalizePath(start_path, mustWork = FALSE)
  for (i in seq_len(8L)) {
    if (
      file.exists(file.path(current, "main.R")) &&
        dir.exists(file.path(current, "results"))
    ) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  normalizePath(start_path, mustWork = FALSE)
}

project_root <- find_project_root()
source(file.path(project_root, "config.R"))
source(file.path(project_root, "functions", "source-all.R"))

results_file <- file.path(
  project_root,
  "results",
  "forecast_project_results.rds"
)

if (!file.exists(results_file)) {
  stop(
    "Existing forecast project RDS was not found: ",
    results_file,
    ". Run source(\"main.R\") first."
  )
}

forecast_project <- readRDS(results_file)
required_components <- c(
  "forecasts",
  "cumulative_backtest_results",
  "configuration"
)
missing_components <- setdiff(required_components, names(forecast_project))
if (length(missing_components) > 0L) {
  stop(
    "Existing forecast project is missing components: ",
    paste(missing_components, collapse = ", ")
  )
}

resolved_horizons <- forecast_project$configuration$forecast_horizons %||%
  forecast_horizons
resolved_horizons <- as.integer(resolved_horizons)

statistical_validation <- run_statistical_validation(
  forecast_results = forecast_project$forecasts,
  cumulative_backtest_results = forecast_project$cumulative_backtest_results,
  forecast_horizons = resolved_horizons,
  loss_functions = statistical_loss_functions,
  benchmark_model = statistical_benchmark_model,
  dm_alternative = dm_alternative,
  dm_varestimator = dm_varestimator,
  dm_p_adjust_method = dm_p_adjust_method,
  gw_alternative = gw_alternative,
  gw_method = gw_method,
  gw_p_adjust_method = gw_p_adjust_method,
  significance_level = statistical_significance_level,
  mcs_alpha = mcs_alpha,
  mcs_bootstrap_samples = mcs_bootstrap_samples,
  mcs_statistic = mcs_statistic,
  mcs_block_length = mcs_block_length,
  mcs_min_block_length = mcs_min_block_length,
  seed = base_seed + 900000L
)

forecast_project$dm_test_results <- statistical_validation$dm_tests
forecast_project$gw_test_results <- statistical_validation$gw_tests
forecast_project$model_confidence_set <- statistical_validation$mcs_models
forecast_project$model_confidence_set_summary <- statistical_validation$mcs_summary
forecast_project$mcs_audit <- statistical_validation$mcs_audit
forecast_project$mcs_audit_summary <- statistical_validation$mcs_audit_summary

forecast_project$configuration$enable_statistical_validation <- TRUE
forecast_project$configuration$primary_evaluation_track <- primary_evaluation_track
forecast_project$configuration$secondary_evaluation_tracks <- secondary_evaluation_tracks
forecast_project$configuration$primary_inference_methods <- primary_inference_methods
forecast_project$configuration$supplementary_inference_methods <- supplementary_inference_methods
forecast_project$configuration$statistical_benchmark_model <- statistical_benchmark_model
forecast_project$configuration$statistical_loss_functions <- statistical_loss_functions
forecast_project$configuration$statistical_significance_level <- statistical_significance_level
forecast_project$configuration$dm_alternative <- dm_alternative
forecast_project$configuration$dm_varestimator <- dm_varestimator
forecast_project$configuration$dm_p_adjust_method <- dm_p_adjust_method
forecast_project$configuration$gw_alternative <- gw_alternative
forecast_project$configuration$gw_method <- gw_method
forecast_project$configuration$gw_p_adjust_method <- gw_p_adjust_method
forecast_project$configuration$gw_reference_rule <- gw_reference_rule
forecast_project$configuration$gw_comparison_label <- gw_comparison_label
forecast_project$configuration$gw_inference_role <- gw_inference_role
forecast_project$configuration$gw_formal_giacomini_white_test <- gw_formal_giacomini_white_test
forecast_project$configuration$gw_loss_function <- "AE"
forecast_project$configuration$gw_post_selection_comparison <- TRUE
forecast_project$configuration$mcs_alpha <- mcs_alpha
forecast_project$configuration$mcs_bootstrap_samples <- mcs_bootstrap_samples
forecast_project$configuration$mcs_statistic <- mcs_statistic
forecast_project$configuration$mcs_block_length <- mcs_block_length
forecast_project$configuration$mcs_min_block_length <- mcs_min_block_length

if (
  is.data.frame(forecast_project$processing_summary) &&
    nrow(forecast_project$processing_summary) > 0L
) {
  forecast_project$processing_summary$statistical_validation_enabled <- TRUE
  forecast_project$processing_summary$dm_test_rows <- nrow(
    forecast_project$dm_test_results
  )
  forecast_project$processing_summary$gw_test_rows <- nrow(
    forecast_project$gw_test_results
  )
  forecast_project$processing_summary$mcs_model_rows <- nrow(
    forecast_project$model_confidence_set
  )
  forecast_project$processing_summary$mcs_summary_rows <- nrow(
    forecast_project$model_confidence_set_summary
  )
  forecast_project$processing_summary$mcs_audit_rows <- nrow(
    forecast_project$mcs_audit
  )
  forecast_project$processing_summary$mcs_audit_summary_rows <- nrow(
    forecast_project$mcs_audit_summary
  )
}

save_forecast_project_results(
  output_directory = file.path(project_root, "results"),
  forecast_project = forecast_project
)

message("Statistical results were rebuilt from the existing forecast RDS.")
message("DM rows: ", nrow(forecast_project$dm_test_results))
message("Supplementary HAC MAE rows: ", nrow(forecast_project$gw_test_results))
message("MCS procedures: ", nrow(forecast_project$model_confidence_set_summary))
message("MCS audit procedures: ", nrow(forecast_project$mcs_audit_summary))

source(file.path(project_root, "scripts", "validate-statistical-results.R"))
