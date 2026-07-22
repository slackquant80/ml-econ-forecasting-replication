###############################################################################
### Result Persistence
###############################################################################

write_csv_if_data_frame <- function(x, path) {
  if (is.data.frame(x)) {
    utils::write.csv(
      x,
      path,
      row.names = FALSE,
      na = ""
    )
  }
  invisible(path)
}

save_forecast_project_results <- function(
    output_directory,
    forecast_project
) {
  if (!is.list(forecast_project)) {
    stop("forecast_project는 list여야 합니다.")
  }

  required_components <- c(
    "forecasts",
    "accuracy",
    "relative_accuracy",
    "rankings",
    "processing_summary",
    "variable_selection",
    "rolling_index",
    "configuration"
  )
  missing_components <- setdiff(required_components, names(forecast_project))
  if (length(missing_components) > 0L) {
    stop(
      "forecast_project에 필요한 구성요소가 없습니다: ",
      paste(missing_components, collapse = ", ")
    )
  }

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  table_files <- list(
    forecast_results = "forecast_results.csv",
    accuracy = "forecast_accuracy.csv",
    relative_accuracy = "relative_accuracy.csv",
    rankings = "model_rankings.csv",
    ensemble_weights = "ensemble_weights.csv",
    processing_summary = "processing_summary.csv",
    pca_em_test_summary = "pca_em_test_summary.csv",
    rolling_window_test_summary = "rolling_window_test_summary.csv",
    training_design_summary = "training_design_summary.csv",
    variable_selection = "variable_selection.csv",
    excluded_variable_summary = "excluded_variable_summary.csv",
    rolling_index = "rolling_index.csv",
    target_imputation_summary = "target_imputation_summary.csv",
    metadata_change_summary = "metadata_change_summary.csv",
    obsolete_variable_metadata = "obsolete_variable_metadata.csv",
    model_registry = "model_registry.csv",
    cumulative_backtest_results = "cumulative_backtest_results.csv",
    cumulative_accuracy = "cumulative_backtest_accuracy.csv",
    cumulative_relative_accuracy = "cumulative_relative_accuracy.csv",
    cumulative_rankings = "cumulative_model_rankings.csv",
    cumulative_ensemble_weights = "cumulative_ensemble_weights.csv",
    cumulative_rolling_index = "cumulative_rolling_index.csv",
    forward_forecasts = "forward_forecasts.csv",
    forward_ensemble_weights = "forward_ensemble_weights.csv",
    forward_best_models = "forward_best_models.csv",
    dm_test_results = "dm_test_results.csv",
    gw_test_results = "gw_test_results.csv",
    model_confidence_set = "model_confidence_set.csv",
    model_confidence_set_summary = "model_confidence_set_summary.csv",
    mcs_audit = "mcs_audit.csv",
    mcs_audit_summary = "mcs_audit_summary.csv",
    boruta_selection_history = "boruta_selection_history.csv",
    boruta_final_selection = "boruta_final_selection.csv",
    boruta_feature_stability = "boruta_feature_stability.csv",
    boruta_stability_summary = "boruta_stability_summary.csv",
    boruta_predictive_comparison = "boruta_predictive_comparison.csv",
    boruta_audit_summary = "boruta_audit_summary.csv"
  )

  project_name_map <- c(
    forecast_results = "forecasts"
  )

  for (table_name in names(table_files)) {
    project_name <- if (table_name %in% names(project_name_map)) {
      unname(project_name_map[table_name])
    } else {
      table_name
    }

    if (!is.null(forecast_project[[project_name]])) {
      write_csv_if_data_frame(
        forecast_project[[project_name]],
        file.path(output_directory, table_files[[table_name]])
      )
    }
  }

  utils::capture.output(
    utils::sessionInfo(),
    file = file.path(output_directory, "session_info.txt")
  )

  saveRDS(
    forecast_project$configuration,
    file.path(output_directory, "configuration.rds")
  )

  project_to_save <- forecast_project
  project_to_save$saved_at <- Sys.time()

  saveRDS(
    project_to_save,
    file.path(output_directory, "forecast_project_results.rds")
  )

  if (
    is.data.frame(forecast_project$forward_forecasts) &&
      nrow(forecast_project$forward_forecasts) > 0L
  ) {
    forward_project <- list(
      forward_forecasts = forecast_project$forward_forecasts,
      forward_best_models = forecast_project$forward_best_models,
      forward_ensemble_weights = forecast_project$forward_ensemble_weights,
      cumulative_accuracy = forecast_project$cumulative_accuracy,
      cumulative_relative_accuracy = forecast_project$cumulative_relative_accuracy,
      cumulative_rankings = forecast_project$cumulative_rankings,
      dm_test_results = forecast_project$dm_test_results,
      gw_test_results = forecast_project$gw_test_results,
      model_confidence_set = forecast_project$model_confidence_set,
      model_confidence_set_summary = forecast_project$model_confidence_set_summary,
      mcs_audit = forecast_project$mcs_audit,
      mcs_audit_summary = forecast_project$mcs_audit_summary,
      boruta_final_selection = forecast_project$boruta_final_selection,
      boruta_feature_stability = forecast_project$boruta_feature_stability,
      boruta_stability_summary = forecast_project$boruta_stability_summary,
      boruta_predictive_comparison = forecast_project$boruta_predictive_comparison,
      boruta_audit_summary = forecast_project$boruta_audit_summary,
      processing_summary = forecast_project$processing_summary,
      configuration = forecast_project$configuration,
      saved_at = Sys.time()
    )

    saveRDS(
      forward_project,
      file.path(output_directory, "forward_forecast_project_results.rds")
    )
  }

  invisible(output_directory)
}
