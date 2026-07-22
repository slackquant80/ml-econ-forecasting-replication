###############################################################################
### Common Forecast Evaluation
###############################################################################

summarize_forecast_accuracy <- function(forecasts) {
  required_columns <- c(
    "model", "model_family", "horizon", "status",
    "evaluation_included", "prediction", "actual",
    "squared_error", "absolute_error", "error"
  )
  missing_columns <- setdiff(required_columns, names(forecasts))
  if (length(missing_columns) > 0L) {
    stop(
      "성과평가 자료에 필요한 열이 없습니다: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  evaluation_data <- forecasts[
    forecasts$status == "ok" &
      !is.na(forecasts$evaluation_included) &
      forecasts$evaluation_included &
      is.finite(forecasts$prediction) &
      is.finite(forecasts$actual) &
      is.finite(forecasts$squared_error) &
      is.finite(forecasts$absolute_error) &
      is.finite(forecasts$error),
    ,
    drop = FALSE
  ]
  if (nrow(evaluation_data) == 0L) {
    stop("평가에 포함할 예측값이 없습니다.")
  }

  group_key <- interaction(
    evaluation_data$model,
    evaluation_data$horizon,
    drop = TRUE,
    lex.order = TRUE
  )
  grouped_data <- split(evaluation_data, group_key)

  accuracy <- do.call(
    rbind,
    lapply(grouped_data, function(x) {
      actual_sd <- stats::sd(x$actual)
      prediction_sd <- stats::sd(x$prediction)

      correlation <- if (
        nrow(x) >= 2L &&
        is.finite(actual_sd) &&
        is.finite(prediction_sd) &&
        actual_sd > 0 &&
        prediction_sd > 0
      ) {
        stats::cor(x$actual, x$prediction)
      } else {
        NA_real_
      }

      data.frame(
        model = x$model[1L],
        model_family = x$model_family[1L],
        horizon = x$horizon[1L],
        n_evaluation = nrow(x),
        RMSE = sqrt(mean(x$squared_error)),
        MAE = mean(x$absolute_error),
        bias = mean(x$error),
        correlation = correlation,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })
  )
  rownames(accuracy) <- NULL
  accuracy[order(accuracy$horizon, accuracy$RMSE, accuracy$model), , drop = FALSE]
}

add_benchmark_relative_accuracy <- function(accuracy, benchmark_model = "RW") {
  if (!is.character(benchmark_model) || length(benchmark_model) != 1L) {
    stop("benchmark_model은 하나의 문자열이어야 합니다.")
  }

  benchmark <- accuracy[
    accuracy$model == benchmark_model,
    c("horizon", "RMSE", "MAE"),
    drop = FALSE
  ]

  expected_horizons <- sort(unique(accuracy$horizon))
  if (
    nrow(benchmark) != length(expected_horizons) ||
    anyDuplicated(benchmark$horizon) > 0L ||
    !setequal(benchmark$horizon, expected_horizons)
  ) {
    stop("Benchmark 모형이 모든 horizon에 정확히 한 행씩 존재하지 않습니다: ", benchmark_model)
  }

  if (
    any(!is.finite(benchmark$RMSE)) ||
    any(!is.finite(benchmark$MAE)) ||
    any(benchmark$RMSE <= 0) ||
    any(benchmark$MAE <= 0)
  ) {
    stop("Benchmark 성과지표가 유효하지 않습니다: ", benchmark_model)
  }

  names(benchmark)[-1L] <- c("benchmark_RMSE", "benchmark_MAE")
  result <- merge(accuracy, benchmark, by = "horizon", all.x = TRUE, sort = FALSE)
  result$relative_RMSE <- result$RMSE / result$benchmark_RMSE
  result$relative_MAE <- result$MAE / result$benchmark_MAE
  result[order(result$horizon, result$RMSE, result$model), , drop = FALSE]
}

rank_models_by_horizon <- function(accuracy) {
  result <- accuracy
  result$RMSE_rank <- ave(
    result$RMSE,
    result$horizon,
    FUN = function(x) rank(x, ties.method = "min", na.last = "keep")
  )
  result$MAE_rank <- ave(
    result$MAE,
    result$horizon,
    FUN = function(x) rank(x, ties.method = "min", na.last = "keep")
  )
  result[order(result$horizon, result$RMSE_rank, result$model), , drop = FALSE]
}
