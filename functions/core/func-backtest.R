###############################################################################
### Common Rolling Backtest Engine
###############################################################################

validate_model_result <- function(model_result) {
  if (!is.list(model_result)) {
    return("모형 함수가 list를 반환하지 않았습니다.")
  }

  if (is.null(model_result$prediction) || length(model_result$prediction) != 1L) {
    return("모형 함수가 하나의 prediction을 반환하지 않았습니다.")
  }

  prediction <- suppressWarnings(as.numeric(model_result$prediction))
  if (length(prediction) != 1L || is.na(prediction) || !is.finite(prediction)) {
    return("예측값이 하나의 유한한 숫자가 아닙니다.")
  }

  if (!is.null(model_result$diagnostics)) {
    if (!is.list(model_result$diagnostics)) {
      return("모형 diagnostics는 NULL 또는 list여야 합니다.")
    }

    scalar_diagnostics <- c(
      "training_observations",
      "n_features",
      "n_selected",
      "tuning_parameter",
      "validation_loss"
    )

    invalid_diagnostics <- vapply(
      scalar_diagnostics,
      function(name) {
        value <- model_result$diagnostics[[name]]
        !is.null(value) && length(value) != 1L
      },
      FUN.VALUE = logical(1)
    )

    if (any(invalid_diagnostics)) {
      return(
        paste0(
          "모형 diagnostics 항목은 각각 하나의 값이어야 합니다: ",
          paste(scalar_diagnostics[invalid_diagnostics], collapse = ", ")
        )
      )
    }
  }

  NULL
}


run_model_set <- function(
    models,
    registry,
    data,
    rolling_index,
    target_name,
    target_display_name,
    forecast_horizons,
    npred,
    window_size,
    feature_settings,
    pca_em_settings,
    base_seed = 1L,
    error_policy = c("stop", "record"),
    show_progress = FALSE,
    save_artifacts = c("none", "last_window", "all")
) {
  error_policy <- match.arg(error_policy)
  save_artifacts <- match.arg(save_artifacts)
  validate_model_registry(registry, models)

  forecast_horizons <- as.integer(forecast_horizons)
  npred <- validate_scalar_integer(npred, "npred", 1L)
  window_size <- validate_scalar_integer(window_size, "window_size", 1L)

  if (anyDuplicated(forecast_horizons) > 0L) {
    stop("forecast_horizons에 중복된 값이 있습니다.")
  }

  need_feature_bundle <- any(
    vapply(
      registry[models],
      function(x) isTRUE(x$requires_feature_bundle),
      FUN.VALUE = logical(1)
    )
  )

  expected_rows <- length(models) * length(forecast_horizons) * npred
  result_list <- vector("list", expected_rows)
  result_position <- 0L
  artifacts <- list()

  states <- setNames(
    lapply(models, function(x) list()),
    models
  )

  require_pca_em_convergence <- isTRUE(
    get_config_value(pca_em_settings, "require_convergence", TRUE)
  )

  for (horizon in forecast_horizons) {
    horizon_key <- as.character(horizon)

    for (forecast_number in seq_len(npred)) {
      window_info <- get_forecast_window(
        forecast_number = forecast_number,
        horizon = horizon,
        rolling_index = rolling_index,
        data = data,
        window_size = window_size,
        target_name = target_name,
        target_display_name = target_display_name,
        apply_pca_em = need_feature_bundle,
        pca_em_factors = pca_em_settings$n_factors,
        pca_em_max_iter = pca_em_settings$max_iter,
        pca_em_tol = pca_em_settings$tol,
        require_pca_em_convergence = require_pca_em_convergence,
        predictor_missing_policy = get_config_value(
          pca_em_settings,
          "predictor_policy",
          NULL
        )
      )

      feature_bundle <- NULL
      if (need_feature_bundle) {
        feature_bundle <- prepare_direct_feature_bundle(
          Y.window = window_info$Y.window,
          horizon = horizon,
          predictor_lags = feature_settings$predictor_lags,
          n_factors = feature_settings$n_factors,
          factor_include_target = feature_settings$factor_include_target,
          factor_scale = feature_settings$factor_scale
        )
      }

      for (model_index in seq_along(models)) {
        model_key <- models[model_index]
        specification <- registry[[model_key]]
        current_state <- states[[model_key]][[horizon_key]]

        context <- list(
          Y.window = window_info$Y.window,
          feature_bundle = feature_bundle,
          horizon = horizon,
          forecast_number = forecast_number,
          window_info = window_info,
          seed = make_model_seed(
            base_seed,
            model_index,
            horizon,
            forecast_number
          )
        )

        model_error <- NULL
        model_result <- tryCatch(
          specification$model_function(
            context = context,
            config = specification$config,
            state = current_state
          ),
          error = function(e) {
            model_error <<- conditionMessage(e)
            NULL
          }
        )

        if (!is.null(model_result)) {
          validation_error <- validate_model_result(model_result)
          if (!is.null(validation_error)) {
            model_error <- validation_error
            model_result <- NULL
          }
        }

        if (is.null(model_result)) {
          if (error_policy == "stop") {
            stop(
              "모형 실행 오류 [", specification$label,
              ", h=", horizon,
              ", forecast=", forecast_number,
              "]: ", model_error
            )
          }

          prediction <- NA_real_
          diagnostics <- list()
          status <- "error"
          status_message <- model_error
        } else {
          prediction <- as.numeric(model_result$prediction)[1L]
          diagnostics <- if (is.null(model_result$diagnostics)) {
            list()
          } else {
            model_result$diagnostics
          }

          states[[model_key]][[horizon_key]] <- model_result$state
          status <- "ok"
          status_message <- NA_character_

          save_this_artifact <- (
            save_artifacts == "all" ||
              (save_artifacts == "last_window" && forecast_number == npred)
          )
          if (save_this_artifact && !is.null(model_result$artifact)) {
            artifact_key <- paste(
              model_key,
              paste0("h", horizon),
              paste0("f", forecast_number),
              sep = "__"
            )
            artifacts[[artifact_key]] <- model_result$artifact
          }
        }

        result_position <- result_position + 1L
        result_list[[result_position]] <- data.frame(
          target_code = target_name,
          target_name = target_display_name,
          model_key = model_key,
          model = specification$label,
          model_family = specification$family,
          horizon = horizon,
          forecast_number = forecast_number,
          window_start_date = as.Date(window_info$window_start_date),
          origin_date = as.Date(window_info$origin_date),
          target_date = as.Date(window_info$target_date),
          actual = as.numeric(window_info$actual),
          prediction = prediction,
          evaluation_included = as.logical(
            window_info$evaluation_included && status == "ok"
          ),
          status = status,
          status_message = status_message,
          missing_before = as.integer(window_info$missing_before),
          short_gap_imputed_count = as.integer(
            window_info$short_gap_imputed_count
          ),
          final_imputed_count = as.integer(
            window_info$final_imputed_count
          ),
          dropped_predictor_count = as.integer(
            window_info$dropped_predictor_count
          ),
          missing_after = as.integer(window_info$missing_after),
          em_iterations = if (is.null(window_info$em_iterations)) {
            NA_integer_
          } else {
            as.integer(window_info$em_iterations)
          },
          em_converged = if (is.null(window_info$em_converged)) {
            NA
          } else {
            as.logical(window_info$em_converged)
          },
          em_last_change = if (is.null(window_info$em_last_change)) {
            NA_real_
          } else {
            as.numeric(window_info$em_last_change)
          },
          training_observations = as.integer(
            get_config_value(diagnostics, "training_observations", NA_integer_)
          ),
          n_features = as.integer(
            get_config_value(diagnostics, "n_features", NA_integer_)
          ),
          n_selected = as.integer(
            get_config_value(diagnostics, "n_selected", NA_integer_)
          ),
          tuning_parameter = as.character(
            get_config_value(diagnostics, "tuning_parameter", NA_character_)
          ),
          validation_loss = as.numeric(
            get_config_value(diagnostics, "validation_loss", NA_real_)
          ),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }

      if (
        isTRUE(show_progress) &&
        (forecast_number %% 10L == 0L || forecast_number == npred)
      ) {
        progress_message <- paste0(
          "Target-month transformed-change backtest: horizon=", horizon,
          ", forecast=", forecast_number,
          "/", npred
        )
        message(
          "Progress: horizon=", horizon,
          ", forecast=", forecast_number,
          "/", npred
        )
        report_experiment_progress(
          stage = "monthly_backtest",
          horizon = horizon,
          forecast_number = forecast_number,
          forecast_total = npred,
          message = progress_message
        )
      }
    }

    if (isTRUE(show_progress)) {
      message("Completed horizon: ", horizon)
    }
  }

  if (result_position != expected_rows || any(vapply(result_list, is.null, logical(1)))) {
    stop("공통 backtest 결과가 예상한 개수만큼 생성되지 않았습니다.")
  }

  forecasts <- do.call(rbind, result_list)
  rownames(forecasts) <- NULL
  forecasts$error <- forecasts$actual - forecasts$prediction
  forecasts$squared_error <- forecasts$error^2
  forecasts$absolute_error <- abs(forecasts$error)

  if (nrow(forecasts) != expected_rows) {
    stop("공통 backtest 결과의 행 수가 예상과 다릅니다.")
  }

  if (error_policy == "stop") {
    expected_grid <- expand.grid(
      model_key = models,
      horizon = forecast_horizons,
      stringsAsFactors = FALSE
    )

    successful_counts <- stats::aggregate(
      x = list(n_success = as.integer(forecasts$status == "ok")),
      by = list(
        model_key = forecasts$model_key,
        horizon = forecasts$horizon
      ),
      FUN = sum
    )

    successful_counts <- merge(
      expected_grid,
      successful_counts,
      by = c("model_key", "horizon"),
      all.x = TRUE,
      sort = FALSE
    )
    successful_counts$n_success[is.na(successful_counts$n_success)] <- 0L

    if (any(successful_counts$n_success != npred)) {
      stop("모형 또는 horizon별 성공 예측 수가 npred와 다릅니다.")
    }
  }

  list(
    forecasts = forecasts,
    artifacts = artifacts,
    final_states = states
  )
}
