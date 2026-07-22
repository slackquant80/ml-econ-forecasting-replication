###############################################################################
### Leakage-Safe Forecast Ensembles
###############################################################################

align_forecast_columns <- function(x, template_names) {
  missing_columns <- setdiff(template_names, names(x))
  for (column_name in missing_columns) x[[column_name]] <- NA
  x[, template_names, drop = FALSE]
}

build_ensemble_forecasts <- function(
    base_forecasts,
    member_models = NULL,
    methods = c("mean", "median", "inverse_rmse"),
    min_members = 2L,
    min_history = 12L,
    weight_epsilon = 1e-8
) {
  required_columns <- c(
    "model", "horizon", "forecast_number", "target_date",
    "origin_date", "actual", "prediction", "status",
    "evaluation_included", "squared_error"
  )
  missing_columns <- setdiff(required_columns, names(base_forecasts))
  if (length(missing_columns) > 0L) {
    stop(
      "앙상블 자료에 필요한 열이 없습니다: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  methods <- match.arg(
    methods,
    choices = c("mean", "median", "inverse_rmse"),
    several.ok = TRUE
  )
  methods <- unique(methods)

  min_members <- validate_scalar_integer(min_members, "min_members", 2L)
  min_history <- validate_scalar_integer(min_history, "min_history", 1L)
  weight_epsilon <- validate_scalar_numeric(
    weight_epsilon,
    "weight_epsilon",
    minimum = 0,
    minimum_inclusive = FALSE
  )

  valid_base <- base_forecasts[
    base_forecasts$status == "ok" &
      is.finite(base_forecasts$prediction) &
      is.finite(base_forecasts$actual),
    ,
    drop = FALSE
  ]

  if (nrow(valid_base) == 0L) {
    stop("앙상블에 사용할 정상 예측값이 없습니다.")
  }

  if (is.null(member_models)) {
    member_models <- unique(valid_base$model)
  }
  member_models <- unique(as.character(member_models))
  member_models <- intersect(member_models, unique(valid_base$model))

  if (length(member_models) < min_members) {
    stop("앙상블 구성에 필요한 기본모형 수가 부족합니다.")
  }

  member_data <- valid_base[
    valid_base$model %in% member_models,
    ,
    drop = FALSE
  ]

  duplicate_key <- duplicated(
    member_data[, c("model", "horizon", "forecast_number")]
  )
  if (any(duplicate_key)) {
    stop("앙상블 입력에 모형·horizon·forecast_number 중복 행이 있습니다.")
  }

  ensemble_rows <- list()
  weight_rows <- list()
  row_counter <- 0L
  weight_counter <- 0L

  for (horizon in sort(unique(member_data$horizon))) {
    horizon_data <- member_data[
      member_data$horizon == horizon,
      ,
      drop = FALSE
    ]
    forecast_numbers <- sort(unique(horizon_data$forecast_number))

    for (forecast_number in forecast_numbers) {
      current <- horizon_data[
        horizon_data$forecast_number == forecast_number,
        ,
        drop = FALSE
      ]

      if (nrow(current) < min_members) next

      actual_values <- unique(current$actual)
      target_dates <- unique(current$target_date)
      origin_dates <- unique(current$origin_date)
      evaluation_flags <- unique(current$evaluation_included)

      if (
        length(actual_values) != 1L ||
        length(target_dates) != 1L ||
        length(origin_dates) != 1L ||
        length(evaluation_flags) != 1L
      ) {
        stop(
          "앙상블 구성 시 actual, 날짜 또는 평가 포함 여부가 모형 간 일치하지 않습니다."
        )
      }

      for (method in methods) {
        weights <- if (method == "median") {
          rep(NA_real_, nrow(current))
        } else {
          rep(1 / nrow(current), nrow(current))
        }
        history_count <- 0L

        if (method == "inverse_rmse") {
          current_origin_date <- as.Date(origin_dates[1L])

          # 현재 forecast origin 시점에 실제값이 이미 발표된 과거 예측만 사용한다.
          # forecast_number만 비교하면 h > 1에서 아직 실현되지 않은 목표월의
          # 오차를 참조하는 look-ahead가 발생할 수 있다.
          historical <- horizon_data[
            horizon_data$forecast_number < forecast_number &
              as.Date(horizon_data$target_date) <= current_origin_date &
              horizon_data$evaluation_included &
              is.finite(horizon_data$squared_error),
            ,
            drop = FALSE
          ]

          history_by_model <- vapply(
            current$model,
            function(model_name) {
              as.integer(sum(historical$model == model_name))
            },
            FUN.VALUE = integer(1)
          )

          if (length(history_by_model) > 0L) {
            history_count <- min(history_by_model)
          }

          rmse_by_model <- vapply(
            current$model,
            function(model_name) {
              model_history <- historical[
                historical$model == model_name,
                ,
                drop = FALSE
              ]
              if (nrow(model_history) < min_history) return(NA_real_)
              sqrt(mean(model_history$squared_error))
            },
            FUN.VALUE = numeric(1)
          )

          available <- is.finite(rmse_by_model)
          if (sum(available) >= min_members) {
            inverse_error <- 1 / pmax(rmse_by_model[available], weight_epsilon)
            weights <- rep(0, nrow(current))
            weights[available] <- inverse_error / sum(inverse_error)
            history_count <- min(history_by_model[available])
          }
        }

        prediction <- switch(
          method,
          mean = mean(current$prediction),
          median = stats::median(current$prediction),
          inverse_rmse = sum(weights * current$prediction)
        )

        if (!is.finite(prediction)) {
          stop("앙상블 예측값이 유한하지 않습니다.")
        }

        ensemble_label <- switch(
          method,
          mean = "Ensemble_Mean",
          median = "Ensemble_Median",
          inverse_rmse = "Ensemble_InvRMSE"
        )

        row_counter <- row_counter + 1L
        reference <- current[1L, , drop = FALSE]
        reference$model_key <- ensemble_label
        reference$model <- ensemble_label
        reference$model_family <- "Ensemble"
        reference$prediction <- prediction
        reference$status <- "ok"
        reference$status_message <- NA_character_
        reference$training_observations <- NA_integer_
        reference$n_features <- NA_integer_
        reference$n_selected <- nrow(current)
        reference$tuning_parameter <- paste0(
          "members=", nrow(current),
          ";history=", history_count
        )
        reference$validation_loss <- NA_real_
        reference$error <- reference$actual - reference$prediction
        reference$squared_error <- reference$error^2
        reference$absolute_error <- abs(reference$error)
        ensemble_rows[[row_counter]] <- reference

        for (j in seq_len(nrow(current))) {
          weight_counter <- weight_counter + 1L
          weight_rows[[weight_counter]] <- data.frame(
            ensemble = ensemble_label,
            horizon = horizon,
            forecast_number = forecast_number,
            target_date = as.Date(reference$target_date),
            member_model = current$model[j],
            weight = weights[j],
            history_count = history_count,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  ensemble_forecasts <- if (length(ensemble_rows) > 0L) {
    do.call(rbind, ensemble_rows)
  } else {
    base_forecasts[0, , drop = FALSE]
  }
  rownames(ensemble_forecasts) <- NULL
  ensemble_forecasts <- align_forecast_columns(
    ensemble_forecasts,
    names(base_forecasts)
  )

  ensemble_weights <- if (length(weight_rows) > 0L) {
    do.call(rbind, weight_rows)
  } else {
    data.frame()
  }

  list(
    forecasts = ensemble_forecasts,
    weights = ensemble_weights,
    members = member_models
  )
}
