###############################################################################
### tcode-Aware Original-Level Backtest and Forward Forecasts
###############################################################################

add_months_first_day <- function(date, months) {
  months <- validate_scalar_integer(months, "months", 1L)
  date <- as.Date(date)

  if (length(date) != 1L || is.na(date)) {
    stop("date는 하나의 유효한 날짜여야 합니다.")
  }

  month_start <- as.Date(format(date, "%Y-%m-01"))
  seq.Date(
    from = month_start,
    by = paste(months, "months"),
    length.out = 2L
  )[2L]
}

make_cumulative_direct_design <- function(
    panel,
    target_level,
    horizon,
    predictor_lags = 4L,
    level_forecast_mode = "cumulative_log_change",
    target_level_observed = rep(TRUE, length(target_level)),
    transformed_target_valid = rep(TRUE, length(target_level))
) {
  panel <- as.matrix(panel)
  storage.mode(panel) <- "double"
  target_level <- as.numeric(target_level)
  target_level_observed <- as.logical(target_level_observed)
  transformed_target_valid <- as.logical(transformed_target_valid)
  level_forecast_mode <- validate_level_forecast_mode(level_forecast_mode)

  horizon <- validate_scalar_integer(horizon, "horizon", 1L)
  predictor_lags <- validate_scalar_integer(predictor_lags, "predictor_lags", 1L)

  if (
    nrow(panel) != length(target_level) ||
    length(target_level_observed) != length(target_level) ||
    length(transformed_target_valid) != length(target_level) ||
    anyNA(target_level_observed) || anyNA(transformed_target_valid)
  ) {
    stop("누적 목표 설계행렬의 level 또는 validity mask 길이가 일치하지 않습니다.")
  }
  if (anyNA(panel) || any(!is.finite(panel))) {
    stop("누적 목표 설계행렬의 panel에 결측치 또는 비유한 값이 있습니다.")
  }
  if (anyNA(target_level) || any(!is.finite(target_level))) {
    stop("누적 목표 설계행렬의 target level에 결측치 또는 비유한 값이 있습니다.")
  }
  if (is.null(colnames(panel))) colnames(panel) <- paste0("X", seq_len(ncol(panel)))

  n <- nrow(panel)
  last_training_origin <- n - horizon
  if (last_training_origin < predictor_lags) {
    stop("누적 목표 direct forecast 설계행렬을 만들 관측치가 부족합니다.")
  }

  candidate_origins <- seq.int(predictor_lags, last_training_origin)
  valid <- make_level_training_valid(
    observed_level = target_level_observed,
    origins = candidate_origins,
    horizon = horizon,
    mode = level_forecast_mode,
    transformed_target_valid = transformed_target_valid,
    predictor_lags = predictor_lags
  )
  training_origins <- candidate_origins[valid]
  if (length(training_origins) < 1L) {
    stop("대체되지 않은 목표값으로 원수준 direct 설계행렬을 만들 수 없습니다.")
  }

  X_blocks <- lapply(
    0:(predictor_lags - 1L),
    function(lag_value) panel[training_origins - lag_value, , drop = FALSE]
  )
  X <- do.call(cbind, X_blocks)
  colnames(X) <- unlist(lapply(
    0:(predictor_lags - 1L),
    function(lag_value) paste0(colnames(panel), "_L", lag_value)
  ), use.names = FALSE)

  response <- make_level_response(
    level = target_level,
    origins = training_origins,
    horizon = horizon,
    mode = level_forecast_mode
  )

  new_x <- unlist(lapply(
    0:(predictor_lags - 1L),
    function(lag_value) panel[n - lag_value, , drop = TRUE]
  ), use.names = FALSE)
  names(new_x) <- colnames(X)

  if (length(new_x) != ncol(X)) {
    stop("누적 목표 direct forecast 설계행렬의 열 정렬에 문제가 있습니다.")
  }
  if (anyNA(response) || any(!is.finite(response))) {
    stop("원수준 학습 response에 결측치 또는 비유한 값이 있습니다.")
  }

  list(
    X = X,
    y = response,
    new_x = new_x,
    training_origins = training_origins,
    training_observations = length(response),
    excluded_training_observations = length(candidate_origins) - length(training_origins),
    feature_names = colnames(X),
    block_size = ncol(panel),
    predictor_lags = predictor_lags,
    response_mode = level_forecast_mode
  )
}

prepare_cumulative_feature_bundle <- function(
    Y.window,
    target_level.window,
    horizon,
    predictor_lags = 4L,
    n_factors = 4L,
    factor_include_target = FALSE,
    factor_scale = TRUE,
    level_forecast_mode = "cumulative_log_change",
    target_level_observed.window = rep(TRUE, nrow(Y.window)),
    transformed_target_valid.window = rep(TRUE, nrow(Y.window))
) {
  Y.window <- as.matrix(Y.window)
  storage.mode(Y.window) <- "double"
  target_level.window <- as.numeric(target_level.window)
  target_level_observed.window <- as.logical(target_level_observed.window)
  transformed_target_valid.window <- as.logical(transformed_target_valid.window)
  level_forecast_mode <- validate_level_forecast_mode(level_forecast_mode)

  if (ncol(Y.window) < 2L) stop("누적 transformed-change 다변량 모형을 위한 설명변수가 없습니다.")
  if (
    nrow(Y.window) != length(target_level.window) ||
    length(target_level_observed.window) != nrow(Y.window) ||
    length(transformed_target_valid.window) != nrow(Y.window)
  ) stop("원수준 feature bundle의 level 또는 mask 길이가 일치하지 않습니다.")
  if (anyNA(Y.window) || any(!is.finite(Y.window))) {
    stop("원수준 feature bundle 입력자료에 결측치 또는 비유한 값이 있습니다.")
  }
  if (anyNA(target_level.window) || any(!is.finite(target_level.window))) {
    stop("원수준 feature bundle의 level에 결측치 또는 비유한 값이 있습니다.")
  }
  if (anyNA(target_level_observed.window) || anyNA(transformed_target_valid.window)) {
    stop("원수준 feature bundle validity mask에 결측치가 있습니다.")
  }

  if (is.null(colnames(Y.window))) {
    colnames(Y.window) <- c("target", paste0("X", seq_len(ncol(Y.window) - 1L)))
  }
  predictor_lags <- validate_scalar_integer(predictor_lags, "predictor_lags", 1L)
  n_factors <- validate_scalar_integer(n_factors, "n_factors", 1L)

  transformed_target <- as.numeric(Y.window[, 1L])
  target_name <- colnames(Y.window)[1L]
  predictor_sd <- apply(Y.window[, -1L, drop = FALSE], 2L, stats::sd)
  active_predictors <- is.finite(predictor_sd) & predictor_sd > sqrt(.Machine$double.eps)
  dropped_predictors <- colnames(Y.window)[-1L][!active_predictors]
  predictor_panel <- Y.window[, -1L, drop = FALSE][, active_predictors, drop = FALSE]
  if (ncol(predictor_panel) < 1L) stop("현재 rolling window에 유효한 원수준 설명변수가 없습니다.")

  pca_input <- if (isTRUE(factor_include_target)) {
    cbind(Y.window[, 1L, drop = FALSE], predictor_panel)
  } else predictor_panel
  active_factor_count <- min(n_factors, nrow(pca_input) - 1L, ncol(pca_input))
  pca_model <- stats::prcomp(
    pca_input, center = TRUE, scale. = isTRUE(factor_scale), rank. = active_factor_count
  )
  factor_scores <- pca_model$x[, seq_len(active_factor_count), drop = FALSE]
  colnames(factor_scores) <- paste0("PC", seq_len(active_factor_count))

  active_Y <- cbind(Y.window[, 1L, drop = FALSE], predictor_panel)
  colnames(active_Y)[1L] <- target_name
  full_panel <- cbind(active_Y, factor_scores)
  factor_panel <- cbind(Y.window[, 1L, drop = FALSE], factor_scores)
  colnames(factor_panel)[1L] <- target_name

  design_args <- list(
    target_level = target_level.window,
    horizon = horizon,
    predictor_lags = predictor_lags,
    level_forecast_mode = level_forecast_mode,
    target_level_observed = target_level_observed.window,
    transformed_target_valid = transformed_target_valid.window
  )
  full_design <- do.call(make_cumulative_direct_design, c(list(panel = full_panel), design_args))
  factor_design <- do.call(make_cumulative_direct_design, c(list(panel = factor_panel), design_args))

  list(
    target = transformed_target,
    target_level = target_level.window,
    target_name = target_name,
    active_Y = active_Y,
    predictor_panel = predictor_panel,
    factor_scores = factor_scores,
    full_panel = full_panel,
    factor_panel = factor_panel,
    full_design = full_design,
    factor_design = factor_design,
    pca_model = pca_model,
    dropped_predictors = dropped_predictors,
    n_factors = active_factor_count,
    predictor_lags = predictor_lags,
    response_mode = level_forecast_mode,
    target_level_observed = target_level_observed.window,
    transformed_target_valid = transformed_target_valid.window
  )
}

forecast_cumulative_rw_model <- function(context, config = list(), state = NULL) {
  target_level <- as.numeric(context$target_level.window)
  mode <- validate_level_forecast_mode(context$target_mode)
  origin_level <- tail(target_level, 1L)

  if (anyNA(target_level) || any(!is.finite(target_level))) {
    stop("원수준 RW의 target level에 결측치 또는 비유한 값이 있습니다.")
  }
  prediction <- switch(
    mode,
    direct_level = origin_level,
    direct_log_level = {
      if (origin_level <= 0) stop("direct_log_level RW origin이 0 이하입니다.")
      log(origin_level)
    },
    cumulative_arithmetic_change = 0,
    cumulative_log_change = 0,
    cumulative_percent_change = 0
  )

  list(
    prediction = unname(prediction),
    diagnostics = list(
      training_observations = max(0L, length(target_level) - context$horizon),
      n_features = 1L,
      n_selected = 1L,
      tuning_parameter = paste0("level RW;mode=", mode),
      validation_loss = NA_real_
    ),
    artifact = NULL,
    state = state
  )
}

forecast_cumulative_ar_model <- function(context, config = list(), state = NULL) {
  transformed_target <- as.numeric(context$Y.window[, 1L])
  target_level <- as.numeric(context$target_level.window)
  level_observed <- as.logical(context$target_level_observed.window)
  transformed_valid <- as.logical(context$transformed_target_valid.window)
  mode <- validate_level_forecast_mode(context$target_mode)
  horizon <- context$horizon
  ar_lags <- validate_scalar_integer(get_config_value(config, "ar_lags", 4L), "ar_lags", 1L)

  n <- length(transformed_target)
  if (
    length(target_level) != n || length(level_observed) != n ||
    length(transformed_valid) != n || anyNA(level_observed) || anyNA(transformed_valid)
  ) stop("원수준 AR 입력 길이 또는 validity mask가 일치하지 않습니다.")
  if (anyNA(transformed_target) || any(!is.finite(transformed_target)) ||
      anyNA(target_level) || any(!is.finite(target_level))) {
    stop("원수준 AR 입력에 결측치 또는 비유한 값이 있습니다.")
  }

  last_training_origin <- n - horizon
  if (last_training_origin < ar_lags) stop("원수준 direct AR 추정을 위한 관측치가 부족합니다.")
  candidate_origins <- seq.int(ar_lags, last_training_origin)
  valid <- make_level_training_valid(
    observed_level = level_observed,
    origins = candidate_origins,
    horizon = horizon,
    mode = mode,
    transformed_target_valid = transformed_valid,
    predictor_lags = ar_lags
  )
  origins <- candidate_origins[valid]
  if (length(origins) <= ar_lags + 1L) stop("원수준 direct AR의 유효 학습 response가 부족합니다.")

  X <- vapply(seq_len(ar_lags), function(j) transformed_target[origins - j + 1L],
              FUN.VALUE = numeric(length(origins)))
  if (ar_lags == 1L) X <- matrix(X, ncol = 1L)
  colnames(X) <- paste0("target_L", 0:(ar_lags - 1L))
  response <- make_level_response(target_level, origins, horizon, mode)
  design <- cbind(`(Intercept)` = 1, X)
  fit <- stats::lm.fit(design, response)
  if (fit$rank < ncol(design) || anyNA(fit$coefficients) || any(!is.finite(fit$coefficients))) {
    stop("원수준 direct AR 회귀행렬이 특이하거나 계수를 추정하지 못했습니다.")
  }
  new_x <- c(1, transformed_target[n - 0:(ar_lags - 1L)])
  names(new_x) <- colnames(design)
  prediction <- sum(new_x * fit$coefficients)

  list(
    prediction = unname(prediction),
    diagnostics = list(
      training_observations = length(response),
      n_features = ar_lags,
      n_selected = ar_lags,
      tuning_parameter = paste0("AR_lags=", ar_lags, ";mode=", mode),
      validation_loss = NA_real_
    ),
    artifact = data.frame(feature = names(fit$coefficients),
                          coefficient = as.numeric(fit$coefficients),
                          stringsAsFactors = FALSE),
    state = state
  )
}

create_cumulative_model_registry <- function(base_registry) {
  if (!is.list(base_registry)) {
    stop("base_registry는 list여야 합니다.")
  }

  required_models <- c("RW", "AR")
  missing_models <- setdiff(required_models, names(base_registry))
  if (length(missing_models) > 0L) {
    stop(
      "누적 목표 registry에 필요한 baseline이 없습니다: ",
      paste(missing_models, collapse = ", ")
    )
  }

  registry <- base_registry
  registry$RW$model_function <- forecast_cumulative_rw_model
  registry$AR$model_function <- forecast_cumulative_ar_model
  registry
}

create_cumulative_rolling_index <- function(
    data,
    target_name,
    target_level,
    target_level_observed,
    transformed_target_valid,
    level_forecast_mode,
    forecast_horizons,
    window_size,
    npred,
    oos_index,
    predictor_lags = 4L,
    allow_imputed_oos_actual = FALSE
) {
  target_level <- as.numeric(target_level)
  target_level_observed <- as.logical(target_level_observed)
  transformed_target_valid <- as.logical(transformed_target_valid)
  level_forecast_mode <- validate_level_forecast_mode(level_forecast_mode)

  if (
    length(target_level) != nrow(data) ||
    length(target_level_observed) != nrow(data) ||
    length(transformed_target_valid) != nrow(data) ||
    anyNA(target_level_observed) || anyNA(transformed_target_valid)
  ) stop("원수준 rolling data와 level/mask 길이가 일치하지 않습니다.")
  if (anyNA(target_level) || any(!is.finite(target_level))) {
    stop("원수준 rolling target level에 결측치 또는 비유한 값이 있습니다.")
  }

  rolling_index <- create_rolling_index(
    data = data,
    target_name = target_name,
    target_eval_exclude = rep(FALSE, nrow(data)),
    forecast_horizons = forecast_horizons,
    window_size = window_size,
    npred = npred,
    oos_index = oos_index
  )

  rolling_index$target_feature_valid <- vapply(
    rolling_index$origin_index,
    function(origin) {
      first_index <- origin - predictor_lags + 1L
      if (first_index < 1L) return(FALSE)
      all(transformed_target_valid[seq.int(first_index, origin)])
    },
    FUN.VALUE = logical(1)
  )
  rolling_index$origin_level <- target_level[rolling_index$origin_index]
  rolling_index$actual_level <- target_level[rolling_index$target_index]
  rolling_index$actual <- vapply(
    seq_len(nrow(rolling_index)),
    function(i) {
      make_level_response(
        level = target_level,
        origins = rolling_index$origin_index[i],
        horizon = rolling_index$horizon[i],
        mode = level_forecast_mode
      )
    },
    FUN.VALUE = numeric(1)
  )
  # 원수준 평가와 RW baseline, percent-scale diagnostics 모두 실제 origin을
  # 사용해야 하므로 mode와 관계없이 origin과 target이 모두 관측된 경우만 평가한다.
  raw_valid <- (
    target_level_observed[rolling_index$target_index] &
      target_level_observed[rolling_index$origin_index]
  )
  rolling_index$evaluation_included <- (
    rolling_index$target_feature_valid &
      if (isTRUE(allow_imputed_oos_actual)) TRUE else raw_valid
  )
  rolling_index$target_mode <- level_forecast_mode

  if (any(!is.finite(rolling_index$origin_level)) ||
      any(!is.finite(rolling_index$actual_level)) ||
      any(!is.finite(rolling_index$actual))) {
    stop("원수준 rolling index에 비유한 실제값이 생성되었습니다.")
  }
  rolling_index
}

augment_cumulative_forecast_scales <- function(forecasts) {
  required_columns <- c("horizon", "origin_level", "actual", "prediction", "target_mode")
  missing_columns <- setdiff(required_columns, names(forecasts))
  if (length(missing_columns) > 0L) {
    stop("원수준 forecast scale 변환에 필요한 열이 없습니다: ", paste(missing_columns, collapse = ", "))
  }
  modes <- unique(as.character(forecasts$target_mode))
  if (length(modes) != 1L) stop("한 forecast table에는 하나의 target_mode만 있어야 합니다.")
  mode <- validate_level_forecast_mode(modes)

  actual_ok <- is.finite(forecasts$actual)
  pred_ok <- is.finite(forecasts$prediction)
  forecasts$actual_level <- NA_real_
  forecasts$forecast_level <- NA_real_
  forecasts$actual_level[actual_ok] <- invert_level_response(
    forecasts$actual[actual_ok], forecasts$origin_level[actual_ok], mode
  )
  forecasts$forecast_level[pred_ok] <- invert_level_response(
    forecasts$prediction[pred_ok], forecasts$origin_level[pred_ok], mode
  )
  forecasts$actual_cumulative_percent <- NA_real_
  forecasts$forecast_cumulative_percent <- NA_real_
  forecasts$actual_annualized_percent <- NA_real_
  forecasts$forecast_annualized_percent <- NA_real_
  forecasts$actual_cumulative_percent[actual_ok] <- level_response_percent_change(
    forecasts$actual[actual_ok], forecasts$origin_level[actual_ok], mode
  )
  forecasts$forecast_cumulative_percent[pred_ok] <- level_response_percent_change(
    forecasts$prediction[pred_ok], forecasts$origin_level[pred_ok], mode
  )
  forecasts$actual_annualized_percent[actual_ok] <- level_response_annualized_percent(
    forecasts$actual[actual_ok], forecasts$origin_level[actual_ok], forecasts$horizon[actual_ok], mode
  )
  forecasts$forecast_annualized_percent[pred_ok] <- level_response_annualized_percent(
    forecasts$prediction[pred_ok], forecasts$origin_level[pred_ok], forecasts$horizon[pred_ok], mode
  )
  forecasts$target_response_error <- forecasts$actual - forecasts$prediction
  forecasts$level_error <- forecasts$actual_level - forecasts$forecast_level
  forecasts$level_squared_error <- forecasts$level_error^2
  forecasts$level_absolute_error <- abs(forecasts$level_error)
  forecasts
}

run_cumulative_model_set <- function(
    models,
    registry,
    data,
    target_level,
    target_level_observed,
    transformed_target_valid,
    level_forecast_mode,
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
  target_level <- as.numeric(target_level)
  target_level_observed <- as.logical(target_level_observed)
  transformed_target_valid <- as.logical(transformed_target_valid)
  level_forecast_mode <- validate_level_forecast_mode(level_forecast_mode)

  if (
    length(target_level) != nrow(data) ||
    length(target_level_observed) != nrow(data) ||
    length(transformed_target_valid) != nrow(data) ||
    anyNA(target_level_observed) || anyNA(transformed_target_valid)
  ) stop("누적 transformed-change / 복원 level backtest 입력 level 또는 mask 길이가 data와 일치하지 않습니다.")

  need_feature_bundle <- any(vapply(
    registry[models], function(x) isTRUE(x$requires_feature_bundle), logical(1)
  ))
  expected_rows <- length(models) * length(forecast_horizons) * npred
  result_list <- vector("list", expected_rows)
  result_position <- 0L
  artifacts <- list()
  states <- setNames(lapply(models, function(x) list()), models)
  require_convergence <- isTRUE(get_config_value(pca_em_settings, "require_convergence", TRUE))

  for (horizon in forecast_horizons) {
    horizon_key <- as.character(horizon)
    for (forecast_number in seq_len(npred)) {
      index_row <- rolling_index[
        rolling_index$horizon == horizon &
          rolling_index$forecast_number == forecast_number,
        , drop = FALSE
      ]
      if (nrow(index_row) != 1L) stop("누적 transformed-change / 복원 level backtest rolling index를 하나로 식별하지 못했습니다.")

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
        require_pca_em_convergence = require_convergence,
        predictor_missing_policy = get_config_value(
          pca_em_settings,
          "predictor_policy",
          NULL
        )
      )

      window_rows <- seq.int(index_row$window_start_index, index_row$window_end_index)
      target_level.window <- target_level[window_rows]
      target_level_observed.window <- target_level_observed[window_rows]
      transformed_target_valid.window <- transformed_target_valid[window_rows]

      feature_bundle <- NULL
      if (need_feature_bundle) {
        feature_bundle <- prepare_cumulative_feature_bundle(
          Y.window = window_info$Y.window,
          target_level.window = target_level.window,
          horizon = horizon,
          predictor_lags = feature_settings$predictor_lags,
          n_factors = feature_settings$n_factors,
          factor_include_target = feature_settings$factor_include_target,
          factor_scale = feature_settings$factor_scale,
          level_forecast_mode = level_forecast_mode,
          target_level_observed.window = target_level_observed.window,
          transformed_target_valid.window = transformed_target_valid.window
        )
      }

      for (model_index in seq_along(models)) {
        model_key <- models[model_index]
        specification <- registry[[model_key]]
        current_state <- states[[model_key]][[horizon_key]]
        context <- list(
          Y.window = window_info$Y.window,
          target_level.window = target_level.window,
          target_level_observed.window = target_level_observed.window,
          transformed_target_valid.window = transformed_target_valid.window,
          target_response_valid.window = transformed_target_valid.window,
          feature_bundle = feature_bundle,
          horizon = horizon,
          forecast_number = forecast_number,
          window_info = window_info,
          target_mode = level_forecast_mode,
          seed = make_model_seed(base_seed + 100000L, model_index, horizon, forecast_number)
        )

        model_error <- NULL
        model_result <- tryCatch(
          specification$model_function(context = context, config = specification$config, state = current_state),
          error = function(e) { model_error <<- conditionMessage(e); NULL }
        )
        if (!is.null(model_result)) {
          validation_error <- validate_model_result(model_result)
          if (!is.null(validation_error)) { model_error <- validation_error; model_result <- NULL }
        }

        if (is.null(model_result)) {
          if (error_policy == "stop") stop(
            "누적 transformed-change 모형 실행 오류 [", specification$label,
            ", h=", horizon, ", forecast=", forecast_number, "]: ", model_error
          )
          prediction <- NA_real_; diagnostics <- list(); status <- "error"; status_message <- model_error
        } else {
          prediction <- as.numeric(model_result$prediction)[1L]
          diagnostics <- if (is.null(model_result$diagnostics)) list() else model_result$diagnostics
          states[[model_key]][[horizon_key]] <- model_result$state
          status <- "ok"; status_message <- NA_character_
          save_this <- save_artifacts == "all" || (save_artifacts == "last_window" && forecast_number == npred)
          if (save_this && !is.null(model_result$artifact)) {
            artifacts[[paste("level", model_key, paste0("h", horizon), paste0("f", forecast_number), sep="__")]] <- model_result$artifact
          }
        }

        result_position <- result_position + 1L
        result_list[[result_position]] <- data.frame(
          target_code = target_name,
          target_name = target_display_name,
          target_mode = level_forecast_mode,
          model_key = model_key,
          model = specification$label,
          model_family = specification$family,
          horizon = horizon,
          forecast_number = forecast_number,
          window_start_date = as.Date(window_info$window_start_date),
          origin_date = as.Date(window_info$origin_date),
          target_date = as.Date(window_info$target_date),
          origin_level = as.numeric(index_row$origin_level),
          actual = as.numeric(index_row$actual),
          prediction = prediction,
          evaluation_included = as.logical(index_row$evaluation_included && status == "ok"),
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
          total_imputed_count = as.integer(
            window_info$short_gap_imputed_count +
              window_info$final_imputed_count
          ),
          missing_after = as.integer(window_info$missing_after),
          em_iterations = as.integer(window_info$em_iterations),
          em_converged = as.logical(window_info$em_converged),
          em_last_change = as.numeric(window_info$em_last_change),
          training_observations = as.integer(get_config_value(diagnostics, "training_observations", NA_integer_)),
          n_features = as.integer(get_config_value(diagnostics, "n_features", NA_integer_)),
          n_selected = as.integer(get_config_value(diagnostics, "n_selected", NA_integer_)),
          tuning_parameter = as.character(get_config_value(diagnostics, "tuning_parameter", NA_character_)),
          validation_loss = as.numeric(get_config_value(diagnostics, "validation_loss", NA_real_)),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }

      if (isTRUE(show_progress) && (forecast_number %% 10L == 0L || forecast_number == npred)) {
        progress_message <- paste0("Cumulative transformed / reconstructed-level backtest: horizon=", horizon,
                                   ", forecast=", forecast_number, "/", npred)
        message(progress_message)
        report_experiment_progress(
          stage = "cumulative_backtest", horizon = horizon,
          forecast_number = forecast_number, forecast_total = npred,
          message = progress_message
        )
      }
    }
  }

  if (result_position != expected_rows || any(vapply(result_list, is.null, logical(1)))) {
    stop("누적 transformed-change backtest 결과가 예상한 개수만큼 생성되지 않았습니다.")
  }
  forecasts <- do.call(rbind, result_list)
  rownames(forecasts) <- NULL
  forecasts$error <- forecasts$actual - forecasts$prediction
  forecasts$squared_error <- forecasts$error^2
  forecasts$absolute_error <- abs(forecasts$error)
  forecasts <- augment_cumulative_forecast_scales(forecasts)

  list(forecasts = forecasts, artifacts = artifacts, final_states = states)
}

summarize_cumulative_forecast_accuracy <- function(forecasts) {
  response_accuracy <- summarize_forecast_accuracy(forecasts)
  modes <- unique(as.character(forecasts$target_mode))
  if (length(modes) != 1L) stop("원수준 accuracy table에는 하나의 target_mode만 있어야 합니다.")
  target_mode <- validate_level_forecast_mode(modes)

  evaluation_data <- forecasts[
    forecasts$status == "ok" & forecasts$evaluation_included &
      is.finite(forecasts$level_error) & is.finite(forecasts$level_squared_error) &
      is.finite(forecasts$level_absolute_error), , drop = FALSE
  ]
  group_key <- interaction(evaluation_data$model, evaluation_data$horizon,
                           drop = TRUE, lex.order = TRUE)
  finite_mean <- function(x) {
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    if (length(x) == 0L) NA_real_ else mean(x)
  }

  level_accuracy <- do.call(rbind, lapply(split(evaluation_data, group_key), function(x) {
    data.frame(
      model = x$model[1L],
      horizon = x$horizon[1L],
      target_mode = x$target_mode[1L],
      level_RMSE = sqrt(mean(x$level_squared_error)),
      level_MAE = mean(x$level_absolute_error),
      level_bias = mean(x$level_error),
      cumulative_percent_MAE = finite_mean(abs(
        x$actual_cumulative_percent - x$forecast_cumulative_percent
      )),
      annualized_percent_MAE = finite_mean(abs(
        x$actual_annualized_percent - x$forecast_annualized_percent
      )),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }))
  rownames(level_accuracy) <- NULL

  names(response_accuracy)[names(response_accuracy) == "RMSE"] <- "target_response_RMSE"
  names(response_accuracy)[names(response_accuracy) == "MAE"] <- "target_response_MAE"
  names(response_accuracy)[names(response_accuracy) == "bias"] <- "target_response_bias"
  names(response_accuracy)[names(response_accuracy) == "correlation"] <- "target_response_correlation"
  output <- merge(response_accuracy, level_accuracy, by = c("model", "horizon"), all.x = TRUE, sort = FALSE)

  output$cumulative_log_RMSE <- if (target_mode == "cumulative_log_change") output$target_response_RMSE else NA_real_
  output$cumulative_log_MAE <- if (target_mode == "cumulative_log_change") output$target_response_MAE else NA_real_
  output$cumulative_log_bias <- if (target_mode == "cumulative_log_change") output$target_response_bias else NA_real_
  output$cumulative_log_correlation <- if (target_mode == "cumulative_log_change") output$target_response_correlation else NA_real_
  output[order(output$horizon, output$level_RMSE, output$model), , drop = FALSE]
}

add_cumulative_benchmark_relative_accuracy <- function(accuracy, benchmark_model = "RW") {
  metric_columns <- c("target_response_RMSE", "target_response_MAE", "level_RMSE", "level_MAE")
  benchmark <- accuracy[accuracy$model == benchmark_model,
                        c("horizon", metric_columns), drop = FALSE]
  expected_horizons <- sort(unique(accuracy$horizon))
  if (nrow(benchmark) != length(expected_horizons) || anyDuplicated(benchmark$horizon) > 0L ||
      !setequal(benchmark$horizon, expected_horizons)) {
    stop("누적 transformed-change / 복원 level benchmark가 모든 horizon에 정확히 한 행씩 존재하지 않습니다.")
  }
  benchmark_metrics <- benchmark[, -1L, drop = FALSE]
  if (any(!is.finite(as.matrix(benchmark_metrics))) || any(as.matrix(benchmark_metrics) <= 0)) {
    stop("누적 transformed-change / 복원 level benchmark 성과지표가 0보다 큰 유한한 값이어야 합니다.")
  }
  names(benchmark)[-1L] <- paste0("benchmark_", names(benchmark)[-1L])
  output <- merge(accuracy, benchmark, by = "horizon", all.x = TRUE, sort = FALSE)
  output$relative_target_response_RMSE <- output$target_response_RMSE / output$benchmark_target_response_RMSE
  output$relative_target_response_MAE <- output$target_response_MAE / output$benchmark_target_response_MAE
  output$relative_level_RMSE <- output$level_RMSE / output$benchmark_level_RMSE
  output$relative_level_MAE <- output$level_MAE / output$benchmark_level_MAE
  output$relative_cumulative_log_RMSE <- ifelse(
    is.finite(output$cumulative_log_RMSE),
    output$relative_target_response_RMSE, NA_real_
  )
  output$relative_cumulative_log_MAE <- ifelse(
    is.finite(output$cumulative_log_MAE),
    output$relative_target_response_MAE, NA_real_
  )
  output[order(output$horizon, output$level_RMSE, output$model), , drop = FALSE]
}

rank_cumulative_models_by_horizon <- function(accuracy) {
  output <- accuracy
  output$level_RMSE_rank <- ave(output$level_RMSE, output$horizon,
                                FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))
  output$target_response_RMSE_rank <- ave(output$target_response_RMSE, output$horizon,
                                          FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))
  output$cumulative_log_RMSE_rank <- ifelse(
    is.finite(output$cumulative_log_RMSE), output$target_response_RMSE_rank, NA_real_
  )
  output[order(output$horizon, output$level_RMSE_rank, output$model), , drop = FALSE]
}

run_forward_base_models <- function(
    models,
    registry,
    data,
    target_level,
    target_level_observed,
    transformed_target_valid,
    level_forecast_mode,
    target_name,
    target_display_name,
    forecast_horizons,
    window_size,
    feature_settings,
    pca_em_settings,
    base_seed = 1L,
    error_policy = c("stop", "record"),
    allow_imputed_origin_level = FALSE
) {
  error_policy <- match.arg(error_policy)
  validate_model_registry(registry, models)
  level_forecast_mode <- validate_level_forecast_mode(level_forecast_mode)
  target_level <- as.numeric(target_level)
  target_level_observed <- as.logical(target_level_observed)
  transformed_target_valid <- as.logical(transformed_target_valid)

  if (
    length(target_level) != nrow(data) ||
    length(target_level_observed) != nrow(data) ||
    length(transformed_target_valid) != nrow(data) ||
    anyNA(target_level_observed) || anyNA(transformed_target_valid)
  ) stop("Forward 원수준 level 또는 mask 길이가 data와 일치하지 않습니다.")
  if (nrow(data) < window_size) stop("Forward forecast를 위한 학습자료가 window_size보다 짧습니다.")

  window_rows <- seq.int(nrow(data) - window_size + 1L, nrow(data))
  Y.window <- as.matrix(data[window_rows, -1L, drop = FALSE])
  storage.mode(Y.window) <- "double"
  target_level.window <- target_level[window_rows]
  target_level_observed.window <- target_level_observed[window_rows]
  transformed_target_valid.window <- transformed_target_valid[window_rows]

  need_feature_bundle <- any(vapply(
    registry[models], function(x) isTRUE(x$requires_feature_bundle), logical(1)
  ))
  Y.window <- prepare_model_window_missing_data(
    Y.window = Y.window,
    prepare_predictors = need_feature_bundle,
    predictor_policy = get_config_value(
      pca_em_settings,
      "predictor_policy",
      NULL
    ),
    pca_em_settings = pca_em_settings
  )

  missing_before <- as.integer(attr(Y.window, "missing_before"))
  short_gap_imputed_count <- as.integer(
    attr(Y.window, "short_gap_imputed_count")
  )
  final_imputed_count <- as.integer(
    attr(Y.window, "final_imputed_count")
  )
  dropped_predictor_count <- as.integer(
    attr(Y.window, "dropped_predictor_count")
  )
  missing_after <- as.integer(attr(Y.window, "missing_after"))

  origin_date <- as.Date(tail(data[[1L]], 1L))
  origin_level <- tail(target_level, 1L)
  origin_observed <- tail(target_level_observed, 1L)
  if (!isTRUE(origin_observed) && !isTRUE(allow_imputed_origin_level)) {
    stop("최신 forecast origin의 원수준 target이 실제 관측값이 아닙니다.")
  }
  predictor_lags <- validate_scalar_integer(
    feature_settings$predictor_lags,
    "predictor_lags",
    1L
  )
  latest_feature_index <- length(transformed_target_valid.window) - 0:(predictor_lags - 1L)
  if (any(latest_feature_index < 1L) || !all(transformed_target_valid.window[latest_feature_index])) {
    stop("최신 forward origin의 target lag에 대체값 영향이 남아 있습니다.")
  }
  window_start_date <- as.Date(data[[1L]][window_rows[1L]])

  output_rows <- list(); artifacts <- list(); row_counter <- 0L
  for (horizon in forecast_horizons) {
    feature_bundle <- NULL
    if (need_feature_bundle) {
      feature_bundle <- prepare_cumulative_feature_bundle(
        Y.window = Y.window,
        target_level.window = target_level.window,
        horizon = horizon,
        predictor_lags = feature_settings$predictor_lags,
        n_factors = feature_settings$n_factors,
        factor_include_target = feature_settings$factor_include_target,
        factor_scale = feature_settings$factor_scale,
        level_forecast_mode = level_forecast_mode,
        target_level_observed.window = target_level_observed.window,
        transformed_target_valid.window = transformed_target_valid.window
      )
    }

    for (model_index in seq_along(models)) {
      model_key <- models[model_index]
      specification <- registry[[model_key]]
      context <- list(
        Y.window = Y.window,
        target_level.window = target_level.window,
        target_level_observed.window = target_level_observed.window,
        transformed_target_valid.window = transformed_target_valid.window,
        target_response_valid.window = transformed_target_valid.window,
        feature_bundle = feature_bundle,
        horizon = horizon,
        forecast_number = 1L,
        forecast_origin = origin_date,
        window_info = list(
          origin_date = origin_date,
          window_start_date = window_start_date,
          target_date = add_months_first_day(origin_date, horizon)
        ),
        target_mode = level_forecast_mode,
        seed = make_model_seed(base_seed + 200000L, model_index, horizon, 1L)
      )

      model_error <- NULL
      model_result <- tryCatch(
        specification$model_function(context = context, config = specification$config, state = NULL),
        error = function(e) { model_error <<- conditionMessage(e); NULL }
      )
      if (!is.null(model_result)) {
        validation_error <- validate_model_result(model_result)
        if (!is.null(validation_error)) { model_error <- validation_error; model_result <- NULL }
      }

      if (is.null(model_result)) {
        if (error_policy == "stop") stop(
          "Forward 모형 실행 오류 [", specification$label, ", h=", horizon, "]: ", model_error
        )
        prediction <- NA_real_; diagnostics <- list(); status <- "error"; status_message <- model_error
      } else {
        prediction <- as.numeric(model_result$prediction)[1L]
        diagnostics <- if (is.null(model_result$diagnostics)) list() else model_result$diagnostics
        status <- "ok"; status_message <- NA_character_
        if (!is.null(model_result$artifact)) {
          artifacts[[paste("forward", model_key, paste0("h", horizon), sep="__")]] <- model_result$artifact
        }
      }

      row_counter <- row_counter + 1L
      output_rows[[row_counter]] <- data.frame(
        target_code = target_name,
        target_name = target_display_name,
        target_mode = level_forecast_mode,
        model_key = model_key,
        model = specification$label,
        model_family = specification$family,
        horizon = as.integer(horizon),
        forecast_origin = origin_date,
        target_date = add_months_first_day(origin_date, horizon),
        window_start_date = window_start_date,
        window_size = as.integer(window_size),
        latest_observed_level = as.numeric(origin_level),
        latest_origin_observed = as.logical(origin_observed),
        prediction = prediction,
        status = status,
        status_message = status_message,
        missing_before = as.integer(missing_before),
        short_gap_imputed_count = as.integer(short_gap_imputed_count),
        final_imputed_count = as.integer(final_imputed_count),
        dropped_predictor_count = as.integer(dropped_predictor_count),
        total_imputed_count = as.integer(
          short_gap_imputed_count + final_imputed_count
        ),
        missing_after = as.integer(missing_after),
        em_iterations = as.integer(attr(Y.window, "em_iterations")),
        em_converged = as.logical(attr(Y.window, "em_converged")),
        em_last_change = as.numeric(attr(Y.window, "em_last_change")),
        training_observations = as.integer(get_config_value(diagnostics, "training_observations", NA_integer_)),
        n_features = as.integer(get_config_value(diagnostics, "n_features", NA_integer_)),
        n_selected = as.integer(get_config_value(diagnostics, "n_selected", NA_integer_)),
        tuning_parameter = as.character(get_config_value(diagnostics, "tuning_parameter", NA_character_)),
        validation_loss = as.numeric(get_config_value(diagnostics, "validation_loss", NA_real_)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  }

  forecasts <- do.call(rbind, output_rows)
  rownames(forecasts) <- NULL
  list(
    forecasts = forecasts,
    artifacts = artifacts,
    origin_date = origin_date,
    origin_level = origin_level,
    origin_observed = origin_observed,
    window_start_date = window_start_date
  )
}

build_forward_ensemble_forecasts <- function(
    base_forward_forecasts,
    cumulative_backtest_forecasts,
    member_models = NULL,
    methods = c("mean", "median", "inverse_rmse"),
    min_members = 2L,
    min_history = 12L,
    weight_epsilon = 1e-8
) {
  methods <- match.arg(
    methods,
    choices = c("mean", "median", "inverse_rmse"),
    several.ok = TRUE
  )
  methods <- unique(methods)

  valid_forward <- base_forward_forecasts[
    base_forward_forecasts$status == "ok" &
      is.finite(base_forward_forecasts$prediction),
    ,
    drop = FALSE
  ]

  if (is.null(member_models)) {
    member_models <- unique(valid_forward$model)
  }
  member_models <- intersect(
    unique(as.character(member_models)),
    unique(valid_forward$model)
  )

  if (length(member_models) < min_members) {
    stop("Forward ensemble에 필요한 기본모형 수가 부족합니다.")
  }

  ensemble_rows <- list()
  weight_rows <- list()
  row_counter <- 0L
  weight_counter <- 0L

  for (horizon in sort(unique(valid_forward$horizon))) {
    current <- valid_forward[
      valid_forward$horizon == horizon &
        valid_forward$model %in% member_models,
      ,
      drop = FALSE
    ]

    if (nrow(current) < min_members) next

    historical <- cumulative_backtest_forecasts[
      cumulative_backtest_forecasts$horizon == horizon &
        cumulative_backtest_forecasts$model %in% current$model &
        cumulative_backtest_forecasts$status == "ok" &
        cumulative_backtest_forecasts$evaluation_included &
        as.Date(cumulative_backtest_forecasts$target_date) <=
          as.Date(current$forecast_origin[1L]) &
        is.finite(cumulative_backtest_forecasts$squared_error),
      ,
      drop = FALSE
    ]

    for (method in methods) {
      weights <- if (method == "median") {
        rep(NA_real_, nrow(current))
      } else {
        rep(1 / nrow(current), nrow(current))
      }
      history_count <- 0L

      if (method == "inverse_rmse") {
        history_by_model <- vapply(
          current$model,
          function(model_name) as.integer(sum(historical$model == model_name)),
          FUN.VALUE = integer(1)
        )
        history_count <- if (length(history_by_model) > 0L) {
          min(history_by_model)
        } else {
          0L
        }

        rmse_by_model <- vapply(
          current$model,
          function(model_name) {
            model_history <- historical[historical$model == model_name, , drop = FALSE]
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

      ensemble_label <- switch(
        method,
        mean = "Ensemble_Mean",
        median = "Ensemble_Median",
        inverse_rmse = "Ensemble_InvRMSE"
      )

      reference <- current[1L, , drop = FALSE]
      reference$model_key <- ensemble_label
      reference$model <- ensemble_label
      reference$model_family <- "Ensemble"
      reference$prediction <- prediction
      reference$n_selected <- nrow(current)
      reference$tuning_parameter <- paste0(
        "members=", nrow(current),
        ";history=", history_count
      )
      reference$training_observations <- NA_integer_
      reference$n_features <- NA_integer_
      reference$validation_loss <- NA_real_
      row_counter <- row_counter + 1L
      ensemble_rows[[row_counter]] <- reference

      for (j in seq_len(nrow(current))) {
        weight_counter <- weight_counter + 1L
        weight_rows[[weight_counter]] <- data.frame(
          ensemble = ensemble_label,
          horizon = horizon,
          forecast_origin = as.Date(reference$forecast_origin),
          target_date = as.Date(reference$target_date),
          member_model = current$model[j],
          weight = weights[j],
          history_count = as.integer(history_count),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
    }
  }

  ensemble_forecasts <- if (length(ensemble_rows) > 0L) {
    do.call(rbind, ensemble_rows)
  } else {
    base_forward_forecasts[0, , drop = FALSE]
  }
  rownames(ensemble_forecasts) <- NULL

  weights <- if (length(weight_rows) > 0L) {
    do.call(rbind, weight_rows)
  } else {
    data.frame()
  }

  list(
    forecasts = ensemble_forecasts,
    weights = weights,
    members = member_models
  )
}

finalize_forward_forecasts <- function(
    forward_forecasts,
    cumulative_rankings,
    latest_origin_imputed = FALSE,
    execution_profile = NA_character_
) {
  output <- forward_forecasts
  modes <- unique(as.character(output$target_mode))
  if (length(modes) != 1L) stop("Forward table에는 하나의 target_mode만 있어야 합니다.")
  mode <- validate_level_forecast_mode(modes)

  output$target_response_forecast <- output$prediction
  output$cumulative_log_forecast <- if (mode == "cumulative_log_change") output$prediction else NA_real_
  output$cumulative_percent_forecast <- NA_real_
  output$annualized_percent_forecast <- NA_real_
  output$raw_level_forecast <- NA_real_
  valid <- is.finite(output$prediction)
  output$raw_level_forecast[valid] <- invert_level_response(
    output$prediction[valid], output$latest_observed_level[valid], mode
  )
  output$cumulative_percent_forecast[valid] <- level_response_percent_change(
    output$prediction[valid], output$latest_observed_level[valid], mode
  )
  output$annualized_percent_forecast[valid] <- level_response_annualized_percent(
    output$prediction[valid], output$latest_observed_level[valid], output$horizon[valid], mode
  )
  output$origin_value_status <- if (isTRUE(latest_origin_imputed)) "Imputed" else "Observed"
  output$level_forecast_formula <- level_mode_formula_label(mode)
  output$execution_profile <- execution_profile

  ranking_columns <- intersect(
    c(
      "model", "horizon", "target_mode",
      "level_RMSE", "relative_level_RMSE", "level_RMSE_rank",
      "target_response_RMSE", "relative_target_response_RMSE",
      "target_response_RMSE_rank", "cumulative_log_RMSE",
      "cumulative_log_RMSE_rank"
    ),
    names(cumulative_rankings)
  )
  if (all(c("model", "horizon") %in% ranking_columns)) {
    merge_keys <- c("model", "horizon")
    if (
      "target_mode" %in% ranking_columns &&
        "target_mode" %in% names(output)
    ) {
      merge_keys <- c(merge_keys, "target_mode")
    }
    output <- merge(
      output,
      cumulative_rankings[, ranking_columns, drop = FALSE],
      by = merge_keys,
      all.x = TRUE,
      sort = FALSE
    )
  }

  preferred_order <- c(
    "target_code", "target_name", "target_mode", "forecast_origin",
    "target_date", "horizon", "model_key", "model", "model_family",
    "latest_observed_level", "origin_value_status", "target_response_forecast",
    "cumulative_log_forecast", "cumulative_percent_forecast",
    "annualized_percent_forecast", "raw_level_forecast",
    "level_forecast_formula", "level_RMSE", "relative_level_RMSE",
    "level_RMSE_rank", "target_response_RMSE", "relative_target_response_RMSE",
    "target_response_RMSE_rank", "cumulative_log_RMSE",
    "cumulative_log_RMSE_rank", "window_start_date", "window_size",
    "training_observations", "n_features", "n_selected", "tuning_parameter",
    "validation_loss", "missing_before", "short_gap_imputed_count",
    "final_imputed_count", "dropped_predictor_count",
    "total_imputed_count", "missing_after",
    "em_iterations", "em_converged", "em_last_change", "execution_profile",
    "status", "status_message"
  )
  existing_order <- intersect(preferred_order, names(output))
  remaining <- setdiff(names(output), existing_order)
  output <- output[, c(existing_order, remaining), drop = FALSE]
  output[order(output$horizon, output$level_RMSE_rank, output$model), , drop = FALSE]
}

