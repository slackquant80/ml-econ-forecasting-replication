###############################################################################
### tcode-Aware Original-Level Target and Inverse-Transformation Utilities
###############################################################################

validate_level_forecast_mode <- function(mode) {
  allowed <- c(
    "direct_level",
    "direct_log_level",
    "cumulative_arithmetic_change",
    "cumulative_log_change",
    "cumulative_percent_change"
  )

  if (
    !is.character(mode) || length(mode) != 1L || is.na(mode) ||
      !(mode %in% allowed)
  ) {
    stop(
      "level_forecast_mode는 다음 중 하나여야 합니다: ",
      paste(allowed, collapse = ", ")
    )
  }

  mode
}

# The stationary monthly track continues to use the selected tcode exactly.
# This mapping defines a separate horizon-specific response that can be
# inverted from one direct h-step prediction without recursively forecasting
# the intermediate months.
target_level_mode_from_tcode <- function(tcode) {
  tcode <- validate_scalar_integer(tcode, "tcode", minimum = 1L)
  if (!(tcode %in% 1:7)) stop("tcode는 1~7이어야 합니다.")

  switch(
    as.character(tcode),
    `1` = "direct_level",
    `2` = "cumulative_arithmetic_change",
    `3` = "cumulative_arithmetic_change",
    `4` = "direct_log_level",
    `5` = "cumulative_log_change",
    `6` = "cumulative_log_change",
    `7` = "cumulative_percent_change"
  )
}

level_mode_uses_origin <- function(mode) {
  mode <- validate_level_forecast_mode(mode)
  mode %in% c(
    "cumulative_arithmetic_change",
    "cumulative_log_change",
    "cumulative_percent_change"
  )
}

level_mode_requires_positive_level <- function(mode) {
  mode <- validate_level_forecast_mode(mode)
  mode %in% c("direct_log_level", "cumulative_log_change")
}

level_mode_requires_nonzero_origin <- function(mode) {
  identical(validate_level_forecast_mode(mode), "cumulative_percent_change")
}

level_mode_formula_label <- function(mode) {
  mode <- validate_level_forecast_mode(mode)
  switch(
    mode,
    direct_level = "forecast_level = predicted_future_level",
    direct_log_level = "forecast_level = exp(predicted_future_log_level)",
    cumulative_arithmetic_change = "forecast_level = origin_level + predicted_change",
    cumulative_log_change = "forecast_level = origin_level * exp(predicted_log_change)",
    cumulative_percent_change = "forecast_level = origin_level * (1 + predicted_decimal_change)"
  )
}

level_mode_supported_by_data <- function(level, mode) {
  level <- as.numeric(level)
  mode <- validate_level_forecast_mode(mode)
  observed <- level[is.finite(level)]
  if (length(observed) == 0L) return(FALSE)

  if (level_mode_requires_positive_level(mode)) return(all(observed > 0))
  if (level_mode_requires_nonzero_origin(mode)) return(all(observed != 0))
  TRUE
}

make_level_response <- function(level, origins, horizon, mode) {
  level <- as.numeric(level)
  origins <- as.integer(origins)
  horizon <- validate_scalar_integer(horizon, "horizon", minimum = 1L)
  mode <- validate_level_forecast_mode(mode)

  future_index <- origins + horizon
  if (
    length(origins) < 1L || anyNA(origins) || any(origins < 1L) ||
      any(future_index > length(level))
  ) {
    stop("원수준 response 인덱스가 level 범위를 벗어났습니다.")
  }

  origin_level <- level[origins]
  future_level <- level[future_index]

  if (anyNA(future_level) || any(!is.finite(future_level))) {
    stop("미래 원수준 response에 결측치 또는 비유한 값이 있습니다.")
  }
  if (
    level_mode_uses_origin(mode) &&
      (anyNA(origin_level) || any(!is.finite(origin_level)))
  ) {
    stop("원수준 response의 origin level에 결측치 또는 비유한 값이 있습니다.")
  }

  if (level_mode_requires_positive_level(mode)) {
    used <- if (identical(mode, "direct_log_level")) {
      future_level
    } else {
      c(origin_level, future_level)
    }
    if (any(used <= 0)) stop("로그 기반 원수준 response에 0 이하의 값이 있습니다.")
  }
  if (level_mode_requires_nonzero_origin(mode) && any(origin_level == 0)) {
    stop("누적 비율변화 response의 origin level에 0이 있습니다.")
  }

  switch(
    mode,
    direct_level = future_level,
    direct_log_level = log(future_level),
    cumulative_arithmetic_change = future_level - origin_level,
    cumulative_log_change = log(future_level) - log(origin_level),
    cumulative_percent_change = future_level / origin_level - 1
  )
}

invert_level_response <- function(prediction, origin_level = NA_real_, mode) {
  prediction <- as.numeric(prediction)
  origin_level <- as.numeric(origin_level)
  mode <- validate_level_forecast_mode(mode)

  if (length(prediction) < 1L || anyNA(prediction) || any(!is.finite(prediction))) {
    stop("역변환할 prediction에 결측치 또는 비유한 값이 있습니다.")
  }

  if (level_mode_uses_origin(mode)) {
    if (length(origin_level) == 1L && length(prediction) > 1L) {
      origin_level <- rep(origin_level, length(prediction))
    }
    if (length(origin_level) != length(prediction)) {
      stop("origin_level과 prediction 길이가 일치하지 않습니다.")
    }
    if (anyNA(origin_level) || any(!is.finite(origin_level))) {
      stop("역변환 origin_level에 결측치 또는 비유한 값이 있습니다.")
    }
  }

  if (
    identical(mode, "cumulative_log_change") &&
      any(origin_level <= 0)
  ) {
    stop("로그 기반 역변환 origin_level에 0 이하의 값이 있습니다.")
  }
  if (level_mode_requires_nonzero_origin(mode) && any(origin_level == 0)) {
    stop("누적 비율변화 역변환 origin_level에 0이 있습니다.")
  }

  output <- switch(
    mode,
    direct_level = prediction,
    direct_log_level = exp(prediction),
    cumulative_arithmetic_change = origin_level + prediction,
    cumulative_log_change = origin_level * exp(prediction),
    cumulative_percent_change = origin_level * (1 + prediction)
  )

  if (any(!is.finite(output))) stop("원수준 역변환 결과에 비유한 값이 있습니다.")
  output
}

level_response_percent_change <- function(response, origin_level, mode) {
  response <- as.numeric(response)
  origin_level <- as.numeric(origin_level)
  mode <- validate_level_forecast_mode(mode)
  level_forecast <- invert_level_response(response, origin_level, mode)

  if (length(origin_level) == 1L && length(level_forecast) > 1L) {
    origin_level <- rep(origin_level, length(level_forecast))
  }
  if (length(origin_level) != length(level_forecast)) {
    stop("origin_level과 response 길이가 일치하지 않습니다.")
  }

  output <- rep(NA_real_, length(level_forecast))
  valid <- is.finite(origin_level) & origin_level != 0 & is.finite(level_forecast)
  output[valid] <- 100 * (level_forecast[valid] / origin_level[valid] - 1)
  output
}

level_response_annualized_percent <- function(response, origin_level, horizon, mode) {
  percent_change <- level_response_percent_change(response, origin_level, mode)
  ratio <- 1 + percent_change / 100
  horizon <- as.numeric(horizon)

  if (length(horizon) == 1L && length(ratio) > 1L) {
    horizon <- rep(horizon, length(ratio))
  }
  if (length(horizon) != length(ratio)) {
    stop("horizon과 response 길이가 일치하지 않습니다.")
  }

  output <- rep(NA_real_, length(ratio))
  valid <- is.finite(ratio) & ratio > 0 & is.finite(horizon) & horizon > 0
  output[valid] <- 100 * (ratio[valid]^(12 / horizon[valid]) - 1)
  output
}

make_level_training_valid <- function(
    observed_level,
    origins,
    horizon,
    mode,
    transformed_target_valid = NULL,
    predictor_lags = 1L
) {
  observed_level <- as.logical(observed_level)
  origins <- as.integer(origins)
  horizon <- validate_scalar_integer(horizon, "horizon", minimum = 1L)
  mode <- validate_level_forecast_mode(mode)
  predictor_lags <- validate_scalar_integer(
    predictor_lags,
    "predictor_lags",
    minimum = 1L
  )

  if (anyNA(observed_level)) stop("observed_level mask에 결측치가 있습니다.")

  future_index <- origins + horizon
  valid <- observed_level[future_index]

  # Even direct-level/log-level modes retain the observed origin requirement for
  # clean level-error and percent-change diagnostics and a common RW benchmark.
  valid <- valid & observed_level[origins]

  if (!is.null(transformed_target_valid)) {
    transformed_target_valid <- as.logical(transformed_target_valid)
    if (
      length(transformed_target_valid) != length(observed_level) ||
        anyNA(transformed_target_valid)
    ) {
      stop("transformed_target_valid mask가 level과 일치하지 않습니다.")
    }

    feature_valid <- vapply(
      origins,
      function(origin) {
        first_index <- origin - predictor_lags + 1L
        if (first_index < 1L) return(FALSE)
        all(transformed_target_valid[seq.int(first_index, origin)])
      },
      FUN.VALUE = logical(1)
    )
    valid <- valid & feature_valid
  }

  valid
}
