###############################################################################
### Horizon-Aligned Rolling Index and Window Extraction
###############################################################################

create_rolling_index <- function(
    data,
    target_name,
    target_eval_exclude,
    forecast_horizons,
    window_size,
    npred,
    oos_index
) {
  if (!is.data.frame(data)) {
    stop("data는 data.frame이어야 합니다.")
  }

  if (!(target_name %in% names(data))) {
    stop("Rolling index용 데이터에 목표변수가 없습니다: ", target_name)
  }

  if (
    length(target_eval_exclude) != nrow(data) ||
    anyNA(target_eval_exclude)
  ) {
    stop("target_eval_exclude가 data 행 수와 일치하는 결측 없는 벡터여야 합니다.")
  }

  if (!is.logical(target_eval_exclude)) {
    stop("target_eval_exclude는 logical 벡터여야 합니다.")
  }

  window_size <- validate_scalar_integer(window_size, "window_size", 1L)
  npred <- validate_scalar_integer(npred, "npred", 1L)

  if (
    length(forecast_horizons) < 1L ||
    anyNA(forecast_horizons) ||
    any(!is.finite(forecast_horizons)) ||
    any(forecast_horizons != as.integer(forecast_horizons)) ||
    any(forecast_horizons < 1L) ||
    anyDuplicated(forecast_horizons) > 0L
  ) {
    stop("forecast_horizons는 중복 없는 1 이상의 정수 벡터여야 합니다.")
  }

  forecast_horizons <- as.integer(forecast_horizons)
  oos_index <- as.integer(oos_index)

  if (length(oos_index) != npred) {
    stop("oos_index 길이가 npred와 일치하지 않습니다.")
  }

  if (
    anyNA(oos_index) ||
    any(oos_index < 1L) ||
    any(oos_index > nrow(data)) ||
    is.unsorted(oos_index) ||
    anyDuplicated(oos_index) > 0L
  ) {
    stop("oos_index가 정렬된 중복 없는 data 범위 내 인덱스여야 합니다.")
  }

  rolling_index <- do.call(
    rbind,
    lapply(
      forecast_horizons,
      function(horizon) {
        target_index <- oos_index
        origin_index <- target_index - horizon
        window_end_index <- origin_index
        window_start_index <- window_end_index - window_size + 1L

        data.frame(
          horizon = horizon,
          forecast_number = seq_along(target_index),
          window_start_index = window_start_index,
          window_end_index = window_end_index,
          origin_index = origin_index,
          target_index = target_index,
          window_start_date = data[[1L]][window_start_index],
          origin_date = data[[1L]][origin_index],
          target_date = data[[1L]][target_index],
          actual = data[[target_name]][target_index],
          evaluation_included = !target_eval_exclude[target_index],
          row.names = NULL
        )
      }
    )
  )

  rownames(rolling_index) <- NULL

  if (
    any(rolling_index$window_start_index < 1L) ||
    any(rolling_index$target_index > nrow(data))
  ) {
    stop("Rolling window 인덱스가 모델링 표본 범위를 벗어났습니다.")
  }

  if (
    anyNA(rolling_index$actual) ||
    any(!is.finite(rolling_index$actual))
  ) {
    stop("Rolling index의 실제 목표값에 결측치 또는 비유한 값이 있습니다.")
  }

  window_lengths <- (
    rolling_index$window_end_index -
      rolling_index$window_start_index +
      1L
  )

  if (any(window_lengths != window_size)) {
    stop("설정한 window_size와 다른 rolling window가 있습니다.")
  }

  if (
    any(
      rolling_index$target_index -
        rolling_index$origin_index !=
        rolling_index$horizon
    )
  ) {
    stop("Forecast horizon 인덱스 정렬에 문제가 있습니다.")
  }

  forecast_count_by_horizon <- table(rolling_index$horizon)

  if (any(forecast_count_by_horizon != npred)) {
    stop("Horizon별 예측 개수가 npred와 일치하지 않습니다.")
  }

  rolling_index
}


get_forecast_window <- function(
    forecast_number,
    horizon,
    rolling_index,
    data,
    window_size,
    target_name,
    target_display_name = target_name,
    apply_pca_em = TRUE,
    pca_em_factors = 4L,
    pca_em_max_iter = 300L,
    pca_em_tol = 1e-5,
    require_pca_em_convergence = TRUE,
    predictor_missing_policy = NULL
) {
  if (!is.data.frame(rolling_index)) {
    stop("rolling_index는 data.frame이어야 합니다.")
  }

  if (!is.data.frame(data)) {
    stop("data는 data.frame이어야 합니다.")
  }

  if (
    length(horizon) != 1L ||
    !is.finite(horizon)
  ) {
    stop("horizon은 하나의 유한한 값이어야 합니다.")
  }

  horizon <- as.integer(horizon)
  available_horizons <- sort(unique(rolling_index$horizon))

  if (!(horizon %in% available_horizons)) {
    stop("지원하지 않는 forecast horizon입니다: ", horizon)
  }

  horizon_rows <- rolling_index$horizon == horizon
  max_forecast_number <- max(rolling_index$forecast_number[horizon_rows])

  if (
    length(forecast_number) != 1L ||
    !is.finite(forecast_number) ||
    forecast_number < 1L ||
    forecast_number > max_forecast_number
  ) {
    stop(
      "forecast_number는 1부터 ",
      max_forecast_number,
      " 사이여야 합니다."
    )
  }

  logical_arguments <- list(
    apply_pca_em = apply_pca_em,
    require_pca_em_convergence = require_pca_em_convergence
  )

  for (argument_name in names(logical_arguments)) {
    argument_value <- logical_arguments[[argument_name]]
    if (
      length(argument_value) != 1L ||
      is.na(argument_value) ||
      !is.logical(argument_value)
    ) {
      stop(argument_name, "은 하나의 TRUE 또는 FALSE여야 합니다.")
    }
  }

  forecast_number <- as.integer(forecast_number)
  window_size <- validate_scalar_integer(window_size, "window_size", 1L)

  index_row <- rolling_index[
    rolling_index$horizon == horizon &
      rolling_index$forecast_number == forecast_number,
    ,
    drop = FALSE
  ]

  if (nrow(index_row) != 1L) {
    stop("Rolling index를 하나로 식별하지 못했습니다.")
  }

  window_rows <- seq.int(
    from = index_row$window_start_index,
    to = index_row$window_end_index
  )

  Y.window <- as.matrix(
    data[
      window_rows,
      -1L,
      drop = FALSE
    ]
  )

  storage.mode(Y.window) <- "double"

  if (nrow(Y.window) != window_size) {
    stop("추출된 학습 window의 크기가 window_size와 일치하지 않습니다.")
  }

  Y.window <- prepare_model_window_missing_data(
    Y.window = Y.window,
    prepare_predictors = apply_pca_em,
    predictor_policy = predictor_missing_policy,
    pca_em_settings = list(
      n_factors = pca_em_factors,
      max_iter = pca_em_max_iter,
      tol = pca_em_tol,
      require_convergence = require_pca_em_convergence
    )
  )

  missing_before <- as.integer(attr(Y.window, "missing_before"))
  missing_after <- as.integer(attr(Y.window, "missing_after"))

  list(
    Y.window = Y.window,
    target_code = target_name,
    target_name = target_display_name,
    horizon = horizon,
    forecast_number = forecast_number,
    window_start_date = index_row$window_start_date,
    origin_date = index_row$origin_date,
    target_date = index_row$target_date,
    actual = index_row$actual,
    evaluation_included = index_row$evaluation_included,
    missing_before = missing_before,
    short_gap_imputed_count = as.integer(
      attr(Y.window, "short_gap_imputed_count")
    ),
    remaining_before_final = as.integer(
      attr(Y.window, "remaining_before_final")
    ),
    final_imputed_count = as.integer(
      attr(Y.window, "final_imputed_count")
    ),
    missing_after = missing_after,
    dropped_predictors = attr(Y.window, "dropped_predictors"),
    dropped_predictor_count = as.integer(
      attr(Y.window, "dropped_predictor_count")
    ),
    em_iterations = attr(Y.window, "em_iterations"),
    em_converged = attr(Y.window, "em_converged"),
    em_last_change = attr(Y.window, "em_last_change")
  )
}
