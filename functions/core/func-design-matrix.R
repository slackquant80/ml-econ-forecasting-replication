###############################################################################
### Common Direct Multi-Horizon Feature Construction
###############################################################################

make_direct_design <- function(panel, target, horizon, predictor_lags = 4L) {
  panel <- as.matrix(panel)
  storage.mode(panel) <- "double"
  target <- as.numeric(target)

  horizon <- validate_scalar_integer(horizon, "horizon", 1L)
  predictor_lags <- validate_scalar_integer(
    predictor_lags,
    "predictor_lags",
    1L
  )

  if (nrow(panel) != length(target)) {
    stop("설명변수 panel과 목표변수 길이가 일치하지 않습니다.")
  }

  if (anyNA(panel) || any(!is.finite(panel))) {
    stop("Direct 설계행렬 입력 panel에 결측치 또는 비유한 값이 있습니다.")
  }

  if (anyNA(target) || any(!is.finite(target))) {
    stop("Direct 설계행렬의 목표변수에 결측치 또는 비유한 값이 있습니다.")
  }

  if (is.null(colnames(panel))) {
    colnames(panel) <- paste0("X", seq_len(ncol(panel)))
  }

  n <- nrow(panel)
  last_training_origin <- n - horizon

  if (last_training_origin < predictor_lags) {
    stop("Direct forecast 설계행렬을 만들기 위한 관측치가 부족합니다.")
  }

  training_origins <- seq.int(predictor_lags, last_training_origin)
  block_size <- ncol(panel)

  X_blocks <- lapply(
    0:(predictor_lags - 1L),
    function(lag_value) {
      panel[
        training_origins - lag_value,
        ,
        drop = FALSE
      ]
    }
  )

  X <- do.call(cbind, X_blocks)
  colnames(X) <- unlist(
    lapply(
      0:(predictor_lags - 1L),
      function(lag_value) {
        paste0(colnames(panel), "_L", lag_value)
      }
    ),
    use.names = FALSE
  )

  response <- target[training_origins + horizon]

  new_x <- unlist(
    lapply(
      0:(predictor_lags - 1L),
      function(lag_value) {
        panel[n - lag_value, , drop = TRUE]
      }
    ),
    use.names = FALSE
  )
  names(new_x) <- colnames(X)

  if (
    ncol(X) != block_size * predictor_lags ||
    length(new_x) != ncol(X)
  ) {
    stop("Direct forecast 설계행렬의 열 정렬에 문제가 있습니다.")
  }

  list(
    X = X,
    y = response,
    new_x = new_x,
    training_origins = training_origins,
    training_observations = length(response),
    feature_names = colnames(X),
    block_size = block_size,
    predictor_lags = predictor_lags
  )
}

prepare_direct_feature_bundle <- function(
    Y.window,
    horizon,
    predictor_lags = 4L,
    n_factors = 4L,
    factor_include_target = FALSE,
    factor_scale = TRUE
) {
  Y.window <- as.matrix(Y.window)
  storage.mode(Y.window) <- "double"

  if (ncol(Y.window) < 2L) {
    stop("다변량 모형을 위한 설명변수가 없습니다.")
  }

  if (anyNA(Y.window) || any(!is.finite(Y.window))) {
    stop("Feature bundle 입력자료에 결측치 또는 비유한 값이 있습니다.")
  }

  if (is.null(colnames(Y.window))) {
    colnames(Y.window) <- c(
      "target",
      paste0("X", seq_len(ncol(Y.window) - 1L))
    )
  }

  predictor_lags <- validate_scalar_integer(
    predictor_lags,
    "predictor_lags",
    1L
  )
  n_factors <- validate_scalar_integer(n_factors, "n_factors", 1L)

  target <- as.numeric(Y.window[, 1L])
  target_name <- colnames(Y.window)[1L]

  predictor_sd <- apply(
    Y.window[, -1L, drop = FALSE],
    2L,
    stats::sd
  )

  active_predictors <- (
    is.finite(predictor_sd) &
      predictor_sd > sqrt(.Machine$double.eps)
  )

  dropped_predictors <- colnames(Y.window)[-1L][!active_predictors]
  predictor_panel <- Y.window[, -1L, drop = FALSE][
    , active_predictors, drop = FALSE
  ]

  if (ncol(predictor_panel) < 1L) {
    stop("현재 rolling window에 유효한 설명변수가 없습니다.")
  }

  pca_input <- if (isTRUE(factor_include_target)) {
    cbind(Y.window[, 1L, drop = FALSE], predictor_panel)
  } else {
    predictor_panel
  }

  max_factors <- min(nrow(pca_input) - 1L, ncol(pca_input))
  active_factor_count <- min(n_factors, max_factors)

  pca_model <- stats::prcomp(
    pca_input,
    center = TRUE,
    scale. = isTRUE(factor_scale),
    rank. = active_factor_count
  )

  factor_scores <- pca_model$x[
    , seq_len(active_factor_count), drop = FALSE
  ]
  colnames(factor_scores) <- paste0("PC", seq_len(active_factor_count))

  active_Y <- cbind(
    Y.window[, 1L, drop = FALSE],
    predictor_panel
  )
  colnames(active_Y)[1L] <- target_name

  full_panel <- cbind(active_Y, factor_scores)
  factor_panel <- cbind(
    Y.window[, 1L, drop = FALSE],
    factor_scores
  )
  colnames(factor_panel)[1L] <- target_name

  full_design <- make_direct_design(
    panel = full_panel,
    target = target,
    horizon = horizon,
    predictor_lags = predictor_lags
  )

  factor_design <- make_direct_design(
    panel = factor_panel,
    target = target,
    horizon = horizon,
    predictor_lags = predictor_lags
  )

  list(
    target = target,
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
    predictor_lags = predictor_lags
  )
}
