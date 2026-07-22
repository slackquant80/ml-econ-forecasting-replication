###############################################################################
### Direct Autoregressive Baseline
###############################################################################

forecast_ar_model <- function(context, config = list(), state = NULL) {
  y <- as.numeric(context$Y.window[, 1L])
  horizon <- context$horizon
  ar_lags <- get_config_value(config, "ar_lags", 4L)
  ar_lags <- validate_scalar_integer(ar_lags, "ar_lags", 1L)

  n <- length(y)
  last_training_origin <- n - horizon

  if (last_training_origin < ar_lags) {
    stop("Direct AR 추정을 위한 유효 관측치가 부족합니다.")
  }

  origins <- seq.int(ar_lags, last_training_origin)
  X <- vapply(
    seq_len(ar_lags),
    function(j) y[origins - j + 1L],
    FUN.VALUE = numeric(length(origins))
  )
  if (ar_lags == 1L) X <- matrix(X, ncol = 1L)
  colnames(X) <- paste0("target_L", 0:(ar_lags - 1L))

  response <- y[origins + horizon]
  design <- cbind(`(Intercept)` = 1, X)
  fit <- stats::lm.fit(design, response)

  if (
    fit$rank < ncol(design) ||
    anyNA(fit$coefficients) ||
    any(!is.finite(fit$coefficients))
  ) {
    stop("Direct AR 회귀행렬이 특이하거나 계수를 추정하지 못했습니다.")
  }

  new_x <- c(1, y[n - 0:(ar_lags - 1L)])
  names(new_x) <- colnames(design)
  prediction <- sum(new_x * fit$coefficients)

  artifact <- data.frame(
    feature = names(fit$coefficients),
    coefficient = as.numeric(fit$coefficients),
    stringsAsFactors = FALSE
  )

  list(
    prediction = unname(prediction),
    diagnostics = list(
      training_observations = length(response),
      n_features = ar_lags,
      n_selected = ar_lags,
      tuning_parameter = paste0("AR_lags=", ar_lags),
      validation_loss = NA_real_
    ),
    artifact = artifact,
    state = state
  )
}
