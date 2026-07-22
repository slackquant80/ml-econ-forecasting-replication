###############################################################################
### Random Walk Baseline
###############################################################################

forecast_rw_model <- function(context, config = list(), state = NULL) {
  y <- as.numeric(context$Y.window[, 1L])

  if (anyNA(y) || any(!is.finite(y))) {
    stop("Random Walk 목표변수에 결측치 또는 비유한 값이 있습니다.")
  }

  list(
    prediction = unname(tail(y, 1L)),
    diagnostics = list(
      training_observations = length(y),
      n_features = 1L,
      n_selected = 1L,
      tuning_parameter = NA_character_,
      validation_loss = NA_real_
    ),
    artifact = NULL,
    state = state
  )
}
