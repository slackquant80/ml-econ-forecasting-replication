###############################################################################
### PCA Factor Regression
###############################################################################

forecast_factor_model <- function(context, config = list(), state = NULL) {
  design <- context$feature_bundle$factor_design
  factor_panel <- context$feature_bundle$factor_panel
  max_lags <- validate_scalar_integer(
    get_config_value(config, "max_lags", design$predictor_lags),
    "factor max_lags",
    1L
  )
  max_lags <- min(max_lags, design$predictor_lags)

  X <- design$X
  y <- design$y
  block_size <- ncol(factor_panel)

  candidate_results <- vector("list", max_lags)
  candidate_bic <- rep(Inf, max_lags)

  for (lag_order in seq_len(max_lags)) {
    selected_columns <- seq_len(block_size * lag_order)
    X_selected <- X[, selected_columns, drop = FALSE]
    lm_design <- cbind(`(Intercept)` = 1, X_selected)
    fit <- stats::lm.fit(lm_design, y)

    if (
      fit$rank < ncol(lm_design) ||
      anyNA(fit$coefficients) ||
      any(!is.finite(fit$coefficients))
    ) {
      next
    }

    residuals <- y - as.numeric(lm_design %*% fit$coefficients)
    sse <- sum(residuals^2)
    n <- length(y)
    k <- ncol(lm_design)
    candidate_bic[lag_order] <- n * log(max(sse / n, .Machine$double.eps)) +
      k * log(n)
    candidate_results[[lag_order]] <- list(
      fit = fit,
      selected_columns = selected_columns
    )
  }

  best_lag_order <- which.min(candidate_bic)
  best <- candidate_results[[best_lag_order]]

  if (is.null(best) || !is.finite(candidate_bic[best_lag_order])) {
    stop("Factor regression의 유효한 BIC 모형을 찾지 못했습니다.")
  }

  new_x <- c(
    1,
    design$new_x[best$selected_columns]
  )
  names(new_x) <- c(
    "(Intercept)",
    colnames(X)[best$selected_columns]
  )
  prediction <- sum(new_x * best$fit$coefficients)

  artifact <- data.frame(
    feature = names(new_x),
    coefficient = as.numeric(best$fit$coefficients),
    stringsAsFactors = FALSE
  )

  list(
    prediction = unname(prediction),
    diagnostics = list(
      training_observations = design$training_observations,
      n_features = ncol(X),
      n_selected = length(best$selected_columns),
      tuning_parameter = paste0("factor_lags=", best_lag_order),
      validation_loss = candidate_bic[best_lag_order]
    ),
    artifact = artifact,
    state = state
  )
}
