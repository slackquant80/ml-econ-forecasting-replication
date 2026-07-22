###############################################################################
### Ridge, LASSO and Elastic Net
###############################################################################

select_glmnet_lambda_time_holdout <- function(X, y, alpha, config) {
  check_required_packages("glmnet")

  validation_fraction <- validate_probability(
    get_config_value(config, "validation_fraction", 0.20),
    "glmnet validation_fraction",
    allow_zero = FALSE,
    allow_one = FALSE
  )
  min_validation <- validate_scalar_integer(
    get_config_value(config, "min_validation", 12L),
    "glmnet min_validation",
    2L
  )
  lambda_rule <- get_config_value(config, "lambda_rule", "min")
  standardize <- isTRUE(get_config_value(config, "standardize", TRUE))
  intercept <- isTRUE(get_config_value(config, "intercept", TRUE))

  if (!(lambda_rule %in% c("min", "1se"))) {
    stop("glmnet lambda_rule은 'min' 또는 '1se'여야 합니다.")
  }

  n <- nrow(X)
  minimum_training <- 20L
  if (n <= minimum_training + 1L) {
    stop("glmnet 시간순 validation을 위한 학습 관측치가 부족합니다.")
  }

  validation_size <- max(
    min_validation,
    as.integer(floor(n * validation_fraction))
  )
  validation_size <- min(validation_size, n - minimum_training)

  if (validation_size < 2L) {
    stop("glmnet 시간순 validation 구간을 확보할 수 없습니다.")
  }

  train_index <- seq_len(n - validation_size)
  validation_index <- (n - validation_size + 1L):n

  path_fit <- glmnet::glmnet(
    x = X[train_index, , drop = FALSE],
    y = y[train_index],
    alpha = alpha,
    standardize = standardize,
    intercept = intercept,
    family = "gaussian"
  )

  validation_prediction <- stats::predict(
    path_fit,
    newx = X[validation_index, , drop = FALSE],
    type = "response"
  )

  squared_errors <- sweep(
    validation_prediction,
    1L,
    y[validation_index],
    "-"
  )^2

  mse_by_lambda <- colMeans(squared_errors)
  if (any(!is.finite(mse_by_lambda))) {
    stop("glmnet validation loss에 비유한 값이 생성되었습니다.")
  }

  best_index <- which.min(mse_by_lambda)

  if (lambda_rule == "1se") {
    se_best <- stats::sd(squared_errors[, best_index]) /
      sqrt(nrow(squared_errors))
    threshold <- mse_by_lambda[best_index] + se_best
    eligible <- which(mse_by_lambda <= threshold)
    # glmnet lambda는 큰 값에서 작은 값 순서이므로 첫 번째가 가장 보수적이다.
    best_index <- eligible[1L]
  }

  list(
    lambda = path_fit$lambda[best_index],
    validation_mse = mse_by_lambda[best_index],
    validation_size = validation_size,
    lambda_rule = lambda_rule,
    standardize = standardize,
    intercept = intercept
  )
}

forecast_glmnet_model <- function(context, config = list(), state = NULL) {
  check_required_packages("glmnet")

  alpha <- validate_scalar_numeric(
    get_config_value(config, "alpha", 1),
    "glmnet alpha",
    minimum = 0,
    maximum = 1
  )

  design <- context$feature_bundle$full_design
  X <- design$X
  y <- design$y
  new_x <- matrix(design$new_x, nrow = 1L)
  colnames(new_x) <- colnames(X)

  selection <- select_glmnet_lambda_time_holdout(
    X = X,
    y = y,
    alpha = alpha,
    config = config
  )

  final_fit <- glmnet::glmnet(
    x = X,
    y = y,
    alpha = alpha,
    lambda = selection$lambda,
    standardize = selection$standardize,
    intercept = selection$intercept,
    family = "gaussian"
  )

  prediction <- as.numeric(
    stats::predict(final_fit, newx = new_x, s = selection$lambda)
  )

  coefficients <- as.matrix(
    stats::coef(final_fit, s = selection$lambda)
  )
  coefficient_table <- data.frame(
    feature = rownames(coefficients),
    coefficient = as.numeric(coefficients[, 1L]),
    stringsAsFactors = FALSE
  )

  selected_count <- sum(
    coefficient_table$feature != "(Intercept)" &
      abs(coefficient_table$coefficient) > sqrt(.Machine$double.eps)
  )

  list(
    prediction = prediction,
    diagnostics = list(
      training_observations = design$training_observations,
      n_features = ncol(X),
      n_selected = selected_count,
      tuning_parameter = format(selection$lambda, digits = 8L),
      validation_loss = selection$validation_mse
    ),
    artifact = coefficient_table,
    state = state
  )
}
