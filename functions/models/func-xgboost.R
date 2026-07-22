###############################################################################
### XGBoost
###############################################################################

xgb_train_version_compatible <- function(
    params,
    data,
    nrounds,
    evaluation_data = NULL,
    early_stopping_rounds = NULL,
    verbose = 0L
) {
  train_arguments <- list(
    params = params,
    data = data,
    nrounds = nrounds,
    verbose = verbose
  )

  if (!is.null(evaluation_data)) {
    formal_names <- names(formals(xgboost::xgb.train))

    if ("evals" %in% formal_names) {
      train_arguments$evals <- evaluation_data
    } else if ("watchlist" %in% formal_names) {
      # XGBoost 1.x 호환
      train_arguments$watchlist <- evaluation_data
    } else {
      stop("현재 xgboost::xgb.train()에서 평가자료 인수를 확인할 수 없습니다.")
    }
  }

  if (!is.null(early_stopping_rounds)) {
    formal_names <- names(formals(xgboost::xgb.train))

    if (!("early_stopping_rounds" %in% formal_names)) {
      stop(
        "현재 xgboost::xgb.train()에서 early_stopping_rounds 인수를 ",
        "확인할 수 없습니다. xgboost 패키지 버전을 확인하세요."
      )
    }

    train_arguments$early_stopping_rounds <- early_stopping_rounds
  }

  do.call(xgboost::xgb.train, train_arguments)
}

get_xgb_evaluation_log <- function(model) {
  evaluation_log <- attr(model, "evaluation_log", exact = TRUE)

  if (is.null(evaluation_log)) {
    evaluation_log <- tryCatch(
      model[["evaluation_log"]],
      error = function(e) NULL
    )
  }

  evaluation_log
}

get_xgb_best_iteration <- function(evaluation_log, metric_pattern) {
  if (is.null(evaluation_log) || nrow(evaluation_log) < 1L) {
    return(list(iteration = NA_integer_, loss = NA_real_))
  }

  metric_column <- grep(
    metric_pattern,
    names(evaluation_log),
    value = TRUE,
    ignore.case = TRUE
  )

  if (length(metric_column) < 1L) {
    return(list(iteration = NA_integer_, loss = NA_real_))
  }

  metric_values <- as.numeric(evaluation_log[[metric_column[1L]]])
  finite_rows <- which(is.finite(metric_values))

  if (length(finite_rows) < 1L) {
    return(list(iteration = NA_integer_, loss = NA_real_))
  }

  best_row <- finite_rows[which.min(metric_values[finite_rows])]

  iteration_column <- intersect(
    c("iter", "iteration"),
    names(evaluation_log)
  )

  if (length(iteration_column) > 0L) {
    iteration_values <- as.integer(evaluation_log[[iteration_column[1L]]])
    best_iteration <- iteration_values[best_row]

    # 일부 구버전 기록이 0부터 시작하는 경우를 보정한다.
    if (min(iteration_values, na.rm = TRUE) == 0L) {
      best_iteration <- best_iteration + 1L
    }
  } else {
    best_iteration <- best_row
  }

  list(
    iteration = max(1L, as.integer(best_iteration)),
    loss = metric_values[best_row]
  )
}

forecast_xgboost_model <- function(context, config = list(), state = NULL) {
  check_required_packages("xgboost")

  design <- context$feature_bundle$full_design
  X <- design$X
  y <- design$y
  n <- nrow(X)

  validation_fraction <- validate_probability(
    get_config_value(config, "validation_fraction", 0.20),
    "XGBoost validation_fraction",
    allow_zero = FALSE,
    allow_one = FALSE
  )
  min_validation <- validate_scalar_integer(
    get_config_value(config, "min_validation", 12L),
    "XGBoost min_validation",
    2L
  )

  minimum_training <- 20L
  if (n <= minimum_training + 1L) {
    stop("XGBoost 시간순 validation을 위한 학습 관측치가 부족합니다.")
  }

  validation_size <- max(
    min_validation,
    as.integer(floor(n * validation_fraction))
  )
  validation_size <- min(validation_size, n - minimum_training)

  if (validation_size < 2L) {
    stop("XGBoost 시간순 validation 구간을 확보할 수 없습니다.")
  }

  train_index <- seq_len(n - validation_size)
  validation_index <- (n - validation_size + 1L):n

  eta <- validate_probability(
    get_config_value(config, "eta", 0.05),
    "XGBoost eta",
    allow_zero = FALSE,
    allow_one = TRUE
  )
  max_depth <- validate_scalar_integer(
    get_config_value(config, "max_depth", 4L),
    "XGBoost max_depth",
    1L
  )
  min_child_weight <- validate_scalar_numeric(
    get_config_value(config, "min_child_weight", 1),
    "XGBoost min_child_weight",
    minimum = 0
  )
  subsample <- validate_probability(
    get_config_value(config, "subsample", 0.90),
    "XGBoost subsample",
    allow_zero = FALSE,
    allow_one = TRUE
  )
  colsample_bytree <- validate_probability(
    get_config_value(config, "colsample_bytree", 0.75),
    "XGBoost colsample_bytree",
    allow_zero = FALSE,
    allow_one = TRUE
  )
  nthread <- validate_scalar_integer(
    get_config_value(config, "nthread", 1L),
    "XGBoost nthread",
    1L
  )

  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    nthread = nthread,
    tree_method = "hist",
    seed = context$seed
  )

  max_nrounds <- validate_scalar_integer(
    get_config_value(config, "max_nrounds", 500L),
    "XGBoost max_nrounds",
    1L
  )
  early_stopping_rounds <- validate_scalar_integer(
    get_config_value(config, "early_stopping_rounds", 25L),
    "XGBoost early_stopping_rounds",
    1L
  )

  dtrain <- xgboost::xgb.DMatrix(
    data = X[train_index, , drop = FALSE],
    label = y[train_index],
    nthread = nthread
  )
  dvalidation <- xgboost::xgb.DMatrix(
    data = X[validation_index, , drop = FALSE],
    label = y[validation_index],
    nthread = nthread
  )

  set.seed(context$seed)
  tuning_fit <- xgb_train_version_compatible(
    params = params,
    data = dtrain,
    nrounds = max_nrounds,
    evaluation_data = list(validation = dvalidation),
    early_stopping_rounds = early_stopping_rounds,
    verbose = 0L
  )

  validation_log <- get_xgb_evaluation_log(tuning_fit)
  best_result <- get_xgb_best_iteration(
    validation_log,
    metric_pattern = "validation.*rmse"
  )

  best_iteration <- best_result$iteration
  validation_loss <- best_result$loss

  if (is.na(best_iteration) || !is.finite(best_iteration)) {
    # 평가기록을 찾지 못한 예외 상황에서는 실제 수행된 round 수를 사용한다.
    best_iteration <- if (!is.null(validation_log)) {
      max(1L, nrow(validation_log))
    } else {
      max_nrounds
    }
  }
  best_iteration <- min(max_nrounds, max(1L, as.integer(best_iteration)))

  dfull <- xgboost::xgb.DMatrix(
    data = X,
    label = y,
    nthread = nthread
  )
  set.seed(context$seed)
  final_fit <- xgb_train_version_compatible(
    params = params,
    data = dfull,
    nrounds = best_iteration,
    evaluation_data = NULL,
    early_stopping_rounds = NULL,
    verbose = 0L
  )

  new_x <- matrix(
    design$new_x,
    nrow = 1L,
    dimnames = list(NULL, colnames(X))
  )
  dnew <- xgboost::xgb.DMatrix(data = new_x, nthread = nthread)
  prediction <- as.numeric(stats::predict(final_fit, dnew))

  importance_table <- xgboost::xgb.importance(
    feature_names = colnames(X),
    model = final_fit
  )
  top_n <- validate_scalar_integer(
    get_config_value(config, "importance_top_n", 20L),
    "XGBoost importance_top_n",
    1L
  )
  importance_table <- head(importance_table, top_n)

  list(
    prediction = prediction,
    diagnostics = list(
      training_observations = design$training_observations,
      n_features = ncol(X),
      n_selected = ncol(X),
      tuning_parameter = paste0("nrounds=", best_iteration),
      validation_loss = validation_loss
    ),
    artifact = importance_table,
    state = state
  )
}
