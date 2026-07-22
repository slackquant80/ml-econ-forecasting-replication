###############################################################################
### Periodically Refreshed Boruta Random Forest
###############################################################################

resolve_boruta_origin_date <- function(context) {
  origin_date <- NULL
  if (
    is.list(context$window_info) &&
      length(context$window_info$origin_date) == 1L
  ) {
    origin_date <- context$window_info$origin_date
  } else if (length(context$forecast_origin) == 1L) {
    origin_date <- context$forecast_origin
  }

  origin_date <- as.Date(origin_date)
  if (length(origin_date) != 1L || is.na(origin_date)) {
    stop("Boruta selection history에 기록할 forecast origin date가 없습니다.")
  }
  origin_date
}

make_boruta_selection_history_rows <- function(
    context,
    selected_features,
    selection_source,
    max_runs
) {
  selected_features <- as.character(selected_features)
  selection_count <- length(selected_features)
  if (selection_count < 1L) {
    stop("Boruta selection history에 기록할 선택 변수가 없습니다.")
  }

  origin_date <- resolve_boruta_origin_date(context)
  data.frame(
    horizon = rep.int(as.integer(context$horizon), selection_count),
    forecast_number = rep.int(
      as.integer(context$forecast_number), selection_count
    ),
    origin_date = rep(origin_date, selection_count),
    seed = rep.int(as.integer(context$seed), selection_count),
    selection_source = rep.int(as.character(selection_source), selection_count),
    n_selected = rep.int(as.integer(selection_count), selection_count),
    max_runs = rep.int(as.integer(max_runs), selection_count),
    feature = selected_features,
    stringsAsFactors = FALSE
  )
}

forecast_boruta_rf_model <- function(context, config = list(), state = NULL) {
  check_required_packages(c("Boruta", "randomForest"))

  design <- context$feature_bundle$full_design
  X <- design$X
  y <- design$y

  selection_frequency <- validate_scalar_integer(
    get_config_value(config, "selection_frequency", 12L),
    "Boruta selection_frequency",
    1L
  )
  max_runs <- validate_scalar_integer(
    get_config_value(config, "max_runs", 50L),
    "Boruta max_runs",
    10L
  )

  refresh_selection <- (
    is.null(state) ||
      is.null(state$selected_features) ||
      ((context$forecast_number - 1L) %% selection_frequency == 0L)
  )

  boruta_stats <- NULL

  if (refresh_selection) {
    previous_history <- if (
      is.list(state) &&
        is.data.frame(state$selection_history)
    ) {
      state$selection_history
    } else {
      data.frame()
    }

    set.seed(context$seed)
    boruta_fit <- Boruta::Boruta(
      x = as.data.frame(X, check.names = FALSE),
      y = y,
      maxRuns = max_runs,
      doTrace = 0,
      holdHistory = TRUE
    )
    boruta_fit <- Boruta::TentativeRoughFix(boruta_fit)
    selected_features <- Boruta::getSelectedAttributes(
      boruta_fit,
      withTentative = FALSE
    )
    boruta_stats <- Boruta::attStats(boruta_fit)
    selection_source <- "boruta_confirmed"

    if (length(selected_features) == 0L) {
      fallback_features <- validate_scalar_integer(
        get_config_value(config, "fallback_features", 10L),
        "Boruta fallback_features",
        1L
      )
      variance_rank <- order(
        apply(X, 2L, stats::var),
        decreasing = TRUE
      )
      selected_features <- colnames(X)[
        head(variance_rank, min(fallback_features, ncol(X)))
      ]
      selection_source <- "variance_fallback"
    }

    selection_history <- make_boruta_selection_history_rows(
      context = context,
      selected_features = selected_features,
      selection_source = selection_source,
      max_runs = max_runs
    )
    if (nrow(previous_history) > 0L) {
      selection_history <- rbind(previous_history, selection_history)
    }
    rownames(selection_history) <- NULL

    state <- list(
      selected_features = selected_features,
      last_refresh = context$forecast_number,
      last_seed = context$seed,
      selection_source = selection_source,
      fallback_used = identical(selection_source, "variance_fallback"),
      max_runs = max_runs,
      boruta_stats = boruta_stats,
      selection_history = selection_history
    )
  }

  selected_features <- intersect(state$selected_features, colnames(X))
  if (length(selected_features) == 0L) {
    stop("Boruta-RF에 사용할 선택 변수가 없습니다.")
  }
  state$selected_features <- selected_features

  X_selected <- X[, selected_features, drop = FALSE]
  ntree <- validate_scalar_integer(
    get_config_value(config, "ntree", 500L), "Boruta-RF ntree", 1L
  )
  nodesize <- validate_scalar_integer(
    get_config_value(config, "nodesize", 5L), "Boruta-RF nodesize", 1L
  )
  mtry <- get_config_value(config, "mtry", NULL)
  if (is.null(mtry)) mtry <- max(1L, floor(sqrt(ncol(X_selected))))
  mtry <- min(
    validate_scalar_integer(mtry, "Boruta-RF mtry", 1L),
    ncol(X_selected)
  )

  set.seed(context$seed)
  rf_fit <- randomForest::randomForest(
    x = X_selected,
    y = y,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize,
    importance = TRUE
  )

  new_x <- matrix(
    design$new_x[selected_features],
    nrow = 1L,
    dimnames = list(NULL, selected_features)
  )
  prediction <- as.numeric(stats::predict(rf_fit, new_x))

  importance_matrix <- as.matrix(randomForest::importance(rf_fit))
  importance_column <- if ("%IncMSE" %in% colnames(importance_matrix)) {
    "%IncMSE"
  } else {
    colnames(importance_matrix)[ncol(importance_matrix)]
  }
  importance_table <- data.frame(
    feature = rownames(importance_matrix),
    importance = as.numeric(importance_matrix[, importance_column]),
    stringsAsFactors = FALSE
  )
  importance_table <- importance_table[
    order(importance_table$importance, decreasing = TRUE),
    , drop = FALSE
  ]
  top_n <- validate_scalar_integer(
    get_config_value(config, "importance_top_n", 20L),
    "Boruta-RF importance_top_n",
    1L
  )
  importance_table <- head(importance_table, top_n)
  importance_table$selection_refreshed <- refresh_selection
  importance_table$selection_source <- as.character(
    state$selection_source %||% "unknown"
  )
  importance_table$last_refresh <- as.integer(
    state$last_refresh %||% NA_integer_
  )

  list(
    prediction = prediction,
    diagnostics = list(
      training_observations = design$training_observations,
      n_features = ncol(X),
      n_selected = length(selected_features),
      tuning_parameter = paste0(
        "ntree=", ntree,
        ";refresh=", refresh_selection,
        ";last_refresh=", state$last_refresh,
        ";source=", state$selection_source %||% "unknown"
      ),
      validation_loss = NA_real_
    ),
    artifact = importance_table,
    state = state
  )
}
