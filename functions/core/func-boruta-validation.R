###############################################################################
### Lightweight Boruta Validation and Stability Summaries
###############################################################################

boruta_track_label <- function(track, target_display_name = NULL) {
  track <- as.character(track)[1L]
  target_display_name <- as.character(target_display_name %||% "Target")[1L]

  if (identical(track, "monthly_transformed")) {
    return("Target-month transformed change")
  }
  if (identical(track, "cumulative_level")) {
    return(paste0(
      target_display_name,
      " cumulative transformed change / reconstructed level"
    ))
  }
  track
}

empty_boruta_selection_history <- function() {
  data.frame(
    target_code = character(0),
    target_name = character(0),
    track = character(0),
    track_label = character(0),
    horizon = integer(0),
    forecast_number = integer(0),
    origin_date = as.Date(character(0)),
    seed = integer(0),
    selection_source = character(0),
    n_selected = integer(0),
    max_runs = integer(0),
    feature = character(0),
    stringsAsFactors = FALSE
  )
}

normalize_boruta_history_columns <- function(
    history,
    state,
    horizon,
    track,
    target_code,
    target_name
) {
  if (!is.data.frame(history) || nrow(history) == 0L) {
    selected <- unique(as.character(state$selected_features %||% character(0)))
    selected <- selected[!is.na(selected) & nzchar(selected)]
    if (length(selected) == 0L) return(empty_boruta_selection_history())

    history <- data.frame(
      horizon = as.integer(horizon),
      forecast_number = as.integer(state$last_refresh %||% NA_integer_),
      origin_date = as.Date(NA_character_),
      seed = as.integer(state$last_seed %||% NA_integer_),
      selection_source = as.character(
        state$selection_source %||% "legacy_selected_state"
      ),
      n_selected = length(selected),
      max_runs = as.integer(state$max_runs %||% NA_integer_),
      feature = selected,
      stringsAsFactors = FALSE
    )
  }

  required_defaults <- list(
    horizon = as.integer(horizon),
    forecast_number = NA_integer_,
    origin_date = as.Date(NA_character_),
    seed = NA_integer_,
    selection_source = "unknown",
    n_selected = NA_integer_,
    max_runs = NA_integer_,
    feature = NA_character_
  )
  for (name in names(required_defaults)) {
    if (!name %in% names(history)) {
      history[[name]] <- rep(required_defaults[[name]], nrow(history))
    }
  }

  history$horizon <- as.integer(history$horizon)
  history$forecast_number <- as.integer(history$forecast_number)
  history$origin_date <- as.Date(history$origin_date)
  history$seed <- as.integer(history$seed)
  history$selection_source <- as.character(history$selection_source)
  history$n_selected <- as.integer(history$n_selected)
  history$max_runs <- as.integer(history$max_runs)
  history$feature <- as.character(history$feature)
  history <- history[!is.na(history$feature) & nzchar(history$feature), , drop = FALSE]
  if (nrow(history) == 0L) return(empty_boruta_selection_history())

  history$target_code <- as.character(target_code)
  history$target_name <- as.character(target_name)
  history$track <- as.character(track)
  history$track_label <- boruta_track_label(track, target_name)

  history <- history[, names(empty_boruta_selection_history()), drop = FALSE]
  rownames(history) <- NULL
  history
}

extract_boruta_selection_history <- function(
    states,
    track,
    target_code,
    target_name
) {
  if (!is.list(states) || length(states) == 0L) {
    return(empty_boruta_selection_history())
  }

  rows <- lapply(names(states), function(horizon_key) {
    state <- states[[horizon_key]]
    if (!is.list(state)) return(empty_boruta_selection_history())
    horizon <- suppressWarnings(as.integer(horizon_key))
    if (is.na(horizon)) horizon <- as.integer(state$horizon %||% NA_integer_)

    normalize_boruta_history_columns(
      history = state$selection_history,
      state = state,
      horizon = horizon,
      track = track,
      target_code = target_code,
      target_name = target_name
    )
  })

  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (length(rows) == 0L) return(empty_boruta_selection_history())

  output <- do.call(rbind, rows)
  output <- output[order(
    output$track,
    output$horizon,
    output$forecast_number,
    output$feature
  ), , drop = FALSE]
  rownames(output) <- NULL
  output
}

empty_boruta_final_selection <- function() {
  data.frame(
    target_code = character(0),
    target_name = character(0),
    track = character(0),
    track_label = character(0),
    horizon = integer(0),
    feature = character(0),
    selection_source = character(0),
    last_refresh = integer(0),
    last_seed = integer(0),
    mean_importance = numeric(0),
    median_importance = numeric(0),
    minimum_importance = numeric(0),
    maximum_importance = numeric(0),
    normalized_hits = numeric(0),
    boruta_decision = character(0),
    stringsAsFactors = FALSE
  )
}

extract_boruta_final_selection <- function(
    states,
    track,
    target_code,
    target_name
) {
  if (!is.list(states) || length(states) == 0L) {
    return(empty_boruta_final_selection())
  }

  rows <- list()
  position <- 0L
  for (horizon_key in names(states)) {
    state <- states[[horizon_key]]
    if (!is.list(state)) next
    selected <- unique(as.character(state$selected_features %||% character(0)))
    selected <- selected[!is.na(selected) & nzchar(selected)]
    if (length(selected) == 0L) next

    stats <- state$boruta_stats
    stats_features <- if (is.data.frame(stats)) rownames(stats) else character(0)
    stats_value <- function(column) {
      if (!is.data.frame(stats) || !column %in% names(stats)) {
        return(rep(NA, length(selected)))
      }
      stats[[column]][match(selected, stats_features)]
    }

    position <- position + 1L
    rows[[position]] <- data.frame(
      target_code = as.character(target_code),
      target_name = as.character(target_name),
      track = as.character(track),
      track_label = boruta_track_label(track, target_name),
      horizon = as.integer(horizon_key),
      feature = selected,
      selection_source = as.character(
        state$selection_source %||% "legacy_selected_state"
      ),
      last_refresh = as.integer(state$last_refresh %||% NA_integer_),
      last_seed = as.integer(state$last_seed %||% NA_integer_),
      mean_importance = as.numeric(stats_value("meanImp")),
      median_importance = as.numeric(stats_value("medianImp")),
      minimum_importance = as.numeric(stats_value("minImp")),
      maximum_importance = as.numeric(stats_value("maxImp")),
      normalized_hits = as.numeric(stats_value("normHits")),
      boruta_decision = as.character(stats_value("decision")),
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) return(empty_boruta_final_selection())
  output <- do.call(rbind, rows)
  output <- output[order(output$track, output$horizon, output$feature), , drop = FALSE]
  rownames(output) <- NULL
  output
}

boruta_refresh_id <- function(history) {
  paste(
    history$track,
    history$horizon,
    history$forecast_number,
    ifelse(is.na(history$origin_date), "NA", as.character(history$origin_date)),
    sep = "__"
  )
}

empty_boruta_feature_stability <- function() {
  data.frame(
    target_code = character(0),
    target_name = character(0),
    track = character(0),
    track_label = character(0),
    horizon = integer(0),
    feature = character(0),
    selection_count = integer(0),
    refresh_count = integer(0),
    selection_frequency = numeric(0),
    stable_core = logical(0),
    stability_threshold = numeric(0),
    stringsAsFactors = FALSE
  )
}

build_boruta_feature_stability <- function(
    selection_history,
    stability_threshold = 0.60
) {
  if (!is.data.frame(selection_history) || nrow(selection_history) == 0L) {
    return(empty_boruta_feature_stability())
  }
  stability_threshold <- validate_scalar_numeric(
    stability_threshold,
    "Boruta stability threshold",
    minimum = 0,
    maximum = 1
  )

  x <- selection_history
  x$refresh_id <- boruta_refresh_id(x)
  groups <- interaction(x$track, x$horizon, drop = TRUE, lex.order = TRUE)
  rows <- lapply(split(x, groups), function(group) {
    refresh_count <- length(unique(group$refresh_id))
    feature_refresh <- unique(group[c("feature", "refresh_id")])
    counts <- table(feature_refresh$feature)
    data.frame(
      target_code = group$target_code[1L],
      target_name = group$target_name[1L],
      track = group$track[1L],
      track_label = group$track_label[1L],
      horizon = as.integer(group$horizon[1L]),
      feature = names(counts),
      selection_count = as.integer(counts),
      refresh_count = as.integer(refresh_count),
      selection_frequency = as.numeric(counts) / refresh_count,
      stable_core = as.numeric(counts) / refresh_count >= stability_threshold,
      stability_threshold = stability_threshold,
      stringsAsFactors = FALSE
    )
  })

  output <- do.call(rbind, rows)
  output <- output[order(
    output$track,
    output$horizon,
    -output$selection_frequency,
    output$feature
  ), , drop = FALSE]
  rownames(output) <- NULL
  output
}

pairwise_set_jaccard <- function(feature_sets) {
  if (length(feature_sets) < 2L) return(NA_real_)
  pairs <- utils::combn(seq_along(feature_sets), 2L)
  values <- apply(pairs, 2L, function(index) {
    a <- unique(feature_sets[[index[1L]]])
    b <- unique(feature_sets[[index[2L]]])
    union_size <- length(union(a, b))
    if (union_size == 0L) return(NA_real_)
    length(intersect(a, b)) / union_size
  })
  values <- values[is.finite(values)]
  if (length(values) == 0L) NA_real_ else mean(values)
}

empty_boruta_stability_summary <- function() {
  data.frame(
    target_code = character(0),
    target_name = character(0),
    track = character(0),
    track_label = character(0),
    horizon = integer(0),
    refresh_count = integer(0),
    average_selected = numeric(0),
    minimum_selected = integer(0),
    maximum_selected = integer(0),
    boruta_refreshes = integer(0),
    fallback_refreshes = integer(0),
    mean_pairwise_jaccard = numeric(0),
    stable_core_count = integer(0),
    stability_threshold = numeric(0),
    stability_basis = character(0),
    stringsAsFactors = FALSE
  )
}

build_boruta_stability_summary <- function(
    selection_history,
    feature_stability,
    stability_threshold = 0.60
) {
  if (!is.data.frame(selection_history) || nrow(selection_history) == 0L) {
    return(empty_boruta_stability_summary())
  }

  x <- selection_history
  x$refresh_id <- boruta_refresh_id(x)
  groups <- interaction(x$track, x$horizon, drop = TRUE, lex.order = TRUE)
  rows <- lapply(split(x, groups), function(group) {
    refresh_info <- unique(group[c(
      "refresh_id", "selection_source", "n_selected"
    )])
    feature_sets <- split(group$feature, group$refresh_id)
    stable_count <- 0L
    if (is.data.frame(feature_stability) && nrow(feature_stability) > 0L) {
      stable_count <- sum(
        feature_stability$track == group$track[1L] &
          feature_stability$horizon == group$horizon[1L] &
          feature_stability$stable_core %in% TRUE,
        na.rm = TRUE
      )
    }

    data.frame(
      target_code = group$target_code[1L],
      target_name = group$target_name[1L],
      track = group$track[1L],
      track_label = group$track_label[1L],
      horizon = as.integer(group$horizon[1L]),
      refresh_count = nrow(refresh_info),
      average_selected = mean(refresh_info$n_selected, na.rm = TRUE),
      minimum_selected = min(refresh_info$n_selected, na.rm = TRUE),
      maximum_selected = max(refresh_info$n_selected, na.rm = TRUE),
      boruta_refreshes = sum(
        refresh_info$selection_source == "boruta_confirmed",
        na.rm = TRUE
      ),
      fallback_refreshes = sum(
        refresh_info$selection_source == "variance_fallback",
        na.rm = TRUE
      ),
      mean_pairwise_jaccard = pairwise_set_jaccard(feature_sets),
      stable_core_count = as.integer(stable_count),
      stability_threshold = stability_threshold,
      stability_basis = "rolling_refresh_windows",
      stringsAsFactors = FALSE
    )
  })

  output <- do.call(rbind, rows)
  output <- output[order(output$track, output$horizon), , drop = FALSE]
  rownames(output) <- NULL
  output
}

empty_boruta_predictive_comparison <- function() {
  data.frame(
    target_code = character(0),
    target_name = character(0),
    track = character(0),
    track_label = character(0),
    evaluation_scale = character(0),
    horizon = integer(0),
    boruta_rmse = numeric(0),
    random_forest_rmse = numeric(0),
    rmse_gain_percent = numeric(0),
    boruta_mae = numeric(0),
    random_forest_mae = numeric(0),
    mae_gain_percent = numeric(0),
    boruta_better_rmse = logical(0),
    boruta_better_mae = logical(0),
    stringsAsFactors = FALSE
  )
}

append_boruta_comparison_rows <- function(
    accuracy,
    target_code,
    target_name,
    track,
    evaluation_scale,
    rmse_column,
    mae_column
) {
  if (
    !is.data.frame(accuracy) || nrow(accuracy) == 0L ||
      !all(c("model", "horizon", rmse_column, mae_column) %in% names(accuracy))
  ) {
    return(empty_boruta_predictive_comparison())
  }

  output <- list()
  position <- 0L
  for (horizon in sort(unique(as.integer(accuracy$horizon)))) {
    boruta <- accuracy[
      accuracy$model == "BorutaRF" & accuracy$horizon == horizon,
      , drop = FALSE
    ]
    rf <- accuracy[
      accuracy$model == "RandomForest" & accuracy$horizon == horizon,
      , drop = FALSE
    ]
    if (nrow(boruta) != 1L || nrow(rf) != 1L) next

    boruta_rmse <- as.numeric(boruta[[rmse_column]][1L])
    rf_rmse <- as.numeric(rf[[rmse_column]][1L])
    boruta_mae <- as.numeric(boruta[[mae_column]][1L])
    rf_mae <- as.numeric(rf[[mae_column]][1L])

    position <- position + 1L
    output[[position]] <- data.frame(
      target_code = as.character(target_code),
      target_name = as.character(target_name),
      track = as.character(track),
      track_label = boruta_track_label(track, target_name),
      evaluation_scale = as.character(evaluation_scale),
      horizon = as.integer(horizon),
      boruta_rmse = boruta_rmse,
      random_forest_rmse = rf_rmse,
      rmse_gain_percent = if (is.finite(rf_rmse) && rf_rmse > 0) {
        100 * (1 - boruta_rmse / rf_rmse)
      } else {
        NA_real_
      },
      boruta_mae = boruta_mae,
      random_forest_mae = rf_mae,
      mae_gain_percent = if (is.finite(rf_mae) && rf_mae > 0) {
        100 * (1 - boruta_mae / rf_mae)
      } else {
        NA_real_
      },
      boruta_better_rmse = is.finite(boruta_rmse) && is.finite(rf_rmse) && boruta_rmse < rf_rmse,
      boruta_better_mae = is.finite(boruta_mae) && is.finite(rf_mae) && boruta_mae < rf_mae,
      stringsAsFactors = FALSE
    )
  }

  if (length(output) == 0L) return(empty_boruta_predictive_comparison())
  do.call(rbind, output)
}

build_boruta_predictive_comparison <- function(
    target_code,
    target_name,
    monthly_accuracy,
    cumulative_accuracy
) {
  monthly <- append_boruta_comparison_rows(
    accuracy = monthly_accuracy,
    target_code = target_code,
    target_name = target_name,
    track = "monthly_transformed",
    evaluation_scale = "target_month_transformed_change",
    rmse_column = "RMSE",
    mae_column = "MAE"
  )
  cumulative_response <- append_boruta_comparison_rows(
    accuracy = cumulative_accuracy,
    target_code = target_code,
    target_name = target_name,
    track = "cumulative_level",
    evaluation_scale = "cumulative_transformed_change",
    rmse_column = "cumulative_log_RMSE",
    mae_column = "cumulative_log_MAE"
  )
  cumulative_level <- append_boruta_comparison_rows(
    accuracy = cumulative_accuracy,
    target_code = target_code,
    target_name = target_name,
    track = "cumulative_level",
    evaluation_scale = "reconstructed_original_level",
    rmse_column = "level_RMSE",
    mae_column = "level_MAE"
  )

  rows <- list(monthly, cumulative_response, cumulative_level)
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (length(rows) == 0L) return(empty_boruta_predictive_comparison())
  output <- do.call(rbind, rows)
  output <- output[order(output$track, output$evaluation_scale, output$horizon), , drop = FALSE]
  rownames(output) <- NULL
  output
}

empty_boruta_audit_summary <- function() {
  data.frame(
    target_code = character(0),
    target_name = character(0),
    track = character(0),
    track_label = character(0),
    horizon = integer(0),
    date_order_check = logical(0),
    selected_state_present = logical(0),
    selected_features_used = logical(0),
    refresh_history_available = logical(0),
    refresh_count = integer(0),
    fallback_refreshes = integer(0),
    random_forest_comparison_available = logical(0),
    audit_status = character(0),
    audit_note = character(0),
    stringsAsFactors = FALSE
  )
}

build_boruta_track_audit <- function(
    forecasts,
    states,
    selection_history,
    predictive_comparison,
    track,
    target_code,
    target_name
) {
  if (!is.data.frame(forecasts) || nrow(forecasts) == 0L) {
    return(empty_boruta_audit_summary())
  }

  boruta_rows <- forecasts[forecasts$model == "BorutaRF", , drop = FALSE]
  if (nrow(boruta_rows) == 0L) return(empty_boruta_audit_summary())

  output <- list()
  position <- 0L
  for (horizon in sort(unique(as.integer(boruta_rows$horizon)))) {
    x <- boruta_rows[boruta_rows$horizon == horizon, , drop = FALSE]
    state <- if (is.list(states)) states[[as.character(horizon)]] else NULL
    selected <- unique(as.character(state$selected_features %||% character(0)))
    selected <- selected[!is.na(selected) & nzchar(selected)]
    last_forecast <- max(x$forecast_number, na.rm = TRUE)
    last_row <- x[x$forecast_number == last_forecast, , drop = FALSE]
    last_selected <- if (nrow(last_row) > 0L && "n_selected" %in% names(last_row)) {
      as.integer(last_row$n_selected[1L])
    } else {
      NA_integer_
    }

    origin_date <- as.Date(x$origin_date)
    target_date <- as.Date(x$target_date)
    window_start_date <- as.Date(x$window_start_date)
    date_order_check <- all(
      !is.na(window_start_date) & !is.na(origin_date) & !is.na(target_date) &
        window_start_date <= origin_date & origin_date < target_date
    )
    selected_state_present <- length(selected) > 0L
    selected_features_used <- selected_state_present &&
      is.finite(last_selected) && identical(as.integer(last_selected), as.integer(length(selected)))

    history <- selection_history[
      selection_history$track == track & selection_history$horizon == horizon,
      , drop = FALSE
    ]
    refresh_ids <- if (nrow(history) > 0L) unique(boruta_refresh_id(history)) else character(0)
    fallback_refreshes <- if (nrow(history) > 0L) {
      length(unique(boruta_refresh_id(history[
        history$selection_source == "variance_fallback",
        , drop = FALSE
      ])))
    } else {
      0L
    }

    comparison_available <- is.data.frame(predictive_comparison) && any(
      predictive_comparison$track == track &
        predictive_comparison$horizon == horizon
    )

    mandatory_pass <- date_order_check && selected_state_present &&
      selected_features_used && length(refresh_ids) > 0L && comparison_available
    audit_status <- if (!mandatory_pass) {
      "FAIL"
    } else if (fallback_refreshes > 0L) {
      "WARN"
    } else {
      "PASS"
    }
    audit_note <- if (identical(audit_status, "PASS")) {
      "Training-window chronology, selected-state use, refresh history, and RF comparison are available."
    } else if (identical(audit_status, "WARN")) {
      "Core checks passed, but at least one refresh used the configured variance fallback."
    } else {
      "One or more required Boruta implementation or result checks are unavailable or inconsistent."
    }

    position <- position + 1L
    output[[position]] <- data.frame(
      target_code = as.character(target_code),
      target_name = as.character(target_name),
      track = as.character(track),
      track_label = boruta_track_label(track, target_name),
      horizon = as.integer(horizon),
      date_order_check = date_order_check,
      selected_state_present = selected_state_present,
      selected_features_used = selected_features_used,
      refresh_history_available = length(refresh_ids) > 0L,
      refresh_count = length(refresh_ids),
      fallback_refreshes = as.integer(fallback_refreshes),
      random_forest_comparison_available = comparison_available,
      audit_status = audit_status,
      audit_note = audit_note,
      stringsAsFactors = FALSE
    )
  }

  if (length(output) == 0L) return(empty_boruta_audit_summary())
  do.call(rbind, output)
}

build_boruta_validation_bundle <- function(
    target_code,
    target_name,
    monthly_forecasts,
    monthly_accuracy,
    monthly_states,
    cumulative_forecasts = data.frame(),
    cumulative_accuracy = data.frame(),
    cumulative_states = list(),
    stability_threshold = 0.60
) {
  monthly_history <- extract_boruta_selection_history(
    states = monthly_states,
    track = "monthly_transformed",
    target_code = target_code,
    target_name = target_name
  )
  cumulative_history <- extract_boruta_selection_history(
    states = cumulative_states,
    track = "cumulative_level",
    target_code = target_code,
    target_name = target_name
  )
  histories <- list(monthly_history, cumulative_history)
  histories <- histories[vapply(histories, nrow, integer(1)) > 0L]
  selection_history <- if (length(histories) > 0L) {
    do.call(rbind, histories)
  } else {
    empty_boruta_selection_history()
  }

  monthly_final <- extract_boruta_final_selection(
    states = monthly_states,
    track = "monthly_transformed",
    target_code = target_code,
    target_name = target_name
  )
  cumulative_final <- extract_boruta_final_selection(
    states = cumulative_states,
    track = "cumulative_level",
    target_code = target_code,
    target_name = target_name
  )
  final_rows <- list(monthly_final, cumulative_final)
  final_rows <- final_rows[vapply(final_rows, nrow, integer(1)) > 0L]
  final_selection <- if (length(final_rows) > 0L) {
    do.call(rbind, final_rows)
  } else {
    empty_boruta_final_selection()
  }

  feature_stability <- build_boruta_feature_stability(
    selection_history,
    stability_threshold = stability_threshold
  )
  stability_summary <- build_boruta_stability_summary(
    selection_history,
    feature_stability,
    stability_threshold = stability_threshold
  )
  predictive_comparison <- build_boruta_predictive_comparison(
    target_code = target_code,
    target_name = target_name,
    monthly_accuracy = monthly_accuracy,
    cumulative_accuracy = cumulative_accuracy
  )

  monthly_audit <- build_boruta_track_audit(
    forecasts = monthly_forecasts,
    states = monthly_states,
    selection_history = selection_history,
    predictive_comparison = predictive_comparison,
    track = "monthly_transformed",
    target_code = target_code,
    target_name = target_name
  )
  cumulative_audit <- build_boruta_track_audit(
    forecasts = cumulative_forecasts,
    states = cumulative_states,
    selection_history = selection_history,
    predictive_comparison = predictive_comparison,
    track = "cumulative_level",
    target_code = target_code,
    target_name = target_name
  )
  audits <- list(monthly_audit, cumulative_audit)
  audits <- audits[vapply(audits, nrow, integer(1)) > 0L]
  audit_summary <- if (length(audits) > 0L) {
    do.call(rbind, audits)
  } else {
    empty_boruta_audit_summary()
  }

  if (nrow(final_selection) > 0L && nrow(feature_stability) > 0L) {
    match_key <- paste(
      feature_stability$track,
      feature_stability$horizon,
      feature_stability$feature,
      sep = "__"
    )
    final_key <- paste(
      final_selection$track,
      final_selection$horizon,
      final_selection$feature,
      sep = "__"
    )
    final_selection$selection_frequency <- feature_stability$selection_frequency[
      match(final_key, match_key)
    ]
    final_selection$stable_core <- feature_stability$stable_core[
      match(final_key, match_key)
    ]
  } else {
    final_selection$selection_frequency <- rep(NA_real_, nrow(final_selection))
    final_selection$stable_core <- rep(NA, nrow(final_selection))
  }

  list(
    selection_history = selection_history,
    final_selection = final_selection,
    feature_stability = feature_stability,
    stability_summary = stability_summary,
    predictive_comparison = predictive_comparison,
    audit_summary = audit_summary
  )
}
