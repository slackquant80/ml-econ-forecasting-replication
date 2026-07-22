###############################################################################
### SSRN Working-Paper Analysis Table and Figure-Data Exports
###############################################################################

ssrn_bind_rows <- function(rows) {
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, rows)
  if (length(rows) == 0L) return(data.frame())
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(all_names, names(x))
    for (name in missing) x[[name]] <- NA
    x[, all_names, drop = FALSE]
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}


validate_ssrn_frozen_release <- function(project_root, validation = NULL) {
  if (is.null(validation)) {
    validation <- validate_ssrn_release(project_root = project_root)
  }
  if (!isTRUE(validation$passed)) {
    stop("SSRN release validation must pass before checking the frozen release.")
  }

  freeze_path <- file.path(
    project_root, "results", "ssrn", "ssrn_release_freeze_summary.csv"
  )
  if (!file.exists(freeze_path)) {
    stop(
      "The SSRN release has not been frozen. Run ",
      "scripts/freeze-ssrn-release.R before exporting paper tables."
    )
  }

  freeze <- utils::read.csv(
    freeze_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  required <- c("data_md5", "frozen_data_path", "status")
  missing <- setdiff(required, names(freeze))
  if (nrow(freeze) != 1L || length(missing) > 0L) {
    stop("The SSRN freeze summary is missing or malformed.")
  }
  if (!identical(as.character(freeze$status[[1L]]), "FROZEN")) {
    stop("The SSRN freeze summary does not have status FROZEN.")
  }

  current_md5 <- as.character(validation$summary$current_data_md5[[1L]])
  frozen_md5 <- as.character(freeze$data_md5[[1L]])
  if (!identical(frozen_md5, current_md5)) {
    stop("The frozen data MD5 does not match the validated release data MD5.")
  }

  frozen_data_path <- ssrn_project_path(
    project_root, as.character(freeze$frozen_data_path[[1L]])
  )
  if (!file.exists(frozen_data_path)) {
    stop("The frozen FRED-MD data file does not exist: ", frozen_data_path)
  }
  observed_md5 <- unname(as.character(tools::md5sum(frozen_data_path)))
  if (!identical(observed_md5, frozen_md5)) {
    stop("The frozen FRED-MD data file checksum does not match its freeze record.")
  }

  list(summary = freeze, data_path = frozen_data_path, data_md5 = frozen_md5)
}

load_ssrn_release_projects <- function(project_root, validation = NULL) {
  if (is.null(validation)) {
    validation <- validate_ssrn_release(project_root = project_root)
  }
  if (!isTRUE(validation$passed)) {
    stop("SSRN release validation must pass before exporting paper tables.")
  }

  manifest <- validation$release_manifest
  projects <- vector("list", nrow(manifest))
  names(projects) <- manifest$target_code

  for (i in seq_len(nrow(manifest))) {
    result_path <- ssrn_project_path(project_root, manifest$result_path[[i]])
    project <- readRDS(result_path)
    projects[[manifest$target_code[[i]]]] <- project
  }
  projects
}

ssrn_add_target_identity <- function(data, target_code, target_name) {
  if (!is.data.frame(data) || nrow(data) == 0L) return(data.frame())
  data$target_code <- as.character(target_code)
  data$target_name <- as.character(target_name)
  data
}

build_ssrn_cumulative_loss_data <- function(forecasts) {
  if (!is.data.frame(forecasts) || nrow(forecasts) == 0L) return(data.frame())
  required <- c(
    "target_code", "target_name", "target_date", "horizon", "model",
    "model_family", "evaluation_included", "status", "error"
  )
  missing <- setdiff(required, names(forecasts))
  if (length(missing) > 0L) {
    stop("Forecast data are missing columns: ", paste(missing, collapse = ", "))
  }

  x <- forecasts[
    forecasts$evaluation_included & forecasts$status == "ok" &
      is.finite(forecasts$error) & !is.na(forecasts$target_date),
    required,
    drop = FALSE
  ]
  x$target_date <- as.Date(x$target_date)
  benchmark <- x[x$model == "RW", c(
    "target_code", "target_date", "horizon", "error"
  ), drop = FALSE]
  names(benchmark)[names(benchmark) == "error"] <- "benchmark_error"

  matched <- merge(
    x,
    benchmark,
    by = c("target_code", "target_date", "horizon"),
    all = FALSE,
    sort = FALSE
  )
  if (nrow(matched) == 0L) return(data.frame())

  rows <- list()
  position <- 0L
  for (loss in c("SE", "AE")) {
    part <- matched
    if (identical(loss, "SE")) {
      part$model_loss <- part$error^2
      part$benchmark_loss <- part$benchmark_error^2
    } else {
      part$model_loss <- abs(part$error)
      part$benchmark_loss <- abs(part$benchmark_error)
    }
    part$loss <- loss
    part$loss_difference <- part$model_loss - part$benchmark_loss
    part$relative_period_loss <- ifelse(
      part$benchmark_loss > 0,
      part$model_loss / part$benchmark_loss,
      NA_real_
    )
    position <- position + 1L
    rows[[position]] <- part
  }
  output <- ssrn_bind_rows(rows)
  output <- output[order(
    output$target_code, output$horizon, output$model,
    output$loss, output$target_date
  ), , drop = FALSE]

  groups <- interaction(
    output$target_code, output$horizon, output$model, output$loss,
    drop = TRUE, lex.order = TRUE
  )
  output$cumulative_loss_difference <- ave(
    output$loss_difference,
    groups,
    FUN = cumsum
  )
  output$cumulative_model_loss <- ave(output$model_loss, groups, FUN = cumsum)
  output$cumulative_benchmark_loss <- ave(
    output$benchmark_loss,
    groups,
    FUN = cumsum
  )
  output$cumulative_relative_loss <- ifelse(
    output$cumulative_benchmark_loss > 0,
    output$cumulative_model_loss / output$cumulative_benchmark_loss,
    NA_real_
  )

  output[, c(
    "target_code", "target_name", "target_date", "horizon", "model",
    "model_family", "loss", "model_loss", "benchmark_loss",
    "loss_difference", "cumulative_loss_difference",
    "cumulative_model_loss", "cumulative_benchmark_loss",
    "cumulative_relative_loss"
  ), drop = FALSE]
}

build_ssrn_rolling_rank_data <- function(forecasts, rolling_months = 12L) {
  rolling_months <- as.integer(rolling_months)
  if (rolling_months < 2L) stop("rolling_months must be at least 2.")
  if (!is.data.frame(forecasts) || nrow(forecasts) == 0L) return(data.frame())

  x <- forecasts[
    forecasts$evaluation_included & forecasts$status == "ok" &
      is.finite(forecasts$error) & !is.na(forecasts$target_date),
    c(
      "target_code", "target_name", "target_date", "horizon",
      "model", "model_family", "error"
    ),
    drop = FALSE
  ]
  x$target_date <- as.Date(x$target_date)

  panels <- split(x, interaction(x$target_code, x$horizon, drop = TRUE))
  rows <- list()
  position <- 0L

  for (panel in panels) {
    dates <- sort(unique(panel$target_date))
    if (length(dates) < rolling_months) next

    for (end_position in seq.int(rolling_months, length(dates))) {
      window_dates <- dates[(end_position - rolling_months + 1L):end_position]
      window <- panel[panel$target_date %in% window_dates, , drop = FALSE]
      model_panels <- split(window, window$model)

      metrics <- lapply(model_panels, function(model_data) {
        data.frame(
          model = as.character(model_data$model[[1L]]),
          model_family = as.character(model_data$model_family[[1L]]),
          n_evaluation = nrow(model_data),
          rolling_RMSE = sqrt(mean(model_data$error^2)),
          rolling_MAE = mean(abs(model_data$error)),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      })
      metrics <- ssrn_bind_rows(metrics)
      metrics$RMSE_rank <- rank(metrics$rolling_RMSE, ties.method = "min")
      metrics$MAE_rank <- rank(metrics$rolling_MAE, ties.method = "min")
      metrics$target_code <- as.character(panel$target_code[[1L]])
      metrics$target_name <- as.character(panel$target_name[[1L]])
      metrics$horizon <- as.integer(panel$horizon[[1L]])
      metrics$rolling_months <- rolling_months
      metrics$window_start <- min(window_dates)
      metrics$window_end <- max(window_dates)

      position <- position + 1L
      rows[[position]] <- metrics
    }
  }

  output <- ssrn_bind_rows(rows)
  if (nrow(output) == 0L) return(output)
  output[order(
    output$target_code, output$horizon, output$window_end,
    output$RMSE_rank, output$model
  ), , drop = FALSE]
}

build_ssrn_winner_stability <- function(rolling_rank_data) {
  if (!is.data.frame(rolling_rank_data) || nrow(rolling_rank_data) == 0L) {
    return(list(
      window_winners = data.frame(),
      turnover = data.frame(),
      frequency = data.frame()
    ))
  }

  winners <- rolling_rank_data[
    rolling_rank_data$RMSE_rank == 1,
    ,
    drop = FALSE
  ]
  winners <- winners[order(
    winners$target_code, winners$horizon, winners$window_end, winners$model
  ), , drop = FALSE]

  window_panels <- split(
    winners,
    interaction(
      winners$target_code, winners$horizon, winners$window_end,
      drop = TRUE, lex.order = TRUE
    )
  )
  window_rows <- lapply(window_panels, function(panel) {
    panel <- panel[order(panel$model), , drop = FALSE]
    data.frame(
      target_code = as.character(panel$target_code[[1L]]),
      target_name = as.character(panel$target_name[[1L]]),
      horizon = as.integer(panel$horizon[[1L]]),
      rolling_months = as.integer(panel$rolling_months[[1L]]),
      window_start = as.Date(panel$window_start[[1L]]),
      window_end = as.Date(panel$window_end[[1L]]),
      n_tied_winners = nrow(panel),
      winner_set = paste(as.character(panel$model), collapse = "|"),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  window_winners <- ssrn_bind_rows(window_rows)
  window_winners <- window_winners[order(
    window_winners$target_code, window_winners$horizon,
    window_winners$window_end
  ), , drop = FALSE]

  panels <- split(
    window_winners,
    interaction(window_winners$target_code, window_winners$horizon, drop = TRUE)
  )
  turnover_rows <- lapply(panels, function(panel) {
    panel <- panel[order(panel$window_end), , drop = FALSE]
    winner_sets <- as.character(panel$winner_set)
    modal_set <- names(sort(table(winner_sets), decreasing = TRUE))[1L]
    data.frame(
      target_code = as.character(panel$target_code[[1L]]),
      target_name = as.character(panel$target_name[[1L]]),
      horizon = as.integer(panel$horizon[[1L]]),
      rolling_months = as.integer(panel$rolling_months[[1L]]),
      n_windows = nrow(panel),
      winner_set_switches = if (nrow(panel) <= 1L) 0L else sum(
        winner_sets[-1L] != winner_sets[-length(winner_sets)]
      ),
      distinct_winner_sets = length(unique(winner_sets)),
      windows_with_tied_winners = sum(panel$n_tied_winners > 1L),
      modal_winner_set = modal_set,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  winners$winner_weight <- ave(
    rep(1, nrow(winners)),
    interaction(
      winners$target_code, winners$horizon, winners$window_end,
      drop = TRUE, lex.order = TRUE
    ),
    FUN = function(z) rep(1 / length(z), length(z))
  )
  frequency <- aggregate(
    x = cbind(
      winner_windows_fractional = winners$winner_weight,
      winner_window_appearances = rep(1L, nrow(winners))
    ),
    by = list(
      target_code = winners$target_code,
      target_name = winners$target_name,
      horizon = winners$horizon,
      model = winners$model,
      model_family = winners$model_family
    ),
    FUN = sum
  )
  total_windows <- aggregate(
    x = rep(1L, nrow(window_winners)),
    by = list(
      target_code = window_winners$target_code,
      horizon = window_winners$horizon
    ),
    FUN = sum
  )
  names(total_windows)[3L] <- "total_windows"
  frequency <- merge(
    frequency,
    total_windows,
    by = c("target_code", "horizon"),
    all.x = TRUE,
    sort = FALSE
  )
  frequency$winner_frequency_fractional <- (
    frequency$winner_windows_fractional / frequency$total_windows
  )
  frequency$winner_appearance_rate <- (
    frequency$winner_window_appearances / frequency$total_windows
  )

  list(
    window_winners = window_winners,
    turnover = ssrn_bind_rows(turnover_rows),
    frequency = frequency[order(
      frequency$target_code, frequency$horizon,
      -frequency$winner_frequency_fractional, frequency$model
    ), , drop = FALSE]
  )
}

build_ssrn_loss_concentration <- function(cumulative_loss_data, top_months = 12L) {
  top_months <- as.integer(top_months)
  if (top_months < 1L) stop("top_months must be a positive integer.")
  if (!is.data.frame(cumulative_loss_data) || nrow(cumulative_loss_data) == 0L) {
    return(data.frame())
  }

  panels <- split(
    cumulative_loss_data,
    interaction(
      cumulative_loss_data$target_code, cumulative_loss_data$horizon,
      cumulative_loss_data$model, cumulative_loss_data$loss,
      drop = TRUE, lex.order = TRUE
    )
  )
  rows <- lapply(panels, function(panel) {
    reduction <- -as.numeric(panel$loss_difference)
    positive <- pmax(reduction, 0)
    deterioration <- pmax(-reduction, 0)
    n_top <- min(top_months, length(reduction))
    top_positive <- if (n_top > 0L) sum(
      head(sort(positive, decreasing = TRUE), n_top)
    ) else 0
    top_deterioration <- if (n_top > 0L) sum(
      head(sort(deterioration, decreasing = TRUE), n_top)
    ) else 0
    gross_positive <- sum(positive)
    gross_deterioration <- sum(deterioration)

    data.frame(
      target_code = as.character(panel$target_code[[1L]]),
      target_name = as.character(panel$target_name[[1L]]),
      horizon = as.integer(panel$horizon[[1L]]),
      model = as.character(panel$model[[1L]]),
      model_family = as.character(panel$model_family[[1L]]),
      loss = as.character(panel$loss[[1L]]),
      n_evaluation = nrow(panel),
      net_loss_reduction_vs_RW = sum(reduction),
      gross_positive_loss_reduction = gross_positive,
      gross_loss_deterioration = gross_deterioration,
      share_months_better_than_RW = mean(reduction > 0),
      top_months = n_top,
      top_months_share_of_positive_reduction = if (gross_positive > 0) {
        top_positive / gross_positive
      } else {
        NA_real_
      },
      top_months_share_of_deterioration = if (gross_deterioration > 0) {
        top_deterioration / gross_deterioration
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  output <- ssrn_bind_rows(rows)
  output[order(
    output$target_code, output$horizon, output$loss,
    -output$net_loss_reduction_vs_RW, output$model
  ), , drop = FALSE]
}

build_ssrn_model_summary <- function(rankings, dm, mcs) {
  if (!is.data.frame(rankings) || nrow(rankings) == 0L) return(data.frame())

  summary <- aggregate(
    cbind(RMSE_rank, MAE_rank, relative_RMSE, relative_MAE) ~ model + model_family,
    data = rankings,
    FUN = mean
  )
  names(summary)[3:6] <- c(
    "mean_RMSE_rank", "mean_MAE_rank",
    "mean_relative_RMSE", "mean_relative_MAE"
  )

  winner_count <- aggregate(
    x = as.integer(rankings$RMSE_rank == 1),
    by = list(
      model = rankings$model,
      model_family = rankings$model_family
    ),
    FUN = sum
  )
  names(winner_count)[3L] <- "RMSE_winner_cells"
  summary <- merge(summary, winner_count, by = c("model", "model_family"), all = TRUE)

  for (loss_name in c("SE", "AE")) {
    subset_mcs <- mcs[mcs$loss == loss_name, , drop = FALSE]
    if (nrow(subset_mcs) > 0L) {
      survival <- aggregate(
        x = as.numeric(subset_mcs$in_mcs),
        by = list(
          model = subset_mcs$model,
          model_family = subset_mcs$model_family
        ),
        FUN = mean
      )
      names(survival)[3L] <- paste0("MCS_survival_rate_", loss_name)
      summary <- merge(summary, survival, by = c("model", "model_family"), all = TRUE)
    }

    subset_dm <- dm[dm$loss == loss_name, , drop = FALSE]
    if (nrow(subset_dm) > 0L) {
      dm_better <- aggregate(
        x = as.integer(subset_dm$significant_better),
        by = list(
          model = subset_dm$model,
          model_family = subset_dm$model_family
        ),
        FUN = sum
      )
      names(dm_better)[3L] <- paste0("DM_better_than_RW_cells_", loss_name)
      summary <- merge(summary, dm_better, by = c("model", "model_family"), all = TRUE)
    }
  }

  summary[order(summary$mean_RMSE_rank, summary$model), , drop = FALSE]
}

build_ssrn_diagnostic_summary <- function(projects) {
  rows <- list()
  position <- 0L
  for (target_code in names(projects)) {
    project <- projects[[target_code]]
    target_name <- as.character(project$configuration$target_name)
    display_name <- if (
      is.data.frame(project$processing_summary) &&
        nrow(project$processing_summary) > 0L &&
        "target_name" %in% names(project$processing_summary)
    ) {
      as.character(project$processing_summary$target_name[[1L]])
    } else {
      target_name
    }

    pca <- project$pca_em_convergence_summary
    pca_windows <- if (is.data.frame(pca)) nrow(pca) else 0L
    pca_required <- if (is.data.frame(pca) && "missing_before" %in% names(pca)) {
      sum(pca$missing_before > 0, na.rm = TRUE)
    } else {
      NA_integer_
    }
    pca_nonconverged <- if (is.data.frame(pca) && "em_converged" %in% names(pca)) {
      sum(!pca$em_converged, na.rm = TRUE)
    } else {
      NA_integer_
    }
    pca_max_iterations <- if (
      is.data.frame(pca) && nrow(pca) > 0L && "em_iterations" %in% names(pca)
    ) {
      max(pca$em_iterations, na.rm = TRUE)
    } else {
      NA_integer_
    }

    boruta <- project$boruta_audit_summary
    boruta_fallbacks <- if (
      is.data.frame(boruta) && "fallback_refreshes" %in% names(boruta)
    ) {
      sum(boruta$fallback_refreshes, na.rm = TRUE)
    } else {
      NA_integer_
    }
    boruta_warn <- if (is.data.frame(boruta) && "audit_status" %in% names(boruta)) {
      sum(toupper(as.character(boruta$audit_status)) == "WARN", na.rm = TRUE)
    } else {
      NA_integer_
    }

    processing <- project$processing_summary
    position <- position + 1L
    rows[[position]] <- data.frame(
      target_code = target_code,
      target_name = display_name,
      pca_em_windows = pca_windows,
      pca_em_required_windows = pca_required,
      pca_em_nonconverged_windows = pca_nonconverged,
      pca_em_max_iterations = pca_max_iterations,
      boruta_fallback_refreshes = boruta_fallbacks,
      boruta_warning_panels = boruta_warn,
      target_imputed_months = if (
        is.data.frame(processing) && "target_imputed_months" %in% names(processing)
      ) processing$target_imputed_months[[1L]] else NA_integer_,
      oos_evaluation_excluded_months = if (
        is.data.frame(processing) && "oos_evaluation_excluded_months" %in% names(processing)
      ) processing$oos_evaluation_excluded_months[[1L]] else NA_integer_,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  ssrn_bind_rows(rows)
}

export_ssrn_analysis_tables <- function(
    project_root,
    output_directory = file.path(project_root, "results", "ssrn", "paper_exports"),
    rolling_months = 12L
) {
  validation <- validate_ssrn_release(project_root = project_root)
  if (!isTRUE(validation$passed)) {
    stop("SSRN release validation failed; paper exports were not generated.")
  }
  frozen_release <- validate_ssrn_frozen_release(project_root, validation)
  projects <- load_ssrn_release_projects(project_root, validation)
  protocol_table <- data.frame(
    field = names(validation$protocol),
    value = vapply(validation$protocol, scalar_manifest_value, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  target_registry <- build_target_registry(
    fred_md_file = file.path(project_root, "data", "current.csv")
  )
  core_target_registry <- target_registry[
    target_registry$target_code %in% validation$protocol$required_targets,
    ,
    drop = FALSE
  ]
  core_target_registry <- core_target_registry[
    match(validation$protocol$required_targets, core_target_registry$target_code),
    ,
    drop = FALSE
  ]

  ranking_rows <- list()
  dm_rows <- list()
  mcs_rows <- list()
  mcs_summary_rows <- list()
  forecast_rows <- list()

  for (target_code in names(projects)) {
    project <- projects[[target_code]]
    target_name <- if (
      is.data.frame(project$processing_summary) &&
        nrow(project$processing_summary) > 0L
    ) as.character(project$processing_summary$target_name[[1L]]) else target_code

    ranking_rows[[target_code]] <- ssrn_add_target_identity(
      project$rankings, target_code, target_name
    )
    dm_rows[[target_code]] <- ssrn_add_target_identity(
      project$dm_test_results[
        project$dm_test_results$track == "monthly_transformed",
        , drop = FALSE
      ],
      target_code, target_name
    )
    mcs_rows[[target_code]] <- ssrn_add_target_identity(
      project$model_confidence_set[
        project$model_confidence_set$track == "monthly_transformed",
        , drop = FALSE
      ],
      target_code, target_name
    )
    mcs_summary_rows[[target_code]] <- ssrn_add_target_identity(
      project$model_confidence_set_summary[
        project$model_confidence_set_summary$track == "monthly_transformed",
        , drop = FALSE
      ],
      target_code, target_name
    )
    forecast_rows[[target_code]] <- ssrn_add_target_identity(
      project$forecasts, target_code, target_name
    )
  }

  rankings <- ssrn_bind_rows(ranking_rows)
  dm <- ssrn_bind_rows(dm_rows)
  mcs <- ssrn_bind_rows(mcs_rows)
  mcs_summary <- ssrn_bind_rows(mcs_summary_rows)
  forecasts <- ssrn_bind_rows(forecast_rows)

  headline <- rankings
  dm_se <- dm[dm$loss == "SE", c(
    "target_code", "horizon", "model", "dm_p_value_adjusted",
    "significant_better", "significant_worse", "conclusion"
  ), drop = FALSE]
  names(dm_se)[4:7] <- c(
    "DM_SE_Holm_p", "DM_SE_better_than_RW",
    "DM_SE_worse_than_RW", "DM_SE_conclusion"
  )
  dm_ae <- dm[dm$loss == "AE", c(
    "target_code", "horizon", "model", "dm_p_value_adjusted",
    "significant_better", "significant_worse", "conclusion"
  ), drop = FALSE]
  names(dm_ae)[4:7] <- c(
    "DM_AE_Holm_p", "DM_AE_better_than_RW",
    "DM_AE_worse_than_RW", "DM_AE_conclusion"
  )
  mcs_se <- mcs[mcs$loss == "SE", c(
    "target_code", "horizon", "model", "in_mcs", "mcs_p_value"
  ), drop = FALSE]
  names(mcs_se)[4:5] <- c("MCS_SE_survivor", "MCS_SE_p_value")
  mcs_ae <- mcs[mcs$loss == "AE", c(
    "target_code", "horizon", "model", "in_mcs", "mcs_p_value"
  ), drop = FALSE]
  names(mcs_ae)[4:5] <- c("MCS_AE_survivor", "MCS_AE_p_value")

  for (join_data in list(dm_se, dm_ae, mcs_se, mcs_ae)) {
    headline <- merge(
      headline,
      join_data,
      by = c("target_code", "horizon", "model"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  headline$headline_RMSE_winner <- headline$RMSE_rank == 1
  headline$headline_MAE_winner <- headline$MAE_rank == 1
  headline <- headline[order(
    headline$target_code, headline$horizon,
    headline$RMSE_rank, headline$model
  ), , drop = FALSE]

  model_summary <- build_ssrn_model_summary(rankings, dm, mcs)
  cumulative_loss <- build_ssrn_cumulative_loss_data(forecasts)
  rolling_rank <- build_ssrn_rolling_rank_data(forecasts, rolling_months)
  winner_stability <- build_ssrn_winner_stability(rolling_rank)
  loss_concentration <- build_ssrn_loss_concentration(
    cumulative_loss, top_months = 12L
  )
  diagnostics <- build_ssrn_diagnostic_summary(projects)

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  outputs <- list(
    research_protocol = protocol_table,
    release_manifest = validation$release_manifest,
    core_target_registry = core_target_registry,
    primary_accuracy = rankings,
    headline_accuracy_inference = headline,
    primary_dm = dm,
    primary_mcs = mcs,
    primary_mcs_summary = mcs_summary,
    model_summary = model_summary,
    cumulative_loss_data = cumulative_loss,
    rolling_rank_data = rolling_rank,
    rolling_window_winners = winner_stability$window_winners,
    winner_turnover = winner_stability$turnover,
    winner_frequency = winner_stability$frequency,
    loss_concentration = loss_concentration,
    diagnostics_summary = diagnostics
  )
  filenames <- c(
    research_protocol = "table_research_protocol.csv",
    release_manifest = "table_release_manifest.csv",
    core_target_registry = "table_core_target_registry.csv",
    primary_accuracy = "table_primary_accuracy.csv",
    headline_accuracy_inference = "table_headline_accuracy_inference.csv",
    primary_dm = "table_primary_dm.csv",
    primary_mcs = "table_primary_mcs.csv",
    primary_mcs_summary = "table_primary_mcs_summary.csv",
    model_summary = "table_model_summary.csv",
    cumulative_loss_data = "figure_cumulative_loss_data.csv",
    rolling_rank_data = "figure_rolling_rank_data.csv",
    rolling_window_winners = "table_rolling_window_winners.csv",
    winner_turnover = "table_winner_turnover.csv",
    winner_frequency = "table_winner_frequency.csv",
    loss_concentration = "table_loss_concentration.csv",
    diagnostics_summary = "table_diagnostics_summary.csv"
  )

  for (name in names(outputs)) {
    atomic_write_csv(outputs[[name]], file.path(output_directory, filenames[[name]]))
  }
  atomic_save_rds(
    list(
      generated_at = format_run_time(),
      frozen_data_md5 = frozen_release$data_md5,
      frozen_data_path = project_relative_path_portable(
        project_root, frozen_release$data_path
      ),
      release_manifest = validation$release_manifest,
      outputs = outputs
    ),
    file.path(output_directory, "ssrn_paper_analysis_bundle.rds")
  )

  inventory <- data.frame(
    output = names(outputs),
    filename = unname(filenames[names(outputs)]),
    rows = vapply(outputs, nrow, integer(1)),
    generated_at = format_run_time(),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  atomic_write_csv(inventory, file.path(output_directory, "paper_export_inventory.csv"))
  invisible(list(outputs = outputs, inventory = inventory))
}
