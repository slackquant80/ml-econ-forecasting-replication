###############################################################################
### Statistical Forecast Validation
###############################################################################

statistical_track_specifications <- function(
    forecast_results,
    cumulative_backtest_results
) {
  level_label <- "Cumulative transformed change / reconstructed level"

  if (
    is.data.frame(cumulative_backtest_results) &&
      nrow(cumulative_backtest_results) > 0L &&
      "target_name" %in% names(cumulative_backtest_results)
  ) {
    target_labels <- unique(
      as.character(cumulative_backtest_results$target_name)
    )
    target_labels <- target_labels[!is.na(target_labels) & nzchar(target_labels)]
    if (length(target_labels) > 0L) {
      level_label <- paste0(
        target_labels[1L],
        " cumulative transformed change / reconstructed level"
      )
    }
  }

  list(
    monthly_transformed = list(
      label = "Target-month transformed change",
      data = forecast_results,
      error_column = "error"
    ),
    cumulative_level = list(
      label = level_label,
      data = cumulative_backtest_results,
      error_column = "level_error"
    )
  )
}

validate_statistical_forecast_data <- function(
    forecasts,
    error_column,
    horizon
) {
  if (!is.data.frame(forecasts)) {
    stop("Statistical validation forecasts must be a data.frame.")
  }

  required_columns <- c(
    "target_date",
    "horizon",
    "model",
    "model_family",
    "evaluation_included",
    "status",
    error_column
  )

  missing_columns <- setdiff(required_columns, names(forecasts))
  if (length(missing_columns) > 0L) {
    stop(
      "Statistical validation input is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (
    length(horizon) != 1L ||
      is.na(horizon) ||
      !is.finite(horizon) ||
      horizon < 1L
  ) {
    stop("horizon must be one positive integer.")
  }

  invisible(TRUE)
}

build_matched_error_panel <- function(
    forecasts,
    horizon,
    error_column
) {
  validate_statistical_forecast_data(
    forecasts = forecasts,
    error_column = error_column,
    horizon = horizon
  )

  x <- forecasts[
    forecasts$horizon == as.integer(horizon) &
      forecasts$evaluation_included %in% TRUE &
      forecasts$status == "ok" &
      is.finite(forecasts[[error_column]]) &
      !is.na(forecasts$target_date) &
      !is.na(forecasts$model),
    c("target_date", "model", "model_family", error_column),
    drop = FALSE
  ]

  if (nrow(x) == 0L) {
    stop("No matched evaluation forecasts are available for statistical validation.")
  }

  x$target_date <- as.Date(x$target_date)
  x$model <- as.character(x$model)
  x$model_family <- as.character(x$model_family)

  duplicate_key <- duplicated(x[c("target_date", "model")])
  if (any(duplicate_key)) {
    duplicated_rows <- x[duplicate_key, c("target_date", "model"), drop = FALSE]
    stop(
      "Duplicate target-date/model rows were found in statistical validation: ",
      paste(
        paste(duplicated_rows$target_date, duplicated_rows$model, sep = "/"),
        collapse = ", "
      )
    )
  }

  model_dates <- split(x$target_date, x$model)
  common_dates <- Reduce(intersect, model_dates)
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))

  if (length(common_dates) < 2L) {
    stop("Fewer than two common evaluation dates are available across models.")
  }

  model_names <- sort(unique(x$model))
  error_matrix <- matrix(
    NA_real_,
    nrow = length(common_dates),
    ncol = length(model_names),
    dimnames = list(as.character(common_dates), model_names)
  )

  family_map <- stats::setNames(
    vapply(
      model_names,
      function(model_name) {
        first <- x$model_family[x$model == model_name]
        first <- first[!is.na(first) & nzchar(first)]
        if (length(first) == 0L) "Unknown" else first[1L]
      },
      FUN.VALUE = character(1)
    ),
    model_names
  )

  for (model_name in model_names) {
    model_rows <- x[x$model == model_name, , drop = FALSE]
    index <- match(common_dates, model_rows$target_date)
    error_matrix[, model_name] <- as.numeric(model_rows[[error_column]][index])
  }

  if (any(!is.finite(error_matrix))) {
    stop("The matched statistical-validation error matrix contains non-finite values.")
  }

  list(
    dates = common_dates,
    errors = error_matrix,
    model_family = family_map,
    horizon = as.integer(horizon),
    error_column = error_column
  )
}

loss_matrix_from_errors <- function(errors, loss = c("SE", "AE")) {
  loss <- match.arg(loss)
  errors <- as.matrix(errors)

  if (loss == "SE") {
    return(errors^2)
  }

  abs(errors)
}

run_dm_tests_against_benchmark <- function(
    panel,
    track,
    track_label,
    loss = c("SE", "AE"),
    benchmark_model = "RW",
    alternative = "two.sided",
    varestimator = "bartlett",
    p_adjust_method = "holm",
    significance_level = 0.05
) {
  loss <- match.arg(loss)

  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop(
      "Package 'forecast' is required for Diebold-Mariano tests. ",
      "Run scripts/install-packages.R."
    )
  }

  errors <- panel$errors
  model_names <- colnames(errors)

  if (!(benchmark_model %in% model_names)) {
    stop("DM benchmark model was not found: ", benchmark_model)
  }

  compared_models <- setdiff(model_names, benchmark_model)
  power <- if (loss == "SE") 2L else 1L
  benchmark_errors <- errors[, benchmark_model]
  benchmark_loss <- loss_matrix_from_errors(benchmark_errors, loss = loss)[, 1L]

  rows <- lapply(
    compared_models,
    function(model_name) {
      model_errors <- errors[, model_name]
      model_loss <- loss_matrix_from_errors(model_errors, loss = loss)[, 1L]
      mean_loss_diff <- mean(model_loss - benchmark_loss)
      relative_loss <- mean(model_loss) / mean(benchmark_loss)

      dm_warning <- character(0)
      dm_result <- tryCatch(
        withCallingHandlers(
          forecast::dm.test(
            e1 = model_errors,
            e2 = benchmark_errors,
            alternative = alternative,
            h = as.integer(panel$horizon),
            power = power,
            varestimator = varestimator
          ),
          warning = function(w) {
            dm_warning <<- c(dm_warning, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) e
      )

      if (inherits(dm_result, "error")) {
        statistic <- NA_real_
        p_value <- NA_real_
        test_status <- "error"
        message <- conditionMessage(dm_result)
      } else {
        statistic <- unname(as.numeric(dm_result$statistic)[1L])
        p_value <- unname(as.numeric(dm_result$p.value)[1L])
        test_status <- "ok"
        message <- if (length(dm_warning) > 0L) {
          paste(unique(dm_warning), collapse = " | ")
        } else {
          NA_character_
        }
      }

      data.frame(
        track = track,
        track_label = track_label,
        horizon = as.integer(panel$horizon),
        loss = loss,
        benchmark_model = benchmark_model,
        model = model_name,
        model_family = unname(panel$model_family[model_name]),
        n_evaluation = nrow(errors),
        mean_model_loss = mean(model_loss),
        mean_benchmark_loss = mean(benchmark_loss),
        mean_loss_difference = mean_loss_diff,
        relative_loss = relative_loss,
        dm_statistic = statistic,
        dm_p_value = p_value,
        dm_p_value_adjusted = NA_real_,
        p_adjust_method = p_adjust_method,
        significance_level = significance_level,
        significant_better = FALSE,
        significant_worse = FALSE,
        conclusion = "Inconclusive",
        dm_test_status = test_status,
        status_message = message,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  )

  output <- do.call(rbind, rows)
  rownames(output) <- NULL

  valid <- is.finite(output$dm_p_value)
  if (any(valid)) {
    output$dm_p_value_adjusted[valid] <- stats::p.adjust(
      output$dm_p_value[valid],
      method = p_adjust_method
    )
  }

  output$significant_better <- (
    output$mean_loss_difference < 0 &
      is.finite(output$dm_p_value_adjusted) &
      output$dm_p_value_adjusted < significance_level
  )
  output$significant_worse <- (
    output$mean_loss_difference > 0 &
      is.finite(output$dm_p_value_adjusted) &
      output$dm_p_value_adjusted < significance_level
  )
  output$conclusion <- ifelse(
    output$significant_better,
    "Significantly better than benchmark",
    ifelse(
      output$significant_worse,
      "Significantly worse than benchmark",
      "No significant difference"
    )
  )

  output[
    order(output$relative_loss, output$dm_p_value_adjusted, output$model),
    ,
    drop = FALSE
  ]
}


###############################################################################
### Supplementary HAC-Adjusted Post-Selection MAE Comparison
###############################################################################

normal_tail_p_value <- function(statistic, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  if (!is.finite(statistic)) {
    if (is.na(statistic)) return(NA_real_)
    if (alternative == "two.sided") return(0)
    if (alternative == "less") return(if (statistic < 0) 0 else 1)
    return(if (statistic > 0) 0 else 1)
  }

  if (alternative == "two.sided") {
    return(2 * stats::pnorm(-abs(statistic)))
  }
  if (alternative == "less") {
    return(stats::pnorm(statistic))
  }

  stats::pnorm(statistic, lower.tail = FALSE)
}

legacy_gw_mae_test <- function(
    candidate_errors,
    reference_errors,
    horizon,
    method = "NeweyWest",
    alternative = c("two.sided", "less", "greater")
) {
  alternative <- match.arg(alternative)
  candidate_errors <- as.numeric(candidate_errors)
  reference_errors <- as.numeric(reference_errors)
  horizon <- as.integer(horizon)

  if (length(candidate_errors) != length(reference_errors)) {
    stop("HAC MAE candidate and reference error series must have the same length.")
  }
  if (length(candidate_errors) < 2L) {
    stop("HAC MAE comparison requires at least two matched forecast errors.")
  }
  if (
    any(!is.finite(candidate_errors)) ||
      any(!is.finite(reference_errors))
  ) {
    stop("HAC MAE comparison received non-finite forecast errors.")
  }
  if (length(horizon) != 1L || is.na(horizon) || horizon < 1L) {
    stop("HAC MAE horizon must be one positive integer.")
  }

  # This reproduces the Korean-project implementation:
  # d_t = |e_candidate,t| - |e_reference,t|.
  loss_difference <- abs(candidate_errors) - abs(reference_errors)
  delta <- mean(loss_difference)

  if (all(abs(loss_difference - delta) <= sqrt(.Machine$double.eps))) {
    statistic <- if (abs(delta) <= sqrt(.Machine$double.eps)) {
      0
    } else {
      sign(delta) * Inf
    }
    return(list(
      statistic = statistic,
      p.value = normal_tail_p_value(statistic, alternative),
      mean_loss_difference = delta,
      standard_error = 0,
      method = if (horizon == 1L) {
        "Standard simple-regression estimator"
      } else {
        paste0(method, " HAC covariance estimator")
      },
      hac_lag = if (horizon == 1L) 0L else horizon
    ))
  }

  intercept <- rep(1, length(loss_difference))
  model <- stats::lm(loss_difference ~ 0 + intercept)

  if (horizon == 1L) {
    coefficient_table <- summary(model)$coefficients
    statistic <- unname(as.numeric(coefficient_table[1L, 3L]))
    standard_error <- unname(as.numeric(coefficient_table[1L, 2L]))
    method_label <- "Standard simple-regression estimator"
    hac_lag <- 0L
  } else {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop(
        "Package 'sandwich' is required for the horizon-aware HAC MAE comparison. ",
        "Run scripts/install-packages.R."
      )
    }
    if (!identical(method, "NeweyWest")) {
      stop("The supplementary HAC MAE comparison currently supports method = 'NeweyWest'.")
    }

    covariance <- sandwich::NeweyWest(
      model,
      lag = horizon,
      prewhite = TRUE,
      adjust = FALSE
    )
    standard_error <- sqrt(as.numeric(covariance[1L, 1L]))
    statistic <- if (is.finite(standard_error) && standard_error > 0) {
      delta / standard_error
    } else if (abs(delta) <= sqrt(.Machine$double.eps)) {
      0
    } else {
      sign(delta) * Inf
    }
    method_label <- "Newey-West HAC covariance estimator"
    hac_lag <- horizon
  }

  list(
    statistic = statistic,
    p.value = normal_tail_p_value(statistic, alternative),
    mean_loss_difference = delta,
    standard_error = standard_error,
    method = method_label,
    hac_lag = hac_lag
  )
}

run_gw_tests_against_mae_winner <- function(
    panel,
    track,
    track_label,
    alternative = "two.sided",
    method = "NeweyWest",
    p_adjust_method = "holm",
    significance_level = 0.05
) {
  errors <- as.matrix(panel$errors)
  model_names <- colnames(errors)

  if (length(model_names) < 2L) {
    stop("HAC MAE comparison requires at least two competing models.")
  }

  mae_by_model <- colMeans(abs(errors))
  best_mae <- min(mae_by_model)
  tie_tolerance <- sqrt(.Machine$double.eps) * max(1, abs(best_mae))
  tied_best_models <- sort(
    names(mae_by_model)[abs(mae_by_model - best_mae) <= tie_tolerance]
  )
  reference_model <- tied_best_models[1L]
  reference_errors <- errors[, reference_model]
  compared_models <- setdiff(model_names, reference_model)

  rows <- lapply(
    compared_models,
    function(model_name) {
      candidate_errors <- errors[, model_name]
      candidate_mae <- unname(mae_by_model[model_name])
      reference_mae <- unname(mae_by_model[reference_model])
      gw_warning <- character(0)

      gw_result <- tryCatch(
        withCallingHandlers(
          legacy_gw_mae_test(
            candidate_errors = candidate_errors,
            reference_errors = reference_errors,
            horizon = panel$horizon,
            method = method,
            alternative = alternative
          ),
          warning = function(w) {
            gw_warning <<- c(gw_warning, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) e
      )

      if (inherits(gw_result, "error")) {
        statistic <- NA_real_
        p_value <- NA_real_
        standard_error <- NA_real_
        hac_lag <- if (panel$horizon == 1L) 0L else as.integer(panel$horizon)
        test_method <- if (panel$horizon == 1L) {
          "Standard simple-regression estimator"
        } else {
          "Newey-West HAC covariance estimator"
        }
        test_status <- "error"
        status_message <- conditionMessage(gw_result)
      } else {
        statistic <- unname(as.numeric(gw_result$statistic)[1L])
        p_value <- unname(as.numeric(gw_result$p.value)[1L])
        standard_error <- unname(as.numeric(gw_result$standard_error)[1L])
        hac_lag <- as.integer(gw_result$hac_lag[1L])
        test_method <- as.character(gw_result$method[1L])
        test_status <- "ok"
        status_message <- if (length(gw_warning) > 0L) {
          paste(unique(gw_warning), collapse = " | ")
        } else {
          NA_character_
        }
      }

      relative_loss <- if (reference_mae > 0) {
        candidate_mae / reference_mae
      } else if (abs(candidate_mae) <= sqrt(.Machine$double.eps)) {
        1
      } else {
        Inf
      }

      data.frame(
        track = track,
        track_label = track_label,
        horizon = as.integer(panel$horizon),
        loss = "AE",
        benchmark_model = reference_model,
        benchmark_family = unname(panel$model_family[reference_model]),
        model = model_name,
        model_family = unname(panel$model_family[model_name]),
        n_evaluation = nrow(errors),
        mean_model_mae = candidate_mae,
        mean_benchmark_mae = reference_mae,
        mean_loss_difference = candidate_mae - reference_mae,
        relative_loss = relative_loss,
        loss_excess_percent = 100 * (relative_loss - 1),
        gw_statistic = statistic,
        gw_standard_error = standard_error,
        gw_p_value = p_value,
        gw_p_value_adjusted = NA_real_,
        p_adjust_method = p_adjust_method,
        significance_level = significance_level,
        significant_worse_than_best = FALSE,
        conclusion = "Inconclusive",
        gw_test_status = test_status,
        status_message = status_message,
        gw_method = test_method,
        hac_lag = hac_lag,
        alternative = alternative,
        winner_rule = "Lowest matched OOS MAE within track and horizon",
        tied_best_models = paste(tied_best_models, collapse = ", "),
        n_tied_best_models = length(tied_best_models),
        alphabetical_tie_break = length(tied_best_models) > 1L,
        post_selection_comparison = TRUE,
        comparison_name = "HAC-adjusted post-selection MAE comparison",
        inference_role = "supplementary_diagnostic",
        formal_giacomini_white_test = FALSE,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  )

  output <- do.call(rbind, rows)
  rownames(output) <- NULL

  valid <- is.finite(output$gw_p_value)
  if (any(valid)) {
    output$gw_p_value_adjusted[valid] <- stats::p.adjust(
      output$gw_p_value[valid],
      method = p_adjust_method
    )
  }

  difference_tolerance <- sqrt(.Machine$double.eps) * max(
    1,
    abs(output$mean_benchmark_mae[1L])
  )
  output$significant_worse_than_best <- (
    output$mean_loss_difference > difference_tolerance &
      is.finite(output$gw_p_value_adjusted) &
      output$gw_p_value_adjusted < significance_level
  )
  output$conclusion <- ifelse(
    output$significant_worse_than_best,
    "Significantly worse than horizon MAE winner",
    "No significant difference from horizon MAE winner"
  )

  output[
    order(output$relative_loss, output$gw_p_value_adjusted, output$model),
    ,
    drop = FALSE
  ]
}

###############################################################################
### Model Confidence Set and Automated Audit
###############################################################################

mcs_audit_check_row <- function(
    track,
    track_label,
    horizon,
    loss,
    check_name,
    passed,
    critical,
    observed = NA_character_,
    expected = NA_character_,
    message = NA_character_
) {
  data.frame(
    track = track,
    track_label = track_label,
    horizon = as.integer(horizon),
    loss = loss,
    check_name = check_name,
    passed = isTRUE(passed),
    critical = isTRUE(critical),
    observed = as.character(observed)[1L],
    expected = as.character(expected)[1L],
    message = as.character(message)[1L],
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

summarise_mcs_audit <- function(audit_rows) {
  n_failed <- sum(!audit_rows$passed)
  critical_failed <- sum(!audit_rows$passed & audit_rows$critical)
  audit_status <- if (critical_failed > 0L) {
    "fail"
  } else if (n_failed > 0L) {
    "warning"
  } else {
    "pass"
  }

  data.frame(
    track = audit_rows$track[1L],
    track_label = audit_rows$track_label[1L],
    horizon = as.integer(audit_rows$horizon[1L]),
    loss = audit_rows$loss[1L],
    n_checks = nrow(audit_rows),
    n_failed = n_failed,
    critical_failed = critical_failed,
    audit_status = audit_status,
    failed_checks = paste(
      audit_rows$check_name[!audit_rows$passed],
      collapse = ", "
    ),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

run_model_confidence_set <- function(
    panel,
    track,
    track_label,
    loss = c("SE", "AE"),
    alpha = 0.10,
    bootstrap_samples = 1000L,
    statistic = "Tmax",
    block_length = NULL,
    min_block_length = 3L,
    seed = 20260716L
) {
  loss <- match.arg(loss)

  if (!requireNamespace("MCS", quietly = TRUE)) {
    stop(
      "Package 'MCS' is required for the Model Confidence Set procedure. ",
      "Run scripts/install-packages.R."
    )
  }

  errors <- as.matrix(panel$errors)
  model_names <- colnames(errors)
  loss_matrix <- loss_matrix_from_errors(errors, loss = loss)
  package_version <- as.character(utils::packageVersion("MCS"))

  model_ids <- sprintf("model_%02d", seq_along(model_names))
  model_map <- stats::setNames(model_names, model_ids)
  colnames(loss_matrix) <- model_ids

  mcs_formals <- names(formals(MCS::MCSprocedure))
  mcs_arguments <- list(
    Loss = loss_matrix,
    alpha = alpha,
    B = as.integer(bootstrap_samples),
    statistic = statistic,
    k = block_length,
    min.k = as.integer(min_block_length),
    verbose = FALSE
  )

  if ("seed" %in% mcs_formals) {
    mcs_arguments$seed <- as.integer(seed)
  } else {
    set.seed(as.integer(seed))
  }

  mcs_result <- tryCatch(
    do.call(MCS::MCSprocedure, mcs_arguments),
    error = function(e) e
  )

  mean_loss <- colMeans(loss_matrix)
  loss_rank <- rank(mean_loss, ties.method = "min")

  base_rows <- data.frame(
    track = track,
    track_label = track_label,
    horizon = as.integer(panel$horizon),
    loss = loss,
    model = model_names,
    model_family = unname(panel$model_family[model_names]),
    n_evaluation = nrow(loss_matrix),
    mean_loss = unname(mean_loss[model_ids]),
    loss_rank = as.integer(unname(loss_rank[model_ids])),
    in_mcs = FALSE,
    mcs_rank = NA_integer_,
    mcs_stage_p_value = NA_real_,
    mcs_p_value = NA_real_,
    mcs_statistic_value = NA_real_,
    mcs_detail_status = "not_available",
    mcs_output_format = NA_character_,
    mcs_statistic = statistic,
    alpha = alpha,
    confidence_level = 1 - alpha,
    bootstrap_samples = as.integer(bootstrap_samples),
    block_length = if (is.null(block_length)) NA_integer_ else as.integer(block_length),
    mcs_package_version = package_version,
    mcs_object_class = if (inherits(mcs_result, "error")) {
      NA_character_
    } else {
      paste(class(mcs_result), collapse = ",")
    },
    mcs_status = if (inherits(mcs_result, "error")) "error" else "ok",
    status_message = if (inherits(mcs_result, "error")) {
      conditionMessage(mcs_result)
    } else {
      NA_character_
    },
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  audit_rows <- list()
  add_audit <- function(...) {
    audit_rows[[length(audit_rows) + 1L]] <<- mcs_audit_check_row(
      track = track,
      track_label = track_label,
      horizon = panel$horizon,
      loss = loss,
      ...
    )
  }

  add_audit(
    check_name = "loss_matrix_is_finite",
    passed = all(is.finite(loss_matrix)),
    critical = TRUE,
    observed = paste(dim(loss_matrix), collapse = " x "),
    expected = "Finite OOS loss matrix"
  )
  add_audit(
    check_name = "model_ids_are_unique",
    passed = anyDuplicated(model_ids) == 0L && anyDuplicated(model_names) == 0L,
    critical = TRUE,
    observed = length(unique(model_names)),
    expected = length(model_names)
  )

  if (inherits(mcs_result, "error")) {
    add_audit(
      check_name = "mcs_procedure_completed",
      passed = FALSE,
      critical = TRUE,
      observed = conditionMessage(mcs_result),
      expected = "MCSprocedure returns an SSM object"
    )

    audit <- do.call(rbind, audit_rows)
    audit_summary <- summarise_mcs_audit(audit)
    summary <- data.frame(
      track = track,
      track_label = track_label,
      horizon = as.integer(panel$horizon),
      loss = loss,
      n_evaluation = nrow(loss_matrix),
      n_models = ncol(loss_matrix),
      n_survivors = NA_integer_,
      alpha = alpha,
      confidence_level = 1 - alpha,
      bootstrap_samples = as.integer(bootstrap_samples),
      block_length = if (is.null(block_length)) NA_integer_ else as.integer(block_length),
      statistic = statistic,
      final_p_value = NA_real_,
      minimum_survivor_mcs_p_value = NA_real_,
      mcs_package_version = package_version,
      mcs_output_format = "failed",
      extraction_status = "failed",
      audit_status = audit_summary$audit_status,
      audit_failed = audit_summary$n_failed,
      audit_critical_failed = audit_summary$critical_failed,
      status = "error",
      status_message = conditionMessage(mcs_result),
      row.names = NULL,
      stringsAsFactors = FALSE
    )

    return(list(
      models = base_rows,
      summary = summary,
      audit = audit,
      audit_summary = audit_summary
    ))
  }

  add_audit(
    check_name = "mcs_procedure_completed",
    passed = methods::is(mcs_result, "SSM"),
    critical = TRUE,
    observed = paste(class(mcs_result), collapse = ","),
    expected = "SSM"
  )

  slot_names <- methods::slotNames(mcs_result)
  show_matrix <- tryCatch(
    as.matrix(methods::slot(mcs_result, "show")),
    error = function(e) NULL
  )
  info <- tryCatch(
    methods::slot(mcs_result, "Info"),
    error = function(e) NULL
  )

  add_audit(
    check_name = "official_ssm_slots_available",
    passed = !is.null(show_matrix) && is.list(info),
    critical = TRUE,
    observed = paste(slot_names, collapse = ", "),
    expected = "show matrix and Info list"
  )

  if (is.null(show_matrix) || !is.list(info)) {
    audit <- do.call(rbind, audit_rows)
    audit_summary <- summarise_mcs_audit(audit)
    summary <- data.frame(
      track = track,
      track_label = track_label,
      horizon = as.integer(panel$horizon),
      loss = loss,
      n_evaluation = nrow(loss_matrix),
      n_models = ncol(loss_matrix),
      n_survivors = NA_integer_,
      alpha = alpha,
      confidence_level = 1 - alpha,
      bootstrap_samples = as.integer(bootstrap_samples),
      block_length = if (is.null(block_length)) NA_integer_ else as.integer(block_length),
      statistic = statistic,
      final_p_value = NA_real_,
      minimum_survivor_mcs_p_value = NA_real_,
      mcs_package_version = package_version,
      mcs_output_format = "unknown",
      extraction_status = "failed",
      audit_status = audit_summary$audit_status,
      audit_failed = audit_summary$n_failed,
      audit_critical_failed = audit_summary$critical_failed,
      status = "error",
      status_message = "The MCS SSM object did not expose its official show/Info slots.",
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    base_rows$mcs_status <- "error"
    base_rows$status_message <- summary$status_message

    return(list(
      models = base_rows,
      summary = summary,
      audit = audit,
      audit_summary = audit_summary
    ))
  }

  show_columns <- colnames(show_matrix)
  is_mcs_020_format <- all(c(
    "Avg.Loss",
    "p-Value for H_{0,M_k}",
    "MCS p-Value"
  ) %in% show_columns)

  legacy_p_column <- if (identical(statistic, "TR")) "MCS_R" else "MCS_M"
  legacy_rank_column <- if (identical(statistic, "TR")) "Rank_R" else "Rank_M"
  legacy_value_column <- if (identical(statistic, "TR")) "v_R" else "v_M"
  is_legacy_format <- all(c(
    legacy_rank_column,
    legacy_p_column,
    legacy_value_column
  ) %in% show_columns)

  output_format <- if (is_mcs_020_format) {
    "MCS_0.2.0"
  } else if (is_legacy_format) {
    "MCS_legacy"
  } else {
    "MCS_unrecognised"
  }
  base_rows$mcs_output_format <- output_format

  add_audit(
    check_name = "mcs_output_format_recognised",
    passed = is_mcs_020_format || is_legacy_format,
    critical = TRUE,
    observed = paste(show_columns, collapse = ", "),
    expected = "MCS 0.2.0 or legacy SSM columns"
  )

  show_ids <- rownames(show_matrix)
  if (is.null(show_ids) || length(show_ids) == 0L) {
    show_ids <- as.character(info$model.names)
  }
  show_ids <- as.character(show_ids)
  show_models <- unname(model_map[show_ids])
  resolved_show_ids <- !is.na(show_models)

  add_audit(
    check_name = "show_ids_resolve_to_models",
    passed = length(show_ids) > 0L && all(resolved_show_ids),
    critical = TRUE,
    observed = paste(show_ids, collapse = ", "),
    expected = "Every SSM row maps to an input model"
  )
  add_audit(
    check_name = "show_matrix_model_count_matches_input",
    passed = is_mcs_020_format && nrow(show_matrix) == ncol(loss_matrix) ||
      is_legacy_format && nrow(show_matrix) >= 1L,
    critical = TRUE,
    observed = nrow(show_matrix),
    expected = if (is_mcs_020_format) ncol(loss_matrix) else "At least one final model"
  )

  if (is_mcs_020_format) {
    survivor_ids <- as.character(info$included)
    if (length(survivor_ids) == 0L) {
      stage_values <- as.numeric(show_matrix[, "p-Value for H_{0,M_k}"])
      survivor_ids <- show_ids[stage_values > alpha]
    }
  } else {
    survivor_ids <- as.character(info$included)
    if (length(survivor_ids) == 0L) survivor_ids <- show_ids
  }

  survivor_models <- unname(model_map[survivor_ids])
  resolved_survivors <- !is.na(survivor_models)
  add_audit(
    check_name = "survivor_ids_resolve_to_models",
    passed = length(survivor_ids) > 0L && all(resolved_survivors),
    critical = TRUE,
    observed = paste(survivor_ids, collapse = ", "),
    expected = "Every included model maps to an input model"
  )

  if (is_mcs_020_format && all(resolved_show_ids)) {
    for (show_id in show_ids) {
      model_name <- unname(model_map[show_id])
      row_index <- match(model_name, base_rows$model)
      base_rows$mcs_stage_p_value[row_index] <- suppressWarnings(
        as.numeric(show_matrix[show_id, "p-Value for H_{0,M_k}"])
      )
      base_rows$mcs_p_value[row_index] <- suppressWarnings(
        as.numeric(show_matrix[show_id, "MCS p-Value"])
      )
      base_rows$mcs_detail_status[row_index] <- "mcs_0.2.0_pvalues_available"
    }
  }

  if (is_legacy_format) {
    for (survivor_id in survivor_ids[resolved_survivors]) {
      model_name <- unname(model_map[survivor_id])
      row_index <- match(model_name, base_rows$model)
      if (legacy_rank_column %in% show_columns) {
        base_rows$mcs_rank[row_index] <- suppressWarnings(
          as.integer(show_matrix[survivor_id, legacy_rank_column])
        )
      }
      if (legacy_p_column %in% show_columns) {
        base_rows$mcs_p_value[row_index] <- suppressWarnings(
          as.numeric(show_matrix[survivor_id, legacy_p_column])
        )
      }
      if (legacy_value_column %in% show_columns) {
        base_rows$mcs_statistic_value[row_index] <- suppressWarnings(
          as.numeric(show_matrix[survivor_id, legacy_value_column])
        )
      }
      base_rows$mcs_detail_status[row_index] <- "legacy_rank_pvalue_statistic_available"
    }
  }

  for (survivor_id in survivor_ids[resolved_survivors]) {
    model_name <- unname(model_map[survivor_id])
    row_index <- match(model_name, base_rows$model)
    base_rows$in_mcs[row_index] <- TRUE
  }

  resolved_block_length <- info$k
  if (is.null(resolved_block_length) || length(resolved_block_length) == 0L) {
    resolved_block_length <- block_length
  }
  if (is.null(resolved_block_length) || length(resolved_block_length) == 0L) {
    resolved_block_length <- NA_integer_
  }
  resolved_block_length <- as.integer(resolved_block_length[1L])
  base_rows$block_length <- resolved_block_length

  info_n_elim <- suppressWarnings(as.integer(info$n_elim)[1L])
  expected_survivors_from_info <- if (is.finite(info_n_elim)) {
    ncol(loss_matrix) - info_n_elim
  } else {
    NA_integer_
  }
  n_survivors <- sum(base_rows$in_mcs)

  add_audit(
    check_name = "survivor_count_matches_info",
    passed = is.finite(expected_survivors_from_info) &&
      n_survivors == expected_survivors_from_info,
    critical = TRUE,
    observed = n_survivors,
    expected = expected_survivors_from_info
  )

  info_excluded <- as.character(info$excluded)
  expected_excluded <- setdiff(model_ids, survivor_ids)
  membership_vectors_match <- setequal(survivor_ids, as.character(info$included)) &&
    setequal(info_excluded, expected_excluded)
  add_audit(
    check_name = "membership_matches_info_vectors",
    passed = membership_vectors_match,
    critical = TRUE,
    observed = paste0(
      "included=", paste(survivor_ids, collapse = ","),
      "; excluded=", paste(info_excluded, collapse = ",")
    ),
    expected = "Info included/excluded partition all input models"
  )

  minimum_loss <- min(mean_loss)
  minimum_tolerance <- sqrt(.Machine$double.eps) * max(1, abs(minimum_loss))
  minimum_loss_ids <- names(mean_loss)[abs(mean_loss - minimum_loss) <= minimum_tolerance]
  minimum_loss_models <- unname(model_map[minimum_loss_ids])
  add_audit(
    check_name = "minimum_mean_loss_model_survives",
    passed = any(minimum_loss_models %in% base_rows$model[base_rows$in_mcs]),
    critical = TRUE,
    observed = paste(base_rows$model[base_rows$in_mcs], collapse = ", "),
    expected = paste(minimum_loss_models, collapse = ", ")
  )

  average_loss_column <- if (is_mcs_020_format) {
    "Avg.Loss"
  } else if ("Loss" %in% show_columns) {
    "Loss"
  } else {
    NA_character_
  }
  average_loss_matches <- FALSE
  if (!is.na(average_loss_column) && all(resolved_show_ids)) {
    show_losses <- as.numeric(show_matrix[show_ids, average_loss_column])
    source_losses <- as.numeric(mean_loss[show_ids])
    average_loss_matches <- isTRUE(all.equal(
      show_losses,
      source_losses,
      tolerance = 1e-10,
      check.attributes = FALSE
    ))
  }
  add_audit(
    check_name = "show_average_loss_matches_recalculated_mean_loss",
    passed = average_loss_matches,
    critical = TRUE,
    observed = if (!is.na(average_loss_column)) {
      paste0(average_loss_column, " column available")
    } else {
      "Average-loss column missing"
    },
    expected = "SSM average loss equals column mean of supplied loss matrix"
  )

  if (is_mcs_020_format) {
    all_stage_p <- base_rows$mcs_stage_p_value
    all_mcs_p <- base_rows$mcs_p_value
    pvalues_complete <- all(is.finite(all_stage_p)) &&
      all(all_stage_p >= 0 & all_stage_p <= 1) &&
      all(is.finite(all_mcs_p)) &&
      all(all_mcs_p >= 0 & all_mcs_p <= 1)
    add_audit(
      check_name = "mcs_020_pvalues_extracted",
      passed = pvalues_complete,
      critical = TRUE,
      observed = paste(show_columns, collapse = ", "),
      expected = "Finite stage and MCS p-values for every model"
    )

    stage_membership <- model_ids[all_stage_p > alpha]
    add_audit(
      check_name = "membership_matches_mcs_020_stage_pvalue_rule",
      passed = setequal(stage_membership, survivor_ids),
      critical = TRUE,
      observed = paste(stage_membership, collapse = ", "),
      expected = paste(survivor_ids, collapse = ", ")
    )

    detail_values_complete <- pvalues_complete
    final_p_value <- NA_real_
    minimum_survivor_mcs_p_value <- if (n_survivors > 0L) {
      min(base_rows$mcs_p_value[base_rows$in_mcs], na.rm = TRUE)
    } else {
      NA_real_
    }
    add_audit(
      check_name = "singular_final_p_value_not_applicable_in_mcs_020",
      passed = TRUE,
      critical = FALSE,
      observed = "MCS 0.2.0 returns model-level MCS p-values",
      expected = "No singular final p-value required"
    )
  } else {
    survivor_detail_rows <- base_rows$in_mcs
    detail_values_complete <- all(is.finite(base_rows$mcs_rank[survivor_detail_rows])) &&
      all(is.finite(base_rows$mcs_p_value[survivor_detail_rows])) &&
      all(is.finite(base_rows$mcs_statistic_value[survivor_detail_rows]))
    add_audit(
      check_name = "legacy_survivor_rank_pvalue_statistic_extracted",
      passed = detail_values_complete,
      critical = FALSE,
      observed = paste(show_columns, collapse = ", "),
      expected = paste(
        c(legacy_rank_column, legacy_p_column, legacy_value_column),
        collapse = ", "
      )
    )

    final_p_value <- suppressWarnings(as.numeric(info$mcs_pvalue)[1L])
    final_p_valid <- is.finite(final_p_value) &&
      final_p_value >= 0 && final_p_value <= 1
    add_audit(
      check_name = "legacy_final_mcs_p_value_available",
      passed = final_p_valid,
      critical = FALSE,
      observed = final_p_value,
      expected = "Finite value in [0, 1]"
    )
    minimum_survivor_mcs_p_value <- if (
      n_survivors > 0L &&
        all(is.finite(base_rows$mcs_p_value[base_rows$in_mcs]))
    ) {
      min(base_rows$mcs_p_value[base_rows$in_mcs])
    } else {
      NA_real_
    }
  }

  info_alpha <- suppressWarnings(as.numeric(info$alpha)[1L])
  info_B <- suppressWarnings(as.integer(info$B)[1L])
  add_audit(
    check_name = "mcs_configuration_matches_request",
    passed = is.finite(info_alpha) && abs(info_alpha - alpha) <= 1e-12 &&
      is.finite(info_B) && info_B == as.integer(bootstrap_samples) &&
      is.finite(resolved_block_length) &&
      resolved_block_length >= as.integer(min_block_length),
    critical = TRUE,
    observed = paste0(
      "alpha=", info_alpha,
      ", B=", info_B,
      ", k=", resolved_block_length
    ),
    expected = paste0(
      "alpha=", alpha,
      ", B=", as.integer(bootstrap_samples),
      ", k>=", as.integer(min_block_length)
    )
  )

  audit <- do.call(rbind, audit_rows)
  rownames(audit) <- NULL
  audit_summary <- summarise_mcs_audit(audit)
  extraction_status <- if (is_mcs_020_format && detail_values_complete) {
    "membership_and_model_pvalues_complete"
  } else if (is_legacy_format && detail_values_complete) {
    "legacy_details_complete"
  } else if (n_survivors > 0L) {
    "membership_complete_details_incomplete"
  } else {
    "failed"
  }

  summary <- data.frame(
    track = track,
    track_label = track_label,
    horizon = as.integer(panel$horizon),
    loss = loss,
    n_evaluation = nrow(loss_matrix),
    n_models = ncol(loss_matrix),
    n_survivors = n_survivors,
    alpha = alpha,
    confidence_level = 1 - alpha,
    bootstrap_samples = as.integer(bootstrap_samples),
    block_length = resolved_block_length,
    statistic = statistic,
    final_p_value = final_p_value,
    minimum_survivor_mcs_p_value = minimum_survivor_mcs_p_value,
    mcs_package_version = package_version,
    mcs_output_format = output_format,
    extraction_status = extraction_status,
    audit_status = audit_summary$audit_status,
    audit_failed = audit_summary$n_failed,
    audit_critical_failed = audit_summary$critical_failed,
    status = if (audit_summary$critical_failed > 0L) "error" else "ok",
    status_message = if (audit_summary$n_failed > 0L) {
      paste0("MCS audit: ", audit_summary$failed_checks)
    } else {
      NA_character_
    },
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (audit_summary$critical_failed > 0L) {
    base_rows$mcs_status <- "error"
    base_rows$status_message <- summary$status_message
  } else if (audit_summary$n_failed > 0L) {
    base_rows$status_message <- summary$status_message
  }

  base_rows <- base_rows[
    order(!base_rows$in_mcs, base_rows$mean_loss, base_rows$model),
    ,
    drop = FALSE
  ]
  rownames(base_rows) <- NULL

  list(
    models = base_rows,
    summary = summary,
    audit = audit,
    audit_summary = audit_summary
  )
}

###############################################################################
### Combined Statistical Validation Pipeline
###############################################################################

run_statistical_validation <- function(
    forecast_results,
    cumulative_backtest_results,
    forecast_horizons,
    loss_functions = c("SE", "AE"),
    benchmark_model = "RW",
    dm_alternative = "two.sided",
    dm_varestimator = "bartlett",
    dm_p_adjust_method = "holm",
    gw_alternative = "two.sided",
    gw_method = "NeweyWest",
    gw_p_adjust_method = "holm",
    significance_level = 0.05,
    mcs_alpha = 0.10,
    mcs_bootstrap_samples = 1000L,
    mcs_statistic = "Tmax",
    mcs_block_length = NULL,
    mcs_min_block_length = 3L,
    seed = 20260716L
) {
  tracks <- statistical_track_specifications(
    forecast_results = forecast_results,
    cumulative_backtest_results = cumulative_backtest_results
  )

  dm_rows <- list()
  gw_rows <- list()
  mcs_model_rows <- list()
  mcs_summary_rows <- list()
  mcs_audit_rows <- list()
  mcs_audit_summary_rows <- list()
  row_counter <- 0L

  for (track_name in names(tracks)) {
    track_spec <- tracks[[track_name]]

    if (!is.data.frame(track_spec$data) || nrow(track_spec$data) == 0L) {
      next
    }

    for (horizon in forecast_horizons) {
      panel <- build_matched_error_panel(
        forecasts = track_spec$data,
        horizon = horizon,
        error_column = track_spec$error_column
      )

      # GW is intentionally MAE-only. The reference is selected separately for
      # every track and forecast horizon from the same matched OOS panel.
      gw_rows[[length(gw_rows) + 1L]] <- run_gw_tests_against_mae_winner(
        panel = panel,
        track = track_name,
        track_label = track_spec$label,
        alternative = gw_alternative,
        method = gw_method,
        p_adjust_method = gw_p_adjust_method,
        significance_level = significance_level
      )

      for (loss in loss_functions) {
        row_counter <- row_counter + 1L

        dm_rows[[length(dm_rows) + 1L]] <- run_dm_tests_against_benchmark(
          panel = panel,
          track = track_name,
          track_label = track_spec$label,
          loss = loss,
          benchmark_model = benchmark_model,
          alternative = dm_alternative,
          varestimator = dm_varestimator,
          p_adjust_method = dm_p_adjust_method,
          significance_level = significance_level
        )

        mcs_output <- run_model_confidence_set(
          panel = panel,
          track = track_name,
          track_label = track_spec$label,
          loss = loss,
          alpha = mcs_alpha,
          bootstrap_samples = mcs_bootstrap_samples,
          statistic = mcs_statistic,
          block_length = mcs_block_length,
          min_block_length = mcs_min_block_length,
          seed = as.integer(seed + row_counter)
        )

        mcs_model_rows[[length(mcs_model_rows) + 1L]] <- mcs_output$models
        mcs_summary_rows[[length(mcs_summary_rows) + 1L]] <- mcs_output$summary
        mcs_audit_rows[[length(mcs_audit_rows) + 1L]] <- mcs_output$audit
        mcs_audit_summary_rows[[length(mcs_audit_summary_rows) + 1L]] <- mcs_output$audit_summary
      }
    }
  }

  bind_or_empty <- function(x) {
    if (length(x) == 0L) return(data.frame())
    output <- do.call(rbind, x)
    rownames(output) <- NULL
    output
  }

  list(
    dm_tests = bind_or_empty(dm_rows),
    gw_tests = bind_or_empty(gw_rows),
    mcs_models = bind_or_empty(mcs_model_rows),
    mcs_summary = bind_or_empty(mcs_summary_rows),
    mcs_audit = bind_or_empty(mcs_audit_rows),
    mcs_audit_summary = bind_or_empty(mcs_audit_summary_rows)
  )
}
