###############################################################################
### Multi-Target Forecast Experiment Runner
###############################################################################

make_experiment_run_id <- function(target_code, execution_profile, now = Sys.time()) {
  paste(
    sanitize_target_component(target_code),
    format(as.POSIXct(now), "%Y%m%d_%H%M%S"),
    toupper(execution_profile),
    sep = "_"
  )
}

validate_experiment_result_directory <- function(
    result_directory,
    expected_target = NULL,
    expected_profile = NULL,
    expect_forward = NULL,
    expect_statistics = NULL,
    validation_output_directory = dirname(result_directory)
) {
  required_files <- c(
    "forecast_project_results.rds",
    "configuration.rds",
    "processing_summary.csv",
    "forecast_results.csv",
    "forecast_accuracy.csv",
    "model_rankings.csv",
    "rolling_index.csv"
  )

  if (isTRUE(expect_forward)) {
    required_files <- c(
      required_files,
      "forward_forecast_project_results.rds",
      "forward_forecasts.csv",
      "cumulative_backtest_results.csv"
    )
  }

  if (isTRUE(expect_statistics)) {
    required_files <- c(
      required_files,
      "dm_test_results.csv",
      "gw_test_results.csv",
      "model_confidence_set.csv",
      "mcs_audit_summary.csv"
    )
  }

  missing_files <- required_files[
    !file.exists(file.path(result_directory, required_files))
  ]
  if (length(missing_files) > 0L) {
    stop("실험 결과 파일이 누락되었습니다: ", paste(missing_files, collapse = ", "))
  }

  project <- readRDS(file.path(result_directory, "forecast_project_results.rds"))
  required_components <- c(
    "forecasts", "accuracy", "rankings", "processing_summary",
    "rolling_index", "configuration"
  )
  missing_components <- setdiff(required_components, names(project))
  if (length(missing_components) > 0L) {
    stop("실험 결과 RDS 구성요소가 누락되었습니다: ", paste(missing_components, collapse = ", "))
  }

  target_in_result <- project$configuration$target_name
  profile_in_result <- project$configuration$execution_profile

  if (!is.null(expected_target) && !identical(target_in_result, expected_target)) {
    stop("결과 target이 요청과 다릅니다: ", target_in_result, " != ", expected_target)
  }
  if (!is.null(expected_profile) && !identical(profile_in_result, expected_profile)) {
    stop("결과 profile이 요청과 다릅니다: ", profile_in_result, " != ", expected_profile)
  }

  if (!is.data.frame(project$forecasts) || nrow(project$forecasts) < 1L) {
    stop("forecast 결과가 비어 있습니다.")
  }
  if (any(project$forecasts$status != "ok", na.rm = TRUE)) {
    stop("forecast 결과에 status != ok 행이 있습니다.")
  }
  if (!is.data.frame(project$accuracy) || nrow(project$accuracy) < 1L) {
    stop("accuracy 결과가 비어 있습니다.")
  }

  boruta_status <- "not_requested"
  configured_models <- project$configuration$models_to_run
  if (is.null(configured_models)) configured_models <- character(0)
  boruta_requested <- "BorutaRF" %in% as.character(configured_models)
  if (isTRUE(boruta_requested)) {
    boruta_files <- c(
      "boruta_selection_history.csv",
      "boruta_final_selection.csv",
      "boruta_feature_stability.csv",
      "boruta_stability_summary.csv",
      "boruta_predictive_comparison.csv",
      "boruta_audit_summary.csv"
    )
    missing_boruta_files <- boruta_files[
      !file.exists(file.path(result_directory, boruta_files))
    ]
    if (length(missing_boruta_files) > 0L) {
      stop(
        "Boruta validation output files are missing: ",
        paste(missing_boruta_files, collapse = ", ")
      )
    }

    boruta_components <- sub("\\.csv$", "", boruta_files)
    missing_boruta_components <- setdiff(boruta_components, names(project))
    if (length(missing_boruta_components) > 0L) {
      stop(
        "Boruta validation RDS components are missing: ",
        paste(missing_boruta_components, collapse = ", ")
      )
    }
    if (
      !is.data.frame(project$boruta_audit_summary) ||
        nrow(project$boruta_audit_summary) < 1L
    ) {
      stop("Boruta audit summary is empty.")
    }
    if (any(project$boruta_audit_summary$audit_status == "FAIL", na.rm = TRUE)) {
      stop("Boruta audit summary contains FAIL.")
    }
    boruta_status <- if (
      any(project$boruta_audit_summary$audit_status == "WARN", na.rm = TRUE)
    ) {
      "warn"
    } else {
      "pass"
    }
  }

  audit_status <- "not_requested"
  if (isTRUE(expect_statistics)) {
    if (!is.data.frame(project$mcs_audit_summary) || nrow(project$mcs_audit_summary) < 1L) {
      stop("MCS audit summary가 비어 있습니다.")
    }
    if (any(tolower(project$mcs_audit_summary$audit_status) != "pass")) {
      stop("MCS audit가 PASS가 아닙니다.")
    }
    audit_status <- "pass"
  }

  validation <- data.frame(
    check = c(
      "required_files",
      "required_rds_components",
      "target_matches_request",
      "profile_matches_request",
      "forecast_rows_present",
      "forecast_status_ok",
      "accuracy_rows_present",
      "mcs_audit",
      "boruta_validation"
    ),
    passed = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      audit_status != "fail",
      boruta_status != "fail"
    ),
    observed = c(
      length(required_files),
      length(required_components),
      target_in_result,
      profile_in_result,
      nrow(project$forecasts),
      sum(project$forecasts$status == "ok", na.rm = TRUE),
      nrow(project$accuracy),
      audit_status,
      boruta_status
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  atomic_write_csv(
    validation,
    file.path(validation_output_directory, "validation_summary.csv")
  )

  list(
    passed = TRUE,
    message = "Required outputs, target/profile alignment, forecast status, statistical audit, and Boruta checks passed.",
    project = project,
    validation = validation
  )
}

run_forecast_experiment <- function(
    project_root,
    target_code,
    execution_profile = c("preview", "quick", "full"),
    publish = FALSE,
    run_id = NULL,
    base_seed = 20260716L,
    enable_statistical_validation = TRUE,
    force = FALSE
) {
  execution_profile <- match.arg(execution_profile)
  if (identical(execution_profile, "preview")) {
    enable_statistical_validation <- FALSE
  }
  project_root <- normalizePath(project_root, mustWork = TRUE)

  registry <- build_target_registry(
    fred_md_file = file.path(project_root, "data", "current.csv")
  )
  target_spec <- resolve_target_spec(registry, target_code, require_eligible = TRUE)

  if (is.null(run_id)) {
    run_id <- make_experiment_run_id(target_code, execution_profile)
  }
  if (!is.character(run_id) || length(run_id) != 1L || !nzchar(run_id)) {
    stop("run_id는 하나의 비어 있지 않은 문자열이어야 합니다.")
  }

  target_path_key <- target_spec$target_path_key[[1L]]
  run_directory <- file.path(
    project_root,
    "results",
    "experiments",
    target_path_key,
    sanitize_target_component(run_id)
  )
  result_directory <- file.path(run_directory, "results")
  log_directory <- file.path(run_directory, "logs")

  if (dir.exists(run_directory)) {
    if (!isTRUE(force)) {
      stop("동일한 run directory가 이미 존재합니다: ", run_directory)
    }
    unlink(run_directory, recursive = TRUE, force = TRUE)
  }

  dir.create(result_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(log_directory, recursive = TRUE, showWarnings = FALSE)

  data_file <- file.path(project_root, "data", "current.csv")
  data_vintage <- as.character(file.info(data_file)$mtime)
  data_md5 <- unname(as.character(tools::md5sum(data_file)))
  forward_enabled <- isTRUE(target_spec$level_forecast_supported[[1L]])

  manifest <- new_experiment_manifest(
    run_id = run_id,
    target_code = target_code,
    target_path_key = target_path_key,
    target_display_name = target_spec$display_name[[1L]],
    execution_profile = execution_profile,
    run_directory = run_directory,
    result_directory = result_directory,
    data_file = data_file,
    data_vintage = data_vintage,
    data_md5 = data_md5,
    base_seed = base_seed,
    forward_forecasts_enabled = forward_enabled,
    statistical_validation_enabled = enable_statistical_validation,
    source = "experiment_runner"
  )
  manifest$project_root <- project_root
  write_experiment_manifest(manifest, run_directory)

  progress <- new_experiment_progress(
    run_id = run_id,
    status = "queued",
    stage = "queued",
    progress_percent = 0,
    message = "Experiment request created."
  )
  write_experiment_progress(progress, run_directory)

  start_time <- Sys.time()
  manifest <- update_experiment_manifest(
    manifest,
    status = "running",
    started_at = format_run_time(start_time)
  )
  progress <- update_experiment_progress(
    progress,
    status = "running",
    stage = "starting",
    progress_percent = 1,
    message = "Starting an independent R process and loading the project.",
    run_directory = run_directory
  )
  refresh_experiment_index(project_root)

  log_file <- file.path(log_directory, "run.log")
  log_connection <- file(log_file, open = "wt")
  log_connection_open <- TRUE
  output_sink_active <- FALSE
  message_sink_active <- FALSE

  close_sinks <- function() {
    if (isTRUE(message_sink_active)) {
      sink(type = "message")
      message_sink_active <<- FALSE
    }
    if (isTRUE(output_sink_active)) {
      sink(type = "output")
      output_sink_active <<- FALSE
    }
    if (isTRUE(log_connection_open)) {
      close(log_connection)
      log_connection_open <<- FALSE
    }
  }

  old_project_root_option <- getOption("ml_forecast.project_root", NULL)
  old_runtime_overrides_option <- getOption(
    "ml_forecast.runtime_overrides",
    NULL
  )
  old_progress_callback_option <- getOption(
    "ml_forecast.progress_callback",
    NULL
  )
  on.exit({
    close_sinks()
    options(
      ml_forecast.project_root = old_project_root_option,
      ml_forecast.runtime_overrides = old_runtime_overrides_option,
      ml_forecast.progress_callback = old_progress_callback_option
    )
  }, add = TRUE)

  overrides <- list(
    target_name = target_code,
    execution_profile = execution_profile,
    results_directory = result_directory,
    save_results = TRUE,
    base_seed = as.integer(base_seed),
    enable_forward_forecasts = forward_enabled,
    enable_statistical_validation = isTRUE(enable_statistical_validation),
    experiment_run_id = run_id,
    experiment_source = "experiment_runner",
    experiment_requested_at = manifest$created_at
  )

  if (identical(execution_profile, "preview")) {
    overrides$npred <- 12L
  }

  if (target_spec$effective_tcode[[1L]] != target_spec$official_tcode[[1L]]) {
    overrides$target_tcode_override <- as.integer(target_spec$effective_tcode[[1L]])
  }

  expected_horizons <- c(1L, 3L, 6L, 12L)
  expected_npred <- if (identical(execution_profile, "preview")) 12L else 90L

  progress_callback <- function(
      stage,
      horizon = NA_integer_,
      forecast_number = NA_integer_,
      forecast_total = expected_npred,
      message = NULL,
      progress_percent = NA_real_
  ) {
    stage <- as.character(stage)[1L]
    horizon <- suppressWarnings(as.integer(horizon)[1L])
    forecast_number <- suppressWarnings(as.integer(forecast_number)[1L])
    forecast_total <- suppressWarnings(as.integer(forecast_total)[1L])

    percent <- suppressWarnings(as.numeric(progress_percent)[1L])
    if (!is.finite(percent)) {
      horizon_position <- match(horizon, expected_horizons)
      if (!is.na(horizon_position) && is.finite(forecast_number) && is.finite(forecast_total)) {
        within_track <- (
          (horizon_position - 1L) * forecast_total + forecast_number
        ) / (length(expected_horizons) * forecast_total)
        percent <- if (identical(stage, "monthly_backtest")) {
          4 + 40 * within_track
        } else if (identical(stage, "cumulative_backtest")) {
          44 + 40 * within_track
        } else {
          3
        }
      } else {
        percent <- switch(
          stage,
          loading = 2,
          monthly_backtest = 4,
          cumulative_backtest = 44,
          statistical_validation = 86,
          saving_results = 93,
          validating_results = 96,
          3
        )
      }
    }

    progress_message <- if (
      is.character(message) && length(message) > 0L &&
        !is.na(message[1L]) && nzchar(message[1L])
    ) {
      message[1L]
    } else {
      stage
    }

    progress <<- update_experiment_progress(
      progress,
      status = "running",
      stage = stage,
      progress_percent = percent,
      message = progress_message,
      horizon = horizon,
      forecast_number = forecast_number,
      forecast_total = forecast_total,
      run_directory = run_directory
    )
    invisible(progress)
  }

  options(
    ml_forecast.project_root = project_root,
    ml_forecast.runtime_overrides = overrides,
    ml_forecast.progress_callback = progress_callback
  )

  run_error <- NULL
  validation <- NULL

  tryCatch(
    {
      sink(log_connection, type = "output", split = TRUE)
      output_sink_active <- TRUE
      sink(log_connection, type = "message")
      message_sink_active <- TRUE

      cat("Forecast experiment started\n")
      cat("Run ID: ", run_id, "\n", sep = "")
      cat("Target: ", target_code, "\n", sep = "")
      cat("Profile: ", execution_profile, "\n", sep = "")
      cat("Forward level track: ", forward_enabled, "\n", sep = "")

      progress_callback(
        stage = "loading",
        message = "Loading data, transformations, and model functions.",
        progress_percent = 2
      )

      run_environment <- new.env(parent = globalenv())
      source(file.path(project_root, "main.R"), local = run_environment)

      close_sinks()

      progress_callback(
        stage = "validating_results",
        message = "Validating result files and audit outputs.",
        progress_percent = 96
      )

      validation <- validate_experiment_result_directory(
        result_directory = result_directory,
        expected_target = target_code,
        expected_profile = execution_profile,
        expect_forward = forward_enabled,
        expect_statistics = isTRUE(enable_statistical_validation)
      )
    },
    error = function(e) {
      run_error <<- e
      close_sinks()
    }
  )

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  if (!is.null(run_error)) {
    error_message <- conditionMessage(run_error)
    writeLines(error_message, file.path(run_directory, "error.txt"), useBytes = TRUE)
    manifest <- update_experiment_manifest(
      manifest,
      status = "failed",
      validation_status = "failed",
      failed_at = format_run_time(end_time),
      elapsed_seconds = elapsed,
      error_message = error_message,
      validation_message = "Experiment did not reach validation."
    )
    progress <- update_experiment_progress(
      progress,
      status = "failed",
      stage = "failed",
      progress_percent = 100,
      message = error_message,
      run_directory = run_directory
    )
    refresh_experiment_index(project_root)
    stop(error_message)
  }

  manifest <- update_experiment_manifest(
    manifest,
    status = "completed",
    validation_status = "passed",
    completed_at = format_run_time(end_time),
    elapsed_seconds = elapsed,
    validation_message = validation$message
  )
  progress <- update_experiment_progress(
    progress,
    status = "completed",
    stage = "completed",
    progress_percent = 100,
    message = "Experiment completed and passed validation.",
    run_directory = run_directory
  )

  if (isTRUE(publish)) {
    write_published_run_pointer(project_root, manifest)
    manifest <- update_experiment_manifest(
      manifest,
      publication_status = "published_default"
    )
  }

  refresh_experiment_index(project_root)

  invisible(
    list(
      manifest = manifest,
      target_spec = target_spec,
      validation = validation$validation,
      run_directory = run_directory,
      result_directory = result_directory
    )
  )
}
