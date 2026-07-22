###############################################################################
### SSRN Working-Paper Release Validation and Freeze Utilities
###############################################################################

ssrn_protocol_specification <- function() {
  list(
    protocol_version = "1.0",
    required_targets = c("CPIAUCSL", "PCEPI", "INDPRO", "UNRATE"),
    execution_profile = "full",
    window_type = "fixed_rolling",
    window_size = 360L,
    npred = 90L,
    forecast_horizons = c(1L, 3L, 6L, 12L),
    data_vintage_mode = "current_vintage_pseudo_oos",
    primary_evaluation_track = "monthly_transformed",
    secondary_evaluation_tracks = c("cumulative_level"),
    required_models = c(
      "RW", "AR(4)", "Ridge", "LASSO", "ElasticNet", "Factor",
      "RandomForest", "XGBoost", "BorutaRF",
      "Ensemble_Mean", "Ensemble_Median", "Ensemble_InvRMSE"
    ),
    benchmark_model = "RW",
    loss_functions = c("SE", "AE"),
    p_adjust_method = "holm",
    mcs_alpha = 0.10,
    mcs_bootstrap_samples = 5000L,
    mcs_statistic = "Tmax",
    primary_inference_methods = c("DM", "MCS"),
    supplementary_inference_methods = c("HAC_POST_SELECTION_MAE"),
    allow_imputed_oos_actual = FALSE,
    allow_imputed_origin_level = FALSE
  )
}

ssrn_normalize_path <- function(x) {
  normalizePath(x, winslash = "/", mustWork = FALSE)
}

ssrn_project_path <- function(project_root, stored_path) {
  stored_path <- gsub("\\\\", "/", as.character(stored_path), fixed = FALSE)
  if (grepl("^(?:[A-Za-z]:/|/)", stored_path, perl = TRUE)) {
    return(stored_path)
  }
  file.path(project_root, stored_path)
}

ssrn_identical_integer_vector <- function(x, expected) {
  x <- suppressWarnings(as.integer(x))
  expected <- as.integer(expected)
  identical(x, expected)
}

ssrn_identical_character_vector <- function(x, expected) {
  identical(as.character(x), as.character(expected))
}

ssrn_first_scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) < 1L) return(default)
  x[[1L]]
}

validate_ssrn_release <- function(
    project_root,
    expected_targets = NULL,
    output_directory = file.path(project_root, "results", "ssrn"),
    write_outputs = TRUE
) {
  project_root <- normalizePath(project_root, mustWork = TRUE)
  protocol <- ssrn_protocol_specification()

  if (!exists("build_target_registry", mode = "function")) {
    source(file.path(project_root, "functions", "registry", "target-registry.R"))
  }
  if (!exists("read_published_target_registry", mode = "function")) {
    source(file.path(project_root, "functions", "experiments", "func-run-manifest.R"))
  }

  target_registry <- build_target_registry(
    fred_md_file = file.path(project_root, "data", "current.csv")
  )

  core_rows <- target_registry[
    target_registry$paper_core & target_registry$eligible_default_run,
    ,
    drop = FALSE
  ]
  registry_targets <- as.character(core_rows$target_code)

  if (is.null(expected_targets)) {
    expected_targets <- protocol$required_targets
  }
  expected_targets <- as.character(expected_targets)

  published <- read_published_target_registry(project_root, migrate_legacy = TRUE)
  current_data_file <- file.path(project_root, "data", "current.csv")
  current_data_md5 <- unname(as.character(tools::md5sum(current_data_file)))

  checks <- list()
  release_rows <- list()
  check_position <- 0L
  release_position <- 0L

  add_check <- function(target_code, run_id, check_name, passed, observed, expected) {
    check_position <<- check_position + 1L
    checks[[check_position]] <<- data.frame(
      target_code = as.character(target_code),
      run_id = as.character(run_id),
      check = as.character(check_name),
      passed = isTRUE(passed),
      observed = paste(as.character(observed), collapse = "|"),
      expected = paste(as.character(expected), collapse = "|"),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  add_check(
    "ALL", NA_character_, "paper_core_targets_match_protocol",
    setequal(registry_targets, expected_targets),
    sort(registry_targets), sort(expected_targets)
  )
  add_check(
    "ALL", NA_character_, "all_protocol_targets_are_eligible",
    all(expected_targets %in% registry_targets),
    registry_targets, expected_targets
  )
  add_check(
    "ALL", NA_character_, "published_registry_has_one_default",
    is.data.frame(published) && nrow(published) > 0L && sum(published$is_default) == 1L,
    if (is.data.frame(published)) sum(published$is_default) else NA_integer_,
    1L
  )

  for (target_code in expected_targets) {
    publication_row <- published[
      as.character(published$target_code) == target_code,
      ,
      drop = FALSE
    ]
    run_id <- if (nrow(publication_row) == 1L) {
      as.character(publication_row$run_id[[1L]])
    } else {
      NA_character_
    }

    add_check(
      target_code, run_id, "one_published_result_for_target",
      nrow(publication_row) == 1L, nrow(publication_row), 1L
    )
    if (nrow(publication_row) != 1L) next

    result_rds <- ssrn_project_path(
      project_root,
      publication_row$result_path[[1L]]
    )
    add_check(
      target_code, run_id, "published_result_rds_exists",
      file.exists(result_rds), result_rds, "existing file"
    )
    if (!file.exists(result_rds)) next

    project <- tryCatch(readRDS(result_rds), error = function(e) e)
    add_check(
      target_code, run_id, "published_result_rds_readable",
      !inherits(project, "error"),
      if (inherits(project, "error")) conditionMessage(project) else "readable",
      "readable"
    )
    if (inherits(project, "error")) next

    configuration <- project$configuration
    processing <- project$processing_summary
    if (!is.list(configuration)) configuration <- list()
    if (!is.data.frame(processing) || nrow(processing) < 1L) {
      processing <- data.frame()
    }

    result_data_md5 <- as.character(
      ssrn_first_scalar(configuration$fred_md_data_md5, NA_character_)
    )
    if (is.na(result_data_md5) || !nzchar(result_data_md5)) {
      run_directory <- ssrn_project_path(
        project_root,
        publication_row$run_directory[[1L]]
      )
      manifest <- tryCatch(read_experiment_manifest(run_directory), error = function(e) NULL)
      result_data_md5 <- as.character(
        ssrn_first_scalar(if (is.null(manifest)) NULL else manifest$data_md5, NA_character_)
      )
    }

    checks_for_target <- list(
      target_matches = identical(as.character(configuration$target_name), target_code),
      profile_is_full = identical(as.character(configuration$execution_profile), protocol$execution_profile),
      publication_profile_is_full = identical(as.character(publication_row$execution_profile[[1L]]), protocol$execution_profile),
      publication_validation_passed = identical(as.character(publication_row$validation_status[[1L]]), "passed"),
      window_type_matches = identical(as.character(configuration$window_type), protocol$window_type),
      window_size_matches = identical(as.integer(configuration$window_size), protocol$window_size),
      npred_matches = identical(as.integer(configuration$npred), protocol$npred),
      horizons_match = ssrn_identical_integer_vector(configuration$forecast_horizons, protocol$forecast_horizons),
      primary_track_matches = identical(as.character(configuration$primary_evaluation_track), protocol$primary_evaluation_track),
      secondary_tracks_match = ssrn_identical_character_vector(configuration$secondary_evaluation_tracks, protocol$secondary_evaluation_tracks),
      benchmark_matches = identical(as.character(configuration$statistical_benchmark_model), protocol$benchmark_model),
      losses_match = ssrn_identical_character_vector(configuration$statistical_loss_functions, protocol$loss_functions),
      dm_adjustment_matches = identical(as.character(configuration$dm_p_adjust_method), protocol$p_adjust_method),
      mcs_alpha_matches = isTRUE(all.equal(as.numeric(configuration$mcs_alpha), protocol$mcs_alpha, tolerance = 1e-12)),
      mcs_bootstrap_matches = identical(as.integer(configuration$mcs_bootstrap_samples), protocol$mcs_bootstrap_samples),
      mcs_statistic_matches = identical(as.character(configuration$mcs_statistic), protocol$mcs_statistic),
      primary_inference_matches = ssrn_identical_character_vector(configuration$primary_inference_methods, protocol$primary_inference_methods),
      supplementary_inference_matches = ssrn_identical_character_vector(configuration$supplementary_inference_methods, protocol$supplementary_inference_methods),
      hac_comparison_is_supplementary = identical(as.character(configuration$gw_inference_role), "supplementary_diagnostic"),
      hac_not_labeled_formal_gw = identical(configuration$gw_formal_giacomini_white_test, FALSE),
      imputed_oos_actuals_excluded = identical(
        configuration$allow_imputed_oos_actual,
        protocol$allow_imputed_oos_actual
      ),
      imputed_origin_levels_excluded = identical(
        configuration$allow_imputed_origin_level,
        protocol$allow_imputed_origin_level
      ),
      required_model_set_complete = setequal(
        as.character(project$rankings$model),
        protocol$required_models
      ),
      data_md5_matches_current = identical(result_data_md5, current_data_md5)
    )

    expected_values <- list(
      target_matches = target_code,
      profile_is_full = protocol$execution_profile,
      publication_profile_is_full = protocol$execution_profile,
      publication_validation_passed = "passed",
      window_type_matches = protocol$window_type,
      window_size_matches = protocol$window_size,
      npred_matches = protocol$npred,
      horizons_match = protocol$forecast_horizons,
      primary_track_matches = protocol$primary_evaluation_track,
      secondary_tracks_match = protocol$secondary_evaluation_tracks,
      benchmark_matches = protocol$benchmark_model,
      losses_match = protocol$loss_functions,
      dm_adjustment_matches = protocol$p_adjust_method,
      mcs_alpha_matches = protocol$mcs_alpha,
      mcs_bootstrap_matches = protocol$mcs_bootstrap_samples,
      mcs_statistic_matches = protocol$mcs_statistic,
      primary_inference_matches = protocol$primary_inference_methods,
      supplementary_inference_matches = protocol$supplementary_inference_methods,
      hac_comparison_is_supplementary = "supplementary_diagnostic",
      hac_not_labeled_formal_gw = FALSE,
      imputed_oos_actuals_excluded = protocol$allow_imputed_oos_actual,
      imputed_origin_levels_excluded = protocol$allow_imputed_origin_level,
      required_model_set_complete = protocol$required_models,
      data_md5_matches_current = current_data_md5
    )

    observed_values <- list(
      target_matches = configuration$target_name,
      profile_is_full = configuration$execution_profile,
      publication_profile_is_full = publication_row$execution_profile[[1L]],
      publication_validation_passed = publication_row$validation_status[[1L]],
      window_type_matches = configuration$window_type,
      window_size_matches = configuration$window_size,
      npred_matches = configuration$npred,
      horizons_match = configuration$forecast_horizons,
      primary_track_matches = configuration$primary_evaluation_track,
      secondary_tracks_match = configuration$secondary_evaluation_tracks,
      benchmark_matches = configuration$statistical_benchmark_model,
      losses_match = configuration$statistical_loss_functions,
      dm_adjustment_matches = configuration$dm_p_adjust_method,
      mcs_alpha_matches = configuration$mcs_alpha,
      mcs_bootstrap_matches = configuration$mcs_bootstrap_samples,
      mcs_statistic_matches = configuration$mcs_statistic,
      primary_inference_matches = configuration$primary_inference_methods,
      supplementary_inference_matches = configuration$supplementary_inference_methods,
      hac_comparison_is_supplementary = configuration$gw_inference_role,
      hac_not_labeled_formal_gw = configuration$gw_formal_giacomini_white_test,
      imputed_oos_actuals_excluded = configuration$allow_imputed_oos_actual,
      imputed_origin_levels_excluded = configuration$allow_imputed_origin_level,
      required_model_set_complete = if (is.data.frame(project$rankings)) {
        sort(unique(as.character(project$rankings$model)))
      } else {
        "missing"
      },
      data_md5_matches_current = result_data_md5
    )

    for (check_name in names(checks_for_target)) {
      add_check(
        target_code,
        run_id,
        check_name,
        checks_for_target[[check_name]],
        observed_values[[check_name]],
        expected_values[[check_name]]
      )
    }

    forecasts_ok <- is.data.frame(project$forecasts) &&
      nrow(project$forecasts) > 0L &&
      all(project$forecasts$status == "ok", na.rm = TRUE)
    add_check(
      target_code, run_id, "all_primary_forecasts_status_ok",
      forecasts_ok,
      if (is.data.frame(project$forecasts)) {
        paste(sort(unique(project$forecasts$status)), collapse = "|")
      } else {
        "missing"
      },
      "ok"
    )

    primary_forecasts <- if (is.data.frame(project$forecasts)) {
      project$forecasts[
        project$forecasts$evaluation_included &
          project$forecasts$status == "ok",
        ,
        drop = FALSE
      ]
    } else {
      data.frame()
    }
    primary_counts <- if (nrow(primary_forecasts) > 0L) {
      stats::aggregate(
        x = rep(1L, nrow(primary_forecasts)),
        by = list(
          horizon = primary_forecasts$horizon,
          model = primary_forecasts$model
        ),
        FUN = sum
      )
    } else {
      data.frame()
    }
    balanced_primary_panel <- nrow(primary_counts) > 0L && all(vapply(
      split(primary_counts$x, primary_counts$horizon),
      function(counts) length(unique(counts)) == 1L,
      logical(1)
    ))
    add_check(
      target_code, run_id, "primary_evaluation_panel_balanced_by_horizon",
      balanced_primary_panel,
      if (nrow(primary_counts) > 0L) {
        paste(
          vapply(
            split(primary_counts$x, primary_counts$horizon),
            function(counts) paste(sort(unique(counts)), collapse = "/"),
            character(1)
          ),
          collapse = "|"
        )
      } else {
        "missing"
      },
      "one common evaluation count per horizon"
    )

    primary_model_sets <- if (nrow(primary_counts) > 0L) {
      split(as.character(primary_counts$model), primary_counts$horizon)
    } else {
      list()
    }
    model_coverage_complete <- (
      setequal(as.integer(names(primary_model_sets)), protocol$forecast_horizons) &&
        all(vapply(
          primary_model_sets,
          function(models) setequal(models, protocol$required_models),
          logical(1)
        ))
    )
    add_check(
      target_code, run_id, "primary_model_coverage_complete_by_horizon",
      model_coverage_complete,
      if (length(primary_model_sets) > 0L) {
        paste(
          vapply(
            primary_model_sets,
            function(models) paste(sort(unique(models)), collapse = "/"),
            character(1)
          ),
          collapse = "|"
        )
      } else {
        "missing"
      },
      paste(sort(protocol$required_models), collapse = "/")
    )

    dm <- project$dm_test_results
    dm_coverage <- is.data.frame(dm) && nrow(dm) > 0L &&
      all(protocol$forecast_horizons %in% unique(dm$horizon[dm$track == protocol$primary_evaluation_track])) &&
      all(protocol$loss_functions %in% unique(dm$loss[dm$track == protocol$primary_evaluation_track]))
    add_check(
      target_code, run_id, "primary_dm_coverage_complete",
      dm_coverage,
      if (is.data.frame(dm)) nrow(dm[dm$track == protocol$primary_evaluation_track, , drop = FALSE]) else 0L,
      "all horizons and losses"
    )

    mcs <- project$model_confidence_set
    mcs_coverage <- is.data.frame(mcs) && nrow(mcs) > 0L &&
      all(protocol$forecast_horizons %in% unique(mcs$horizon[mcs$track == protocol$primary_evaluation_track])) &&
      all(protocol$loss_functions %in% unique(mcs$loss[mcs$track == protocol$primary_evaluation_track]))
    add_check(
      target_code, run_id, "primary_mcs_coverage_complete",
      mcs_coverage,
      if (is.data.frame(mcs)) nrow(mcs[mcs$track == protocol$primary_evaluation_track, , drop = FALSE]) else 0L,
      "all horizons and losses"
    )

    mcs_audit <- project$mcs_audit_summary
    mcs_audit_pass <- is.data.frame(mcs_audit) && nrow(mcs_audit) > 0L &&
      all(tolower(as.character(mcs_audit$audit_status)) == "pass")
    add_check(
      target_code, run_id, "mcs_audit_all_pass",
      mcs_audit_pass,
      if (is.data.frame(mcs_audit)) paste(unique(mcs_audit$audit_status), collapse = "|") else "missing",
      "pass"
    )

    boruta_audit <- project$boruta_audit_summary
    boruta_no_fail <- is.data.frame(boruta_audit) && nrow(boruta_audit) > 0L &&
      !any(toupper(as.character(boruta_audit$audit_status)) == "FAIL", na.rm = TRUE)
    add_check(
      target_code, run_id, "boruta_audit_has_no_fail",
      boruta_no_fail,
      if (is.data.frame(boruta_audit)) paste(unique(boruta_audit$audit_status), collapse = "|") else "missing",
      "PASS or WARN"
    )

    oos_start <- if (nrow(processing) > 0L && "oos_start" %in% names(processing)) {
      as.character(processing$oos_start[[1L]])
    } else {
      NA_character_
    }
    oos_end <- if (nrow(processing) > 0L && "oos_end" %in% names(processing)) {
      as.character(processing$oos_end[[1L]])
    } else {
      NA_character_
    }

    release_position <- release_position + 1L
    release_rows[[release_position]] <- data.frame(
      release_protocol_version = protocol$protocol_version,
      target_code = target_code,
      target_display_name = as.character(publication_row$target_display_name[[1L]]),
      run_id = run_id,
      execution_profile = as.character(publication_row$execution_profile[[1L]]),
      result_path = as.character(publication_row$result_path[[1L]]),
      published_at = as.character(publication_row$published_at[[1L]]),
      data_md5 = result_data_md5,
      oos_start = oos_start,
      oos_end = oos_end,
      window_size = as.integer(configuration$window_size),
      npred = as.integer(configuration$npred),
      horizons = paste(as.integer(configuration$forecast_horizons), collapse = ","),
      primary_evaluation_track = as.character(configuration$primary_evaluation_track),
      primary_inference = paste(as.character(configuration$primary_inference_methods), collapse = ","),
      supplementary_inference = paste(as.character(configuration$supplementary_inference_methods), collapse = ","),
      mcs_bootstrap_samples = as.integer(configuration$mcs_bootstrap_samples),
      base_models = paste(as.character(configuration$models_to_run), collapse = ","),
      validation_status = as.character(publication_row$validation_status[[1L]]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  validation <- if (length(checks) > 0L) {
    do.call(rbind, checks)
  } else {
    data.frame()
  }
  release_manifest <- if (length(release_rows) > 0L) {
    do.call(rbind, release_rows)
  } else {
    data.frame()
  }

  all_passed <- nrow(validation) > 0L && all(validation$passed)
  summary <- data.frame(
    protocol_version = protocol$protocol_version,
    validated_at = format_run_time(),
    current_data_md5 = current_data_md5,
    expected_targets = paste(expected_targets, collapse = ","),
    n_checks = nrow(validation),
    n_failed = sum(!validation$passed),
    release_status = if (all_passed) "PASS" else "FAIL",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (isTRUE(write_outputs)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    atomic_write_csv(validation, file.path(output_directory, "ssrn_release_validation.csv"))
    atomic_write_csv(summary, file.path(output_directory, "ssrn_release_validation_summary.csv"))
    atomic_write_csv(release_manifest, file.path(output_directory, "ssrn_release_manifest.csv"))
    atomic_save_rds(
      list(
        protocol = protocol,
        validation = validation,
        summary = summary,
        release_manifest = release_manifest
      ),
      file.path(output_directory, "ssrn_release_bundle.rds")
    )
  }

  list(
    passed = all_passed,
    protocol = protocol,
    validation = validation,
    summary = summary,
    release_manifest = release_manifest
  )
}

freeze_ssrn_release <- function(
    project_root,
    output_directory = file.path(project_root, "results", "ssrn")
) {
  validation <- validate_ssrn_release(
    project_root = project_root,
    output_directory = output_directory,
    write_outputs = TRUE
  )
  if (!isTRUE(validation$passed)) {
    stop("SSRN release validation failed. Review results/ssrn/ssrn_release_validation.csv.")
  }

  data_file <- file.path(project_root, "data", "current.csv")
  data_md5 <- unname(as.character(tools::md5sum(data_file)))
  frozen_data_directory <- file.path(output_directory, "data_vintage")
  dir.create(frozen_data_directory, recursive = TRUE, showWarnings = FALSE)
  frozen_data_path <- file.path(
    frozen_data_directory,
    paste0("fred_md_current_", data_md5, ".csv")
  )
  if (!file.exists(frozen_data_path)) {
    copied <- file.copy(data_file, frozen_data_path, overwrite = FALSE, copy.date = TRUE)
    if (!isTRUE(copied)) stop("Failed to freeze the FRED-MD data vintage.")
  }

  freeze_summary <- data.frame(
    frozen_at = format_run_time(),
    data_md5 = data_md5,
    frozen_data_path = project_relative_path_portable(project_root, frozen_data_path),
    release_manifest_path = project_relative_path_portable(
      project_root,
      file.path(output_directory, "ssrn_release_manifest.csv")
    ),
    status = "FROZEN",
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  atomic_write_csv(
    freeze_summary,
    file.path(output_directory, "ssrn_release_freeze_summary.csv")
  )
  atomic_save_rds(
    freeze_summary,
    file.path(output_directory, "ssrn_release_freeze_summary.rds")
  )

  invisible(list(validation = validation, freeze_summary = freeze_summary))
}
