###############################################################################
### Target Registry for Multi-Target Forecast Experiments
###############################################################################

if (!exists("validate_scalar_integer", mode = "function")) {
  source(
    file.path(project_root, "functions", "core", "func-utils.R"),
    local = TRUE
  )
}
if (!exists("target_level_mode_from_tcode", mode = "function")) {
  source(
    file.path(project_root, "functions", "core", "func-target-processing.R"),
    local = TRUE
  )
}

target_tcode_labels <- c(
  `1` = "Level",
  `2` = "First difference",
  `3` = "Second difference",
  `4` = "Log level",
  `5` = "First difference of log",
  `6` = "Second difference of log",
  `7` = "First difference of growth rate"
)

default_target_label_map <- c(
  CPIAUCSL = "Consumer Price Index: All Items",
  INDPRO = "Industrial Production Index",
  UNRATE = "Civilian Unemployment Rate",
  PAYEMS = "All Employees: Total Nonfarm Payrolls",
  FEDFUNDS = "Effective Federal Funds Rate",
  PCEPI = "Personal Consumption Expenditures Price Index",
  HOUST = "Housing Starts",
  RPI = "Real Personal Income",
  `S&P 500` = "S&P 500 Index",
  VIXCLSx = "CBOE Volatility Index"
)

# Curated dashboard target universe. The registry and predictor pool retain every
# FRED-MD series, while only explicitly enabled targets are exposed in the
# interactive local/web dashboards.
default_dashboard_target_codes <- c("CPIAUCSL", "PCEPI", "INDPRO", "UNRATE")
default_paper_core_target_codes <- c("CPIAUCSL", "PCEPI", "INDPRO", "UNRATE")
default_web_target_code <- "CPIAUCSL"

default_target_category_map <- c(
  CPIAUCSL = "Prices",
  PCEPI = "Prices",
  UNRATE = "Labor Market",
  INDPRO = "Output and Income",
  PAYEMS = "Labor Market",
  FEDFUNDS = "Interest Rates",
  HOUST = "Housing",
  RPI = "Output and Income",
  `S&P 500` = "Financial Markets",
  VIXCLSx = "Financial Markets"
)

sanitize_target_component <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "TARGET")
}

max_internal_na_gap <- function(x) {
  x <- is.na(x)
  if (!any(x)) return(0L)

  observed <- which(!x)
  if (length(observed) == 0L) return(length(x))

  x <- x[min(observed):max(observed)]
  runs <- rle(x)
  if (!any(runs$values)) 0L else max(runs$lengths[runs$values])
}


prepare_registry_target_series <- function(x, tcode, max_target_gap) {
  x <- as.numeric(x)
  observed <- which(!is.na(x) & is.finite(x))

  if (length(observed) == 0L) {
    return(list(
      imputed = x,
      transformed = rep(NA_real_, max(0L, length(x) - 2L)),
      first_observed = NA_integer_,
      last_observed = NA_integer_,
      max_internal_gap = length(x),
      transformation_valid = FALSE,
      transformation_message = "no_observed_values"
    ))
  }

  first_observed <- min(observed)
  last_observed <- max(observed)
  imputed <- x
  gap_length <- 0L
  max_gap <- 0L

  for (i in first_observed:last_observed) {
    if (is.na(imputed[i]) || !is.finite(imputed[i])) {
      gap_length <- gap_length + 1L
      max_gap <- max(max_gap, gap_length)
      if (i == 1L || gap_length > max_target_gap) next
      imputed[i] <- imputed[i - 1L]
    } else {
      gap_length <- 0L
    }
  }

  transformation_valid <- TRUE
  transformation_message <- "ok"

  if (tcode %in% c(4L, 5L, 6L)) {
    used <- imputed[first_observed:last_observed]
    if (any(!is.na(used) & used <= 0)) {
      transformation_valid <- FALSE
      transformation_message <- "nonpositive_value_for_log_transform"
    }
  }

  if (tcode == 7L) {
    denominator <- imputed[-length(imputed)]
    if (any(!is.na(denominator) & denominator == 0)) {
      transformation_valid <- FALSE
      transformation_message <- "zero_denominator_for_tcode7"
    }
  }

  transformed <- rep(NA_real_, max(0L, length(imputed) - 2L))

  if (isTRUE(transformation_valid) && length(imputed) >= 3L) {
    transformed <- if (tcode == 1L) {
      imputed[-c(1L, 2L)]
    } else if (tcode == 2L) {
      diff(imputed[-1L])
    } else if (tcode == 3L) {
      diff(imputed, differences = 2L)
    } else if (tcode == 4L) {
      log(imputed[-c(1L, 2L)])
    } else if (tcode == 5L) {
      diff(log(imputed[-1L]))
    } else if (tcode == 6L) {
      diff(log(imputed), differences = 2L)
    } else {
      growth_rate <- imputed[-1L] / imputed[-length(imputed)] - 1
      diff(growth_rate)
    }
  }

  list(
    imputed = imputed,
    transformed = transformed,
    first_observed = first_observed,
    last_observed = last_observed,
    max_internal_gap = max_internal_na_gap(x),
    transformation_valid = transformation_valid,
    transformation_message = transformation_message
  )
}

read_fred_md_registry_input <- function(fred_md_file) {
  if (!file.exists(fred_md_file)) {
    stop("FRED-MD 파일을 찾을 수 없습니다: ", fred_md_file)
  }

  raw <- utils::read.csv(
    fred_md_file,
    check.names = FALSE,
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )

  if (nrow(raw) < 3L || ncol(raw) < 2L) {
    stop("Target Registry를 만들기에 FRED-MD 파일이 충분하지 않습니다.")
  }

  if (anyDuplicated(names(raw)) > 0L) {
    stop("FRED-MD 파일에 중복된 변수명이 있습니다.")
  }

  tcodes <- suppressWarnings(
    as.integer(unlist(raw[1L, -1L, drop = FALSE], use.names = FALSE))
  )
  names(tcodes) <- names(raw)[-1L]

  if (anyNA(tcodes) || any(!(tcodes %in% 1:7))) {
    stop("Target Registry 입력에 유효하지 않은 transformation code가 있습니다.")
  }

  data <- raw[-1L, , drop = FALSE]
  rownames(data) <- NULL

  date_raw <- trimws(as.character(data[[1L]]))
  dates <- as.Date(date_raw, format = "%m/%d/%Y")
  missing_dates <- is.na(dates)
  if (any(missing_dates)) {
    dates[missing_dates] <- as.Date(date_raw[missing_dates], format = "%Y-%m-%d")
  }
  if (anyNA(dates)) {
    stop("Target Registry 입력의 날짜를 변환하지 못했습니다.")
  }
  data[[1L]] <- dates

  for (i in 2:ncol(data)) {
    raw_value <- trimws(as.character(data[[i]]))
    numeric_value <- suppressWarnings(as.numeric(raw_value))
    invalid <- is.na(numeric_value) & !is.na(raw_value) & nzchar(raw_value)
    if (any(invalid)) {
      stop("Target Registry 입력을 숫자형으로 변환하지 못했습니다: ", names(data)[i])
    }
    data[[i]] <- numeric_value
  }

  list(data = data, tcodes = tcodes)
}

build_target_registry <- function(
    fred_md_file,
    target_tcode_overrides = c(CPIAUCSL = 5L, PCEPI = 5L),
    window_size = 360L,
    npred = 90L,
    forecast_horizons = c(1L, 3L, 6L, 12L),
    max_target_gap = 3L,
    min_obs_ratio = 0.90,
    label_map = default_target_label_map,
    category_map = default_target_category_map,
    dashboard_enabled_codes = default_dashboard_target_codes,
    paper_core_codes = default_paper_core_target_codes,
    web_default_target = default_web_target_code
) {
  input <- read_fred_md_registry_input(fred_md_file)
  data <- input$data
  tcodes <- input$tcodes

  required_transformed_months <- as.integer(
    window_size + npred + max(forecast_horizons) - 1L
  )
  required_raw_months <- required_transformed_months + 2L

  records <- lapply(
    names(tcodes),
    function(code) {
      x <- as.numeric(data[[code]])
      observed <- which(!is.na(x) & is.finite(x))
      n_observed <- length(observed)

      official_tcode <- as.integer(tcodes[[code]])
      effective_tcode <- if (code %in% names(target_tcode_overrides)) {
        as.integer(unname(target_tcode_overrides[[code]]))
      } else {
        official_tcode
      }

      prepared <- prepare_registry_target_series(
        x = x,
        tcode = effective_tcode,
        max_target_gap = max_target_gap
      )

      if (n_observed > 0L) {
        first_obs <- min(observed)
        last_obs <- max(observed)
        sample_months <- last_obs - first_obs + 1L
        positive_level <- all(x[observed] > 0)
        internal_gap <- prepared$max_internal_gap
        observed_start <- data[[1L]][first_obs]
        observed_end <- data[[1L]][last_obs]
      } else {
        sample_months <- 0L
        positive_level <- FALSE
        internal_gap <- length(x)
        observed_start <- as.Date(NA)
        observed_end <- as.Date(NA)
      }

      transformed_rows <- which(
        !is.na(prepared$transformed) & is.finite(prepared$transformed)
      )
      transformed_sample_months <- if (length(transformed_rows) > 0L) {
        max(transformed_rows) - min(transformed_rows) + 1L
      } else {
        0L
      }

      initial_window_obs_ratio <- NA_real_
      initial_window_sd <- NA_real_
      if (
        isTRUE(prepared$transformation_valid) &&
          transformed_sample_months >= required_transformed_months
      ) {
        transformed_target <- prepared$transformed[
          min(transformed_rows):max(transformed_rows)
        ]
        model_target <- tail(transformed_target, required_transformed_months)
        initial_target <- head(model_target, window_size)
        initial_window_obs_ratio <- mean(
          !is.na(initial_target) & is.finite(initial_target)
        )
        initial_window_sd <- stats::sd(initial_target, na.rm = TRUE)
      }

      label <- if (code %in% names(label_map)) {
        unname(label_map[[code]])
      } else {
        code
      }
      category <- if (code %in% names(category_map)) {
        unname(category_map[[code]])
      } else {
        "Other"
      }

      eligible_history <- (
        isTRUE(prepared$transformation_valid) &&
          transformed_sample_months >= required_transformed_months &&
          internal_gap <= max_target_gap &&
          is.finite(initial_window_obs_ratio) &&
          initial_window_obs_ratio >= min_obs_ratio &&
          is.finite(initial_window_sd) &&
          initial_window_sd > sqrt(.Machine$double.eps)
      )

      data.frame(
        target_code = code,
        target_path_key = sanitize_target_component(code),
        target_label = label,
        display_name = paste0(label, " [", code, "]"),
        target_category = category,
        dashboard_enabled = code %in% dashboard_enabled_codes,
        paper_core = code %in% paper_core_codes,
        web_default = identical(code, web_default_target),
        official_tcode = official_tcode,
        effective_tcode = effective_tcode,
        transformation = unname(target_tcode_labels[as.character(effective_tcode)]),
        transformation_source = if (effective_tcode == official_tcode) {
          "Official FRED-MD"
        } else {
          "Target registry override"
        },
        observed_start = observed_start,
        observed_end = observed_end,
        n_observed = n_observed,
        sample_months = sample_months,
        transformed_sample_months = transformed_sample_months,
        max_internal_gap = internal_gap,
        transformation_valid = prepared$transformation_valid,
        transformation_message = prepared$transformation_message,
        initial_window_obs_ratio = initial_window_obs_ratio,
        initial_window_sd = initial_window_sd,
        positive_level = positive_level,
        monthly_track_supported = n_observed > 0L,
        level_forecast_mode = target_level_mode_from_tcode(effective_tcode),
        level_forecast_formula = level_mode_formula_label(
          target_level_mode_from_tcode(effective_tcode)
        ),
        level_forecast_supported = level_mode_supported_by_data(
          x,
          target_level_mode_from_tcode(effective_tcode)
        ),
        # Legacy column names are retained for dashboard/result compatibility.
        cumulative_level_supported = level_mode_supported_by_data(
          x,
          target_level_mode_from_tcode(effective_tcode)
        ),
        cumulative_level_mode = target_level_mode_from_tcode(effective_tcode),
        target_missing_policy = "causal_locf_internal_only",
        target_max_carry_gap = as.integer(max_target_gap),
        predictor_missing_policy = "rolling_window_causal_locf_then_final_imputation",
        eligible_default_run = eligible_history && level_mode_supported_by_data(
          x,
          target_level_mode_from_tcode(effective_tcode)
        ),
        eligibility_status = if (eligible_history && level_mode_supported_by_data(
          x,
          target_level_mode_from_tcode(effective_tcode)
        )) {
          "ready"
        } else if (!level_mode_supported_by_data(
          x,
          target_level_mode_from_tcode(effective_tcode)
        )) {
          "level_forecast_mode_not_supported_by_data"
        } else if (!isTRUE(prepared$transformation_valid)) {
          prepared$transformation_message
        } else if (transformed_sample_months < required_transformed_months) {
          "insufficient_history"
        } else if (internal_gap > max_target_gap) {
          "target_gap_exceeds_limit"
        } else if (!is.finite(initial_window_obs_ratio) || initial_window_obs_ratio < min_obs_ratio) {
          "initial_window_observation_ratio_below_limit"
        } else {
          "initial_window_has_no_variation"
        },
        required_raw_months = required_raw_months,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  )

  registry <- do.call(rbind, records)
  rownames(registry) <- NULL
  validate_target_registry(registry)
  registry
}

validate_target_registry <- function(registry) {
  if (!is.data.frame(registry)) {
    stop("target registry는 data.frame이어야 합니다.")
  }

  required <- c(
    "target_code", "target_path_key", "display_name",
    "target_category", "dashboard_enabled", "paper_core", "web_default",
    "official_tcode", "effective_tcode", "transformation",
    "positive_level", "monthly_track_supported",
    "level_forecast_supported", "level_forecast_mode",
    "level_forecast_formula",
    "cumulative_level_supported", "cumulative_level_mode",
    "target_missing_policy", "predictor_missing_policy",
    "eligible_default_run", "eligibility_status"
  )
  missing <- setdiff(required, names(registry))
  if (length(missing) > 0L) {
    stop("target registry에 필요한 열이 없습니다: ", paste(missing, collapse = ", "))
  }

  if (nrow(registry) < 1L) stop("target registry가 비어 있습니다.")
  if (anyNA(registry$target_code) || any(!nzchar(registry$target_code))) {
    stop("target registry에 비어 있는 target_code가 있습니다.")
  }
  if (anyDuplicated(registry$target_code) > 0L) {
    stop("target registry에 중복된 target_code가 있습니다.")
  }
  if (anyDuplicated(registry$target_path_key) > 0L) {
    stop("target registry의 target_path_key가 중복됩니다.")
  }
  for (flag in c("dashboard_enabled", "paper_core", "web_default")) {
    if (!is.logical(registry[[flag]]) || anyNA(registry[[flag]])) {
      stop("target registry의 ", flag, "는 NA가 없는 logical이어야 합니다.")
    }
  }
  if (sum(registry$web_default) != 1L) {
    stop("target registry에는 web_default target이 정확히 하나 있어야 합니다.")
  }
  if (any(!(registry$official_tcode %in% 1:7))) {
    stop("target registry의 official_tcode가 유효하지 않습니다.")
  }
  if (any(!(registry$effective_tcode %in% 1:7))) {
    stop("target registry의 effective_tcode가 유효하지 않습니다.")
  }

  allowed_modes <- c(
    "direct_level",
    "direct_log_level",
    "cumulative_arithmetic_change",
    "cumulative_log_change",
    "cumulative_percent_change"
  )
  if (any(!(registry$level_forecast_mode %in% allowed_modes))) {
    stop("target registry의 level_forecast_mode가 유효하지 않습니다.")
  }

  invisible(TRUE)
}

resolve_target_spec <- function(registry, target_code, require_eligible = TRUE) {
  validate_target_registry(registry)

  if (!is.character(target_code) || length(target_code) != 1L || !nzchar(target_code)) {
    stop("target_code는 하나의 비어 있지 않은 문자열이어야 합니다.")
  }

  matched <- which(registry$target_code == target_code)
  if (length(matched) != 1L) {
    stop("Target Registry에서 목표변수를 찾을 수 없습니다: ", target_code)
  }

  spec <- registry[matched, , drop = FALSE]
  if (isTRUE(require_eligible) && !isTRUE(spec$eligible_default_run[[1L]])) {
    stop(
      "목표변수가 기본 Quick/Full 실행 조건을 충족하지 않습니다: ",
      target_code,
      " / ",
      spec$eligibility_status[[1L]]
    )
  }

  spec
}
