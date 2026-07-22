###############################################################################
### Target-Aware and Rolling-Window-Safe Missing-Data Utilities
###############################################################################

max_na_run_length <- function(x) {
  missing <- is.na(x) | !is.finite(x)
  if (!any(missing)) return(0L)

  observed <- which(!missing)
  if (length(observed) == 0L) return(length(missing))

  internal <- missing[min(observed):max(observed)]
  runs <- rle(internal)
  if (!any(runs$values)) 0L else max(runs$lengths[runs$values])
}


normalize_missing_policy <- function(policy = NULL) {
  defaults <- list(
    target = list(
      method = "causal_locf",
      max_gap = 3L,
      long_gap_action = "stop",
      allow_imputed_oos_actual = FALSE,
      allow_imputed_origin_level = FALSE
    ),
    predictor = list(
      short_gap_method = "causal_locf",
      max_gap = 3L,
      min_obs_ratio = 0.90,
      min_observed = 24L,
      final_method = "pca_em",
      drop_insufficient = TRUE
    )
  )

  if (is.null(policy)) policy <- list()
  if (!is.list(policy)) stop("missing-data policy는 list여야 합니다.")

  merge_section <- function(default_section, supplied_section) {
    if (is.null(supplied_section)) supplied_section <- list()
    if (!is.list(supplied_section)) stop("missing-data policy section은 list여야 합니다.")
    utils::modifyList(default_section, supplied_section, keep.null = TRUE)
  }

  output <- list(
    target = merge_section(defaults$target, policy$target),
    predictor = merge_section(defaults$predictor, policy$predictor)
  )

  target_method <- as.character(output$target$method)[1L]
  if (!(target_method %in% c("causal_locf", "none"))) {
    stop("target missing method는 'causal_locf' 또는 'none'이어야 합니다.")
  }
  output$target$method <- target_method

  target_gap <- validate_scalar_integer(
    output$target$max_gap,
    "target missing max_gap",
    minimum = 0L
  )
  output$target$max_gap <- target_gap

  long_gap_action <- as.character(output$target$long_gap_action)[1L]
  if (!(long_gap_action %in% c("stop", "leave"))) {
    stop("target long_gap_action은 'stop' 또는 'leave'여야 합니다.")
  }
  output$target$long_gap_action <- long_gap_action

  for (field in c(
    "allow_imputed_oos_actual",
    "allow_imputed_origin_level"
  )) {
    value <- output$target[[field]]
    if (length(value) != 1L || is.na(value) || !is.logical(value)) {
      stop("target missing policy의 ", field, "는 하나의 TRUE/FALSE여야 합니다.")
    }
  }

  predictor_method <- as.character(output$predictor$short_gap_method)[1L]
  if (!(predictor_method %in% c("causal_locf", "none"))) {
    stop("predictor short_gap_method는 'causal_locf' 또는 'none'이어야 합니다.")
  }
  output$predictor$short_gap_method <- predictor_method

  output$predictor$max_gap <- validate_scalar_integer(
    output$predictor$max_gap,
    "predictor missing max_gap",
    minimum = 0L
  )

  min_obs_ratio <- as.numeric(output$predictor$min_obs_ratio)[1L]
  if (!is.finite(min_obs_ratio) || min_obs_ratio <= 0 || min_obs_ratio > 1) {
    stop("predictor min_obs_ratio는 0보다 크고 1 이하여야 합니다.")
  }
  output$predictor$min_obs_ratio <- min_obs_ratio

  output$predictor$min_observed <- validate_scalar_integer(
    output$predictor$min_observed,
    "predictor min_observed",
    minimum = 2L
  )

  final_method <- as.character(output$predictor$final_method)[1L]
  if (!(final_method %in% c("pca_em", "median", "none"))) {
    stop("predictor final_method는 'pca_em', 'median', 'none' 중 하나여야 합니다.")
  }
  output$predictor$final_method <- final_method

  drop_insufficient <- output$predictor$drop_insufficient
  if (
    length(drop_insufficient) != 1L ||
      is.na(drop_insufficient) ||
      !is.logical(drop_insufficient)
  ) {
    stop("predictor drop_insufficient는 하나의 TRUE/FALSE여야 합니다.")
  }

  output
}


causal_locf_limited <- function(
    x,
    max_gap = 3L,
    fill_leading = FALSE,
    fill_trailing = TRUE
) {
  x <- as.numeric(x)
  max_gap <- validate_scalar_integer(max_gap, "max_gap", minimum = 0L)

  logical_args <- list(
    fill_leading = fill_leading,
    fill_trailing = fill_trailing
  )
  for (name in names(logical_args)) {
    value <- logical_args[[name]]
    if (length(value) != 1L || is.na(value) || !is.logical(value)) {
      stop(name, "은 하나의 TRUE/FALSE여야 합니다.")
    }
  }

  invalid <- is.na(x) | !is.finite(x)
  x[!is.finite(x)] <- NA_real_
  observed <- which(!invalid)

  imputed <- rep(FALSE, length(x))
  if (length(observed) == 0L || max_gap == 0L) {
    return(list(
      values = x,
      imputed = imputed,
      unresolved = is.na(x),
      max_internal_gap = max_na_run_length(x)
    ))
  }

  first_observed <- min(observed)
  last_observed <- max(observed)

  if (isTRUE(fill_leading) && first_observed > 1L) {
    leading_length <- first_observed - 1L
    if (leading_length <= max_gap) {
      x[seq_len(leading_length)] <- x[first_observed]
      imputed[seq_len(leading_length)] <- TRUE
    }
  }

  run_length <- 0L
  scan_end <- if (isTRUE(fill_trailing)) length(x) else last_observed

  if (first_observed < scan_end) {
    for (i in seq.int(first_observed + 1L, scan_end)) {
      if (is.na(x[i])) {
        run_length <- run_length + 1L
        if (run_length <= max_gap && !is.na(x[i - 1L])) {
          x[i] <- x[i - 1L]
          imputed[i] <- TRUE
        }
      } else {
        run_length <- 0L
      }
    }
  }

  list(
    values = x,
    imputed = imputed,
    unresolved = is.na(x),
    max_internal_gap = max_na_run_length(x)
  )
}


prepare_target_missing_data <- function(
    x,
    dates = NULL,
    target_code = "target",
    policy = NULL
) {
  normalized <- normalize_missing_policy(list(target = policy))$target
  x <- as.numeric(x)

  if (is.null(dates)) dates <- seq_along(x)
  if (length(dates) != length(x)) {
    stop("target dates와 값의 길이가 일치하지 않습니다.")
  }

  original <- x
  invalid <- is.na(x) | !is.finite(x)
  x[!is.finite(x)] <- NA_real_

  if (all(invalid)) {
    stop("목표변수에 이용 가능한 관측값이 없습니다: ", target_code)
  }

  prepared <- if (normalized$method == "causal_locf") {
    causal_locf_limited(
      x,
      max_gap = normalized$max_gap,
      fill_leading = FALSE,
      fill_trailing = FALSE
    )
  } else {
    list(
      values = x,
      imputed = rep(FALSE, length(x)),
      unresolved = is.na(x),
      max_internal_gap = max_na_run_length(x)
    )
  }

  observed <- which(!is.na(original) & is.finite(original))
  first_observed <- min(observed)
  last_observed <- max(observed)
  unresolved_internal <- prepared$unresolved
  unresolved_internal[seq_len(first_observed - 1L)] <- FALSE
  if (last_observed < length(x)) {
    unresolved_internal[seq.int(last_observed + 1L, length(x))] <- FALSE
  }

  if (any(unresolved_internal) && normalized$long_gap_action == "stop") {
    first_problem <- which(unresolved_internal)[1L]
    stop(
      "목표변수에 허용 범위를 초과한 내부 결측구간이 있습니다: ",
      target_code,
      " / 확인 시점: ",
      as.character(dates[first_problem])
    )
  }

  list(
    values = prepared$values,
    original = original,
    observed = !invalid,
    imputed = prepared$imputed,
    unresolved = prepared$unresolved,
    unresolved_internal = unresolved_internal,
    first_observed = first_observed,
    last_observed = last_observed,
    max_internal_gap = prepared$max_internal_gap,
    policy = normalized
  )
}


target_imputation_effect_mask <- function(imputed, tcode) {
  imputed <- as.logical(imputed)
  if (anyNA(imputed)) stop("imputed mask에 NA가 있습니다.")

  tcode <- validate_scalar_integer(tcode, "tcode", minimum = 1L)
  if (!(tcode %in% 1:7)) stop("tcode는 1~7이어야 합니다.")

  effect_length <- if (tcode %in% c(1L, 4L)) {
    0L
  } else if (tcode %in% c(2L, 5L)) {
    1L
  } else {
    2L
  }

  output <- rep(FALSE, length(imputed))
  affected <- which(imputed)
  if (length(affected) == 0L) return(output)

  indices <- unique(unlist(
    lapply(affected, function(i) i + 0:effect_length),
    use.names = FALSE
  ))
  indices <- indices[indices <= length(output)]
  output[indices] <- TRUE
  output
}


summarize_target_imputation <- function(
    prepared,
    dates,
    target_code,
    target_name = target_code
) {
  if (!is.list(prepared) || is.null(prepared$imputed) || is.null(prepared$values)) {
    stop("prepared target missing-data result가 올바르지 않습니다.")
  }
  if (length(dates) != length(prepared$values)) {
    stop("target imputation summary의 날짜 길이가 일치하지 않습니다.")
  }

  index <- which(prepared$imputed)
  data.frame(
    date = as.Date(dates[index]),
    variable_code = rep(target_code, length(index)),
    variable_name = rep(target_name, length(index)),
    imputed_value = prepared$values[index],
    method = rep(prepared$policy$method, length(index)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


median_impute_predictors <- function(Y.window) {
  output <- as.matrix(Y.window)
  for (j in seq.int(2L, ncol(output))) {
    missing <- is.na(output[, j])
    if (!any(missing)) next
    replacement <- stats::median(output[, j], na.rm = TRUE)
    if (!is.finite(replacement)) {
      stop("중앙값 대체값을 계산할 수 없는 변수가 있습니다: ", colnames(output)[j])
    }
    output[missing, j] <- replacement
  }
  output
}


prepare_model_window_missing_data <- function(
    Y.window,
    prepare_predictors = TRUE,
    predictor_policy = NULL,
    pca_em_settings = list(
      n_factors = 4L,
      max_iter = 300L,
      tol = 1e-5,
      require_convergence = TRUE
    )
) {
  Y.original <- as.matrix(Y.window)
  storage.mode(Y.original) <- "double"

  if (nrow(Y.original) < 3L || ncol(Y.original) < 1L) {
    stop("결측치 처리용 학습 window의 크기가 충분하지 않습니다.")
  }
  if (is.null(colnames(Y.original))) {
    colnames(Y.original) <- c(
      "target",
      if (ncol(Y.original) > 1L) paste0("X", seq_len(ncol(Y.original) - 1L)) else NULL
    )
  }
  if (any(is.infinite(Y.original)) || any(is.nan(Y.original))) {
    stop("학습 window에 Inf 또는 NaN이 있습니다.")
  }
  if (anyNA(Y.original[, 1L])) {
    stop("학습 window의 목표변수에 결측치가 있습니다.")
  }

  normalized <- normalize_missing_policy(
    list(predictor = predictor_policy)
  )$predictor

  missing_before <- sum(is.na(Y.original))

  if (!isTRUE(prepare_predictors) || ncol(Y.original) == 1L) {
    output <- Y.original[, 1L, drop = FALSE]
    attr(output, "missing_before") <- missing_before
    attr(output, "short_gap_imputed_count") <- 0L
    attr(output, "remaining_before_final") <- 0L
    attr(output, "final_imputed_count") <- 0L
    attr(output, "missing_after") <- 0L
    attr(output, "dropped_predictors") <- colnames(Y.original)[-1L]
    attr(output, "dropped_predictor_count") <- max(0L, ncol(Y.original) - 1L)
    attr(output, "em_iterations") <- NA_integer_
    attr(output, "em_converged") <- NA
    attr(output, "em_last_change") <- NA_real_
    attr(output, "imputed_count") <- 0L
    return(output)
  }

  predictor_original <- Y.original[, -1L, drop = FALSE]
  observed_count <- colSums(!is.na(predictor_original))
  observed_ratio <- observed_count / nrow(predictor_original)
  # A matrix passed directly to vapply() is traversed element-by-element,
  # not column-by-column. Compute one standard deviation per predictor column.
  observed_sd <- vapply(
    seq_len(ncol(predictor_original)),
    function(j) stats::sd(predictor_original[, j], na.rm = TRUE),
    FUN.VALUE = numeric(1)
  )
  names(observed_sd) <- colnames(predictor_original)

  effective_min_observed <- min(
    normalized$min_observed,
    nrow(predictor_original)
  )

  active <- (
    observed_count >= effective_min_observed &
      observed_ratio >= normalized$min_obs_ratio &
      is.finite(observed_sd) &
      observed_sd > sqrt(.Machine$double.eps)
  )
  active[is.na(active)] <- FALSE
  names(active) <- colnames(predictor_original)

  if (!isTRUE(normalized$drop_insufficient) && any(!active)) {
    stop(
      "현재 rolling window에서 결측률 또는 변동성 조건을 충족하지 못한 변수가 있습니다: ",
      paste(colnames(predictor_original)[!active], collapse = ", ")
    )
  }

  dropped_predictors <- colnames(predictor_original)[!active]
  active_predictors <- predictor_original[, active, drop = FALSE]

  if (ncol(active_predictors) < 1L) {
    stop("현재 rolling window에 결측치 정책을 통과한 설명변수가 없습니다.")
  }

  short_gap_imputed_count <- 0L
  if (normalized$short_gap_method == "causal_locf" && normalized$max_gap > 0L) {
    for (j in seq_len(ncol(active_predictors))) {
      current <- causal_locf_limited(
        active_predictors[, j],
        max_gap = normalized$max_gap,
        fill_leading = FALSE,
        fill_trailing = TRUE
      )
      active_predictors[, j] <- current$values
      short_gap_imputed_count <- short_gap_imputed_count + sum(current$imputed)
    }
  }

  prepared <- cbind(Y.original[, 1L, drop = FALSE], active_predictors)
  remaining_before_final <- sum(is.na(prepared))
  final_imputed_count <- 0L
  em_iterations <- NA_integer_
  em_converged <- NA
  em_last_change <- NA_real_

  if (remaining_before_final > 0L) {
    if (normalized$final_method == "pca_em") {
      prepared <- pca_em_impute(
        prepared,
        n_factors = pca_em_settings$n_factors,
        max_iter = pca_em_settings$max_iter,
        tol = pca_em_settings$tol
      )
      final_imputed_count <- as.integer(attr(prepared, "imputed_count"))
      em_iterations <- as.integer(attr(prepared, "em_iterations"))
      em_converged <- as.logical(attr(prepared, "em_converged"))
      em_last_change <- as.numeric(attr(prepared, "em_last_change"))

      if (
        isTRUE(get_config_value(pca_em_settings, "require_convergence", TRUE)) &&
          !isTRUE(em_converged)
      ) {
        stop("학습 window의 PCA-EM이 수렴하지 않았습니다.")
      }
    } else if (normalized$final_method == "median") {
      prepared <- median_impute_predictors(prepared)
      final_imputed_count <- remaining_before_final
    } else {
      stop("최종 결측치 대체를 사용하지 않아 학습 window에 NA가 남았습니다.")
    }
  } else if (normalized$final_method == "pca_em") {
    em_iterations <- 0L
    em_converged <- TRUE
    em_last_change <- 0
  }

  missing_after <- sum(is.na(prepared))
  if (missing_after > 0L || any(!is.finite(prepared))) {
    stop("결측치 처리 후 학습 window에 NA 또는 비유한 값이 남았습니다.")
  }

  attr(prepared, "missing_before") <- missing_before
  attr(prepared, "short_gap_imputed_count") <- as.integer(short_gap_imputed_count)
  attr(prepared, "remaining_before_final") <- as.integer(remaining_before_final)
  attr(prepared, "final_imputed_count") <- as.integer(final_imputed_count)
  attr(prepared, "missing_after") <- as.integer(missing_after)
  attr(prepared, "dropped_predictors") <- dropped_predictors
  attr(prepared, "dropped_predictor_count") <- length(dropped_predictors)
  attr(prepared, "active_predictors") <- colnames(prepared)[-1L]
  attr(prepared, "em_iterations") <- em_iterations
  attr(prepared, "em_converged") <- em_converged
  attr(prepared, "em_last_change") <- em_last_change
  attr(prepared, "imputed_count") <- as.integer(
    short_gap_imputed_count + final_imputed_count
  )

  prepared
}
