###############################################################################
### Common Utilities
###############################################################################

merge_lists <- function(x, y) {
  if (is.null(x)) x <- list()
  if (is.null(y)) y <- list()
  utils::modifyList(x, y, keep.null = TRUE)
}

get_config_value <- function(x, name, default = NULL) {
  if (is.null(x) || is.null(x[[name]])) default else x[[name]]
}

check_required_packages <- function(packages) {
  packages <- unique(packages[nzchar(packages)])
  missing_packages <- packages[
    !vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]

  if (length(missing_packages) > 0L) {
    stop(
      "다음 R 패키지가 필요합니다: ",
      paste(missing_packages, collapse = ", "),
      ". install.packages()로 설치한 후 다시 실행하세요."
    )
  }

  invisible(TRUE)
}

make_model_seed <- function(base_seed, model_index, horizon, forecast_number) {
  as.integer(
    (as.double(base_seed) +
       100000 * as.double(model_index) +
       1000 * as.double(horizon) +
       as.double(forecast_number)) %%
      .Machine$integer.max
  )
}

validate_scalar_integer <- function(x, name, minimum = NULL) {
  if (
    length(x) != 1L ||
    is.na(x) ||
    !is.finite(x) ||
    x != as.integer(x) ||
    (!is.null(minimum) && x < minimum)
  ) {
    stop(name, " 설정이 유효하지 않습니다.")
  }
  as.integer(x)
}

validate_scalar_numeric <- function(
    x,
    name,
    minimum = NULL,
    maximum = NULL,
    minimum_inclusive = TRUE,
    maximum_inclusive = TRUE
) {
  invalid <- (
    length(x) != 1L ||
      is.na(x) ||
      !is.finite(x)
  )

  if (!invalid && !is.null(minimum)) {
    invalid <- if (minimum_inclusive) x < minimum else x <= minimum
  }

  if (!invalid && !is.null(maximum)) {
    invalid <- if (maximum_inclusive) x > maximum else x >= maximum
  }

  if (invalid) {
    stop(name, " 설정이 유효하지 않습니다.")
  }

  as.numeric(x)
}

validate_probability <- function(
    x,
    name,
    allow_zero = FALSE,
    allow_one = TRUE
) {
  validate_scalar_numeric(
    x = x,
    name = name,
    minimum = 0,
    maximum = 1,
    minimum_inclusive = allow_zero,
    maximum_inclusive = allow_one
  )
}

as_named_numeric <- function(x, names_value = NULL) {
  x <- as.numeric(x)
  if (!is.null(names_value)) names(x) <- names_value
  x
}

report_experiment_progress <- function(
    stage,
    horizon = NA_integer_,
    forecast_number = NA_integer_,
    forecast_total = NA_integer_,
    message = NULL,
    progress_percent = NA_real_
) {
  callback <- getOption("ml_forecast.progress_callback", NULL)
  if (!is.function(callback)) return(invisible(NULL))

  try(
    callback(
      stage = stage,
      horizon = horizon,
      forecast_number = forecast_number,
      forecast_total = forecast_total,
      message = message,
      progress_percent = progress_percent
    ),
    silent = TRUE
  )

  invisible(NULL)
}
