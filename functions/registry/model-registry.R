###############################################################################
### Model Registry
###############################################################################

create_model_registry <- function(model_control) {
  list(
    RW = list(
      label = "RW",
      family = "Baseline",
      model_function = forecast_rw_model,
      requires_feature_bundle = FALSE,
      required_packages = character(0),
      config = list()
    ),
    AR = list(
      label = paste0("AR(", model_control$ar$ar_lags, ")"),
      family = "Baseline",
      model_function = forecast_ar_model,
      requires_feature_bundle = FALSE,
      required_packages = character(0),
      config = model_control$ar
    ),
    Ridge = list(
      label = "Ridge",
      family = "Regularization",
      model_function = forecast_glmnet_model,
      requires_feature_bundle = TRUE,
      required_packages = "glmnet",
      config = merge_lists(model_control$glmnet, list(alpha = 0))
    ),
    LASSO = list(
      label = "LASSO",
      family = "Regularization",
      model_function = forecast_glmnet_model,
      requires_feature_bundle = TRUE,
      required_packages = "glmnet",
      config = merge_lists(model_control$glmnet, list(alpha = 1))
    ),
    ElasticNet = list(
      label = "ElasticNet",
      family = "Regularization",
      model_function = forecast_glmnet_model,
      requires_feature_bundle = TRUE,
      required_packages = "glmnet",
      config = merge_lists(model_control$glmnet, list(alpha = 0.5))
    ),
    Factor = list(
      label = "Factor",
      family = "Factor",
      model_function = forecast_factor_model,
      requires_feature_bundle = TRUE,
      required_packages = character(0),
      config = model_control$factor
    ),
    RandomForest = list(
      label = "RandomForest",
      family = "Tree",
      model_function = forecast_rf_model,
      requires_feature_bundle = TRUE,
      required_packages = "randomForest",
      config = model_control$random_forest
    ),
    XGBoost = list(
      label = "XGBoost",
      family = "Boosting",
      model_function = forecast_xgboost_model,
      requires_feature_bundle = TRUE,
      required_packages = "xgboost",
      config = model_control$xgboost
    ),
    BorutaRF = list(
      label = "BorutaRF",
      family = "FeatureSelection+Tree",
      model_function = forecast_boruta_rf_model,
      requires_feature_bundle = TRUE,
      required_packages = c("Boruta", "randomForest"),
      config = model_control$boruta_rf
    )
  )
}

validate_model_registry <- function(registry, models_to_run) {
  if (length(models_to_run) < 1L) {
    stop("실행할 모형이 하나 이상 필요합니다.")
  }
  if (anyDuplicated(models_to_run) > 0L) {
    stop("models_to_run에 중복된 모형이 있습니다.")
  }

  unknown_models <- setdiff(models_to_run, names(registry))
  if (length(unknown_models) > 0L) {
    stop(
      "model_registry에 없는 모형이 지정되었습니다: ",
      paste(unknown_models, collapse = ", ")
    )
  }

  required_fields <- c(
    "label",
    "family",
    "model_function",
    "requires_feature_bundle",
    "required_packages",
    "config"
  )

  for (model_key in names(registry)) {
    missing_fields <- setdiff(required_fields, names(registry[[model_key]]))
    if (length(missing_fields) > 0L) {
      stop(
        "model_registry 항목에 필요한 필드가 없습니다 [",
        model_key,
        "]: ",
        paste(missing_fields, collapse = ", ")
      )
    }

    specification <- registry[[model_key]]

    if (
      !is.character(specification$label) ||
      length(specification$label) != 1L ||
      !nzchar(specification$label)
    ) {
      stop("등록된 model label이 유효하지 않습니다: ", model_key)
    }

    if (
      !is.character(specification$family) ||
      length(specification$family) != 1L ||
      !nzchar(specification$family)
    ) {
      stop("등록된 model family가 유효하지 않습니다: ", model_key)
    }

    if (!is.function(specification$model_function)) {
      stop("등록된 model_function이 함수가 아닙니다: ", model_key)
    }

    if (
      !is.logical(specification$requires_feature_bundle) ||
      length(specification$requires_feature_bundle) != 1L ||
      is.na(specification$requires_feature_bundle)
    ) {
      stop("requires_feature_bundle 설정이 유효하지 않습니다: ", model_key)
    }

    if (!is.character(specification$required_packages)) {
      stop("required_packages는 character 벡터여야 합니다: ", model_key)
    }

    if (!is.list(specification$config)) {
      stop("모형 config는 list여야 합니다: ", model_key)
    }
  }

  model_labels <- vapply(
    registry,
    function(x) x$label,
    FUN.VALUE = character(1)
  )

  if (anyDuplicated(model_labels) > 0L) {
    duplicate_labels <- unique(model_labels[duplicated(model_labels)])
    stop(
      "model_registry의 출력 label이 중복됩니다: ",
      paste(duplicate_labels, collapse = ", ")
    )
  }

  required_packages <- unique(
    unlist(
      lapply(registry[models_to_run], `[[`, "required_packages"),
      use.names = FALSE
    )
  )
  check_required_packages(required_packages)
  invisible(TRUE)
}

summarize_model_registry <- function(registry) {
  data.frame(
    model_key = names(registry),
    model = vapply(registry, `[[`, character(1), "label"),
    model_family = vapply(registry, `[[`, character(1), "family"),
    requires_feature_bundle = vapply(
      registry,
      function(x) isTRUE(x$requires_feature_bundle),
      logical(1)
    ),
    required_packages = vapply(
      registry,
      function(x) paste(x$required_packages, collapse = ", "),
      character(1)
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

resolve_ensemble_member_labels <- function(ensemble_members, registry) {
  if (is.null(ensemble_members)) {
    return(NULL)
  }

  labels <- vapply(registry, `[[`, character(1), "label")
  output <- as.character(ensemble_members)

  key_match <- match(output, names(registry))
  matched_keys <- !is.na(key_match)
  output[matched_keys] <- labels[key_match[matched_keys]]

  unknown <- setdiff(output, labels)
  if (length(unknown) > 0L) {
    stop(
      "ensemble_members에 등록되지 않은 모형이 있습니다: ",
      paste(unknown, collapse = ", ")
    )
  }

  unique(output)
}
