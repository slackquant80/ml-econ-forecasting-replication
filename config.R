###############################################################################
### FRED-MD Forecasting Project Configuration
###############################################################################

# 대시보드/실험 실행기에서 전달하는 선택적 runtime override.
# 기본 main.R 실행에서는 빈 list이므로 기존 config 값과 완전히 동일하게 작동한다.
runtime_config_overrides <- getOption(
  "ml_forecast.runtime_overrides",
  list()
)

if (is.null(runtime_config_overrides)) {
  runtime_config_overrides <- list()
}

if (!is.list(runtime_config_overrides)) {
  stop("ml_forecast.runtime_overrides option은 list여야 합니다.")
}

has_runtime_config_override <- function(name) {
  name %in% names(runtime_config_overrides)
}

get_runtime_config_override <- function(name, default) {
  if (has_runtime_config_override(name)) {
    runtime_config_overrides[[name]]
  } else {
    default
  }
}

# 데이터와 목표변수
fred_md_file <- get_runtime_config_override(
  "fred_md_file",
  file.path(project_root, "data", "current.csv")
)
target_name <- get_runtime_config_override("target_name", "CPIAUCSL")
fred_md_data_md5 <- if (file.exists(fred_md_file)) {
  unname(as.character(tools::md5sum(fred_md_file)))
} else {
  NA_character_
}

# 목표변수별 선택적 transformation-code override.
# 목록에 없는 목표변수는 FRED-MD 공식 tcode를 그대로 사용한다.
# CPIAUCSL과 PCEPI는 가격수준 자체가 아니라 월별 인플레이션을 기본
# transformed target으로 사용한다. 두 변수 모두 공식 FRED-MD tcode 6 대신
# effective tcode 5를 적용한다. 공식 tcode를 그대로 사용하려면 integer(0)으로
# 바꿀 수 있다.
target_tcode_overrides <- get_runtime_config_override(
  "target_tcode_overrides",
  c(CPIAUCSL = 5L, PCEPI = 5L)
)

default_target_tcode_override <- if (target_name %in% names(target_tcode_overrides)) {
  as.integer(unname(target_tcode_overrides[target_name]))
} else {
  NULL
}

target_tcode_override <- get_runtime_config_override(
  "target_tcode_override",
  default_target_tcode_override
)

# 누적 transformed-change / 원수준 복원 track은 effective tcode에서 자동 결정한다.
# 특별한 연구설정이 필요할 때만 명시적으로 override한다.
target_level_mode_override <- get_runtime_config_override(
  "target_level_mode_override",
  NULL
)

# Rolling backtest
# 주 사양: 각 OOS 예측시점에서 forecast origin까지의 최근 360개월(30년)을
# 고정 길이 rolling window로 사용한다. OOS 목표월은 최신 90개월을 유지한다.
window_size <- get_runtime_config_override("window_size", 360L)
npred <- get_runtime_config_override("npred", 90L)
forecast_horizons <- get_runtime_config_override(
  "forecast_horizons",
  c(1L, 3L, 6L, 12L)
)
max_horizon <- max(forecast_horizons)

# 데이터 전처리
# Target과 predictor는 역할이 다르므로 결측치 정책을 분리한다.
# Target은 짧은 내부 결측만 과거값으로 보완하고, 대체된 actual은 평가에서 제외한다.
# Predictor는 각 rolling window 내부에서만 짧은 gap을 causal LOCF로 보완한 뒤
# 남은 결측을 PCA-EM으로 처리한다.
min_obs_ratio <- get_runtime_config_override("min_obs_ratio", 0.90)
target_missing_method <- get_runtime_config_override(
  "target_missing_method",
  "causal_locf"
)
max_target_gap <- get_runtime_config_override("max_target_gap", 3L)
target_long_gap_action <- get_runtime_config_override(
  "target_long_gap_action",
  "stop"
)
allow_imputed_oos_actual <- get_runtime_config_override(
  "allow_imputed_oos_actual",
  FALSE
)
allow_imputed_origin_level <- get_runtime_config_override(
  "allow_imputed_origin_level",
  FALSE
)

predictor_short_gap_method <- get_runtime_config_override(
  "predictor_short_gap_method",
  "causal_locf"
)
max_predictor_gap <- get_runtime_config_override("max_predictor_gap", 3L)
predictor_window_min_obs_ratio <- get_runtime_config_override(
  "predictor_window_min_obs_ratio",
  min_obs_ratio
)
predictor_window_min_observed <- get_runtime_config_override(
  "predictor_window_min_observed",
  24L
)
predictor_final_imputation <- get_runtime_config_override(
  "predictor_final_imputation",
  "pca_em"
)
predictor_drop_insufficient <- get_runtime_config_override(
  "predictor_drop_insufficient",
  TRUE
)

pca_em_factors <- 4L
pca_em_max_iter <- 300L
# PCA-EM은 표준화 자료에서 1e-4 이하의 갱신 차이를 수렴으로 본다.
# 최대 반복 후에도 수렴하지 않은 window는 중단하지 않고 진단값으로 기록한다.
pca_em_tol <- 1e-4
pca_em_require_convergence <- FALSE

# 공통 예측 설계
predictor_lags <- get_runtime_config_override("predictor_lags", 4L)
factor_count <- get_runtime_config_override("factor_count", 4L)
factor_include_target <- get_runtime_config_override(
  "factor_include_target",
  FALSE
)
factor_scale <- get_runtime_config_override("factor_scale", TRUE)

# 실행 강도: "preview"는 축소 OOS 미리보기, "quick"은 개발·검증용,
# "full"은 최종 연구용
execution_profile <- get_runtime_config_override(
  "execution_profile",
  "quick"
)

# 실행할 기본모형
models_to_run <- get_runtime_config_override(
  "models_to_run",
  c(
    "RW",
    "AR",
    "Ridge",
    "LASSO",
    "ElasticNet",
    "Factor",
    "RandomForest",
    "XGBoost",
    "BorutaRF"
  )
)

# 오류가 발생하면 즉시 중단하여 조용한 실패를 방지한다.
error_policy <- "stop"  # "stop" 또는 "record"
show_progress <- get_runtime_config_override("show_progress", TRUE)
base_seed <- get_runtime_config_override("base_seed", 20260716L)

# 마지막 rolling window의 계수·중요도만 저장한다.
save_artifacts <- "last_window"  # "none", "last_window", "all"
save_results <- get_runtime_config_override("save_results", TRUE)
results_directory <- get_runtime_config_override(
  "results_directory",
  file.path(project_root, "results")
)

# 모형별 설정
# predictor lag와 factor 수는 feature_settings를 통해 공통 전달하므로
# model_control에는 실제로 각 모형이 사용하는 설정만 둔다.
model_control <- list(
  ar = list(
    ar_lags = 4L
  ),
  glmnet = list(
    standardize = TRUE,
    intercept = TRUE,
    validation_fraction = 0.20,
    min_validation = 12L,
    lambda_rule = "min"  # "min" 또는 "1se"
  ),
  factor = list(
    max_lags = predictor_lags
  ),
  random_forest = list(
    ntree = if (execution_profile %in% c("preview", "quick")) 250L else 600L,
    mtry = NULL,
    nodesize = 5L,
    importance_top_n = 20L
  ),
  xgboost = list(
    max_nrounds = if (execution_profile %in% c("preview", "quick")) 250L else 800L,
    early_stopping_rounds = 25L,
    validation_fraction = 0.20,
    min_validation = 12L,
    eta = 0.05,
    max_depth = 4L,
    min_child_weight = 1,
    subsample = 0.90,
    colsample_bytree = 0.75,
    nthread = 1L,
    importance_top_n = 20L
  ),
  boruta_rf = list(
    # 각 horizon의 첫 window와 이후 frequency 간격마다 변수선택을 갱신한다.
    selection_frequency = if (execution_profile %in% c("preview", "quick")) 12L else 6L,
    max_runs = if (execution_profile %in% c("preview", "quick")) 40L else 100L,
    ntree = if (execution_profile %in% c("preview", "quick")) 250L else 600L,
    mtry = NULL,
    nodesize = 5L,
    importance_top_n = 20L,
    fallback_features = 10L
  )
)

# Boruta 검증 요약
# 별도의 반복 Boruta를 추가로 실행하지 않고, 실제 rolling selection refresh
# 이력으로 선택 빈도·Jaccard 안정성·fallback 여부를 요약한다.
enable_boruta_validation_summary <- get_runtime_config_override(
  "enable_boruta_validation_summary",
  TRUE
)
boruta_stability_threshold <- get_runtime_config_override(
  "boruta_stability_threshold",
  0.60
)

# 앙상블
ensemble_methods <- c("mean", "median", "inverse_rmse")
# NULL이면 실행에 성공한 모든 기본모형을 사용한다.
# 모형 key(Ridge, AR 등)와 출력 label(AR(4) 등)을 모두 허용한다.
ensemble_members <- NULL
ensemble_min_members <- 2L
ensemble_min_history <- 12L
ensemble_weight_epsilon <- 1e-8

# 미래 원자료 수준 예측
# 기존 target-month transformed-change backtest는 유지한다. 별도 누적
# transformed-change target을 horizon별로 직접 예측한 뒤, 원시계열 수준으로
# 역변환하여 OOS level 평가와 최신 1·3·6·12개월 forward forecast를 만든다.
enable_forward_forecasts <- get_runtime_config_override(
  "enable_forward_forecasts",
  TRUE
)

# SSRN working-paper 평가 protocol.
# Primary track은 target-month transformed value이며 cumulative/level track은
# 경제적 해석과 robustness를 위한 secondary track으로 사용한다.
primary_evaluation_track <- "monthly_transformed"
secondary_evaluation_tracks <- c("cumulative_level")
primary_inference_methods <- c("DM", "MCS")
supplementary_inference_methods <- c("HAC_POST_SELECTION_MAE")

# 통계적 예측력 검증
# DM: 각 모형 vs RW, horizon별 직렬상관 조정 + Holm 보정.
# 기존 gw_* 결과 객체는 하위호환성을 위해 유지하지만, 실질적으로는
# track/horizon별 matched-OOS MAE winner를 사후 선택한 뒤 평균 절대오차
# 차이를 Newey-West HAC로 비교하는 supplementary diagnostic이다. 정식
# conditional Giacomini-White test로 해석하지 않는다.
# MCS: 손실함수별 superior set을 block bootstrap으로 구성하고 자동 audit한다.
# quick은 개발용 1,000회, full은 5,000회다.
enable_statistical_validation <- get_runtime_config_override(
  "enable_statistical_validation",
  TRUE
)
statistical_benchmark_model <- "RW"
statistical_loss_functions <- c("SE", "AE")
statistical_significance_level <- 0.05
dm_alternative <- "two.sided"
dm_varestimator <- "bartlett"
dm_p_adjust_method <- "holm"
gw_alternative <- "two.sided"
gw_method <- "NeweyWest"
gw_p_adjust_method <- "holm"
gw_reference_rule <- "horizon_specific_mae_winner"
gw_comparison_label <- "HAC-adjusted post-selection MAE comparison"
gw_inference_role <- "supplementary_diagnostic"
gw_formal_giacomini_white_test <- FALSE
mcs_alpha <- 0.10
mcs_bootstrap_samples <- if (execution_profile %in% c("preview", "quick")) 1000L else 5000L
mcs_statistic <- "Tmax"
mcs_block_length <- NULL
mcs_min_block_length <- 3L

# 실험 실행 메타데이터. 일반 main.R 실행에서는 NA/direct_main이다.
experiment_run_id <- get_runtime_config_override(
  "experiment_run_id",
  NA_character_
)
experiment_source <- get_runtime_config_override(
  "experiment_source",
  "direct_main"
)
experiment_requested_at <- get_runtime_config_override(
  "experiment_requested_at",
  NA_character_
)

###############################################################################
### Configuration Validation
###############################################################################

if (!is.character(target_name) || length(target_name) != 1L || !nzchar(target_name)) {
  stop("target_name은 하나의 비어 있지 않은 문자열이어야 합니다.")
}

for (field_name in c("experiment_run_id", "experiment_source", "experiment_requested_at")) {
  field_value <- get(field_name, inherits = FALSE)
  if (
    length(field_value) != 1L ||
      (!is.na(field_value) && (!is.character(field_value) || !nzchar(field_value)))
  ) {
    stop(field_name, "는 NA 또는 하나의 비어 있지 않은 문자열이어야 합니다.")
  }
}

if (length(target_tcode_overrides) > 0L) {
  if (
    is.null(names(target_tcode_overrides)) ||
    any(!nzchar(names(target_tcode_overrides))) ||
    anyDuplicated(names(target_tcode_overrides)) > 0L ||
    anyNA(target_tcode_overrides) ||
    any(!(target_tcode_overrides %in% 1:7))
  ) {
    stop("target_tcode_overrides는 이름이 있는 1~7 정수 벡터여야 합니다.")
  }
}

integer_settings <- list(
  window_size = window_size,
  npred = npred,
  max_target_gap = max_target_gap,
  max_predictor_gap = max_predictor_gap,
  predictor_window_min_observed = predictor_window_min_observed,
  pca_em_factors = pca_em_factors,
  pca_em_max_iter = pca_em_max_iter,
  predictor_lags = predictor_lags,
  factor_count = factor_count,
  base_seed = base_seed,
  ensemble_min_members = ensemble_min_members,
  ensemble_min_history = ensemble_min_history,
  mcs_bootstrap_samples = mcs_bootstrap_samples,
  mcs_min_block_length = mcs_min_block_length
)

for (setting_name in names(integer_settings)) {
  setting_value <- integer_settings[[setting_name]]
  if (
    length(setting_value) != 1L ||
    is.na(setting_value) ||
    !is.finite(setting_value) ||
    setting_value != as.integer(setting_value) ||
    setting_value < 1L
  ) {
    stop(setting_name, "는 1 이상의 정수여야 합니다.")
  }
}

if (
  length(forecast_horizons) < 1L ||
  anyNA(forecast_horizons) ||
  any(!is.finite(forecast_horizons)) ||
  any(forecast_horizons != as.integer(forecast_horizons)) ||
  any(forecast_horizons < 1L) ||
  anyDuplicated(forecast_horizons) > 0L
) {
  stop("forecast_horizons는 중복 없는 1 이상의 정수 벡터여야 합니다.")
}

minimum_window_size <- max(forecast_horizons) + predictor_lags

if (window_size < minimum_window_size) {
  stop(
    "window_size는 최대 horizon과 predictor_lags를 고려하여 최소 ",
    minimum_window_size,
    "개월 이상이어야 합니다."
  )
}

if (predictor_window_min_observed > window_size) {
  stop("predictor_window_min_observed는 window_size 이하여야 합니다.")
}

if (window_size <= npred) {
  warning(
    "window_size가 npred 이하입니다. 학습기간이 OOS 평가기간보다 짧은 설정인지 확인하십시오.",
    call. = FALSE
  )
}

if (
  length(min_obs_ratio) != 1L ||
  is.na(min_obs_ratio) ||
  !is.finite(min_obs_ratio) ||
  min_obs_ratio <= 0 ||
  min_obs_ratio > 1
) {
  stop("min_obs_ratio는 0보다 크고 1 이하이어야 합니다.")
}

if (
  length(predictor_window_min_obs_ratio) != 1L ||
  is.na(predictor_window_min_obs_ratio) ||
  !is.finite(predictor_window_min_obs_ratio) ||
  predictor_window_min_obs_ratio <= 0 ||
  predictor_window_min_obs_ratio > 1
) {
  stop("predictor_window_min_obs_ratio는 0보다 크고 1 이하이어야 합니다.")
}


if (!is.null(target_level_mode_override)) {
  allowed_target_level_modes <- c(
    "direct_level",
    "direct_log_level",
    "cumulative_arithmetic_change",
    "cumulative_log_change",
    "cumulative_percent_change"
  )
  if (
    !is.character(target_level_mode_override) ||
      length(target_level_mode_override) != 1L ||
      is.na(target_level_mode_override) ||
      !(target_level_mode_override %in% allowed_target_level_modes)
  ) {
    stop("target_level_mode_override가 유효하지 않습니다.")
  }
}

if (!(target_missing_method %in% c("causal_locf", "none"))) {
  stop("target_missing_method는 'causal_locf' 또는 'none'이어야 합니다.")
}

if (!(target_long_gap_action %in% c("stop", "leave"))) {
  stop("target_long_gap_action은 'stop' 또는 'leave'여야 합니다.")
}

if (!(predictor_short_gap_method %in% c("causal_locf", "none"))) {
  stop("predictor_short_gap_method는 'causal_locf' 또는 'none'이어야 합니다.")
}

if (!(predictor_final_imputation %in% c("pca_em", "median", "none"))) {
  stop("predictor_final_imputation은 'pca_em', 'median', 'none' 중 하나여야 합니다.")
}

if (
  length(pca_em_tol) != 1L ||
  is.na(pca_em_tol) ||
  !is.finite(pca_em_tol) ||
  pca_em_tol <= 0
) {
  stop("pca_em_tol은 0보다 큰 유한한 값이어야 합니다.")
}

logical_settings <- list(
  allow_imputed_oos_actual = allow_imputed_oos_actual,
  allow_imputed_origin_level = allow_imputed_origin_level,
  predictor_drop_insufficient = predictor_drop_insufficient,
  pca_em_require_convergence = pca_em_require_convergence,
  enable_forward_forecasts = enable_forward_forecasts,
  enable_statistical_validation = enable_statistical_validation,
  factor_include_target = factor_include_target,
  factor_scale = factor_scale,
  show_progress = show_progress,
  save_results = save_results
)

for (setting_name in names(logical_settings)) {
  setting_value <- logical_settings[[setting_name]]
  if (length(setting_value) != 1L || is.na(setting_value) || !is.logical(setting_value)) {
    stop(setting_name, "는 하나의 TRUE 또는 FALSE여야 합니다.")
  }
}

if (!(execution_profile %in% c("preview", "quick", "full"))) {
  stop("execution_profile은 'preview', 'quick', 또는 'full'이어야 합니다.")
}

if (
  length(models_to_run) < 1L ||
  anyNA(models_to_run) ||
  any(!nzchar(models_to_run)) ||
  anyDuplicated(models_to_run) > 0L
) {
  stop("models_to_run은 중복 없는 비어 있지 않은 문자열 벡터여야 합니다.")
}

if (!(error_policy %in% c("stop", "record"))) {
  stop("error_policy는 'stop' 또는 'record'여야 합니다.")
}

if (
  length(statistical_loss_functions) < 1L ||
  anyNA(statistical_loss_functions) ||
  anyDuplicated(statistical_loss_functions) > 0L ||
  any(!(statistical_loss_functions %in% c("SE", "AE")))
) {
  stop("statistical_loss_functions는 중복 없는 'SE'와 'AE' 조합이어야 합니다.")
}

if (
  !is.character(statistical_benchmark_model) ||
  length(statistical_benchmark_model) != 1L ||
  !nzchar(statistical_benchmark_model)
) {
  stop("statistical_benchmark_model은 하나의 비어 있지 않은 문자열이어야 합니다.")
}

if (!identical(primary_evaluation_track, "monthly_transformed")) {
  stop("primary_evaluation_track은 현재 'monthly_transformed'여야 합니다.")
}

if (
  length(secondary_evaluation_tracks) < 1L ||
  anyNA(secondary_evaluation_tracks) ||
  any(!nzchar(secondary_evaluation_tracks)) ||
  anyDuplicated(secondary_evaluation_tracks) > 0L ||
  any(!(secondary_evaluation_tracks %in% c("cumulative_level")))
) {
  stop("secondary_evaluation_tracks 설정이 유효하지 않습니다.")
}

if (!identical(primary_inference_methods, c("DM", "MCS"))) {
  stop("primary_inference_methods는 c('DM', 'MCS')여야 합니다.")
}

if (!identical(supplementary_inference_methods, "HAC_POST_SELECTION_MAE")) {
  stop("supplementary_inference_methods 설정이 유효하지 않습니다.")
}

if (
  length(statistical_significance_level) != 1L ||
  is.na(statistical_significance_level) ||
  !is.finite(statistical_significance_level) ||
  statistical_significance_level <= 0 ||
  statistical_significance_level >= 1
) {
  stop("statistical_significance_level은 0과 1 사이여야 합니다.")
}

if (!(dm_alternative %in% c("two.sided", "less", "greater"))) {
  stop("dm_alternative 설정이 유효하지 않습니다.")
}

if (!(dm_varestimator %in% c("acf", "bartlett"))) {
  stop("dm_varestimator는 'acf' 또는 'bartlett'이어야 합니다.")
}

if (!(dm_p_adjust_method %in% stats::p.adjust.methods)) {
  stop("dm_p_adjust_method가 stats::p.adjust.methods에 없습니다.")
}

if (!(gw_alternative %in% c("two.sided", "less", "greater"))) {
  stop("gw_alternative 설정이 유효하지 않습니다.")
}

if (!identical(gw_method, "NeweyWest")) {
  stop("gw_method는 현재 'NeweyWest'만 지원합니다.")
}

if (!(gw_p_adjust_method %in% stats::p.adjust.methods)) {
  stop("gw_p_adjust_method가 stats::p.adjust.methods에 없습니다.")
}

if (!identical(gw_reference_rule, "horizon_specific_mae_winner")) {
  stop("gw_reference_rule은 'horizon_specific_mae_winner'여야 합니다.")
}

if (
  !is.character(gw_comparison_label) ||
  length(gw_comparison_label) != 1L ||
  is.na(gw_comparison_label) ||
  !nzchar(gw_comparison_label)
) {
  stop("gw_comparison_label은 하나의 비어 있지 않은 문자열이어야 합니다.")
}

if (!identical(gw_inference_role, "supplementary_diagnostic")) {
  stop("gw_inference_role은 'supplementary_diagnostic'이어야 합니다.")
}

if (!identical(gw_formal_giacomini_white_test, FALSE)) {
  stop("현재 사후 MAE 비교는 정식 Giacomini-White 검정으로 표시할 수 없습니다.")
}

if (
  length(mcs_alpha) != 1L ||
  is.na(mcs_alpha) ||
  !is.finite(mcs_alpha) ||
  mcs_alpha <= 0 ||
  mcs_alpha >= 1
) {
  stop("mcs_alpha는 0과 1 사이여야 합니다.")
}

if (!(mcs_statistic %in% c("Tmax", "TR"))) {
  stop("mcs_statistic은 'Tmax' 또는 'TR'이어야 합니다.")
}

if (!is.null(mcs_block_length)) {
  if (
    length(mcs_block_length) != 1L ||
    is.na(mcs_block_length) ||
    !is.finite(mcs_block_length) ||
    mcs_block_length != as.integer(mcs_block_length) ||
    mcs_block_length < 1L
  ) {
    stop("mcs_block_length는 NULL 또는 1 이상의 정수여야 합니다.")
  }
}

if (!(save_artifacts %in% c("none", "last_window", "all"))) {
  stop("save_artifacts는 'none', 'last_window', 'all' 중 하나여야 합니다.")
}

if (
  length(ensemble_methods) < 1L ||
  anyDuplicated(ensemble_methods) > 0L ||
  any(!(ensemble_methods %in% c("mean", "median", "inverse_rmse")))
) {
  stop("ensemble_methods 설정이 유효하지 않습니다.")
}

if (
  length(ensemble_weight_epsilon) != 1L ||
  is.na(ensemble_weight_epsilon) ||
  !is.finite(ensemble_weight_epsilon) ||
  ensemble_weight_epsilon <= 0
) {
  stop("ensemble_weight_epsilon은 0보다 큰 유한한 값이어야 합니다.")
}
