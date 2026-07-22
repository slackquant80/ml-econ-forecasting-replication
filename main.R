###############################################################################
### FRED-MD Multi-Model Forecasting Project
###############################################################################

# main.R을 source()하거나 Rscript로 실행하는 경우 모두 프로젝트 경로를 찾는다.
resolve_project_root <- function() {
  configured_root <- getOption("ml_forecast.project_root", NULL)

  if (
    is.character(configured_root) &&
      length(configured_root) == 1L &&
      !is.na(configured_root) &&
      nzchar(configured_root)
  ) {
    return(normalizePath(configured_root, mustWork = FALSE))
  }

  command_args <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command_args, value = TRUE)

  if (length(file_argument) > 0L) {
    script_path <- sub("^--file=", "", file_argument[1L])
    return(dirname(normalizePath(script_path, mustWork = FALSE)))
  }

  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
    },
    FUN.VALUE = character(1)
  )
  source_files <- source_files[!is.na(source_files) & nzchar(source_files)]

  if (length(source_files) > 0L) {
    return(dirname(normalizePath(tail(source_files, 1L), mustWork = FALSE)))
  }

  normalizePath(getwd(), mustWork = FALSE)
}

project_root <- resolve_project_root()
source(file.path(project_root, "config.R"))
source(file.path(project_root, "functions", "source-all.R"))

###############################################################################
### Load FRED-MD Data
###############################################################################

if (!file.exists(fred_md_file)) {
  stop("FRED-MD 파일을 찾을 수 없습니다: ", fred_md_file)
}

fred_md_raw <- read.csv(
  fred_md_file,
  check.names = FALSE,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

if (
  nrow(fred_md_raw) < 3L ||
  ncol(fred_md_raw) < 2L
) {
  stop("FRED-MD 파일의 행 또는 열이 충분하지 않습니다.")
}

if (anyDuplicated(names(fred_md_raw)) > 0L) {
  stop("FRED-MD 데이터에 중복된 변수명이 있습니다.")
}


###############################################################################
### Transformation Codes
###############################################################################

# 첫 번째 행은 변수별 transformation code
# 첫 번째 열은 sasdate이므로 제외
fred_md_tcode <- suppressWarnings(
  as.numeric(
    unlist(
      fred_md_raw[1, -1, drop = FALSE],
      use.names = FALSE
    )
  )
)

names(fred_md_tcode) <- names(fred_md_raw)[-1]

if (
  anyNA(fred_md_tcode) ||
  any(!(fred_md_tcode %in% 1:7))
) {
  stop("유효하지 않은 transformation code가 있습니다.")
}


###############################################################################
### Actual Time-Series Data
###############################################################################

# 두 번째 행부터 실제 월별 데이터
fred_md_data <- fred_md_raw[
  -1,
  ,
  drop = FALSE
]

rownames(fred_md_data) <- NULL

# 날짜 열을 제외한 경제변수를 숫자형으로 변환
for (i in 2:ncol(fred_md_data)) {
  x_raw <- trimws(
    as.character(fred_md_data[[i]])
  )

  x_numeric <- suppressWarnings(
    as.numeric(x_raw)
  )

  invalid_value <- (
    is.na(x_numeric) &
      !is.na(x_raw) &
      nzchar(x_raw)
  )

  if (any(invalid_value)) {
    stop(
      "숫자형으로 변환할 수 없는 값이 있습니다: ",
      names(fred_md_data)[i]
    )
  }

  fred_md_data[[i]] <- x_numeric
}

if (
  length(fred_md_tcode) !=
    ncol(fred_md_data) - 1L
) {
  stop("경제변수 수와 transformation code 수가 일치하지 않습니다.")
}


###############################################################################
### Date Conversion and Validation
###############################################################################

fred_md_date_raw <- trimws(
  as.character(fred_md_data[[1]])
)

fred_md_date <- as.Date(
  fred_md_date_raw,
  format = "%m/%d/%Y"
)

date_na <- is.na(fred_md_date)

if (any(date_na)) {
  fred_md_date[date_na] <- as.Date(
    fred_md_date_raw[date_na],
    format = "%Y-%m-%d"
  )
}

if (anyNA(fred_md_date)) {
  stop("날짜 형식을 변환하지 못한 값이 있습니다.")
}

fred_md_data[[1]] <- fred_md_date

if (
  is.unsorted(fred_md_data[[1]]) ||
  anyDuplicated(fred_md_data[[1]]) > 0L
) {
  stop("FRED-MD 날짜의 정렬 또는 중복 문제가 있습니다.")
}

fred_md_month_index <- (
  as.integer(format(fred_md_data[[1]], "%Y")) * 12L +
    as.integer(format(fred_md_data[[1]], "%m"))
)

if (any(diff(fred_md_month_index) != 1L)) {
  stop("FRED-MD 날짜에 연속되지 않은 월이 있습니다.")
}


###############################################################################
### Variable Metadata: Readable Names and Categories
###############################################################################

# 실제 데이터 열 이름은 FRED-MD 코드를 그대로 유지한다.
# 화면, 표, 그래프에서는 display_name을 사용한다.
variable_metadata <- data.frame(
variable_code = c(
    "RPI",
    "W875RX1",
    "DPCERA3M086SBEA",
    "CMRMTSPLx",
    "RETAILx",
    "INDPRO",
    "IPFPNSS",
    "IPFINAL",
    "IPCONGD",
    "IPDCONGD",
    "IPNCONGD",
    "IPBUSEQ",
    "IPMAT",
    "IPDMAT",
    "IPNMAT",
    "IPMANSICS",
    "IPB51222S",
    "IPFUELS",
    "CUMFNS",
    "HWI",
    "HWIURATIO",
    "CLF16OV",
    "CE16OV",
    "UNRATE",
    "UEMPMEAN",
    "UEMPLT5",
    "UEMP5TO14",
    "UEMP15OV",
    "UEMP15T26",
    "UEMP27OV",
    "CLAIMSx",
    "PAYEMS",
    "USGOOD",
    "CES1021000001",
    "USCONS",
    "MANEMP",
    "DMANEMP",
    "NDMANEMP",
    "SRVPRD",
    "USTPU",
    "USWTRADE",
    "USTRADE",
    "USFIRE",
    "USGOVT",
    "CES0600000007",
    "AWOTMAN",
    "AWHMAN",
    "HOUST",
    "HOUSTNE",
    "HOUSTMW",
    "HOUSTS",
    "HOUSTW",
    "PERMIT",
    "PERMITNE",
    "PERMITMW",
    "PERMITS",
    "PERMITW",
    "ACOGNO",
    "AMDMNOx",
    "ANDENOx",
    "AMDMUOx",
    "BUSINVx",
    "ISRATIOx",
    "M1SL",
    "M2SL",
    "M2REAL",
    "BOGMBASE",
    "TOTRESNS",
    "NONBORRES",
    "BUSLOANS",
    "REALLN",
    "NONREVSL",
    "CONSPI",
    "S&P 500",
    "S&P div yield",
    "S&P PE ratio",
    "FEDFUNDS",
    "CP3Mx",
    "TB3MS",
    "TB6MS",
    "GS1",
    "GS5",
    "GS10",
    "AAA",
    "BAA",
    "COMPAPFFx",
    "TB3SMFFM",
    "TB6SMFFM",
    "T1YFFM",
    "T5YFFM",
    "T10YFFM",
    "AAAFFM",
    "BAAFFM",
    "TWEXAFEGSMTHx",
    "EXSZUSx",
    "EXJPUSx",
    "EXUSUKx",
    "EXCAUSx",
    "WPSFD49207",
    "WPSFD49502",
    "WPSID61",
    "WPSID62",
    "OILPRICEx",
    "PPICMM",
    "CPIAUCSL",
    "CPIAPPSL",
    "CPITRNSL",
    "CPIMEDSL",
    "CUSR0000SAC",
    "CUSR0000SAD",
    "CUSR0000SAS",
    "CPIULFSL",
    "CUSR0000SA0L2",
    "CUSR0000SA0L5",
    "PCEPI",
    "DDURRG3M086SBEA",
    "DNDGRG3M086SBEA",
    "DSERRG3M086SBEA",
    "CES0600000008",
    "CES2000000008",
    "CES3000000008",
    "UMCSENTx",
    "DTCOLNVHFNM",
    "DTCTHFNM",
    "INVEST",
    "VIXCLSx"
  ),
  variable_label = c(
    "Real Personal Income",
    "Real Personal Income Excluding Transfer Receipts",
    "Real Personal Consumption Expenditures",
    "Real Manufacturing and Trade Industries Sales",
    "Retail and Food Services Sales",
    "Industrial Production Index",
    "Industrial Production: Final Products and Nonindustrial Supplies",
    "Industrial Production: Final Products",
    "Industrial Production: Consumer Goods",
    "Industrial Production: Durable Consumer Goods",
    "Industrial Production: Nondurable Consumer Goods",
    "Industrial Production: Business Equipment",
    "Industrial Production: Materials",
    "Industrial Production: Durable Materials",
    "Industrial Production: Nondurable Materials",
    "Industrial Production: Manufacturing",
    "Industrial Production: Residential Utilities",
    "Industrial Production: Fuels",
    "Capacity Utilization: Manufacturing",
    "Help-Wanted Index",
    "Help-Wanted Index to Unemployed Ratio",
    "Civilian Labor Force",
    "Civilian Employment",
    "Civilian Unemployment Rate",
    "Average Duration of Unemployment",
    "Unemployed Less Than 5 Weeks",
    "Unemployed 5 to 14 Weeks",
    "Unemployed 15 Weeks and Over",
    "Unemployed 15 to 26 Weeks",
    "Unemployed 27 Weeks and Over",
    "Initial Unemployment Insurance Claims",
    "Total Nonfarm Payroll Employment",
    "Goods-Producing Employment",
    "Mining and Logging Employment",
    "Construction Employment",
    "Manufacturing Employment",
    "Durable Goods Manufacturing Employment",
    "Nondurable Goods Manufacturing Employment",
    "Service-Providing Employment",
    "Trade, Transportation, and Utilities Employment",
    "Wholesale Trade Employment",
    "Retail Trade Employment",
    "Financial Activities Employment",
    "Government Employment",
    "Average Weekly Hours: Goods-Producing",
    "Average Weekly Overtime Hours: Manufacturing",
    "Average Weekly Hours: Manufacturing",
    "Housing Starts: Total",
    "Housing Starts: Northeast",
    "Housing Starts: Midwest",
    "Housing Starts: South",
    "Housing Starts: West",
    "Housing Permits: Total",
    "Housing Permits: Northeast",
    "Housing Permits: Midwest",
    "Housing Permits: South",
    "Housing Permits: West",
    "New Orders: Consumer Goods",
    "New Orders: Durable Goods",
    "New Orders: Nondefense Capital Goods",
    "Unfilled Orders: Durable Goods",
    "Total Business Inventories",
    "Business Inventories-to-Sales Ratio",
    "M1 Money Stock",
    "M2 Money Stock",
    "Real M2 Money Stock",
    "Monetary Base",
    "Total Reserves of Depository Institutions",
    "Nonborrowed Reserves of Depository Institutions",
    "Commercial and Industrial Loans",
    "Real Estate Loans",
    "Nonrevolving Consumer Credit",
    "Nonrevolving Consumer Credit to Personal Income",
    "S&P 500 Index",
    "S&P 500 Dividend Yield",
    "S&P 500 Price-Earnings Ratio",
    "Effective Federal Funds Rate",
    "3-Month AA Financial Commercial Paper Rate",
    "3-Month Treasury Bill Rate",
    "6-Month Treasury Bill Rate",
    "1-Year Treasury Yield",
    "5-Year Treasury Yield",
    "10-Year Treasury Yield",
    "Moody's Seasoned Aaa Corporate Bond Yield",
    "Moody's Seasoned Baa Corporate Bond Yield",
    "Commercial Paper Rate Minus Federal Funds Rate",
    "3-Month Treasury Bill Minus Federal Funds Rate",
    "6-Month Treasury Bill Minus Federal Funds Rate",
    "1-Year Treasury Yield Minus Federal Funds Rate",
    "5-Year Treasury Yield Minus Federal Funds Rate",
    "10-Year Treasury Yield Minus Federal Funds Rate",
    "Aaa Corporate Bond Yield Minus Federal Funds Rate",
    "Baa Corporate Bond Yield Minus Federal Funds Rate",
    "Trade-Weighted U.S. Dollar Index",
    "Swiss Franc per U.S. Dollar Exchange Rate",
    "Japanese Yen per U.S. Dollar Exchange Rate",
    "U.S. Dollar per British Pound Exchange Rate",
    "Canadian Dollar per U.S. Dollar Exchange Rate",
    "Producer Price Index: Finished Goods",
    "Producer Price Index: Finished Consumer Goods",
    "Producer Price Index: Intermediate Materials",
    "Producer Price Index: Crude Materials",
    "Crude Oil Price: Spliced WTI Series",
    "Producer Price Index: Metals and Metal Products",
    "Consumer Price Index: All Items",
    "Consumer Price Index: Apparel",
    "Consumer Price Index: Transportation",
    "Consumer Price Index: Medical Care",
    "Consumer Price Index: Commodities",
    "Consumer Price Index: Durables",
    "Consumer Price Index: Services",
    "Consumer Price Index: All Items Less Food",
    "Consumer Price Index: All Items Less Shelter",
    "Consumer Price Index: All Items Less Medical Care",
    "Personal Consumption Expenditures Price Index",
    "PCE Price Index: Durable Goods",
    "PCE Price Index: Nondurable Goods",
    "PCE Price Index: Services",
    "Average Hourly Earnings: Goods-Producing",
    "Average Hourly Earnings: Construction",
    "Average Hourly Earnings: Manufacturing",
    "University of Michigan Consumer Sentiment",
    "Consumer Motor Vehicle Loans Outstanding",
    "Total Consumer Loans and Leases Outstanding",
    "Securities in Bank Credit at Commercial Banks",
    "CBOE Volatility Index (VIX)"
  ),
  category = c(
    "Output and Income",
    "Output and Income",
    "Consumption, Orders, and Inventories",
    "Consumption, Orders, and Inventories",
    "Consumption, Orders, and Inventories",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Output and Income",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Housing",
    "Housing",
    "Housing",
    "Housing",
    "Housing",
    "Housing",
    "Housing",
    "Housing",
    "Housing",
    "Housing",
    "Consumption, Orders, and Inventories",
    "Consumption, Orders, and Inventories",
    "Consumption, Orders, and Inventories",
    "Consumption, Orders, and Inventories",
    "Consumption, Orders, and Inventories",
    "Consumption, Orders, and Inventories",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Stock Market",
    "Stock Market",
    "Stock Market",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Interest and Exchange Rates",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Prices",
    "Labor Market",
    "Labor Market",
    "Labor Market",
    "Consumption, Orders, and Inventories",
    "Money and Credit",
    "Money and Credit",
    "Money and Credit",
    "Stock Market"
  ),
  stringsAsFactors = FALSE,
  row.names = NULL
)

if (anyDuplicated(variable_metadata$variable_code) > 0L) {
  stop("변수 메타데이터에 중복된 variable_code가 있습니다.")
}

# 수동으로 관리하는 설명 사전의 원본을 보존한다.
variable_metadata_reference <- variable_metadata

current_variable_codes <- names(fred_md_tcode)
metadata_variable_codes <- variable_metadata_reference$variable_code

# 원데이터 개편 여부 확인
unmapped_variables <- setdiff(
  current_variable_codes,
  metadata_variable_codes
)

obsolete_variables <- setdiff(
  metadata_variable_codes,
  current_variable_codes
)

# 새 변수는 모델 실행을 중단하지 않고 원래 코드를 임시 설명으로 사용한다.
if (length(unmapped_variables) > 0L) {
  warning(
    "설명 메타데이터에 등록되지 않은 신규 변수가 있습니다. ",
    "원래 변수 코드를 임시 이름으로 사용합니다: ",
    paste(unmapped_variables, collapse = ", ")
  )

  variable_metadata <- rbind(
    variable_metadata,
    data.frame(
      variable_code = unmapped_variables,
      variable_label = unmapped_variables,
      category = rep(
        "Unclassified",
        length(unmapped_variables)
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  )
}

# 사전에는 있으나 현재 데이터에서 사라진 변수는 기록만 하고 제외한다.
if (length(obsolete_variables) > 0L) {
  message(
    "현재 FRED-MD 데이터에서 제외되었거나 코드가 변경된 변수가 있습니다: ",
    paste(obsolete_variables, collapse = ", ")
  )
}

obsolete_variable_metadata <- variable_metadata_reference[
  variable_metadata_reference$variable_code %in%
    obsolete_variables,
  ,
  drop = FALSE
]

# 현재 current.csv에 실제로 존재하는 변수만 남기고 열 순서에 맞춘다.
variable_metadata <- variable_metadata[
  match(
    current_variable_codes,
    variable_metadata$variable_code
  ),
  ,
  drop = FALSE
]

if (anyNA(variable_metadata$variable_code)) {
  stop("신규 변수 fallback 처리 후에도 메타데이터 연결에 실패했습니다.")
}

variable_metadata$metadata_status <- ifelse(
  variable_metadata$variable_code %in% unmapped_variables,
  "Fallback",
  "Mapped"
)

variable_metadata$tcode <- as.integer(
  fred_md_tcode[variable_metadata$variable_code]
)

variable_metadata$display_name <- paste0(
  variable_metadata$variable_label,
  " [",
  variable_metadata$variable_code,
  "]"
)

# 원데이터 개편 내역을 대시보드 진단 화면에서 사용할 수 있도록 보존한다.
metadata_change_summary <- rbind(
  if (length(unmapped_variables) > 0L) {
    data.frame(
      variable_code = unmapped_variables,
      change_type = "New or unmapped variable",
      action = "Fallback label applied",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    NULL
  },
  if (length(obsolete_variables) > 0L) {
    data.frame(
      variable_code = obsolete_variables,
      change_type = "Removed or renamed variable",
      action = "Excluded from active metadata",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    NULL
  }
)

if (is.null(metadata_change_summary)) {
  metadata_change_summary <- data.frame(
    variable_code = character(0),
    change_type = character(0),
    action = character(0),
    stringsAsFactors = FALSE
  )
}

# Shiny selectInput에서 사용할 수 있는 choices
# 화면에는 읽기 쉬운 이름, 내부 value에는 원래 코드가 들어간다.
target_choices <- setNames(
  variable_metadata$variable_code,
  variable_metadata$display_name
)

get_variable_label <- function(
    variable_codes,
    include_code = TRUE
) {
  matched <- match(
    variable_codes,
    variable_metadata$variable_code
  )

  output <- as.character(variable_codes)
  known <- !is.na(matched)

  if (include_code) {
    output[known] <- variable_metadata$display_name[
      matched[known]
    ]

    output[!known] <- paste0(
      variable_codes[!known],
      " [",
      variable_codes[!known],
      "]"
    )
  } else {
    output[known] <- variable_metadata$variable_label[
      matched[known]
    ]
  }

  output
}


###############################################################################
### Target Setting and Missing Value Handling
###############################################################################

if (!(target_name %in% names(fred_md_data))) {
  stop("목표변수를 찾을 수 없습니다: ", target_name)
}

target_tcode_official <- as.integer(
  fred_md_tcode[target_name]
)

if (
  length(target_tcode_official) != 1L ||
  is.na(target_tcode_official)
) {
  stop("목표변수의 공식 transformation code를 확인할 수 없습니다.")
}

if (is.null(target_tcode_override)) {
  target_tcode <- target_tcode_official
  target_transform_source <- "Official FRED-MD"
} else {
  if (
    length(target_tcode_override) != 1L ||
    is.na(target_tcode_override) ||
    !(target_tcode_override %in% 1:7)
  ) {
    stop("target_tcode_override는 NULL 또는 1~7의 정수여야 합니다.")
  }

  target_tcode <- as.integer(target_tcode_override)
  target_transform_source <- "User override"
}

target_level_mode <- if (is.null(target_level_mode_override)) {
  target_level_mode_from_tcode(target_tcode)
} else {
  validate_level_forecast_mode(target_level_mode_override)
}
target_level_formula <- level_mode_formula_label(target_level_mode)

# 설명변수는 공식 tcode를 유지하고 목표변수에만 선택한 tcode를 적용한다.
fred_md_tcode_effective <- fred_md_tcode
fred_md_tcode_effective[target_name] <- target_tcode

# 대시보드 메타데이터에도 공식·실제 적용 tcode를 함께 보존한다.
variable_metadata$official_tcode <- as.integer(
  fred_md_tcode[variable_metadata$variable_code]
)
variable_metadata$effective_tcode <- as.integer(
  fred_md_tcode_effective[variable_metadata$variable_code]
)

target_display_name <- get_variable_label(
  target_name,
  include_code = TRUE
)

# 결측치 처리 전 원자료 보존
fred_md_data_original <- fred_md_data

target_missing_policy <- list(
  method = target_missing_method,
  max_gap = max_target_gap,
  long_gap_action = target_long_gap_action,
  allow_imputed_oos_actual = allow_imputed_oos_actual,
  allow_imputed_origin_level = allow_imputed_origin_level
)

predictor_missing_policy <- list(
  short_gap_method = predictor_short_gap_method,
  max_gap = max_predictor_gap,
  min_obs_ratio = predictor_window_min_obs_ratio,
  min_observed = predictor_window_min_observed,
  final_method = predictor_final_imputation,
  drop_insufficient = predictor_drop_insufficient
)

missing_data_policy <- normalize_missing_policy(list(
  target = target_missing_policy,
  predictor = predictor_missing_policy
))

target_prepared <- prepare_target_missing_data(
  x = fred_md_data[[target_name]],
  dates = fred_md_data[[1L]],
  target_code = target_name,
  policy = missing_data_policy$target
)

fred_md_data[[target_name]] <- target_prepared$values
target_raw <- target_prepared$values
target_observed_raw <- target_prepared$observed
target_imputed <- target_prepared$imputed
target_imputed_dates <- fred_md_data[[1L]][target_imputed]

target_imputation_summary <- summarize_target_imputation(
  prepared = target_prepared,
  dates = fred_md_data[[1L]],
  target_code = target_name,
  target_name = target_display_name
)


###############################################################################
### Evaluation Exclusion Dates
###############################################################################

# 대체된 원자료가 정상성 변환 결과에 영향을 주는 기간은
# target tcode에 따라 자동 계산한다. 대체값을 실제 OOS actual로 평가하지 않는다.
target_eval_exclude_mask_raw <- if (isTRUE(allow_imputed_oos_actual)) {
  rep(FALSE, length(target_imputed))
} else {
  target_imputation_effect_mask(
    imputed = target_imputed,
    tcode = target_tcode
  )
}

target_eval_exclude_dates <- fred_md_data[[1L]][
  target_eval_exclude_mask_raw
]


###############################################################################
### Stationary Transformation
###############################################################################

# 2차 차분까지 고려하여 모든 변수를 세 번째 관측월부터 정렬
tdata <- fred_md_data[
  -c(1L, 2L),
  ,
  drop = FALSE
]

rownames(tdata) <- NULL

for (i in 2:ncol(fred_md_data)) {
  tcode_i <- fred_md_tcode_effective[i - 1L]
  x <- fred_md_data[[i]]
  variable_name <- names(fred_md_data)[i]

  if (
    tcode_i %in% c(4L, 5L, 6L) &&
    any(x <= 0, na.rm = TRUE)
  ) {
    stop(
      "로그 변환 대상 변수에 0 이하의 값이 있습니다: ",
      variable_name
    )
  }

  if (
    tcode_i == 7L &&
    any(x[-length(x)] == 0, na.rm = TRUE)
  ) {
    stop(
      "tcode 7 적용 과정에서 분모가 0인 값이 있습니다: ",
      variable_name
    )
  }

  if (tcode_i == 1L) {
    # No transformation: x_t
    tdata[[i]] <- x[-c(1L, 2L)]

  } else if (tcode_i == 2L) {
    # First difference: Delta x_t
    tdata[[i]] <- diff(x[-1L])

  } else if (tcode_i == 3L) {
    # Second difference: Delta^2 x_t
    tdata[[i]] <- diff(
      x,
      differences = 2L
    )

  } else if (tcode_i == 4L) {
    # Log level: log(x_t)
    tdata[[i]] <- log(
      x[-c(1L, 2L)]
    )

  } else if (tcode_i == 5L) {
    # First difference of log: Delta log(x_t)
    tdata[[i]] <- diff(
      log(x[-1L])
    )

  } else if (tcode_i == 6L) {
    # Second difference of log: Delta^2 log(x_t)
    tdata[[i]] <- diff(
      log(x),
      differences = 2L
    )

  } else if (tcode_i == 7L) {
    # First difference of the growth rate
    growth_rate <- (
      x[-1L] / x[-length(x)]
    ) - 1

    tdata[[i]] <- diff(
      growth_rate
    )
  }
}

if (
  nrow(tdata) !=
    nrow(fred_md_data) - 2L
) {
  stop("정상성 변환 후 행 개수가 예상과 다릅니다.")
}

non_finite_count <- vapply(
  tdata[, -1, drop = FALSE],
  function(x) {
    sum(
      is.infinite(x) |
        is.nan(x)
    )
  },
  FUN.VALUE = integer(1)
)

if (any(non_finite_count > 0L)) {
  problem_variables <- names(
    non_finite_count[
      non_finite_count > 0L
    ]
  )

  stop(
    "변환 과정에서 Inf 또는 NaN이 생성된 변수가 있습니다: ",
    paste(problem_variables, collapse = ", ")
  )
}

target_level_tdata <- as.numeric(
  fred_md_data[[target_name]]
)[-c(1L, 2L)]

target_level_observed_tdata <- as.logical(
  target_observed_raw
)[-c(1L, 2L)]

# A raw target imputation can affect one or more transformed observations,
# depending on the tcode. Those transformed target values are not allowed in
# original-level training features or responses.
transformed_target_valid_tdata <- !target_imputation_effect_mask(
  imputed = target_imputed,
  tcode = target_tcode
)[-c(1L, 2L)]

if (
  length(target_level_tdata) != nrow(tdata) ||
    length(target_level_observed_tdata) != nrow(tdata) ||
    length(transformed_target_valid_tdata) != nrow(tdata)
) {
  stop("원수준 target 또는 validity mask와 tdata 행 수가 일치하지 않습니다.")
}

if (isTRUE(enable_forward_forecasts)) {
  if (!level_mode_supported_by_data(target_level_tdata, target_level_mode)) {
    stop(
      "선택한 원수준 예측방식이 target 자료와 호환되지 않습니다: ",
      target_level_mode
    )
  }
}

target_eval_exclude <- (
  tdata[[1]] %in%
    target_eval_exclude_dates
)


###############################################################################
### Target Sample
###############################################################################

target_rows <- which(
  !is.na(tdata[[target_name]])
)

if (length(target_rows) == 0L) {
  stop("변환 후 목표변수에 이용 가능한 관측값이 없습니다.")
}

first_target_row <- min(target_rows)
last_target_row <- max(target_rows)

tdata_target <- tdata[
  first_target_row:last_target_row,
  ,
  drop = FALSE
]

rownames(tdata_target) <- NULL

target_eval_exclude_target <- target_eval_exclude[
  first_target_row:last_target_row
]

target_level_target <- target_level_tdata[
  first_target_row:last_target_row
]
target_level_observed_target <- target_level_observed_tdata[
  first_target_row:last_target_row
]
transformed_target_valid_target <- transformed_target_valid_tdata[
  first_target_row:last_target_row
]

if (
  isTRUE(enable_forward_forecasts) &&
    (
      anyNA(target_level_target) ||
        any(!is.finite(target_level_target)) ||
        anyNA(target_level_observed_target) ||
        anyNA(transformed_target_valid_target)
    )
) {
  stop("원수준 target 또는 validity mask 사용기간에 문제가 있습니다.")
}

if (anyNA(tdata_target[[target_name]])) {
  missing_dates <- tdata_target[[1]][
    is.na(tdata_target[[target_name]])
  ]

  stop(
    "목표변수의 사용기간에 처리되지 않은 결측치가 있습니다: ",
    paste(missing_dates, collapse = ", ")
  )
}


###############################################################################
### Modeling Sample
###############################################################################

# 동일한 npred개 목표월에 대해 모든 horizon을 평가하려면
# window_size + npred + (최대 horizon - 1)개월이 필요하다.
# 현재 주 사양은 360개월 rolling window와 최신 90개 OOS 목표월이다.
required_rows <- (
  window_size +
    npred +
    max_horizon -
    1L
)

if (nrow(tdata_target) < required_rows) {
  stop(
    "목표변수의 이용 가능 기간이 필요한 ",
    required_rows,
    "개월보다 짧습니다."
  )
}

tdata_model <- tail(
  tdata_target,
  required_rows
)

rownames(tdata_model) <- NULL

target_eval_exclude_model <- tail(
  target_eval_exclude_target,
  required_rows
)

target_level_model <- tail(target_level_target, required_rows)
target_level_observed_model <- tail(
  target_level_observed_target,
  required_rows
)
transformed_target_valid_model <- tail(
  transformed_target_valid_target,
  required_rows
)

if (
  length(target_level_model) != nrow(tdata_model) ||
    length(target_level_observed_model) != nrow(tdata_model) ||
    length(transformed_target_valid_model) != nrow(tdata_model)
) {
  stop("원수준 model target 또는 validity mask 길이가 tdata_model과 일치하지 않습니다.")
}

# 가장 긴 horizon의 최초 forecast origin까지 이용 가능한 고정 길이 학습 window
initial_window <- tdata_model[
  seq_len(window_size),
  ,
  drop = FALSE
]


###############################################################################
### Initial Variable Selection
###############################################################################

min_obs_count <- ceiling(
  window_size * min_obs_ratio
)

obs_count <- colSums(
  !is.na(
    initial_window[
      ,
      -1,
      drop = FALSE
    ]
  )
)

obs_ratio <- obs_count / window_size

variable_sd <- vapply(
  initial_window[
    ,
    -1,
    drop = FALSE
  ],
  function(x) {
    sd(
      x,
      na.rm = TRUE
    )
  },
  FUN.VALUE = numeric(1)
)

selected_variables <- names(obs_count)[
  obs_count >= min_obs_count &
    is.finite(variable_sd) &
    variable_sd > sqrt(.Machine$double.eps)
]

if (!(target_name %in% selected_variables)) {
  stop(
    "목표변수가 최초 학습기간의 관측률 또는 변동성 조건을 ",
    "충족하지 않습니다: ",
    target_name
  )
}


###############################################################################
### Selected Modeling Data
###############################################################################

date_name <- names(tdata_model)[1]

selected_model_variables <- c(
  target_name,
  setdiff(
    selected_variables,
    target_name
  )
)

tdata_selected <- tdata_model[
  ,
  c(
    date_name,
    selected_model_variables
  ),
  drop = FALSE
]

rownames(tdata_selected) <- NULL

target_eval_exclude_selected <- target_eval_exclude_model
target_level_selected <- target_level_model
target_level_observed_selected <- target_level_observed_model
transformed_target_valid_selected <- transformed_target_valid_model

selected_variable_metadata <- variable_metadata[
  match(
    selected_model_variables,
    variable_metadata$variable_code
  ),
  ,
  drop = FALSE
]

selected_variable_choices <- setNames(
  selected_variable_metadata$variable_code,
  selected_variable_metadata$display_name
)

# 모든 horizon이 공통으로 예측하는 마지막 90개 목표월
oos_index <- seq.int(
  from = window_size + max_horizon,
  to = nrow(tdata_selected)
)

if (length(oos_index) != npred) {
  stop("OOS 평가기간의 길이가 npred와 일치하지 않습니다.")
}

oos_dates <- tdata_selected[[1]][
  oos_index
]

# Direct forecast 설계에서 horizon별 실제 supervised-learning 행 수를 명시한다.
# make_direct_design()의 training origin은 predictor_lags부터
# window_size - horizon까지이므로 행 수는 다음 식과 같다.
expected_training_observations <- (
  window_size - forecast_horizons - predictor_lags + 1L
)

if (any(expected_training_observations < 1L)) {
  stop("학습 window가 horizon별 direct 설계행렬을 만들기에 너무 짧습니다.")
}

training_design_summary <- data.frame(
  horizon = as.integer(forecast_horizons),
  window_size_months = rep(as.integer(window_size), length(forecast_horizons)),
  predictor_lags = rep(as.integer(predictor_lags), length(forecast_horizons)),
  expected_training_observations = as.integer(expected_training_observations),
  row.names = NULL
)


###############################################################################
### Variable Selection Summary
###############################################################################

metadata_match <- match(
  names(obs_count),
  variable_metadata$variable_code
)

variable_selection <- data.frame(
  variable_code = names(obs_count),
  variable_name = variable_metadata$variable_label[metadata_match],
  display_name = variable_metadata$display_name[metadata_match],
  category = variable_metadata$category[metadata_match],
  metadata_status = variable_metadata$metadata_status[metadata_match],
  official_tcode = as.integer(
    fred_md_tcode[names(obs_count)]
  ),
  effective_tcode = as.integer(
    fred_md_tcode_effective[names(obs_count)]
  ),
  observed = as.integer(obs_count),
  obs_ratio = round(
    as.numeric(obs_ratio),
    3L
  ),
  standard_deviation = as.numeric(variable_sd),
  selected = names(obs_count) %in% selected_variables,
  row.names = NULL
)

###############################################################################
### Horizon-Aligned Rolling Index
###############################################################################

rolling_index <- create_rolling_index(
  data = tdata_selected,
  target_name = target_name,
  target_eval_exclude = target_eval_exclude_selected,
  forecast_horizons = forecast_horizons,
  window_size = window_size,
  npred = npred,
  oos_index = oos_index
)


###############################################################################
### Initial PCA-EM Test
###############################################################################

Y_initial <- as.matrix(
  tdata_selected[
    seq_len(window_size),
    -1L,
    drop = FALSE
  ]
)

Y_initial_em <- prepare_model_window_missing_data(
  Y.window = Y_initial,
  prepare_predictors = TRUE,
  predictor_policy = missing_data_policy$predictor,
  pca_em_settings = list(
    n_factors = pca_em_factors,
    max_iter = pca_em_max_iter,
    tol = pca_em_tol,
    require_convergence = pca_em_require_convergence
  )
)

initial_missing_count <- as.integer(attr(Y_initial_em, "missing_before"))
remaining_missing_count <- as.integer(attr(Y_initial_em, "missing_after"))

initial_retained <- Y_initial[
  ,
  colnames(Y_initial_em),
  drop = FALSE
]

observed_values_unchanged <- isTRUE(
  all.equal(
    as.numeric(initial_retained[!is.na(initial_retained)]),
    as.numeric(Y_initial_em[!is.na(initial_retained)]),
    tolerance = 0
  )
)

if (remaining_missing_count > 0L) {
  stop("PCA-EM 처리 후에도 결측치가 남아 있습니다.")
}

if (
  isTRUE(pca_em_require_convergence) &&
  !isTRUE(attr(Y_initial_em, "em_converged"))
) {
  stop("최초 학습 window의 PCA-EM이 설정한 반복 횟수 안에 수렴하지 않았습니다.")
}

if (any(!is.finite(Y_initial_em))) {
  stop("PCA-EM 처리 후 비유한 값이 남아 있습니다.")
}

if (!observed_values_unchanged) {
  stop("PCA-EM 과정에서 원래 관측값이 변경되었습니다.")
}

retained_missing_count <- sum(is.na(initial_retained))
if (
  attr(Y_initial_em, "imputed_count") !=
    retained_missing_count
) {
  stop("학습 window 결측치 처리 개수가 일치하지 않습니다.")
}


###############################################################################
### Rolling Window Test
###############################################################################

first_window_test <- get_forecast_window(
  forecast_number = 1L,
  horizon = max_horizon,
  rolling_index = rolling_index,
  data = tdata_selected,
  window_size = window_size,
  target_name = target_name,
  target_display_name = target_display_name,
  apply_pca_em = TRUE,
  pca_em_factors = pca_em_factors,
  pca_em_max_iter = pca_em_max_iter,
  pca_em_tol = pca_em_tol,
  require_pca_em_convergence = pca_em_require_convergence,
  predictor_missing_policy = missing_data_policy$predictor
)

last_window_test <- get_forecast_window(
  forecast_number = npred,
  horizon = min(forecast_horizons),
  rolling_index = rolling_index,
  data = tdata_selected,
  window_size = window_size,
  target_name = target_name,
  target_display_name = target_display_name,
  apply_pca_em = TRUE,
  pca_em_factors = pca_em_factors,
  pca_em_max_iter = pca_em_max_iter,
  pca_em_tol = pca_em_tol,
  require_pca_em_convergence = pca_em_require_convergence,
  predictor_missing_policy = missing_data_policy$predictor
)

if (
  nrow(first_window_test$Y.window) != window_size ||
  nrow(last_window_test$Y.window) != window_size
) {
  stop("Rolling window 테스트에서 window 크기가 일치하지 않습니다.")
}

if (
  first_window_test$missing_after > 0L ||
  last_window_test$missing_after > 0L
) {
  stop("Rolling window 테스트 후 결측치가 남아 있습니다.")
}


###############################################################################
### Registered Models: Baselines through Machine Learning
###############################################################################

model_registry <- create_model_registry(model_control)
validate_model_registry(model_registry, models_to_run)

feature_settings <- list(
  predictor_lags = predictor_lags,
  n_factors = factor_count,
  factor_include_target = factor_include_target,
  factor_scale = factor_scale
)

pca_em_settings <- list(
  n_factors = pca_em_factors,
  max_iter = pca_em_max_iter,
  tol = pca_em_tol,
  require_convergence = pca_em_require_convergence,
  predictor_policy = missing_data_policy$predictor
)

report_experiment_progress(
  stage = "monthly_backtest",
  message = "Starting rolling forecasts for target-month transformed changes.",
  progress_percent = 4
)

base_backtest <- run_model_set(
  models = models_to_run,
  registry = model_registry,
  data = tdata_selected,
  rolling_index = rolling_index,
  target_name = target_name,
  target_display_name = target_display_name,
  forecast_horizons = forecast_horizons,
  npred = npred,
  window_size = window_size,
  feature_settings = feature_settings,
  pca_em_settings = pca_em_settings,
  base_seed = base_seed,
  error_policy = error_policy,
  show_progress = show_progress,
  save_artifacts = save_artifacts
)

base_forecasts <- base_backtest$forecasts
base_model_labels <- vapply(
  model_registry[models_to_run],
  function(x) x$label,
  FUN.VALUE = character(1)
)

active_ensemble_members <- if (is.null(ensemble_members)) {
  base_model_labels
} else {
  resolve_ensemble_member_labels(
    ensemble_members = ensemble_members,
    registry = model_registry[models_to_run]
  )
}

ensemble_result <- build_ensemble_forecasts(
  base_forecasts = base_forecasts,
  member_models = active_ensemble_members,
  methods = ensemble_methods,
  min_members = ensemble_min_members,
  min_history = ensemble_min_history,
  weight_epsilon = ensemble_weight_epsilon
)

forecast_results <- rbind(
  base_forecasts,
  ensemble_result$forecasts
)
rownames(forecast_results) <- NULL

forecast_accuracy <- summarize_forecast_accuracy(
  forecast_results
)

relative_accuracy <- add_benchmark_relative_accuracy(
  forecast_accuracy,
  benchmark_model = "RW"
)

model_rankings <- rank_models_by_horizon(
  relative_accuracy
)


###############################################################################
### Cumulative-Target Backtest and Forward Level Forecasts
###############################################################################

cumulative_backtest_results <- data.frame()
cumulative_accuracy <- data.frame()
cumulative_relative_accuracy <- data.frame()
cumulative_rankings <- data.frame()
cumulative_ensemble_weights <- data.frame()
cumulative_rolling_index <- data.frame()
forward_forecasts <- data.frame()
forward_ensemble_weights <- data.frame()
forward_best_models <- data.frame()
forward_artifacts <- list()
forward_origin_date <- as.Date(NA)
forward_origin_level <- NA_real_
forward_window_start_date <- as.Date(NA)

if (isTRUE(enable_forward_forecasts)) {
  report_experiment_progress(
    stage = "cumulative_backtest",
    message = "Starting cumulative transformed-change backtests and reconstructed-level forecasts.",
    progress_percent = 44
  )

  cumulative_rolling_index <- create_cumulative_rolling_index(
    data = tdata_selected,
    target_name = target_name,
    target_level = target_level_selected,
    target_level_observed = target_level_observed_selected,
    transformed_target_valid = transformed_target_valid_selected,
    level_forecast_mode = target_level_mode,
    forecast_horizons = forecast_horizons,
    window_size = window_size,
    npred = npred,
    oos_index = oos_index,
    predictor_lags = predictor_lags,
    allow_imputed_oos_actual = allow_imputed_oos_actual
  )

  cumulative_registry <- create_cumulative_model_registry(
    model_registry
  )
  validate_model_registry(cumulative_registry, models_to_run)

  cumulative_base_backtest <- run_cumulative_model_set(
    models = models_to_run,
    registry = cumulative_registry,
    data = tdata_selected,
    target_level = target_level_selected,
    target_level_observed = target_level_observed_selected,
    transformed_target_valid = transformed_target_valid_selected,
    level_forecast_mode = target_level_mode,
    rolling_index = cumulative_rolling_index,
    target_name = target_name,
    target_display_name = target_display_name,
    forecast_horizons = forecast_horizons,
    npred = npred,
    window_size = window_size,
    feature_settings = feature_settings,
    pca_em_settings = pca_em_settings,
    base_seed = base_seed,
    error_policy = error_policy,
    show_progress = show_progress,
    save_artifacts = save_artifacts
  )

  cumulative_base_forecasts <- cumulative_base_backtest$forecasts

  cumulative_ensemble_result <- build_ensemble_forecasts(
    base_forecasts = cumulative_base_forecasts,
    member_models = active_ensemble_members,
    methods = ensemble_methods,
    min_members = ensemble_min_members,
    min_history = ensemble_min_history,
    weight_epsilon = ensemble_weight_epsilon
  )

  cumulative_backtest_results <- rbind(
    cumulative_base_forecasts,
    cumulative_ensemble_result$forecasts
  )
  rownames(cumulative_backtest_results) <- NULL
  cumulative_backtest_results <- augment_cumulative_forecast_scales(
    cumulative_backtest_results
  )

  cumulative_accuracy <- summarize_cumulative_forecast_accuracy(
    cumulative_backtest_results
  )

  cumulative_relative_accuracy <- add_cumulative_benchmark_relative_accuracy(
    cumulative_accuracy,
    benchmark_model = "RW"
  )

  cumulative_rankings <- rank_cumulative_models_by_horizon(
    cumulative_relative_accuracy
  )

  cumulative_ensemble_weights <- cumulative_ensemble_result$weights

  forward_base_result <- run_forward_base_models(
    models = models_to_run,
    registry = cumulative_registry,
    data = tdata_selected,
    target_level = target_level_selected,
    target_level_observed = target_level_observed_selected,
    transformed_target_valid = transformed_target_valid_selected,
    level_forecast_mode = target_level_mode,
    target_name = target_name,
    target_display_name = target_display_name,
    forecast_horizons = forecast_horizons,
    window_size = window_size,
    feature_settings = feature_settings,
    pca_em_settings = pca_em_settings,
    base_seed = base_seed,
    error_policy = error_policy,
    allow_imputed_origin_level = allow_imputed_origin_level
  )

  forward_ensemble_result <- build_forward_ensemble_forecasts(
    base_forward_forecasts = forward_base_result$forecasts,
    cumulative_backtest_forecasts = cumulative_base_forecasts,
    member_models = active_ensemble_members,
    methods = ensemble_methods,
    min_members = ensemble_min_members,
    min_history = ensemble_min_history,
    weight_epsilon = ensemble_weight_epsilon
  )

  latest_origin_imputed <- !isTRUE(forward_base_result$origin_observed)

  forward_forecasts <- finalize_forward_forecasts(
    forward_forecasts = rbind(
      forward_base_result$forecasts,
      forward_ensemble_result$forecasts
    ),
    cumulative_rankings = cumulative_rankings,
    latest_origin_imputed = latest_origin_imputed,
    execution_profile = execution_profile
  )
  rownames(forward_forecasts) <- NULL

  forward_ensemble_weights <- forward_ensemble_result$weights
  forward_artifacts <- forward_base_result$artifacts
  forward_origin_date <- forward_base_result$origin_date
  forward_origin_level <- forward_base_result$origin_level
  forward_window_start_date <- forward_base_result$window_start_date

  forward_best_models <- forward_forecasts[
    is.finite(forward_forecasts$level_RMSE_rank) &
      forward_forecasts$level_RMSE_rank == 1,
    ,
    drop = FALSE
  ]
  rownames(forward_best_models) <- NULL

  expected_forward_rows <- (
    length(models_to_run) +
      length(ensemble_methods)
  ) * length(forecast_horizons)

  if (nrow(forward_forecasts) != expected_forward_rows) {
    stop(
      "Forward forecast 행 수가 예상한 ",
      expected_forward_rows,
      "개와 일치하지 않습니다."
    )
  }

  if (
    any(forward_forecasts$forecast_origin != max(tdata_selected[[1L]])) ||
      any(
        as.integer(format(forward_forecasts$target_date, "%Y")) * 12L +
          as.integer(format(forward_forecasts$target_date, "%m")) -
          (
            as.integer(format(forward_forecasts$forecast_origin, "%Y")) * 12L +
              as.integer(format(forward_forecasts$forecast_origin, "%m"))
          ) != forward_forecasts$horizon
      )
  ) {
    stop("Forward forecast의 origin과 target date 정렬에 문제가 있습니다.")
  }

  if (
    any(
      !is.finite(
        forward_forecasts$raw_level_forecast[
          forward_forecasts$status == "ok"
        ]
      )
    )
  ) {
    stop("Forward raw-level forecast에 비유한 값이 있습니다.")
  }
}


###############################################################################
### Statistical Forecast Validation
###############################################################################

dm_test_results <- data.frame()
gw_test_results <- data.frame()
model_confidence_set <- data.frame()
model_confidence_set_summary <- data.frame()
mcs_audit <- data.frame()
mcs_audit_summary <- data.frame()

if (isTRUE(enable_statistical_validation)) {
  report_experiment_progress(
    stage = "statistical_validation",
    message = "Running DM, supplementary HAC MAE, and Model Confidence Set validation.",
    progress_percent = 86
  )

  statistical_validation <- run_statistical_validation(
    forecast_results = forecast_results,
    cumulative_backtest_results = cumulative_backtest_results,
    forecast_horizons = forecast_horizons,
    loss_functions = statistical_loss_functions,
    benchmark_model = statistical_benchmark_model,
    dm_alternative = dm_alternative,
    dm_varestimator = dm_varestimator,
    dm_p_adjust_method = dm_p_adjust_method,
    gw_alternative = gw_alternative,
    gw_method = gw_method,
    gw_p_adjust_method = gw_p_adjust_method,
    significance_level = statistical_significance_level,
    mcs_alpha = mcs_alpha,
    mcs_bootstrap_samples = mcs_bootstrap_samples,
    mcs_statistic = mcs_statistic,
    mcs_block_length = mcs_block_length,
    mcs_min_block_length = mcs_min_block_length,
    seed = base_seed + 900000L
  )

  dm_test_results <- statistical_validation$dm_tests
  gw_test_results <- statistical_validation$gw_tests
  model_confidence_set <- statistical_validation$mcs_models
  model_confidence_set_summary <- statistical_validation$mcs_summary
  mcs_audit <- statistical_validation$mcs_audit
  mcs_audit_summary <- statistical_validation$mcs_audit_summary
}


###############################################################################
### Processing Summaries
###############################################################################

processing_summary <- data.frame(
  experiment_run_id = experiment_run_id,
  experiment_source = experiment_source,
  execution_profile = execution_profile,
  results_directory = normalizePath(results_directory, mustWork = FALSE),
  fred_md_file = normalizePath(fred_md_file, mustWork = FALSE),
  fred_md_data_md5 = fred_md_data_md5,
  target_code = target_name,
  target_name = target_display_name,
  target_tcode_official = target_tcode_official,
  target_tcode_effective = target_tcode,
  target_transform_source = target_transform_source,
  target_level_mode = target_level_mode,
  target_level_formula = target_level_formula,
  raw_start = min(fred_md_data[[1]]),
  raw_end = max(fred_md_data[[1]]),
  transformed_start = min(tdata[[1]]),
  transformed_end = max(tdata[[1]]),
  model_start = min(tdata_selected[[1]]),
  model_end = max(tdata_selected[[1]]),
  initial_window_start = tdata_selected[[1]][1L],
  initial_window_end = tdata_selected[[1]][window_size],
  oos_start = min(oos_dates),
  oos_end = max(oos_dates),
  forward_forecasts_enabled = enable_forward_forecasts,
  forward_origin = forward_origin_date,
  forward_origin_level = forward_origin_level,
  forward_window_start = forward_window_start_date,
  forward_target_dates = if (nrow(forward_forecasts) > 0L) {
    paste(
      format(
        sort(unique(as.Date(forward_forecasts$target_date))),
        "%Y-%m"
      ),
      collapse = ", "
    )
  } else {
    NA_character_
  },
  forward_forecast_rows = nrow(forward_forecasts),
  cumulative_backtest_rows = nrow(cumulative_backtest_results),
  statistical_validation_enabled = enable_statistical_validation,
  dm_test_rows = nrow(dm_test_results),
  gw_test_rows = nrow(gw_test_results),
  mcs_model_rows = nrow(model_confidence_set),
  mcs_summary_rows = nrow(model_confidence_set_summary),
  mcs_audit_rows = nrow(mcs_audit),
  mcs_audit_summary_rows = nrow(mcs_audit_summary),
  model_months = nrow(tdata_selected),
  window_type = "Fixed rolling",
  window_size = window_size,
  window_size_years = window_size / 12,
  required_model_months = required_rows,
  oos_months = npred,
  horizons = paste(forecast_horizons, collapse = ", "),
  requested_base_models = length(models_to_run),
  generated_ensemble_models = length(unique(ensemble_result$forecasts$model)),
  total_forecast_rows = nrow(forecast_results),
  total_variables = ncol(tdata_model) - 1L,
  selected_variables = length(selected_model_variables),
  metadata_mapped_variables = sum(
    variable_metadata$metadata_status == "Mapped"
  ),
  metadata_fallback_variables = sum(
    variable_metadata$metadata_status == "Fallback"
  ),
  obsolete_metadata_variables = length(obsolete_variables),
  target_imputed_months = sum(
    target_imputed_dates %in%
      tdata_model[[1]]
  ),
  oos_evaluation_excluded_months = sum(
    target_eval_exclude_selected[
      oos_index
    ]
  ),
  row.names = NULL
)

pca_em_test_summary <- data.frame(
  missing_before = initial_missing_count,
  short_gap_imputed_count = attr(
    Y_initial_em,
    "short_gap_imputed_count"
  ),
  final_imputed_count = attr(
    Y_initial_em,
    "final_imputed_count"
  ),
  dropped_predictor_count = attr(
    Y_initial_em,
    "dropped_predictor_count"
  ),
  missing_after = remaining_missing_count,
  imputed_count = attr(
    Y_initial_em,
    "imputed_count"
  ),
  iterations = attr(
    Y_initial_em,
    "em_iterations"
  ),
  converged = attr(
    Y_initial_em,
    "em_converged"
  ),
  last_change = attr(
    Y_initial_em,
    "em_last_change"
  ),
  observed_values_unchanged = observed_values_unchanged,
  row.names = NULL
)

rolling_window_test_summary <- data.frame(
  test = c(
    "First maximum-horizon forecast",
    "Last minimum-horizon forecast"
  ),
  window_rows = c(
    nrow(first_window_test$Y.window),
    nrow(last_window_test$Y.window)
  ),
  horizon = c(
    first_window_test$horizon,
    last_window_test$horizon
  ),
  window_start = c(
    as.character(first_window_test$window_start_date),
    as.character(last_window_test$window_start_date)
  ),
  origin_date = c(
    as.character(first_window_test$origin_date),
    as.character(last_window_test$origin_date)
  ),
  target_date = c(
    as.character(first_window_test$target_date),
    as.character(last_window_test$target_date)
  ),
  short_gap_imputed_count = c(
    first_window_test$short_gap_imputed_count,
    last_window_test$short_gap_imputed_count
  ),
  final_imputed_count = c(
    first_window_test$final_imputed_count,
    last_window_test$final_imputed_count
  ),
  dropped_predictor_count = c(
    first_window_test$dropped_predictor_count,
    last_window_test$dropped_predictor_count
  ),
  missing_after = c(
    first_window_test$missing_after,
    last_window_test$missing_after
  ),
  em_converged = c(
    first_window_test$em_converged,
    last_window_test$em_converged
  ),
  em_last_change = c(
    first_window_test$em_last_change,
    last_window_test$em_last_change
  ),
  row.names = NULL
)

excluded_variable_summary <- variable_selection[
  !variable_selection$selected,
  ,
  drop = FALSE
]

model_registry_summary <- summarize_model_registry(model_registry)

pca_em_convergence_summary <- unique(
  base_forecasts[
    !is.na(base_forecasts$em_converged),
    c(
      "horizon",
      "forecast_number",
      "window_start_date",
      "origin_date",
      "missing_before",
      "em_iterations",
      "em_converged",
      "em_last_change"
    ),
    drop = FALSE
  ]
)
rownames(pca_em_convergence_summary) <- NULL

###############################################################################
### Lightweight Boruta Validation Summary
###############################################################################

boruta_validation_bundle <- list(
  selection_history = empty_boruta_selection_history(),
  final_selection = empty_boruta_final_selection(),
  feature_stability = empty_boruta_feature_stability(),
  stability_summary = empty_boruta_stability_summary(),
  predictive_comparison = empty_boruta_predictive_comparison(),
  audit_summary = empty_boruta_audit_summary()
)

if (
  isTRUE(enable_boruta_validation_summary) &&
    "BorutaRF" %in% models_to_run
) {
  monthly_boruta_states <- base_backtest$final_states$BorutaRF %||% list()
  cumulative_boruta_states <- if (exists("cumulative_base_backtest")) {
    cumulative_base_backtest$final_states$BorutaRF %||% list()
  } else {
    list()
  }
  cumulative_boruta_forecasts <- if (exists("cumulative_base_forecasts")) {
    cumulative_base_forecasts
  } else {
    data.frame()
  }

  boruta_validation_bundle <- build_boruta_validation_bundle(
    target_code = target_name,
    target_name = target_display_name,
    monthly_forecasts = base_forecasts,
    monthly_accuracy = forecast_accuracy,
    monthly_states = monthly_boruta_states,
    cumulative_forecasts = cumulative_boruta_forecasts,
    cumulative_accuracy = cumulative_accuracy,
    cumulative_states = cumulative_boruta_states,
    stability_threshold = boruta_stability_threshold
  )
}

forecast_project <- list(
  forecasts = forecast_results,
  accuracy = forecast_accuracy,
  relative_accuracy = relative_accuracy,
  rankings = model_rankings,
  artifacts = base_backtest$artifacts,
  model_states = base_backtest$final_states,
  boruta_selection_history = boruta_validation_bundle$selection_history,
  boruta_final_selection = boruta_validation_bundle$final_selection,
  boruta_feature_stability = boruta_validation_bundle$feature_stability,
  boruta_stability_summary = boruta_validation_bundle$stability_summary,
  boruta_predictive_comparison = boruta_validation_bundle$predictive_comparison,
  boruta_audit_summary = boruta_validation_bundle$audit_summary,
  ensemble_weights = ensemble_result$weights,
  ensemble_members = ensemble_result$members,
  cumulative_backtest_results = cumulative_backtest_results,
  cumulative_accuracy = cumulative_accuracy,
  cumulative_relative_accuracy = cumulative_relative_accuracy,
  cumulative_rankings = cumulative_rankings,
  cumulative_ensemble_weights = cumulative_ensemble_weights,
  cumulative_rolling_index = cumulative_rolling_index,
  cumulative_artifacts = if (exists("cumulative_base_backtest")) {
    cumulative_base_backtest$artifacts
  } else {
    list()
  },
  cumulative_model_states = if (exists("cumulative_base_backtest")) {
    cumulative_base_backtest$final_states
  } else {
    list()
  },
  forward_forecasts = forward_forecasts,
  forward_ensemble_weights = forward_ensemble_weights,
  forward_best_models = forward_best_models,
  forward_artifacts = forward_artifacts,
  dm_test_results = dm_test_results,
  gw_test_results = gw_test_results,
  model_confidence_set = model_confidence_set,
  model_confidence_set_summary = model_confidence_set_summary,
  mcs_audit = mcs_audit,
  mcs_audit_summary = mcs_audit_summary,
  processing_summary = processing_summary,
  pca_em_test_summary = pca_em_test_summary,
  pca_em_convergence_summary = pca_em_convergence_summary,
  rolling_window_test_summary = rolling_window_test_summary,
  training_design_summary = training_design_summary,
  variable_selection = variable_selection,
  excluded_variable_summary = excluded_variable_summary,
  rolling_index = rolling_index,
  target_imputation_summary = target_imputation_summary,
  metadata_change_summary = metadata_change_summary,
  obsolete_variable_metadata = obsolete_variable_metadata,
  model_registry = model_registry_summary,
  configuration = list(
    experiment_run_id = experiment_run_id,
    experiment_source = experiment_source,
    experiment_requested_at = experiment_requested_at,
    results_directory = normalizePath(results_directory, mustWork = FALSE),
    fred_md_file = normalizePath(fred_md_file, mustWork = FALSE),
    fred_md_data_md5 = fred_md_data_md5,
    execution_profile = execution_profile,
    models_to_run = models_to_run,
    forecast_horizons = forecast_horizons,
    window_type = "fixed_rolling",
    window_size = window_size,
    window_size_years = window_size / 12,
    npred = npred,
    target_name = target_name,
    target_tcode_overrides = target_tcode_overrides,
    target_tcode_override = target_tcode_override,
    target_level_mode_override = target_level_mode_override,
    target_level_mode = target_level_mode,
    target_level_formula = target_level_formula,
    min_obs_ratio = min_obs_ratio,
    target_missing_policy = missing_data_policy$target,
    predictor_missing_policy = missing_data_policy$predictor,
    max_target_gap = max_target_gap,
    allow_imputed_oos_actual = allow_imputed_oos_actual,
    allow_imputed_origin_level = allow_imputed_origin_level,
    max_predictor_gap = max_predictor_gap,
    pca_em_factors = pca_em_factors,
    pca_em_max_iter = pca_em_max_iter,
    pca_em_tol = pca_em_tol,
    pca_em_require_convergence = pca_em_require_convergence,
    predictor_lags = predictor_lags,
    factor_count = factor_count,
    factor_include_target = factor_include_target,
    factor_scale = factor_scale,
    model_control = model_control,
    enable_boruta_validation_summary = enable_boruta_validation_summary,
    boruta_stability_threshold = boruta_stability_threshold,
    boruta_stability_basis = "rolling_refresh_windows",
    ensemble_methods = ensemble_methods,
    ensemble_members = ensemble_result$members,
    ensemble_min_members = ensemble_min_members,
    ensemble_min_history = ensemble_min_history,
    base_seed = base_seed,
    error_policy = error_policy,
    save_artifacts = save_artifacts,
    enable_forward_forecasts = enable_forward_forecasts,
    forward_target_mode = target_level_mode,
    forward_level_formula = target_level_formula,
    enable_statistical_validation = enable_statistical_validation,
    primary_evaluation_track = primary_evaluation_track,
    secondary_evaluation_tracks = secondary_evaluation_tracks,
    primary_inference_methods = primary_inference_methods,
    supplementary_inference_methods = supplementary_inference_methods,
    statistical_benchmark_model = statistical_benchmark_model,
    statistical_loss_functions = statistical_loss_functions,
    statistical_significance_level = statistical_significance_level,
    dm_alternative = dm_alternative,
    dm_varestimator = dm_varestimator,
    dm_p_adjust_method = dm_p_adjust_method,
    gw_alternative = gw_alternative,
    gw_method = gw_method,
    gw_p_adjust_method = gw_p_adjust_method,
    gw_reference_rule = gw_reference_rule,
    gw_comparison_label = gw_comparison_label,
    gw_inference_role = gw_inference_role,
    gw_formal_giacomini_white_test = gw_formal_giacomini_white_test,
    gw_loss_function = "AE",
    gw_post_selection_comparison = TRUE,
    mcs_alpha = mcs_alpha,
    mcs_bootstrap_samples = mcs_bootstrap_samples,
    mcs_statistic = mcs_statistic,
    mcs_block_length = mcs_block_length,
    mcs_min_block_length = mcs_min_block_length
  )
)

if (isTRUE(save_results)) {
  report_experiment_progress(
    stage = "saving_results",
    message = "Saving forecasts, diagnostics, and experiment metadata.",
    progress_percent = 93
  )

  save_forecast_project_results(
    output_directory = results_directory,
    forecast_project = forecast_project
  )
}

print(processing_summary)
print(training_design_summary)
print(pca_em_test_summary)
print(rolling_window_test_summary)
if (
  isTRUE(enable_boruta_validation_summary) &&
    nrow(boruta_validation_bundle$audit_summary) > 0L
) {
  print(boruta_validation_bundle$audit_summary)
  print(boruta_validation_bundle$stability_summary)
}

if (isTRUE(enable_statistical_validation)) {
  print(model_confidence_set_summary)
  print(mcs_audit_summary)
}

nonconverged_pca_windows <- pca_em_convergence_summary[
  !pca_em_convergence_summary$em_converged,
  ,
  drop = FALSE
]

if (nrow(nonconverged_pca_windows) > 0L) {
  message(
    "PCA-EM 미수렴 window: ",
    nrow(nonconverged_pca_windows),
    "개. 예측은 계속되며 상세 내용은 ",
    "forecast_project$pca_em_convergence_summary에서 확인할 수 있습니다."
  )
}

print(model_rankings)

if (nrow(target_imputation_summary) > 0L) {
  print(target_imputation_summary)
}

if (nrow(excluded_variable_summary) > 0L) {
  print(excluded_variable_summary)
}

if (nrow(metadata_change_summary) > 0L) {
  print(metadata_change_summary)
}
