###############################################################################
### PCA-EM Missing Value Imputation
###############################################################################

pca_em_impute <- function(
    Y.window,
    n_factors = 4L,
    max_iter = 300L,
    tol = 1e-5
) {
  Y.original <- as.matrix(Y.window)
  storage.mode(Y.original) <- "double"

  if (
    nrow(Y.original) < 3L ||
    ncol(Y.original) < 2L
  ) {
    stop("PCA-EM을 수행하기 위한 행 또는 열의 수가 부족합니다.")
  }

  if (
    length(n_factors) != 1L ||
    !is.finite(n_factors) ||
    n_factors < 1
  ) {
    stop("n_factors는 1 이상의 유한한 값이어야 합니다.")
  }

  if (
    length(max_iter) != 1L ||
    !is.finite(max_iter) ||
    max_iter < 1
  ) {
    stop("max_iter는 1 이상의 유한한 값이어야 합니다.")
  }

  if (
    length(tol) != 1L ||
    !is.finite(tol) ||
    tol <= 0
  ) {
    stop("tol은 0보다 큰 유한한 값이어야 합니다.")
  }

  n_factors <- as.integer(n_factors)
  max_iter <- as.integer(max_iter)

  if (
    any(is.infinite(Y.original)) ||
    any(is.nan(Y.original))
  ) {
    stop("PCA-EM 입력자료에 Inf 또는 NaN이 있습니다.")
  }

  # 첫 번째 열은 목표변수이며 PCA-EM으로 대체하지 않는다.
  if (anyNA(Y.original[, 1])) {
    stop("PCA-EM 입력 window의 목표변수에 결측치가 있습니다.")
  }

  missing_index <- is.na(Y.original)

  if (!any(missing_index)) {
    Y.output <- Y.original

    attr(Y.output, "em_iterations") <- 0L
    attr(Y.output, "em_converged") <- TRUE
    attr(Y.output, "em_last_change") <- 0
    attr(Y.output, "imputed_count") <- 0L

    return(Y.output)
  }

  column_mean <- colMeans(
    Y.original,
    na.rm = TRUE
  )

  column_sd <- apply(
    Y.original,
    2L,
    sd,
    na.rm = TRUE
  )

  invalid_column <- (
    !is.finite(column_mean) |
      !is.finite(column_sd) |
      column_sd <= sqrt(.Machine$double.eps)
  )

  if (any(invalid_column)) {
    invalid_names <- colnames(Y.original)[
      invalid_column
    ]

    if (is.null(invalid_names)) {
      invalid_names <- which(invalid_column)
    }

    stop(
      "PCA-EM window에 전체 결측 또는 분산이 0인 변수가 있습니다: ",
      paste(invalid_names, collapse = ", ")
    )
  }

  # 관측값으로 계산한 고정 평균과 표준편차를 사용한다.
  Z.filled <- sweep(
    Y.original,
    2L,
    column_mean,
    "-"
  )

  Z.filled <- sweep(
    Z.filled,
    2L,
    column_sd,
    "/"
  )

  # 표준화된 평균 0으로 결측값 초기화
  Z.filled[missing_index] <- 0

  max_factors <- min(
    nrow(Z.filled) - 1L,
    ncol(Z.filled)
  )

  n_factors <- min(
    n_factors,
    max_factors
  )

  if (n_factors < 1L) {
    stop("PCA factor 수가 1보다 작습니다.")
  }

  previous_missing <- Z.filled[
    missing_index
  ]

  converged <- FALSE
  iteration <- 0L
  change <- Inf

  for (iteration in seq_len(max_iter)) {
    pca_model <- prcomp(
      Z.filled,
      center = TRUE,
      scale. = FALSE,
      rank. = n_factors
    )

    active_factors <- seq_len(
      ncol(pca_model$rotation)
    )

    Z.hat <- (
      pca_model$x[
        ,
        active_factors,
        drop = FALSE
      ] %*%
        t(
          pca_model$rotation[
            ,
            active_factors,
            drop = FALSE
          ]
        )
    )

    if (!is.null(pca_model$center)) {
      Z.hat <- sweep(
        Z.hat,
        2L,
        pca_model$center,
        "+"
      )
    }

    current_missing <- Z.hat[
      missing_index
    ]

    if (any(!is.finite(current_missing))) {
      stop("PCA-EM 반복 과정에서 비유한 결측 추정값이 생성되었습니다.")
    }

    change <- sqrt(
      mean(
        (current_missing - previous_missing)^2
      )
    )

    Z.filled[missing_index] <- current_missing

    if (change < tol) {
      converged <- TRUE
      break
    }

    previous_missing <- current_missing
  }

  Y.filled <- sweep(
    Z.filled,
    2L,
    column_sd,
    "*"
  )

  Y.filled <- sweep(
    Y.filled,
    2L,
    column_mean,
    "+"
  )

  # 원래 관측값은 정확히 보존
  Y.filled[!missing_index] <- Y.original[!missing_index]
  dimnames(Y.filled) <- dimnames(Y.original)

  attr(Y.filled, "em_iterations") <- iteration
  attr(Y.filled, "em_converged") <- converged
  attr(Y.filled, "em_last_change") <- as.numeric(change)
  attr(Y.filled, "imputed_count") <- sum(missing_index)

  Y.filled
}

