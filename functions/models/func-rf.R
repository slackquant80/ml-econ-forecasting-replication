###############################################################################
### Random Forest
###############################################################################

forecast_rf_model <- function(context, config = list(), state = NULL) {
  check_required_packages("randomForest")

  design <- context$feature_bundle$full_design
  X <- design$X
  y <- design$y

  ntree <- validate_scalar_integer(
    get_config_value(config, "ntree", 500L), "RF ntree", 1L
  )
  nodesize <- validate_scalar_integer(
    get_config_value(config, "nodesize", 5L), "RF nodesize", 1L
  )
  mtry <- get_config_value(config, "mtry", NULL)
  if (is.null(mtry)) mtry <- max(1L, floor(sqrt(ncol(X))))
  mtry <- min(validate_scalar_integer(mtry, "RF mtry", 1L), ncol(X))

  set.seed(context$seed)
  fit <- randomForest::randomForest(
    x = X,
    y = y,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize,
    importance = TRUE
  )

  new_x <- matrix(
    design$new_x,
    nrow = 1L,
    dimnames = list(NULL, colnames(X))
  )
  prediction <- as.numeric(
    stats::predict(fit, new_x)
  )

  importance_matrix <- as.matrix(randomForest::importance(fit))
  importance_column <- if ("%IncMSE" %in% colnames(importance_matrix)) {
    "%IncMSE"
  } else {
    colnames(importance_matrix)[ncol(importance_matrix)]
  }

  importance_table <- data.frame(
    feature = rownames(importance_matrix),
    importance = as.numeric(importance_matrix[, importance_column]),
    stringsAsFactors = FALSE
  )
  importance_table <- importance_table[
    order(importance_table$importance, decreasing = TRUE),
    , drop = FALSE
  ]
  top_n <- validate_scalar_integer(
    get_config_value(config, "importance_top_n", 20L),
    "RF importance_top_n",
    1L
  )
  importance_table <- head(importance_table, top_n)

  list(
    prediction = prediction,
    diagnostics = list(
      training_observations = design$training_observations,
      n_features = ncol(X),
      n_selected = ncol(X),
      tuning_parameter = paste0("ntree=", ntree, ";mtry=", mtry),
      validation_loss = NA_real_
    ),
    artifact = importance_table,
    state = state
  )
}
