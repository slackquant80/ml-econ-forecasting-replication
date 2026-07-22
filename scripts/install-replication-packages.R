required_packages <- c("glmnet", "randomForest", "xgboost", "Boruta", "forecast", "sandwich", "MCS")
missing <- required_packages[!vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org", dependencies = TRUE)
versions <- vapply(required_packages, function(x) as.character(utils::packageVersion(x)), character(1))
print(data.frame(package = required_packages, installed_version = versions), row.names = FALSE)
