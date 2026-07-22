###############################################################################
### Source All Project Functions
###############################################################################

source(file.path(project_root, "functions", "core", "func-utils.R"))
source(file.path(project_root, "functions", "core", "func-target-processing.R"))
source(file.path(project_root, "functions", "core", "func-pca-em.R"))
source(file.path(project_root, "functions", "core", "func-missing-data.R"))
source(file.path(project_root, "functions", "core", "func-rolling.R"))
source(file.path(project_root, "functions", "core", "func-design-matrix.R"))

source(file.path(project_root, "functions", "models", "func-rw.R"))
source(file.path(project_root, "functions", "models", "func-ar.R"))
source(file.path(project_root, "functions", "models", "func-glmnet.R"))
source(file.path(project_root, "functions", "models", "func-factor.R"))
source(file.path(project_root, "functions", "models", "func-rf.R"))
source(file.path(project_root, "functions", "models", "func-xgboost.R"))
source(file.path(project_root, "functions", "models", "func-boruta-rf.R"))

source(file.path(project_root, "functions", "registry", "model-registry.R"))
source(file.path(project_root, "functions", "core", "func-backtest.R"))
source(file.path(project_root, "functions", "core", "func-ensemble.R"))
source(file.path(project_root, "functions", "core", "func-evaluation.R"))
source(file.path(project_root, "functions", "core", "func-forward.R"))
source(file.path(project_root, "functions", "core", "func-statistical-validation.R"))
source(file.path(project_root, "functions", "core", "func-boruta-validation.R"))
source(file.path(project_root, "functions", "core", "func-results.R"))
