# Run from the repository root. This installs current packages, not a lockfile.
packages <- c("rpart", "rpart.plot", "ggplot2", "data.table", "randomForest")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) install.packages(missing, repos="https://cloud.r-project.org")
