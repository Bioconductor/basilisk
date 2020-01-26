#' @importFrom basilisk BasiliskEnvironment
env1 <- BasiliskEnvironment("env1", pkgname="son.of.basilisk",
    packages=c("pandas==0.24.1", "python-dateutil==2.7.1", "pytz==2018.7"))

#' @importFrom basilisk BasiliskEnvironment
env2 <- BasiliskEnvironment("env2", pkgname="son.of.basilisk",
    packages=c("scikit-learn==0.21.1", "joblib==0.13.1"))
