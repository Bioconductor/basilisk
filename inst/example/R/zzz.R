.onLoad <- function(libname, pkgname) {
    basilisk::setupVirtualEnv("env1", 
        c("pandas==0.25.1", "python-dateutil==2.8.1", "pytz==2019.3"),
        pkgname=pkgname)

    basilisk::setupVirtualEnv("env2", 
        c("scikit-learn==0.21.0", "joblib==0.14.0"), 
        pkgname=pkgname)
}
