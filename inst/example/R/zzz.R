.onLoad <- function(libname, pkgname) {
    # Numbers are deliberately chosen here to not be the same
    # as the core installation, so it forces some installation.
    basilisk::setupBasiliskEnv("env1", 
        c("pandas==0.24.1", "python-dateutil==2.7.1", "pytz==2018.7"),
        pkgname=pkgname)

    basilisk::setupBasiliskEnv("env2", 
        c("scikit-learn", "joblib"), 
        pkgname=pkgname)
}
