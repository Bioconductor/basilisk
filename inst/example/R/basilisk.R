# these values should be taken from package-accessible function
BASILISK_SCIKIT_LEARN_VERSION="1.3.2"
BASILISK_PANDAS_VERSION="2.1.4"
BASILISK_PYTHON_DATEUTIL_VERSION="2.8.2"
BASILISK_PYTHON_DATEUTIL_VERSION_OLD="2.8.1"
BASILISK_PYTZ_VERSION="2022.2"
BASILISK_PYTZ_VERSION_OLD="2022.1"

#' @importFrom basilisk BasiliskEnvironment
env1 <- BasiliskEnvironment("env1", pkgname="son.of.basilisk",
    packages=c(sprintf("pandas==%s", son.of.basilisk:::BASILISK_PANDAS_VERSION), 
              sprintf("python-dateutil==%s", son.of.basilisk:::BASILISK_PYTHON_DATEUTIL_VERSION), 
              sprintf("pytz==%s", son.of.basilisk:::BASILISK_PYTZ_VERSION)))

#' @importFrom basilisk BasiliskEnvironment
env2 <- BasiliskEnvironment("env2", pkgname="son.of.basilisk",
    packages=c(sprintf("scikit-learn==%s", son.of.basilisk:::BASILISK_SCIKIT_LEARN_VERSION), 
              sprintf("python-dateutil==%s", son.of.basilisk:::BASILISK_PYTHON_DATEUTIL_VERSION_OLD), 
              sprintf("pytz==%s", son.of.basilisk:::BASILISK_PYTZ_VERSION_OLD)))

#' @importFrom basilisk BasiliskEnvironment
env3 <- BasiliskEnvironment("env3", pkgname="son.of.basilisk",
    packages=character(0), path="test_dummy")
