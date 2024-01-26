#' @importFrom basilisk BasiliskEnvironment
env1 <- BasiliskEnvironment("env1", pkgname="son.of.basilisk",
    packages=c(basilisk::pandas_spec_pip(), basilisk::python_dateutil_spec_pip(), 
      basilisk::pytz_spec_pip()))

#' @importFrom basilisk BasiliskEnvironment
env2 <- BasiliskEnvironment("env2", pkgname="son.of.basilisk",
    packages=c(basilisk::scikit_learn_spec_pip(), 
         basilisk::python_dateutil_spec_pip("2.8.1"), 
         basilisk::pytz_spec_pip("2022.1")))

#' @importFrom basilisk BasiliskEnvironment
env3 <- BasiliskEnvironment("env3", pkgname="son.of.basilisk",
    packages=character(0), path="test_dummy")
