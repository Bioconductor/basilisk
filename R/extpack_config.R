
#' produce a string to define a pandas version for a BasiliskEnvironment
#' @param val character(1) valid version for conda package to be used
#' @note We use conda notation, "pandas=2.1.4", while pip would require "=="
#' @export
pandas_spec = function(val="2.1.4") sprintf("pandas=%s", val)

#' produce a string in pip format to define a pandas version for a BasiliskEnvironment
#' @param val character(1) valid version for conda package to be used
#' @note We use pip notation, "pandas==2.1.4", while pip would require "=="
#' @export
pandas_spec_pip = function(val="2.1.4") sprintf("pandas==%s", val)

#' produce a string to define a scikit-learn version for a BasiliskEnvironment
#' @param val character(1) valid version for conda package to be used
#' @note We use conda notation, "scikit-learn=1.3.4", while pip would require "=="
#' @export
scikit_learn_spec = function(val="1.3.0") sprintf("scikit-learn=%s", val)

#' produce a pip version string to define a scikit-learn version for a BasiliskEnvironment
#' @param val character(1) valid version for conda package to be used
#' @note We use conda notation, "scikit-learn=1.3.4", while pip would require "=="
#' @export
scikit_learn_spec_pip = function(val="1.3.0") sprintf("scikit-learn==%s", val)

#' produce a string to define a python-dateutil version for a BasiliskEnvironment
#' for use in longtests with son.of.basilisk
#' @param val character(1) valid version for pip package to be used
#' @export
python_dateutil_spec_pip = function(val="2.8.2") sprintf("python-dateutil==%s", val)

#' produce a string to define a pytz version for a BasiliskEnvironment
#' for use in longtests with son.of.basilisk
#' @param val character(1) valid version for pip package to be used
#' @export
pytz_spec_pip = function(val="2022.2.1") sprintf("pytz==%s", val)
