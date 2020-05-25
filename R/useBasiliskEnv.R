#' Use \pkg{basilisk} environments
#'
#' Use \pkg{basilisk} environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the \pkg{basilisk} environment to use. 
#' @param required Logical scalar indicating whether an error should be raised if the requested environment cannot be found.
#' 
#' @return 
#' The function will attempt to load the specified \pkg{basilisk} environment into the R session,
#' possibly with the modification of some environment variables (see Details).
#' It returns a logical scalar indicating whether the Python version in \code{envpath} was correctly loaded.
#'
#' @details
#' It is unlikely that developers should ever need to call \code{\link{useBasiliskEnv}} directly.
#' Rather, this interaction should be automatically handled by \code{\link{basiliskStart}}.
#' 
#' By default, this function will modify a suite of environment variables as a side effect
#' - see \code{\link{activateEnvironment}} for details.
#' Exceptions are:
#' \itemize{
#' \item \code{required=FALSE} and any Python instance is already loaded into the current R session.
#' \item the loading of the environment in \code{envpath} was not successful (i.e., \code{loaded} is \code{FALSE}).
#' }
#' In these cases, no modification of environment variables is performed.
#'
#' @author Aaron Lun
#' 
#' @examples
#' # This may return TRUE or FALSE, depending on the available Python.
#' tmploc <- file.path(tempdir(), "my_package_B")
#' setupBasiliskEnv(tmploc, c('pandas==0.25.1',
#'     "python-dateutil=2.8.0", "pytz=2019.3"))
#' useBasiliskEnv(tmploc, required=FALSE) 
#'
#' # This will return FALSE, as the available Python is already set.
#' baseloc <- basilisk.utils::getBasiliskDir()
#' useBasiliskEnv(baseloc, required=FALSE)
#'
#' @seealso
#' \code{\link{basiliskStart}}, for how these \pkg{basilisk} environments should be used.
#'
#' @export
#' @importFrom reticulate py_available use_condaenv 
#' @importFrom basilisk.utils getBasiliskDir getPythonBinary 
#' activateEnvironment deactivateEnvironment
useBasiliskEnv <- function(envpath, required=TRUE) {
    envpath <- normalizePath(envpath, mustWork=TRUE)
    if (dry) {
        if (!py_available()) {
            return(FALSE)
        } else {
            return(.same_as_loaded(envpath))
        }
    }

    if (!required && py_available()) {
        return(.same_as_loaded(envpath))
    }

    previous <- activateEnvironment(envpath)
    use_condaenv(envpath, required=TRUE)
    same <- .same_as_loaded(envpath)

    # Make life a bit easier and restore old environment variables
    # if the current version was not successfully loaded.
    if (!same) {
        deactivateEnvironment(previous)
    }

    same
}

#' @importFrom reticulate py_config 
#' @importFrom basilisk.utils getPythonBinary 
.same_as_loaded <- function(envpath) 
# Checking whether we're the same as the existing python instance,
# which would indicate that we correctly loaded `envpath`.
{
    expected <- normalizePath(getPythonBinary(envpath)) 
    actual <- normalizePath(py_config()$python)
    identical(expected, actual)
}
