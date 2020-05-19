#' Use \pkg{basilisk} environments
#'
#' Use \pkg{basilisk} environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the \pkg{basilisk} environment to use. 
#' @param dry Logical scalar indicating whether only the directory should be returned without loading the environment.
#' @param required Logical scalar indicating whether an error should be raised if the requested environment cannot be found.
#' 
#' @return 
#' The function will attempt to load the specified \pkg{basilisk} environment into the R session,
#' possibly with the modification of some environment variables (see Details).
#'
#' It returns a list containing:
#' \itemize{
#' \item \code{loaded}, a logical scalar indicating whether the Python version in \code{envpath} was correctly loaded.
#' \item \code{previous}, a list of environment variables with their values prior to running this function.
#' }
#' If \code{dry=TRUE}, only the logical scalar in \code{loaded} is returned directly.
#'
#' @details
#' It is unlikely that developers should ever need to call \code{\link{useBasiliskEnv}} directly.
#' Rather, this interaction should be automatically handled by \code{\link{basiliskStart}}.
#' 
#' Direct use of this function with \code{dry=FALSE} will modify some environment variables for the current R session:
#' \itemize{
#' \item The \code{"PYTHONPATH"} environment variable is unset.
#' This is a deliberate choice to avoid compromising the version guarantees if \code{\link{import}} is allowed to search other locations beyond the specified \pkg{basilisk} environment.
#' \item The \code{"RETICULATE_PYTHON"} environment variable is unset.
#' This would otherwise override any choice of Python, including explicit specification via \code{\link{use_condaenv}}!
#' \item Certain conda-related variables are modified to mimic activation of the desired conda environment in \code{envpath}.
#' Actual activation seems to require modification of various files (e.g., \code{.bashrc}) which is undesirable.
#' }
#' 
#' If \code{dry=TRUE}, no environment variables are modified.
#' Similarly, if the loading of the environment in \code{envpath} was not successful (i.e., \code{loaded} is \code{FALSE}), no environment variables are modified and \code{previous} is an empty list.
#' Further note that \code{\link{basiliskStop}} will restore these environment variables to their state prior to running \code{\link{basiliskStart}}.
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
useBasiliskEnv <- function(envpath, dry=FALSE, required=TRUE) {
    envpath <- normalizePath(envpath, mustWork=TRUE)
    if (dry) {
        if (!py_available()) {
            return(FALSE)
        } else {
            return(.same_as_loaded(envpath))
        }
    }

    if (!required && py_available()) {
        return(list(loaded=.same_as_loaded(envpath), previous=list()))
    }

    previous <- .coerce_env_vars(envpath)
    use_condaenv(envpath, required=TRUE)
    same <- .same_as_loaded(envpath)

    # Make life a bit easier and restore old environment variables
    # if the current version was not successfully loaded.
    if (!same) {
        .restore_env_vars(previous)
        previous <- list()
    }

    list(loaded=same, previous=previous)
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
