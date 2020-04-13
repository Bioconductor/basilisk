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
#' setupBasiliskEnv(tmploc, 'pandas==0.25.1')
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
#' @importFrom basilisk.utils getBasiliskDir getPythonBinary isWindows
useBasiliskEnv <- function(envpath, dry=FALSE, required=TRUE) {
    envpath <- normalizePath(envpath, mustWork=TRUE)
    if (dry) {
        if (!py_available()) {
            return(FALSE)
        } else {
            return(.same_as_loaded(envpath))
        }
    }

    previous <- .coerce_env_vars()
    use_condaenv(envpath, required=required)
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
    identical(getPythonBinary(envpath), py_config()$python)
}

#' @importFrom basilisk.utils isWindows
.coerce_env_vars <- function() {
    ADD <- function(listing, var) {
        listing[[var]] <- Sys.getenv(var, unset=NA)
        listing
    }

    output <- list()
    output <- ADD(output, "PYTHONPATH")

    # Don't even try to be nice and add an on.exit() clause to protect the
    # global session. This is deliberate; if we're using a virtual environment,
    # and someone tries to import package in the global session, and Python
    # looks somewhere else other than our virtual environment via the
    # PYTHONPATH, we can get the wrong package loaded. 
    Sys.unsetenv("PYTHONPATH")

    # This also needs to be unset otherwise it seems to take priority over
    # everything, even if you explicitly request to use a specific conda env's
    # Python (see LTLA/basilisk#1).
    output <- ADD(output, "RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")

    output <- ADD(output, "RETICULATE_PYTHON_ENV")
    Sys.unsetenv("RETICULATE_PYTHON_ENV")

    if (isWindows()) {
        output <- ADD(output, "CONDA_DLL_SEARCH_MODIFICATION_ENABLE")

        # Motivated by ContinuumIO/anaconda-issues#10576, mimic the effect of
        # activation, at least for dynamic linking.
        Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE=1)
    }

    output
}

.restore_env_vars <- function(listing) {
    for (x in names(listing)) {
        if (is.na(listing[[x]])) {
            Sys.unsetenv(x)
        } else {
            do.call(Sys.setenv, listing[x])
        }
    }
}
