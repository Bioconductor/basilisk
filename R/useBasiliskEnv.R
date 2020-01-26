#' Use \pkg{basilisk} environments
#'
#' Use \pkg{basilisk} environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the environment to use. 
#' @param dry Logical scalar indicating whether only the directory should be returned without loading the environment.
#' @param required Logical scalar indicating whether an error should be raised if the requested environment cannot be found.
#' 
#' @return 
#' The function will load the specified \pkg{basilisk} environment into the R session.
#' It returns a string specifying the path to the environment.
#' If \code{dry=TRUE}, the character vector is returned without loading the environment.
#'
#' @details
#' It is unlikely that developers should ever need to call \code{\link{useBasiliskEnv}} directly.
#' Rather, this interaction should be automatically handled by \code{\link{basiliskStart}}.
#' 
#' A side-effect of \code{useBasiliskEnv} with \code{dry=FALSE} is that the \code{"PYTHONPATH"} environment variable is unset for the duration of the R session
#' (or \pkg{basilisk} process, depending on the back-end chosen by \code{\link{basiliskStart}}).
#' This is a deliberate choice to avoid compromising the version guarantees if \code{\link{import}} is allowed to search other locations beyond the specified environment.
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
#' \code{\link{basiliskStart}}, for how these virtual environments should be used.
#'
#' @export
#' @importFrom reticulate py_config use_condaenv use_python
#' @importFrom basilisk.utils getBasiliskDir
useBasiliskEnv <- function(envpath, dry=FALSE, required=TRUE) {
    old.retpy <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old.retpy!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old.retpy))
    }

    envpath <- normalizePath(envpath, mustWork=TRUE)

    if (!dry) {
        # Don't even try to be nice and add an on.exit() clause to protect the
        # global session. This is deliberate; if we're using a virtual
        # environment, and someone tries to import package in the global session,
        # and Python looks somewhere else other than our virtual environment via
        # the PYTHONPATH, we can get the wrong package loaded. 
        Sys.unsetenv("PYTHONPATH")

        use_condaenv(envpath, required=required)
    }

    # Checking whether we're the same as the existing python instance,
    # which would indicate that we correctly loaded ourselves.
    identical(.get_py_cmd(envpath), py_config()$python)
}
