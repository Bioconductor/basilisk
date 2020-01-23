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
#' We require \code{envpath} (rather than the usual \code{envname} and \code{pkgname}) to avoid difficult errors with \code{\link{system.file}} when the package has not yet been loaded in the child process of \code{\link{basiliskStart}}.
#' 
#' A side-effect of \code{useBasiliskEnv} with \code{dry=FALSE} is that the \code{"PYTHONPATH"} environment variable is unset for the duration of the R session
#' (or \pkg{basilisk} process, depending on the back-end chosen by \code{\link{basiliskStart}}).
#' This is a deliberate choice to avoid compromising the version guarantees if \code{\link{import}} is allowed to search other locations beyond the virtual environment.
#'
#' @author Aaron Lun
#' 
#' @examples
#' tmploc <- file.path(tempdir(), "my_package_B")
#' setupBasiliskEnv(tmploc, 'pandas')
#' useBasiliskEnv(tmploc) # TRUE or FALSE, depending on global Python.
#'
#' tmploc2 <- file.path(tempdir(), "my_package_B_alt")
#' setupBasiliskEnv(tmploc2, 'pandas==0.24.1')
#' useBasiliskEnv(tmploc2) # FALSE, as global Python is already occupied.
#'
#' @seealso
#' \code{\link{basiliskStart}}, for how these virtual environments should be used.
#'
#' @export
#' @importFrom reticulate use_virtualenv py_config use_condaenv
useBasiliskEnv <- function(envpath, dry=FALSE, required=TRUE) {
    old.retpy <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old.retpy!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old.retpy))
    }

    mode <- "virtualenv"
    if (!file.exists(envpath)) {
        mode <- "common"
    } else if (any(grepl("conda", list.files(envpath)))) {
        mode <- "conda"
    }

    if (mode!="common") {
        envpath <- normalizePath(envpath, mustWork=TRUE)
    } else {
        envpath <- normalizePath(.get_basilisk_dir())
    }

    if (!dry) {
        # Don't even try to be nice and add an on.exit() clause to protect the
        # global session. This is deliberate; if we're using a virtual
        # environment, and someone tries to import package in the global session,
        # and Python looks somewhere else other than our virtual environment via
        # the PYTHONPATH, we can get the wrong package loaded. 
        Sys.unsetenv("PYTHONPATH")

        if (mode=="conda") {
            use_condaenv(envpath, required=required)
        } else if (mode=="virtualenv") {
            use_virtualenv(envpath, required=required)
        } else {
            use_python(.get_py_cmd(envpath), required=required)
        }
    }

    # Checking whether we're the same as the existing python instance,
    # which would indicate that we correctly loaded ourselves.
    if (mode=="virtualenv") {
        identical(envpath, py_config()$virtualenv)
    } else {
        identical(.get_py_cmd(envpath), py_config()$python)
    }
}
