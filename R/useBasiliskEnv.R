#' Use \pkg{basilisk} environments
#'
#' Use \pkg{basilisk} environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @inheritParams setupBasiliskEnv
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
#' If \code{pkgname} is specified, \code{useBasiliskEnv} will search in the installation directory of \code{pkgname} for the specified \code{envname}
#' Otherwise, the function will treat \code{envname} as the path to the desired environment.
#' 
#' A side-effect of \code{useBasiliskEnv} with \code{dry=FALSE} is that the \code{"PYTHONPATH"} environment variable is unset for the duration of the R session
#' (or \pkg{basilisk} process, depending on the back-end chosen by \code{\link{basiliskStart}}).
#' This is a deliberate choice to avoid compromising the version guarantees if \code{\link{import}} is allowed to search other locations beyond the virtual environment.
#'
#' @author Aaron Lun
#' 
#' @examples
#' ##################################################
#' # Creating virtualenvs in a temporary directory to 
#' # avoid polluting the user's WORKON_HOME.
#' tmploc <- file.path(tempdir(), "basilisk")
#' dir.create(tmploc)
#' old <- Sys.getenv("WORKON_HOME")
#' Sys.setenv(WORKON_HOME=tmploc)
#' ##################################################
#'
#' setupBasiliskEnv('my_package_A_alt', 'pandas')
#' useBasiliskEnv("my_package_A_alt")
#' 
#' ##################################################
#' # Restoring the old WORKON_HOME.
#' Sys.setenv(WORKON_HOME=old)
#' ##################################################
#' @seealso
#' \code{\link{basiliskStart}}, for how these virtual environments should be used.
#'
#' @export
#' @importFrom reticulate use_virtualenv py_config use_condaenv
useBasiliskEnv <- function(envname, pkgname=NULL, dry=FALSE, required=TRUE) {
    old.retpy <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old.retpy!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old.retpy))
    }

    if (is.null(pkgname)) {
        envdir <- normalizePath(envname, mustWork=TRUE)
    } else {
        vdir <- .choose_env_dir(pkgname, mustWork=TRUE)
        envdir <- file.path(vdir, envname)
    }

    mode <- "virtualenv"
    if (!file.exists(envdir)) {
        mode <- "common"
    } else if (any(grepl("conda", list.files(envdir)))) {
        mode <- "conda"
    }

    if (mode!="common") {
        envdir <- normalizePath(envdir, mustWork=TRUE)
    } else {
        envdir <- normalizePath(.get_basilisk_dir())
    }

    if (!dry) {
        # Don't even try to be nice and add an on.exit() clause to protect the
        # global session. This is deliberate; if we're using a virtual
        # environment, and someone tries to import package in the global session,
        # and Python looks somewhere else other than our virtual environment via
        # the PYTHONPATH, we can get the wrong package loaded. 
        Sys.unsetenv("PYTHONPATH")

        if (mode=="conda") {
            use_condaenv(envdir, required=required)
        } else if (mode=="virtualenv") {
            use_virtualenv(envdir, required=required)
        } else {
            use_python(.get_py_cmd(envdir), required=required)
        }
    }

    # Checking whether we're the same as the existing python instance,
    # which would indicate that we correctly loaded ourselves.
    if (mode=="virtualenv") {
        identical(envdir, py_config()$virtualenv)
    } else {
        identical(.get_py_cmd(envdir), py_config()$python)
    }
}
