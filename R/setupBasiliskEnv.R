#' Set up a \pkg{basilisk} environments
#'
#' Set up a Python virtual or conda environment (depending on the operating system)
#' for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the environment to use. 
#' @param packages Character vector containing the names of Python packages to install into the environment.
#' It is required to include version numbers in each string.
#' @param conda Logical scalar indicating whether a conda environment should be created.
#' 
#' @return 
#' A virtual or conda environment is created at \code{envpath} containing the specified \code{packages}.
#' Under certain conditions (see Details), no environment is created;
#' the function will return a logical scalar indicating whether any creation was performed.
#'
#' @details
#' Use of \pkg{basilisk} environments avoids version conflicts within an R session when different Bioconductor packages (or even different functions within a single package) require incompatible versions of Python packages.
#' We call these \pkg{basilisk} environments as the function will automatically switch between virtual and conda environments depending on the operating system.
#' MacOSX and Linux default to virtual environments to enable re-use of dependencies from the core installation, while Windows can only conda environments.
#' Developers can force the former to conda environments with \code{conda=TRUE}.
#' 
#' If all of the requested packages fall into the \dQuote{core} list of packages (see \code{?\link{listCorePackages}}),
#' this function is a no-op.
#' Any attempt to use \code{envname} in \code{\link{basiliskStart}} will simply fall back to the core Anaconda instance.
#' This enables multiple client packages to use the same Python for greater efficiency with \code{\link{basiliskStart}}.
#' In addition, if a \pkg{basilisk} environment is already present at \code{envpath}, \code{setupBasiliskEnv} is a no-op.
#' This ensures that the function only installs the packages once.
#'
#' Developers of client packages should never need to call this function directly.
#' For typical usage, \code{setupBasiliskEnv} is automatically called by \code{\link{basiliskStart}} to perform lazy installation.
#' Developers should create \code{configure(.win)} files to call \code{\link{configureBasiliskEnv}},
#' which will call \code{setupBasiliskEnv} during R package installation when \code{BASILISK_USE_SYSTEM_DIR} is set.
#'
##' @section Dealing with versioning: 
#' Pinned version numbers must be present for all requested packages in \code{packages}.
#' This improved predictability makes debugging much easier when the R package is installed and executed on different systems.
#' Explicit versions are also mandatory for all dependencies of requested packages,
#' which is again necessary to ensure that the same versions are deployed across systems.
#'
#' The only exception to the above rule is for \dQuote{core} packages that are installed into the \pkg{basilisk} Python instance.
#' Such core packages do not need be listed with explicit version numbers in \code{packages} (or at all, for that matter).
#' You can also request a core package without specifying the version, which will automatically be pinned for you.
#' A full list of core packages with pinned versions is provided at \code{\link{listCorePackages}}.
#'
#' The exception to the exception occurs when your requested packages are not compatible with the pinned versions of the core packages.
#' In such cases, the compatible versions of the core packages must again be explicitly listed in \code{packages}.
#'
#' @examples
#' tmploc <- file.path(tempdir(), "my_package_A")
#' setupBasiliskEnv(tmploc, c('pandas==0.25.3',
#'     "python-dateutil==2.8.1", "pytz==2019.3"))
#'
#' # No need to list versions of core packages, 
#' # or to list them at all if they are dependencies.
#' tmploc2 <- paste0(tmploc, "_alt")
#' setupBasiliskEnv(tmploc2, 'pandas')
#'
#' @seealso
#' \code{\link{listCorePackages}}, for a list of core Python packages with pinned versions.
#'
#' @export
#' @importFrom basilisk.utils getBasiliskDir isWindows installAnaconda
setupBasiliskEnv <- function(envpath, packages, conda=FALSE) {
    if (file.exists(envpath)) {
        return(FALSE)
    }

    installAnaconda() # no-ops if it's already there.

    versioned <- grepl("==", packages)
    if (!all(versioned)) {
        unversioned <- packages[!versioned]
        core.set <- listCorePackages()
        core.match <- match(unversioned, core.set$package)

        if (!any(is.na(core.match))) {
            # Core packages only need their name specified, 
            # and we will automatically use the core version.
            packages[!versioned] <- core.set$full[core.match]
        } else {
            stop(sprintf("version must be explicitly specified for '%s'", unversioned[is.na(core.match)][1]))
        }
    }

    pkg.names <- .full2pkg(packages)
    if (dup <- anyDuplicated(pkg.names)) {
        stop(sprintf("redundant listing of '%s'", pkg.names[dup]))
    }

    #############################

    # Unsetting this variable, otherwise it seems to override the python=
    # argument in virtualenv_create() (see LTLA/basilisk#1).
    old.retpy <- Sys.getenv("RETICULATE_PYTHON")
    if (old.retpy!="") {
        Sys.unsetenv("RETICULATE_PYTHON")
        on.exit(Sys.setenv(RETICULATE_PYTHON=old.retpy), add=TRUE)
    }

    old.pypath <- Sys.getenv("PYTHONPATH")
    if (old.pypath!="") {
        Sys.unsetenv("PYTHONPATH")
        on.exit(Sys.setenv(PYTHONPATH=old.pypath), add=TRUE)
    }

    if (isWindows() || conda) {
        .setup_condaenv(envpath, packages)
    } else {
        .setup_virtualenv(envpath, packages)
    }
}

#' @importFrom basilisk.utils getBasiliskDir
#' @importFrom reticulate virtualenv_create virtualenv_install virtualenv_remove virtualenv_root
.setup_virtualenv <- function(envpath, packages) {
    # Creating a virtual environment in an appropriate location.
    vdir <- dirname(envpath)
    envname <- basename(envpath)

    old.work <- Sys.getenv("WORKON_HOME")
    Sys.setenv(WORKON_HOME=vdir)
    on.exit(Sys.setenv(WORKON_HOME=old.work), add=TRUE)

    # Effective no-op if the everything is perfectly satisfied by the core
    # installation (flagged by creating an empty dir).
    py.cmd <- .get_py_cmd(getBasiliskDir())
    previous <- .basilisk_freeze(py.cmd)
    if (all(packages %in% previous)) {
        dir.create(envpath, showWarnings=FALSE)
        return(FALSE)
    }
 
    virtualenv_create(envname, python=py.cmd)
    env.cmd <- .get_py_cmd(envpath)
    virtualenv_install(envname, packages, ignore_installed=FALSE)
    updated <- .basilisk_freeze(env.cmd)

    if (any(!updated %in% c(previous, packages))) {
        added <- setdiff(updated, c(previous, packages))
        stop(sprintf("need to list dependency on '%s'", added[1]))
    } 

    TRUE 
}

#' @importFrom basilisk.utils getBasiliskDir
#' @importFrom reticulate conda_create conda_install
.setup_condaenv <- function(envpath, packages) {
    # Effective no-op if the everything is perfectly satisfied by the core
    # installation (flagged by creating an empty dir).
    py.cmd <- .get_py_cmd(getBasiliskDir())
    previous <- .basilisk_freeze(py.cmd)
    if (all(packages %in% previous)) {
        dir.create(envpath, showWarnings=FALSE)
        return(FALSE)
    }

    # This is where it gets a bit crazy. We will do two installations; one to
    # check what unlisted dependencies of the listed packages get pulled down,
    # and another to actually enforce the versions of those dependencies.
    # 
    # Unfortunately, we can't clone the base environment and just replace the
    # packages that conflict, much like a virtual environment does. Well, we
    # could, but the conda solver takes ages to run, so it is actually faster
    # to do two installations. (To be fair to conda, it actually does fail if
    # there is a version conflict, so that's got to count for something).

    conda.cmd <- file.path(getBasiliskDir(), .retrieve_conda())
    version <- sub("^Python ", "", system2(py.cmd, "--version", stdout=TRUE))

    DEPLOY <- function(PKG) {
        conda_create(envname=envpath, packages=paste0("python=", version), conda=conda.cmd)
        conda_install(envname=envpath, packages=PKG, python_version=version, conda=conda.cmd)
    }

    DEPLOY(packages)
    env.cmd <- .get_py_cmd(envpath)
    updated <- .basilisk_freeze(env.cmd)

    if (any(!updated %in% c(previous, packages))) {
        added <- setdiff(updated, c(previous, packages))
        stripped <- .full2pkg(added)
        available <- .full2pkg(previous)

        replace <- match(stripped, available)
        reattempt <- c(packages, previous[replace[!is.na(replace)]])
        unlink(envpath, recursive=TRUE)
        DEPLOY(reattempt)

        # Checking that our reattempt corrected all the unlisted dependencies.
        updated <- .basilisk_freeze(env.cmd)
        added <- setdiff(updated, c(previous, packages))
        stripped <- .full2pkg(added)
        available <- .full2pkg(previous)
        if (any(lost <- !stripped %in% available)) {
            stop(sprintf("need to list dependency on '%s'", added[lost][1]))
        }
    }

    TRUE 
}

.basilisk_freeze <- function(py.cmd) {
    # Unsetting the PYTHONPATH to avoid freezing other versions.
    old.pypath <- Sys.getenv("PYTHONPATH")
    if (old.pypath!="") {
        Sys.unsetenv("PYTHONPATH")
        on.exit(Sys.setenv(PYTHONPATH=old.pypath), add=TRUE)
    }
    system2(py.cmd, c("-m", "pip", "freeze"), stdout=TRUE)
}
