#' Set up a \pkg{basilisk} environments
#'
#' Set up a Python virtual or conda environment (depending on the operating system)
#' for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envname String containing the name of the environment to create.
#' @param packages Character vector containing the names of Python packages to install into the environment.
#' It is required to include version numbers in each string.
#' @param pkgname String specifying the name of the R package that owns the environment.
#' @param conda Logical scalar indicating whether a conda environment should be created.
#' 
#' @return 
#' A virtual or conda environment is created containing the specified \code{packages}.
#' If \code{pkgname} is specified, the environment is located in the installation directory of \code{pkgname}.
#' Otherwise, it treats \code{envname} as a path to the desired location of the environment.
#' The function itself returns a \code{NULL} value invisibly.
#'
#' @details
#' Use of \pkg{basilisk} environments is the recommended approach for Bioconductor packages to interact with the \pkg{basilisk} Python instance.
#' This avoids version conflicts within an R session when different Bioconductor packages (or even different functions within a single package) require incompatible versions of Python packages.
#' 
#' If all of the requested packages fall into the \dQuote{core} list of packages (see \code{?\link{listCorePackages}}),
#' this function is a no-op.
#' Any attempt to use \code{envname} in \code{\link{basiliskStart}} will simply fall back to the core Anaconda instance.
#' This enables multiple client packages to use the same Python for greater efficiency with \code{\link{basiliskStart}}.
#' 
#' If \code{pkgname} is specified and the \pkg{basilisk} environment is already present with all requested packages, \code{setupBasiliskEnv} is a no-op.
#' This ensures that the function only installs the packages once at the first load during R package installation.
#'
#' We call these \pkg{basilisk} environments as the function will automatically switch between virtual and conda environments depending on the operating system.
#' MacOSX and Linux default to virtual environments to enable re-use of dependencies from the core installation, while Windows can only conda environments.
#' Developers can force the former to conda environments with \code{conda=TRUE}.
#'
#' Developers of Bioconductor packages should call \code{\link{configureBasiliskEnv}} in their \code{configure} files,
#' which will create the environments upon installation on Linux via \code{setupBasiliskEnv}.
#' For other operating systems, \code{setupBasiliskEnv} is called lazily by \code{\link{basiliskStart}} 
#' so no developer intervention is required.
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
#' @importFrom basilisk.utils getBasiliskDir
setupBasiliskEnv <- function(envname, packages, pkgname=NULL, conda=FALSE) {
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
        .setup_condaenv(envname, packages, pkgname)
    } else {
        .setup_virtualenv(envname, packages, pkgname)
    }
}

#' @importFrom basilisk.utils getBasiliskDir
#' @importFrom reticulate virtualenv_create virtualenv_install virtualenv_remove virtualenv_root
.setup_virtualenv <- function(envname, packages, pkgname) {
    # Creating a virtual environment in an appropriate location.
    if (is.null(pkgname)) {
        vdir <- dirname(envname)
        envname <- basename(envname)
    } else {
        vdir <- .choose_env_dir(pkgname)
        dir.create(vdir, recursive=TRUE, showWarnings=FALSE)
    }

    old.work <- Sys.getenv("WORKON_HOME")
    Sys.setenv(WORKON_HOME=vdir)
    on.exit(Sys.setenv(WORKON_HOME=old.work), add=TRUE)

    # Effective no-op if the environment already exists, or if everything is
    # perfectly satisfied by the core installation.
    target <- file.path(path.expand(vdir), envname)
    if (file.exists(target)) {
        return(NULL)
    }

    py.cmd <- .get_py_cmd(getBasiliskDir())
    previous <- .basilisk_freeze(py.cmd)
    if (all(packages %in% previous)) {
        return(NULL)
    }
 
    virtualenv_create(envname, python=py.cmd)
    env.cmd <- .get_py_cmd(target)
    virtualenv_install(envname, packages, ignore_installed=FALSE)
    updated <- .basilisk_freeze(env.cmd)

    if (any(!updated %in% c(previous, packages))) {
        added <- setdiff(updated, c(previous, packages))
        stop(sprintf("need to list dependency on '%s'", added[1]))
    } 

    NULL
}

#' @importFrom basilisk.utils getBasiliskDir
#' @importFrom reticulate conda_create conda_install
.setup_condaenv <- function(envname, packages, pkgname) {
    if (is.null(pkgname)) {
        envdir <- envname
    } else {
        vdir <- .choose_env_dir(pkgname)
        dir.create(vdir, recursive=TRUE, showWarnings=FALSE)
        envdir <- file.path(vdir, envname)
    }

    # Effective no-op if the environment already exists, or if everything is
    # perfectly satisfied by the core installation.
    if (file.exists(envdir)) {
        return(NULL)
    }

    py.cmd <- .get_py_cmd(getBasiliskDir())
    previous <- .basilisk_freeze(py.cmd)
    if (all(packages %in% previous)) {
        return(NULL)
    }

    # This is where it gets a bit crazy. We will do two installations; one to
    # check what unlisted dependencies of the listed packages get pulled down,
    # and another to actually enforce the versions of those dependencies.
    conda.cmd <- file.path(getBasiliskDir(), .retrieve_conda())
    version <- sub("^Python ", "", system2(py.cmd, "--version", stdout=TRUE))

    DEPLOY <- function(PKG) {
        conda_create(envname=envdir, packages=paste0("python=", version), conda=conda.cmd)
        conda_install(envname=envdir, packages=PKG, python_version=version, conda=conda.cmd)
    }

    DEPLOY(packages)
    env.cmd <- .get_py_cmd(envdir)
    updated <- .basilisk_freeze(env.cmd)

    if (any(!updated %in% c(previous, packages))) {
        added <- setdiff(updated, c(previous, packages))
        stripped <- .full2pkg(added)
        available <- .full2pkg(previous)

        replace <- match(stripped, available)
        reattempt <- c(packages, previous[replace[!is.na(replace)]])
        unlink(envdir, recursive=TRUE)
        DEPLOY(reattempt)

        updated <- .basilisk_freeze(env.cmd)
        added <- setdiff(updated, c(previous, packages))
        stripped <- .full2pkg(added)
        available <- .full2pkg(previous)
        if (any(lost <- !stripped %in% available)) {
            stop(sprintf("need to list dependency on '%s'", added[lost][1]))
        }
    }

    NULL
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
