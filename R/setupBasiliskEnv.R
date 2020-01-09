#' Set up a \pkg{basilisk} environments
#'
#' Set up a Python virtual or conda environment (depending on the operating system)
#' for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envname String containing the name of the environment to create.
#' @param packages Character vector containing the names of Python packages to install into the environment.
#' It is required to include version numbers in each string.
#' @param pkgname String specifying the name of the R package that owns the environment.
#' 
#' @return 
#' A virtual or conda environment is created in the installation directory of \code{pkgname} if specified, 
#' or at the default location for virtual environments otherwise (see \code{?\link{virtualenv_root}}).
#' The function returns a \code{NULL} value, invisibly.
#'
#' @details
#' Use of \pkg{basilisk} environments is the recommended approach for Bioconductor packages to interact with the \pkg{basilisk} Python instance.
#' This avoids version conflicts within an R session when different Bioconductor packages (or even different functions within a single package) require incompatible versions of Python packages.
#' We call these \pkg{basilisk} environments as we will automatically switch between virtual and conda environments depending on the operating system.
#'
#' Developers of Bioconductor packages should call \code{setupBasiliskEnv} with an appropriate \code{pkgname} in an \code{.onLoad} function.
#' This will create the \pkg{basilisk} environment and install the relevant Python packages upon R package installation.
#' The \pkg{son.of.basilisk} example in the \code{inst} directory of \pkg{basilisk} can be used as an example.
#'
#' If all of the requested packages fall into the \dQuote{core} list of packages (see \code{?\link{listCorePackages}}),
#' a link is created to a common environment in the \pkg{basilisk} installation directory.
#' This enables multiple client packages to use the same environment for greater efficiency with \code{\link{basiliskStart}}.
#' 
#' If \code{pkgname} is specified and the \pkg{basilisk} environment is already present with all requested packages, \code{setupBasiliskEnv} is a no-op.
#' This ensures that the function only installs the packages once at the first load during R package installation.
#'
#' @section Dealing with versioning: 
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
#' ##################################################
#' # Creating virtualenvs in a temporary directory to 
#' # avoid polluting the user's WORKON_HOME.
#' tmploc <- file.path(tempdir(), "basilisk")
#' dir.create(tmploc)
#' old <- Sys.getenv("WORKON_HOME")
#' Sys.setenv(WORKON_HOME=tmploc)
#' ##################################################
#' 
#' setupBasiliskEnv('my_package_A', c('pandas==0.25.3',
#'     "python-dateutil==2.8.1", "pytz==2019.3"))
#' useBasiliskEnv("my_package_A")
#' X <- reticulate::import("pandas")
#' X$`__version__`
#'
#' # No need to list versions of core packages, 
#' # or to list them at all if they are dependencies.
#' setupBasiliskEnv('my_package_A_alt', 'pandas')
#'
#' ##################################################
#' # Restoring the old WORKON_HOME.
#' Sys.setenv(WORKON_HOME=old)
#' ##################################################
#' @seealso
#' \code{\link{listCorePackages}}, for a list of core Python packages with pinned versions.
#'
#' @export
#' @importFrom utils read.delim
setupBasiliskEnv <- function(envname, packages, pkgname=NULL, use.conda=FALSE) {
    # This clause solely exists to avoid weirdness due to devtools::document().
    if (!is.null(pkgname) && basename(system.file(package=pkgname))!=pkgname) {
        return(invisible(NULL))
    }

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

    if (.is_windows() || use.conda) {
        .setup_condaenv(envname, packages, pkgname)
    } else {
        .setup_virtualenv(envname, packages, pkgname)
    }
}

#' @importFrom reticulate virtualenv_create virtualenv_install virtualenv_remove virtualenv_root
.setup_virtualenv <- function(envname, packages, pkgname) {
    # Use environment variable for testing purposes only;
    # this should not be exposed to the clients.
    base.dir <- Sys.getenv("BASILISK_TEST_CORE", .get_basilisk_dir())
    py.cmd <- .get_py_cmd(base.dir)

    # Creating a virtual environment in an appropriate location.
    if (!is.null(pkgname)) {
        vdir <- file.path(system.file(package=pkgname), .env_dir)
        dir.create(vdir, recursive=TRUE, showWarnings=FALSE)
        old.work <- Sys.getenv("WORKON_HOME")
        Sys.setenv(WORKON_HOME=vdir)
        on.exit(Sys.setenv(WORKON_HOME=old.work), add=TRUE)
    }

    target <- file.path(path.expand(virtualenv_root()), envname)

    # Effective no-op if virtualenv already exists.
    if (file.exists(target)) {
        return(NULL)
    }

    # If everything is perfectly satisfied by the core installation, we just
    # make a symlink to the common virtual environment.
    available <- .basilisk_freeze(py.cmd)
    if (all(packages %in% available)) {
        file.symlink(.get_common_env(), target)
        return(NULL)
    }
 
    virtualenv_create(envname, python=py.cmd)

    # Code only reaches this point if we're creating the common basilisk environment.
    if (length(packages)==0L) {
        return(NULL)
    }

    env.cmd <- .get_py_cmd(target)
    previous <- .basilisk_freeze(env.cmd)
    virtualenv_install(envname, packages, ignore_installed=FALSE)
    updated <- .basilisk_freeze(env.cmd)

    if (any(!updated %in% c(previous, packages))) {
        added <- setdiff(updated, c(previous, packages))
        stop(sprintf("need to list dependency on '%s'", added[1]))
    } 

    NULL
}

#' @importFrom reticulate conda_create 
.setup_condaenv <- function(envname, packages, pkgname) {
    if (!is.null(pkgname)) { 
        vdir <- file.path(system.file(package=pkgname), .env_dir)
        dir.create(vdir, recursive=TRUE, showWarnings=FALSE)
        envdir <- file.path(vdir, envname)
    } else {
        envdir <- file.path(getwd(), envname)
    }

    base.dir <- Sys.getenv("BASILISK_TEST_CORE", .get_basilisk_dir())
    py.cmd <- .get_py_cmd(base.dir)

    # If everything is perfectly satisfied by the core installation, we just
    # make a symlink to that conda installation. 
    previous <- .basilisk_freeze(py.cmd)
    if (all(packages %in% previous)) {
        file.symlink(base.dir, target)
        return(NULL)
    }

    suffix <- if (.is_windows()) {
        "Scripts/conda.exe"
    } else {
        "bin/conda"
    }

    # This is where it gets a bit crazy. We will do two installations; one to
    # check what unlisted dependencies of the listed packages get pulled down,
    # and another to actually enforce the versions of those dependencies.
    conda_create(envname=envdir,
        conda=file.path(.get_basilisk_dir(), suffix),
        packages=sub("==", "=", packages)
    )

    env.cmd <- .get_py_cmd(envdir)
    updated <- .basilisk_freeze(env.cmd)

    if (any(!updated %in% c(previous, packages))) {
        added <- setdiff(updated, c(previous, packages))
        stripped <- .full2pkg(added)
        available <- .full2pkg(previous)

        replace <- match(stripped, available)
        if (any(lost <- is.na(replace))) {
            stop(sprintf("need to list dependency on '%s'", added[lost][1]))
        }

        reattempt <- c(packages, previous[replace])
        unlink(envdir, recursive=TRUE)
        conda_create(envname=envdir,
            conda=file.path(.get_basilisk_dir(), suffix),
            packages=sub("==", "=", packages)
        )
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
