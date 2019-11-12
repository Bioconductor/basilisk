#' Set up and use virtual environments
#'
#' Set up and use Python virtual environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envname String containing the name of the virtual environment to create (for \code{setupVirtualEnv}) or use (other functions).
#' @param packages Character vector containing the names of Python packages to install into the virtual environment.
#' It is required to include version numbers in each string.
#' @param pkgname String specifying the name of the R package that owns the virtual environment.
#' 
#' @return 
#' A virtual environment is created in the installation directory of \code{pkgname} if specified, 
#' or at the default location for virtual environments otherwise (see \code{?\link{virtualenv_root}}).
#' The function returns a \code{NULL} value, invisibly.
#'
#' @details
#' Use of virtual environments is the recommended approach for Bioconductor packages to interact with the \pkg{basilisk} Python instance.
#' This avoids version conflicts within an R session when different Bioconductor packages (or even different functions within a single package) require incompatible versions of Python packages.
#'
#' Developers of Bioconductor packages should call \code{setupVirtualEnv} with an appropriate \code{pkgname} in an \code{.onLoad} function.
#' This will create the virtual environment and install the relevant Python packages upon R package installation.
#' The \pkg{son.of.basilisk} example in the \code{inst} directory of \pkg{basilisk} can be used as an example.
#'
#' If \code{pkgname} is specified and the virtual environment is already present, \code{setupVirtualEnv} is a no-op.
#' This ensures that the function only installs once at the first load during R package installation.
#'
#' @section Dealing with versioning: 
#' Pinned version numbers must be present for all requested packages in \code{packages}.
#' This improved predictability makes debugging much easier when the R package is installed and executed on different systems.
#' Explicit versions are also mandatory for all dependencies of requested packages,
#' which is again necessary to ensure that the same versions are deployed across systems.
#'
#' The only exception to the above rule is for \dQuote{core} packages that are installed into the \pkg{basilisk} Python instance.
#' If your requested packages depend on these core packages, \code{setupVirtualEnv} will automatically install them without requiring them to be listed with explicit version numbers in \code{packages}.
#' You can also request a core package without specifying the version, which will automatically be pinned for you.
#' A full list of core packages with pinned versions is provided at \code{\link{listCorePackages}}.
#'
#' The exception to the exception occurs when your requested packages are not compatible with the pinned versions of the core packages.
#' In such cases, the compatible versions of the core packages must again be explicitly listed in \code{packages}.
#'
#' @examples
#' setupVirtualEnv('my_package_A', c('pandas==0.25.3',
#'     "python-dateutil==2.8.1", "pytz==2019.3"))
#' useVirtualEnv("my_package_A")
#' X <- reticulate::import("pandas")
#' X$`__version__`
#'
#' # No need to list versions of core packages, 
#' # or to list them at all if they are dependencies.
#' setupVirtualEnv('my_package_A_alt', 'pandas')
#' 
#' @seealso
#' \code{\link{listCorePackages}}, for a list of core Python packages with pinned versions.
#'
#' @export
#' @importFrom reticulate virtualenv_create virtualenv_install virtualenv_remove
#' virtualenv_root
setupVirtualEnv <- function(envname, packages, pkgname=NULL) {
    if (!is.null(pkgname)) {
        instdir <- system.file(package=pkgname)
        if (basename(instdir)!=pkgname) {
            # Avoid weirdness due to devtools::document().
            return(NULL)
        }
        if (file.exists(file.path(instdir, "basilisk", envname))) {
            return(NULL)
        }
    }

    core.data <- listCorePackages()
    core.full <- core.data$full
    core.names <- core.data$name

    versioned <- grepl("==", packages)
    if (!all(versioned)) {
        unversioned <- packages[!versioned]
        core.match <- match(unversioned, core.names)
        if (!any(is.na(core.match))) {
            # Core packages only need their name specified, 
            # and we will automatically use the core version.
            packages[!versioned] <- core.full[core.match]
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

    # Use environment variable for testing purposes only;
    # this should not be exposed to the clients.
    py.cmd <- Sys.getenv("BASILISK_TEST_PYTHON", useBasilisk())

    #############################

    # Creating a virtual environment in an appropriate location.
    if (!is.null(pkgname)) {
        vdir <- file.path(instdir, "basilisk")
        dir.create(vdir, recursive=TRUE, showWarnings=FALSE)
        old.work <- Sys.getenv("WORKON_HOME")
        Sys.setenv(WORKON_HOME=vdir)
        on.exit(Sys.setenv(WORKON_HOME=old.work), add=TRUE)
    }

    target <- file.path(path.expand(virtualenv_root()), envname)
    if (file.exists(target)) {
        unlink(target, recursive=TRUE)
    }

    virtualenv_create(envname, python=py.cmd)
    env.cmd <- .get_py_cmd(target)

    # Code only reaches this point if we're creating the common basilisk environment.
    if (length(packages)==0L) {
        return(invisible(NULL))
    }

    #############################
    # ROUND 1: Seeing what the incoming packages need. We rely on pip to tell us the
    # identity of the dependencies for the requested version of each package. It also 
    # smoothly handles 'extras' and 'python_version', which would require manual parsing 
    # if we did a GET to PyPi to determine the dependencies. (And the GET itself would
    # require a dependency on httr, which is undesirable for faster loads.) We will 
    # delete this installation, but the downloads are cached, so little time is wasted.

    previous <- .basilisk_freeze(env.cmd)
    virtualenv_install(envname, packages, ignore_installed=FALSE)
    updated <- .basilisk_freeze(env.cmd)

    # Identifying the implicitly added packages. 
    implicit.added <- setdiff(updated, previous)

    virtualenv_remove(envname, confirm=FALSE) # Removing Round 1 to start from a fresh installation.

    #############################
    # Figuring out if any of the newly downloaded packages are core packages.
    # If so, we install them to the base installation if we have access;
    # otherwise, we add it to our virtual environment.

    added.names <- .full2pkg(implicit.added)
    clean.venv <- TRUE 
    if (any(overlaps <- core.names %in% added.names)) {
        to.install <- core.names[overlaps]

        # Adding dependencies as well, to make sure those don't slip through the net
        # if the latest version of a core dependency has different dependencies altogether
        # compared to a pinned version of the dependency.
        stuff <- read.delim(system.file("core_deps", package="basilisk"), header=FALSE)
        to.install <- c(to.install, stuff[stuff[,1] %in% to.install,2])
        to.install <- unique(to.install)
        to.install <- core.full[core.names %in% to.install]

        if (file.access(system.file(package="basilisk"), 2)==0L) {
            .basilisk_install(to.install, py.cmd=py.cmd)
            virtualenv_create(envname, python=py.cmd) 
        } else {
            virtualenv_create(envname, python=py.cmd) 
            virtualenv_install(envname, to.install, ignore_installed=FALSE)
            clean.venv <- FALSE
        }
    } else {
        virtualenv_create(envname, python=py.cmd) 
    }

    #############################
    # ROUND 2: Trying again after lazy installation of the core packages,
    # to see if the dependencies are now satisfied. We rely on pip again
    # to tell us (empirically) whether the dependencies are satisfied or 
    # if an upgrade is necessary.

    previous <- .basilisk_freeze(env.cmd)
    virtualenv_install(envname, packages, ignore_installed=FALSE)
    updated <- .basilisk_freeze(env.cmd)

    if (any(!updated %in% c(previous, packages))) {
        added <- setdiff(updated, c(previous, packages))
        stop(sprintf("need to list dependency on '%s'", added[1]))
    } else if (identical(previous, updated) && clean.venv) {
        # If everything is perfectly satisfied by the core installation, we
        # remove the venv and make a symlink. 
        virtualenv_remove(envname, confirm=FALSE) 
        file.symlink(.get_common_env(), target)
    }

    invisible(NULL)
}

.basilisk_install <- function(packages, py.cmd=useBasilisk()) {
    system2(py.cmd, c("-m", "pip", "install", packages))
}

.basilisk_freeze <- function(py.cmd) {
    system2(py.cmd, c("-m", "pip", "freeze"), stdout=TRUE)
}
