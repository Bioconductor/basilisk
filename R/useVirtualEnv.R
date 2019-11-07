#' Set up and use virtual environments
#'
#' Set up and use Python virtual environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envname String containing the name of the virtual environment to create (for \code{setupVirtualEnv}) or use (other functions).
#' @param packages Character vector containing the names of Python packages to install into the virtual environment.
#' It is strongly recommended to include version numbers in each string.
#' @param pkgpath String specifying the path to the R package installation directory, usually used in an R package installation script.
#' If \code{NULL}, it defaults to \code{\link{virtualenv_root}}.
#' @param ignore_installed Logical scalar indicating whether \dQuote{core} packages installed with \pkg{basilisk} should be ignored when constructing the virtual environment.
#' @param pkgname String specifying the package name, if the function is used inside an R package.
#' 
#' @return 
#' \code{setupVirtualEnv} will create a virtual environment - at the designated location if \code{pkgpath} is specified, or at the default location for virtual environments otherwise.
#' It returns a character vector containing \code{packages}.
#'
#' \code{useVirtualEnv} will load the specified virtual environment into the R session.
#' It returns a string specifying the path to the virtual environment.
#' If \code{dry=TRUE}, the character vector is returned without loading the virtual environment.
#'
#' @details
#' Use of virtual environments is the recommended approach for Bioconductor packages to interact with the \pkg{basilisk} Python instance.
#' This avoids conflicts when different Bioconductor packages (or even different functions within a single package) require incompatible versions of Python packages.
#'
#' Developers of Bioconductor packages should call \code{setupVirtualEnv} with an appropriate \code{pkgpath} in an \code{configure} script (usually \code{${R_PACKAGBE_DIR}}),
#' to install the relevant Python packages during R package installation process.
#' The \pkg{son.of.basilisk} example in the \code{inst} directory of \pkg{basilisk} can be used as an example.
#'
#' When calling \code{setupVirtualEnv} during R package installation, version numbers must be present in \code{packages}.
#' This makes debugging much easier when the R package is installed and executed on different systems.
#' Even outside of package contexts, it is strongly recommended to set the version number to ensure reproducibility.
#'
#' @section Reusing core packages:
#' By default, \code{ignore_installed=FALSE} to avoid redundant installations of the same Python packages and reduce the size of the installed R package.
#' This differs from the default in \code{\link{virtualenv_install}} and may need to be set to \code{TRUE} if there are version conflicts with any \dQuote{core} Python packages.
#'
#' Core Python packages are installed along with \pkg{basilisk} and refer to common Python infrastructure that is likely required by many packages.
#' Currently, the core packages are:
#' \itemize{
#' \item pandas, 0.25.1
#' \item scipy, 1.17.3
#' \item numpy, 1.3.1
#' \item matplotlib, 3.1.1
#' }
#' Please contact the \pkg{basilisk} maintainers if you feel that a package should be added to this list.
#'
#' @section Wrangling the PYTHONPATH:
#' A side-effect of \code{useVirtualEnv} with \code{dry=FALSE} is that the \code{"PYTHONPATH"} environment variable is unset for the duration of the R session
#' (or \pkg{basilisk} process, depending on the back-end chosen by \code{\link{basiliskStart}}).
#' This is a deliberate choice to avoid compromising the version guarantees if \code{\link{import}} is allowed to search other locations beyond the virtual environment.
#'
#' @author Aaron Lun
#' 
#' @examples
#' setupVirtualEnv('my_package_A', 'pandas==0.25.1')
#' useVirtualEnv("my_package_A")
#' X <- reticulate::import("pandas")
#' X
#' @seealso
#' \code{\link{basiliskStart}}, for how these virtual environments should be used.
#'
#' @export
#' @importFrom reticulate virtualenv_create virtualenv_install virtualenv_remove
#' virtualenv_root
setupVirtualEnv <- function(envname, packages, pkgpath=NULL, ignore_installed=FALSE) {
    versioned <- grepl("==", packages)
    if (!all(versioned)) {
        stop("Python package versions must be explicitly specified")
    }

    # Unsetting this variable, otherwise it seems to override the python=
    # argument in virtualenv_create() (see LTLA/basilisk#1).
    old.retpy <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old.retpy!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old.retpy))
    }

    py.cmd <- useBasilisk()

    # Creating a virtual environment in an appropriate location.
    if (!is.null(pkgpath)) {
        vdir <- file.path(pkgpath, "inst", "basilisk")
        dir.create(vdir, recursive=TRUE, showWarnings=FALSE)
        old.work <- Sys.getenv("WORKON_HOME")
        Sys.setenv(WORKON_HOME=vdir)
        on.exit(Sys.setenv(WORKON_HOME=old.work), add=TRUE)
    }

    # ROUND 1: Seeing what the incoming packages need.
    virtualenv_create(envname, python=py.cmd)
    env.cmd <- file.path(normalizePath(virtualenv_root()), envname, "bin", "python3")

    previous <- system2(env.cmd, c("-m", "pip", "freeze"), stdout=TRUE)
    virtualenv_install(envname, packages, ignore_installed=ignore_installed)
    updated <- system2(env.cmd, c("-m", "pip", "freeze"), stdout=TRUE)

    if (identical(sort(union(previous, packages)), sort(updated))) {
        # If all newly added packages are accounted for, we finish up.
        return(NULL)
    }

    # Figuring out if any of the newly downloaded packages are core packages.
    core.pkgs <- readLines(system.file("core_list", package="basilisk"))
    added <- setdiff(updated, c(previous, packages))
    core.names <- .full2pkg(core.pkgs)
    added.names <- .full2pkg(added)

    if (any(unlisted.noncore <- !added.names %in% core.names)) {
        stop(sprintf("need to list dependency on '%s'", added[unlisted.noncore][1]))
    }
    overlaps <- core.names %in% added.names # no error _and_ non-empty 'added' implies that we have at least one TRUE. 
    system2(py.cmd, c("-m", "pip", "install", core.pkgs[overlaps]))

    # ROUND 2: Trying again after lazy installation of the core packages.
    virtualenv_remove(envname, confirm=FALSE)
    virtualenv_create(envname, python=py.cmd) # Removing round 1 packages to potentially allow use of new core packages.

    previous <- system2(env.cmd, c("-m", "pip", "freeze"), stdout=TRUE)
    virtualenv_install(envname, packages, ignore_installed=ignore_installed)
    updated <- system2(env.cmd, c("-m", "pip", "freeze"), stdout=TRUE)

    if (identical(sort(union(previous, packages)), sort(updated))) {
        return(NULL)
    } else {
        added <- setdiff(updated, c(previous, packages))
        stop(sprintf("need to list dependency on '%s'", added[1]))
    }
}

.full2pkg <- function(packages) {
    sub("[><=]+.*", "", packages)
}

#' @export
#' @rdname setupVirtualEnv
#' @param dry Logical scalar indicating whether only the directory should be returned without loading the virtual environment.
#' @param required Logical scalar indicating whether an error should be raised if the requested virtual environment cannot be found.
#' @importFrom reticulate use_virtualenv virtualenv_root
useVirtualEnv <- function(envname, pkgname=NULL, dry=FALSE, required=TRUE) {
    old.retpy <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old.retpy!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old.retpy))
    }

    if (!is.null(pkgname)) {
        vdir <- system.file("inst", "basilisk", package=pkgname, mustWork=TRUE)
    } else {
        vdir <- normalizePath(virtualenv_root())
    }
    vdir <- file.path(vdir, envname)

    if (!dry) {
        # Don't even try to be nice and add an on.exit() clause to protect the
        # global session. This is deliberate; if we're using a virtual
        # environment, and someone tries to import package in the global session,
        # and Python looks somewhere else other than our virtual environment via
        # the PYTHONPATH, we can get the wrong package loaded. 
        Sys.unsetenv("PYTHONPATH")
        use_virtualenv(vdir, required=required)
    }
    vdir
}
