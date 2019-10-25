#' Set up and use virtual environments
#'
#' Set up and use Python virtual environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envname String containing the name of the virtual environment to create (for \code{setupVirtualEnv}) or use (other functions).
#' @param packages Character vector containing the names of Python packages to install into the virtual environment.
#' It is strongly recommended to include version numbers in each string.
#' @param pkgpath String specifying the path to the R package installation directory, usually used in an R package installation script.
#' If \code{NULL}, it defaults to \code{\link{virtualenv_root}}.
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
#' @importFrom reticulate virtualenv_create virtualenv_install
setupVirtualEnv <- function(envname, packages, pkgpath=NULL) {
    # Unsetting this variable, otherwise it seems to override everything.
    old <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old))
    }

    pypath <- useBasilisk()

    # Creating a virtual environment in an appropriate location.
    if (!is.null(pkgpath)) {
        vdir <- file.path(pkgpath, "inst", "basilisk")
        dir.create(vdir, recursive=TRUE, showWarnings=FALSE)
        old <- Sys.getenv("WORKON_HOME")
        Sys.setenv(WORKON_HOME=vdir)
        on.exit(Sys.setenv(WORKON_HOME=old))
    }
    virtualenv_create(envname, python=pypath)

    # Choosing a package version, if we haven't done so already.
    versioned <- grepl("==", packages)
    if (!is.null(pkgpath) && !all(versioned)) {
        stop("Python package versions must be explicitly specified")
    }

    virtualenv_install(envname, packages)
}

#' @export
#' @rdname setupVirtualEnv
#' @param dry Logical scalar indicating whether only the directory should be returned without loading the virtual environment.
#' @importFrom reticulate use_virtualenv virtualenv_root
useVirtualEnv <- function(envname, pkgname=NULL, dry=FALSE) {
    old <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old))
    }

    if (!is.null(pkgname)) {
        vdir <- system.file("inst", "basilisk", package=pkgname, mustWork=TRUE)
    } else {
        vdir <- normalizePath(virtualenv_root())
    }
    vdir <- file.path(vdir, envname)

    if (!dry) {
        use_virtualenv(vdir, required=TRUE)
    }
    vdir
}
