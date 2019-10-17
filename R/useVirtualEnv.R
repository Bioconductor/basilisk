#' Set up and use virtual environments
#'
#' Set up and use Python virtual environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envname String containing the name of the virtual environment to create (for \code{setupVirtualEnv}) or use (other functions).
#' @param packages Character vector containing the names of Python packages to install into the virtual environment.
#' It is strongly recommended to include version numbers in each name.
#' @param pkgpath String specifying the path to the R package installation directory, usually used in an R package installation script.
#' @param pkgname String specifying the package name, if the function is used inside an R package.
#' @param FUN A function to execute in the context of the virtual environment.
#' Any calls to non-base functions within \code{FUN} should be prefixed with the namespace.
#' @param ... Further arguments to pass to \code{FUN} or to \code{\link{r}}.
#' 
#' @return 
#' \code{setupVirtualEnv} will return the output of \code{\link{virtualenv_install}}.
#'
#' \code{useVirtualEnv} will return the output of \code{\link{use_virtualenv}}.
#'
#' \code{callVirtualEnv} will return the output of \code{FUN}.
#'
#' @details
#' Use of virtual environments is the recommended approach for Bioconductor packages to interact with the \pkg{basilisk} Python instance.
#' This avoids conflicts when different Bioconductor packages require incompatible versions of Python packages.
#'
#' Developers of Bioconductor packages should call \code{setupVirtualEnv} with an appropriate \code{pkgpath} in an \code{configure} script (usually \code{${R_PACKAGBE_DIR}}),
#' to install the relevant Python packages during R package installation process.
#' Then, functions can simply call \code{callVirtualEnv} to take advantage of the installed packages.
#' The \pkg{son.of.basilisk} example in the \code{inst} directory of \pkg{basilisk} can be used as an example.
#'
#' @section Running Python code:
#' The \code{callVirtualEnv} function allows multiple virtual environments to be used by different R packages in a single R session.
#' This is done by creating an isolated R process with the \pkg{callr} package and loading the requested virtual environment in that session.
#' Thus, we avoid a limitation of \pkg{reticulate} where an R session is irrevocably tied to a version of Python and/or modules.
#'
#' Writers of \code{FUN} can assume that the virtual environment specified by \code{envname} has already been loaded.
#' This means that \code{\link{import}} and related functions will work correctly, though developers should namespace the calls with \code{reticulate::}.
#' The same namespacing process applies for any other non-base functions that are used within \code{FUN}.
#'
#' @section Python package version control:
#' When calling \code{setupVirtualEnv} during R package installation, it is strongly recommended to have version numbers in \code{packages}.
#' This makes debugging much easier when the R package is installed and executed on different systems.
#' If version numbers are not provided for any package, we will use a version number selected by \code{\link{findVersionUpTo}}. 
#'
#' The nature of Python package management means that conflicts can arise if two Python packages have mutually incompatible dependencies.
#' This is best handled by setting up separate virtual environments for these packages and calling them separately.
#' 
#' @author Aaron Lun
#' 
#' @examples
#' setupVirtualEnv('my_package_A', 'pandas==0.25.1')
#' setupVirtualEnv('my_package_B', 'pandas==0.24.0')
#'
#' callVirtualEnv("my_package_A", FUN=reticulate::py_config)
#' callVirtualEnv("my_package_B", FUN=reticulate::py_config)
#'
#' @export
#' @importFrom reticulate virtualenv_create virtualenv_install
setupVirtualEnv <- function(envname, packages, pkgpath=NULL) {
    # Unsetting this variable, otherwise it seems to override everything.
    old <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    on.exit(Sys.setenv(RETICULATE_PYTHON=old))

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
    unversioned <- grep("==", packages, invert=TRUE)
    for (i in unversioned) {
        v <- findVersionUpTo(packages[i], DATE_LIMIT)
        packages[i] <- paste0(packages[i], "==", v)
    }

    virtualenv_install(envname, packages)
}

#' @export
#' @rdname setupVirtualEnv 
#' @importFrom reticulate use_virtualenv virtualenv_root
useVirtualEnv <- function(envname, pkgname=NULL) {
    old <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    on.exit(Sys.setenv(RETICULATE_PYTHON=old))

    if (!is.null(pkgname)) {
        vdir <- system.file("inst", "basilisk", package=pkgname, mustWork=TRUE)
    } else {
        vdir <- virtualenv_root() 
    }
    use_virtualenv(file.path(vdir, envname), required=TRUE)
}

#' @export
#' @rdname setupVirtualEnv 
#' @importFrom callr r
callVirtualEnv <- function(envname, FUN, ..., pkgname=NULL) {
    r(func=function(envname, pkgname, FUN, ...) {
        Sys.unsetenv("RETICULATE_PYTHON")
        basilisk::useVirtualEnv(envname, pkgname)
        FUN(...)
    }, args=list(envname=envname, pkgname=pkgname, FUN=FUN, ...))
}
