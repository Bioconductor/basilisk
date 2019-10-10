#' Set up and use virtual environments
#'
#' Set up and use virtual environments for isolated execution of Python code.
#' 
#' @param envname String containing the name of the virtual environment to create (for \code{setupVirtualEnv}) or use (other functions).
#' For package developers, this should be the name of the package.
#' @param packages Character vector containing the names of Python packages to install into the virtual environment.
#' This is strongly recommended to have version numbers.
#' @param pkgname String specifying the package name 
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
#' Developers of Bioconductor packages should call \code{setupVirtualEnv} with an appropriate \code{pkgname} in an \code{configure} script,
#' to install the relevant Python packages during R package installation process.
#' Then, functions can simply call \code{callVirtualEnv} to take advantage of the installed packages.
#'
#' @section Running Python code:
#' The \code{callVirtualEnv} function allows multiple virtual environments to be used by different R packages in a single R session.
#' This avoids a limitation of \pkg{reticulate} where an R session is irrevocably tied to a version of Python and/or modules.
#'
#' Writers of \code{FUN} can assume that the virtual environment specified by \code{envname} has already been loaded.
#' Thus, \code{\link{import}} and related functions will work correctly (though one should prefix them with \code{reticulate::}).
#'
#' @section Python package version control:
#' When calling \code{setupVirtualEnv}, it is strongly recommended to have version numbers in \code{packages}.
#' If version numbers are not provided, we will use those defined by \code{\link{findVersionUpTo}}. 
#'
#' The nature of Python package management means that problems can potentially arise if two Python packages have mutually incompatible dependencies.
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
setupVirtualEnv <- function(envname, packages, pkgname=NULL) {
    useBiocPython()

    # Creating a virtual environment in an appropriate location.
    if (!is.null(pkgname)) {
        vdir <- .get_basilisk_envdir(pkgname)
        dir.create(vdir)
        old <- Sys.getenv("WORKON_HOME")
        Sys.setenv(WORKON_HOME=vdir)
        on.exit(Sys.setenv(WORKON_HOME=old))
    }
    virtualenv_create(envname)

    # Choosing a package version, if we haven't done so already.
    unversioned <- grep("==", packages, invert=TRUE)
    for (i in unversioned) {
        v <- findVersionUpTo(packages[i], DATE_LIMIT)
        packages[i] <- paste0(packages[i], "==", v)
    }

    virtualenv_install(envname, packages)
}

.get_basilisk_envdir <- function(pkgname) {
    instdir <- system.file("inst", package=pkgname, mustWork=TRUE)
    file.path(instdir, "basilisk")
}

#' @export
#' @importFrom reticulate use_virtualenv virtualenv_root
useVirtualEnv <- function(envname, pkgname=NULL) {
    if (!is.null(pkgname)) {
        vdir <- .get_basilisk_envdir(pkgname)
    } else {
        vdir <- virtualenv_root() 
    }
    use_virtualenv(file.path(vdir, envname), required=TRUE)
}

#' @export
#' @importFrom callr r
callVirtualEnv <- function(envname, FUN, ...) {
    r(func=function(envname, FUN, ...) {
        basilisk::useVirtualEnv(envname)
        FUN(...)
    }, args=list(envname=envname, FUN=FUN, ...))
}
