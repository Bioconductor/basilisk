#' Configure client environments
#'
#' Configure the \pkg{basilisk} environments in the \code{configure} file of client packages.
#'
#' @param src String containing path to a R source file that defines one or more \linkS4class{BasiliskEnvironment} objects.
#'
#' @return One or more \pkg{basilisk} environments are created 
#' corresponding to the \linkS4class{BasiliskEnvironment} objects in \code{src}.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @details 
#' This function is designed to be called in the \code{configure} file of client packages,
#' triggering the construction of \pkg{basilisk} environments during package installation.
#' It will only run if the \code{BASILISK_USE_SYSTEM_DIR} environment variable is set to \code{"1"}.
#'
#' We take a source file as input to avoid duplicated definitions of the \linkS4class{BasiliskEnvironment}s.
#' These objects are used in \code{\link{basiliskStart}} in the body of the package, so they naturally belong in \code{R/}; 
#' we then ask \code{configure} to pull out that file (named \code{"basilisk.R"} by convention) 
#' to create these objects during installation.
#'
#' The source file in \code{src} should be executable on its own, 
#' i.e., you can \code{\link{source}} it without loading any other packages (beside \pkg{basilisk}, obviously).
#' Non-\linkS4class{BasiliskEnvironment} objects can be created but are simply ignored in this function.
#'
#' @examples
#' \dontrun{
#' configureBasiliskEnv()
#' }
#' 
#' @seealso
#' \code{\link{setupBasiliskEnv}}, which does the heavy lifting of setting up the environments.
#'
#' @export
#' @importFrom methods is
#' @importFrom basilisk.utils useSystemDir getSystemDir dir.create2 setVariable
configureBasiliskEnv <- function(src="R/basilisk.R") {
    if (!useSystemDir()) {
        return(invisible(NULL))
    }

    envir <- new.env()
    eval(parse(file=src), envir=envir)

    # Only retaining those that are Basilisk environments.
    env.vars <- ls(envir)

    keep <- vapply(env.vars, function(nm) {
        current <- get(nm, envir=envir, inherits=FALSE)
        is(current, "BasiliskEnvironment")
    }, TRUE)

    if (!any(keep)) {
        return(invisible(NULL))
    }

    env.vars <- env.vars[keep]
    tmp <- get(env.vars[1], envir=envir, inherits=FALSE)
    pkgname <- .getPkgName(tmp)
    envdir <- .get_env_system_dir(pkgname)

    # Setting this so that conda doesn't try to dump the requested packages
    # into the (conceptually, if not actually, read-only) base installation.
    old <- setVariable("CONDA_PKGS_DIRS", NA)
    on.exit(setVariable("CONDA_PKGS_DIRS", old))

    new.pkg.dir <- file.path(envdir, "_pkgs")
    dir.create2(new.pkg.dir)
    Sys.setenv(CONDA_PKGS_DIRS=normalizePath(new.pkg.dir))

    # Actually creating the environments.
    for (nm in env.vars) {
        current <- get(nm, envir=envir, inherits=FALSE)
        setupBasiliskEnv(
            envpath=file.path(envdir, .getEnvName(current)),
            packages=.getPackages(current),
            channels=.getChannels(current),
            pip=.getPipPackages(current),
            paths=file.path("inst", .getPipPaths(current))
        )
    }

    invisible(NULL)
}

#' @importFrom basilisk.utils getSystemDir
.get_env_system_dir <- function(pkgname, installed=FALSE) {
    vdir <- getSystemDir(pkgname, installed=installed)
    file.path(vdir, "basilisk")
}
