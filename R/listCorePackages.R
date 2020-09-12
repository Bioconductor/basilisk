#' List packages
#'
#' List the set of Python packages (and their version numbers) that are installed in an conda environment.
#'
#' @inheritParams basiliskStart
#'
#' @details
#' This is provided for informational purposes only;
#' developers should not expect the same core packages to be present across operating systems.
#' \code{?\link{installConda}} has some more comments on the version of the conda installer used for each operating system.
#'
#' @author Aaron Lun
#'
#' @return A data.frame containing the \code{full}, a versioned package string, and \code{package}, the package name.
#' 
#' @examples
#' listPackages()
#' 
#' @export
listPackages <- function(env=NULL) {
    envpath <- .obtainEnvironmentPath(env)
    out <- .basilisk_freeze(envpath)
    data.frame(full=out, package=.full2pkg(out), stringsAsFactors=FALSE)
}

#' @importFrom basilisk.utils getPythonBinary
.basilisk_freeze <- function(envpath) {
    previous <- .activateEnvironment(envpath)
    on.exit(.deactivateEnvironment(previous))
    system2(getPythonBinary(envpath), c("-m", "pip", "list", "--format", "freeze"), stdout=TRUE)
}

.full2pkg <- function(packages) {
    sub("[><=]+.*", "", packages)
}

#' @export
#' @rdname listPackages
listCorePackages <- function() {
    .Deprecated(new="listPackages")
    listPackages()
}
