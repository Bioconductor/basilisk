#' List core packages
#'
#' List the set of core Python packages (and their version numbers) that are provided by \pkg{basilisk}.
#'
#' @details
#' This is provided for informational purposes only;
#' developers should not expect the same core packages to be present across operating systems.
#' \code{?\link{installConda}} has some more comments on the version of the conda installer used for each OS.
#'
#' @author Aaron Lun
#'
#' @return A data.frame containing the \code{full}, a versioned package string, and \code{package}, the package name.
#' 
#' @examples
#' listCorePackages()
#' 
#' @export
#' @importFrom basilisk.utils getBasiliskDir installConda getPythonBinary
listCorePackages <- function() {
    installConda()
    out <- .basilisk_freeze(getPythonBinary(getBasiliskDir()))
    data.frame(full=out, package=.full2pkg(out), stringsAsFactors=FALSE)
}

.basilisk_freeze <- function(py.cmd) {
    previous <- .coerce_env_vars()
    on.exit(.restore_env_vars(previous))
    system2(py.cmd, c("-m", "pip", "freeze"), stdout=TRUE)
}


.full2pkg <- function(packages) {
    sub("[><=]+.*", "", packages)
}
