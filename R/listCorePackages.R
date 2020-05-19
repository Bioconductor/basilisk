#' List core packages
#'
#' List the set of core Python packages (and their version numbers) that are provided by \pkg{basilisk}.
#'
#' @details
#' The composition of the core list is determined by the Miniconda list for Python 3.7 (\url{https://docs.anaconda.com/anaconda/packages/pkg-docs/}).
#' Note that there are subtle differences between the package lists for different operating systems;
#' developers of clients of \pkg{basilisk} should avoid such OS-specific core packages.
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
