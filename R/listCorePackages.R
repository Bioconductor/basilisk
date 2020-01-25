#' List core packages
#'
#' List the set of core Python packages (and their version numbers) that are provided by \pkg{basilisk}.
#'
#' @details
#' Core Python packages are usually infrastructure packages of some sort that are required by many other Python packages,
#' so maintaining a core installation avoids redundancy and reduces the installation footprint.
#' The composition of the core list is determined by the Anaconda 2019.10 package list for Python 3.7 (\url{https://docs.anaconda.com/anaconda/packages/pkg-docs/}).
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
#' @importFrom basilisk.utils getBasiliskDir installAnaconda
listCorePackages <- function() {
    installAnaconda()
    out <- .basilisk_freeze(.get_py_cmd(getBasiliskDir()))
    data.frame(full=out, package=.full2pkg(out), stringsAsFactors=FALSE)
}

.full2pkg <- function(packages) {
    sub("[><=]+.*", "", packages)
}
