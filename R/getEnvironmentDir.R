#' Get the \pkg{basilisk} environment directory
#'
#' Find the installation directory for the \pkg{basilisk} Python environments for a particular client package.
#'
#' @param pkgname String containing the name of the \pkg{basilisk} client package responsible for generating the environment. 
#' @param installed Logical scalar indicating whether the client package is already installed.
#'
#' @return String containing the path to the environment directory.
#'
#' @details
#' By default, \pkg{basilisk} environments are installed to a location specified by \code{\link{getExternalDir}}.
#' This ensures that R package build systems do not attempt to generate binaries that include the Python/conda packages;
#' such binaries are not relocatable due to the presence of hard-coded paths, resulting in run-time failures.
#' 
#' If the \code{BASILISK_USE_SYSTEM_DIR} environment variable is set to \code{"1"},
#' the function will return a path to a location inside the client package's system installation directory.
#' This is the ideal approach when installing from source as we guarantee synchronization in Python and R re-installations.
#' It also ensures that any R process that can load the client package will also have permissions to access its environments,
#' which makes life easier for sysadmins of clusters or other shared resources.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Setting the environment variable to run this example: 
#' # all other modes rely on installation of the client.
#' old <- Sys.getenv("BASILISK_USE_SYSTEM_DIR")
#' Sys.setenv(BASILISK_USE_SYSTEM_DIR=1)
#'
#' getEnvironmentDir("client.of.basilisk", installed=FALSE)
#'
#' Sys.setenv(BASILISK_USE_SYSTEM_DIR=old)
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom basilisk.utils getExternalDir getSystemDir
getEnvironmentDir <- function(pkgname, installed=TRUE) {
    if (!useSystemDir()) {
        vdir <- file.path(getExternalDir(), paste0(pkgname, "-", packageVersion(pkgname)))
    } else {
        vdir <- getSystemDir(pkgname, installed)
        file.path(vdir, "basilisk")
    }
}
