#' The BasiliskEnvironment class
#'
#' The BasiliskEnvironment class provides a simple structure 
#' containing all of the information to construct a \pkg{basilisk} environment.
#' It is used by \code{\link{basiliskStart}} to perform lazy installation.
#'
#' @author Aaron lun
#'
#' @section Constructor:
#' \code{BasiliskEnvironment(envname, pkgname, packages)} will return a BasiliskEnvironment object, given:
#' \itemize{
#' \item \code{envname}, string containing the name of the environment.
#' Environment names starting with an underscore are reserved for internal use.
#' \item \code{pkgname}, string containing the name of the package that owns the environment.
#' \item \code{packages}, character vector containing the names of the required Python packages from conda,
#' see \code{\link{setupBasiliskEnv}} for requirements.
#' \item \code{channels}, character vector specifying the Conda channels to search,
#' see \code{\link{setupBasiliskEnv}} for detials.
#' \item \code{pip}, character vector containing names of additional Python packages from PyPi,
#' see \code{\link{setupBasiliskEnv}} for requirements.
#' \item \code{paths}, character vector containing relative paths to Python packages to be installed via \code{pip}.
#' These paths are interpreted relative to the system directory of \code{pkgname}, i.e., they are appended to the output of \code{\link{system.file}}.
#' }
#' 
#' @examples
#' BasiliskEnvironment("my_env1", "AaronPackage", 
#'     packages=c("scikit-learn=1.1.1", "pandas=1.43.1"))
#' @docType class
#' @name BasiliskEnvironment-class
#' @aliases BasiliskEnvironment-class
#' BasiliskEnvironment
NULL

#' @export
setClass("BasiliskEnvironment", slots=c(envname="character", pkgname="character", 
    packages="character", channels="character", pip="character", paths="character"))

#' @export
#' @import methods 
BasiliskEnvironment <- function(envname, pkgname, packages, channels="conda-forge", pip=character(0), paths=character(0)) {
    new("BasiliskEnvironment", envname=envname, pkgname=pkgname, packages=packages, channels=channels, pip=pip, paths=paths)
}

setValidity("BasiliskEnvironment", function(object) {
    msg <- character(0)

    if (length(val <- .getEnvName(object))!=1 || is.na(val) || !is.character(val)){ 
        msg <- c(msg, "'envname' should be a non-NA string")
    }
    if (grepl("^_", val)) {
        msg <- c(msg, "environment names starting with an underscore are reserved")
    }

    if (length(val <- .getPkgName(object))!=1 || is.na(val) || !is.character(val)){ 
        msg <- c(msg, "'pkgname' should be a non-NA string")
    }

    if (any(is.na(.getChannels(object)))) {
        msg <- c(msg, "'channels' should not contain NA strings")
    }

    if (any(is.na(.getPackages(object)))) {
        msg <- c(msg, "'packages' should not contain NA strings")
    }

    if (any(is.na(.getPipPackages(object)))) {
        msg <- c(msg, "'pip' should not contain NA strings")
    }

    if (any(is.na(.getPipPaths(object)))) {
        msg <- c(msg, "'paths' should not contain NA strings")
    }

    if (length(msg)) {
        msg
    } else {
        TRUE
    }
})

setGeneric(".getEnvName", function(x) standardGeneric(".getEnvName"))

setGeneric(".getPkgName", function(x) standardGeneric(".getPkgName"))

setMethod(".getEnvName", "BasiliskEnvironment", function(x) x@envname)

setMethod(".getPkgName", "BasiliskEnvironment", function(x) x@pkgname)

setMethod(".getEnvName", "character", identity)

setMethod(".getPkgName", "character", function(x) NULL) 

# Until all dependencies bump their version numbers to trigger a rebuild.
.getChannels <- function(x) tryCatch(x@channels, error=function(e) "conda-forge")

.getPackages <- function(x) x@packages

.getPipPackages <- function(x) x@pip

.getPipPaths <- function(x) x@paths
