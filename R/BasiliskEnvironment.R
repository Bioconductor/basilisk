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
#' \item \code{pkgname}, string containing the name of the package that owns the environment.
#' \item \code{packages}, character vector containing the names of the required Python packages,
#' see \code{\link{setupBasiliskEnv}} for requirements.
#' }
#' 
#' @examples
#' BasiliskEnvironment("my_env1", "AaronPackage", packages=c("sklearn", "pandas"))
#' @docType class
#' @name BasiliskEnvironment-class
#' @aliases BasiliskEnvironment-class
#' BasiliskEnvironment
NULL

#' @export
setClass("BasiliskEnvironment", slots=c(envname="character", pkgname="character", packages="character"))

#' @export
#' @import methods 
BasiliskEnvironment <- function(envname, pkgname, packages) {
    new("BasiliskEnvironment", envname=envname, pkgname=pkgname, packages=packages)
}

setValidity("BasiliskEnvironment", function(object) {
    msg <- character(0)

    if (length(val <- .getEnvName(object))!=1 || is.na(val) || !is.character(val)){ 
        msg <- c(msg, "'envname' should be a non-NA string")
    }

    if (length(val <- .getPkgName(object))!=1 || is.na(val) || !is.character(val)){ 
        msg <- c(msg, "'pkgname' should be a non-NA string")
    }

    if (any(is.na(.getPackages(object)))) {
        msg <- c(msg, "'packages' should not contain NA strings")
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

.getPackages <- function(x) x@packages
