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
#' @return 
#' For \code{listPackages}, a data.frame containing the \code{full}, a versioned package string, and \code{package}, the package name.
#'
#' For \code{listPythonVersion}, a string containing the default version of Python.
#' 
#' @examples
#' listPackages()
#' listPythonVersion()
#' 
#' @export
listPackages <- function(env=NULL) {
    envpath <- obtainEnvironmentPath(env)
    out <- .basilisk_freeze(envpath)
    data.frame(full=out, package=.full2pkg(out), stringsAsFactors=FALSE)
}

.basilisk_freeze <- function(envpath) {
    previous <- activateEnvironment(envpath)
    on.exit(deactivateEnvironment(previous))
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

#' @export
#' @rdname listPackages
listPythonVersion <- function(env=NULL) {
    envpath <- obtainEnvironmentPath(env)
    .python_version(envpath)
}

.python_version <- function(dir) {
    py.cmd <- getPythonBinary(dir)
    dump <- system2(py.cmd, "--version", stdout=TRUE, stderr=TRUE)
    pattern <- "^Python "
    sub(pattern, "", dump[grep(pattern, dump)[1]])
}

