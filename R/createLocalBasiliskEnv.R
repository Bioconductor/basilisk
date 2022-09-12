#' Create a local conda environment manually
#'
#' Manually create a local conda environment with versioning and thread safety.
#' This is intended for use in analysis workflows rather than package development.
#'
#' @param dir String containing the path to a directory in which the local environment is to be stored.
#' @param ... Further arguments to pass to \code{\link{setupBasiliskEnv}}.
#'
#' @return String containing a path to the newly created environment, or to an existing environment if one was previously created.
#' This can be used in \code{\link{basiliskRun}}.
#'
#' @author Aaron Lun
#'
#' @details
#' This function is intended for end users who wish to use the \pkg{basilisk} machinery for coordinating one or more Python environments in their analysis workflows.
#' It can be inserted into, e.g., Rmarkdown reports to automatically provision and cache an environment on first compilation,
#' which will be automatically re-used in later compilations.
#' Some care is taken to ensure that the cached environment is refreshed when \pkg{basilisk} is updated,
#' and that concurrent access to the environment is done safely.
#'
#' @examples
#' tmploc <- file.path(tempdir(), "my_package_C")
#' tmp <- createLocalBasiliskEnv(tmploc, packages="pandas==1.4.3")
#' basiliskRun(env=tmp, fun=function() { 
#'     X <- reticulate::import("pandas"); X$`__version__` 
#' })
#'
#' @export
#' @importFrom basilisk.utils installConda
#' @importFrom dir.expiry lockDirectory unlockDirectory touchDirectory
createLocalBasiliskEnv <- function(dir, ...) {
    installConda()

    dir.create(dir, showWarnings=FALSE)
    dir <- normalizePath(dir)

    version <- as.character(packageVersion("basilisk"))
    envpath <- file.path(dir, version)

    # Decide whether we want to destroy things.
    do.destroy <- destroyOldVersions()

    # Locking the environment directory; this ensures we will wait for
    # any concurrently running installations to finish. Do NOT assign
    # the existence of envpath to a variable for re-use in the
    # conditional below. We want to recheck existance just in case the
    # directory was created after waiting to acquire the lock.
    lck <- lockDirectory(envpath, exclusive=!file.exists(envpath))
    on.exit(unlockDirectory(lck, clear=do.destroy), add=TRUE, after=FALSE)

    if (!file.exists(envpath)) {
        setupBasiliskEnv(envpath, ...)
    }

    # Touching both the individual package directory _and_ the conda
    # directory on successful acquisition of the path.
    touchDirectory(envpath)

    envpath
}
