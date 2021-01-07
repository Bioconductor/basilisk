#' Obtain the environment path
#'
#' Obtain a path to a Conda environment, lazily installing it if necessary.
#'
#' @param env A string or \linkS4class{BasiliskEnvironment} object specifying the environment.
#'
#' @return String containing the path to an instantiated Conda environment.
#' It will also lazily create the environment if \code{useSystemDir()} returns \code{FALSE}
#' and the environment does not already exist.
#'
#' @details
#' A lot of this function used to belong to \code{\link{setupBasiliskEnv}}.
#' I rolled it out into a separate entity so that \code{\link{setupBasiliskEnv}} only needs to focus on creating the environment.
#' It should not worry about whether or not the environment needs to be created, which is rather context-dependent anyway.
#' Indeed, such decisions only really need to be made during lazy installation and not in other contexts.
#' 
#' @author Aaron Lun
#'
#' @importFrom filelock lock unlock
#' @importFrom utils packageVersion
#' @rdname INTERNAL_obtainEnvironmentPath
.obtainEnvironmentPath <- function(env) {
    if (is.null(env)) {
        installConda()
        envpath <- getCondaDir()

    } else {
        envname <- .getEnvName(env)
        pkgname <- .getPkgName(env)

        if (is.null(pkgname)) {
            envpath <- envname

        } else if (useSystemDir()) {
            envpath <- file.path(.get_env_system_dir(pkgname, installed=TRUE), envname)
            if (!file.exists(envpath)) {
                stop(sprintf("environment '%s' should have been created during '%s' installation", envname, pkgname))
            }

        } else {
            # This step must be done before establishing the lock on the
            # 'locfile' below, as it will clearExternalDir() and destroy all
            # files for the current major version.
            installConda() 

            envdir <- file.path(getExternalDir(), paste0(pkgname, "-", packageVersion(pkgname)))
            envpath <- file.path(envdir, envname)

            # See ?lockExternalDir and the installConda code
            # for the rationale behind 'exclusive='.
            dir.create(envdir, recursive=TRUE, showWarnings=FALSE)
            locfile <- paste0(sub("/+$", "", envpath), "-00LOCK")
            loc <- lock(locfile, exclusive=!file.exists(envpath))
            on.exit(unlock(loc))

            if (!file.exists(envpath)) {
                setupBasiliskEnv(envpath, 
                    packages=.getPackages(env), 
                    channels=.getChannels(env),
                    pip=.getPipPackages(env),
                    paths=file.path(getSystemDir(pkgname, installed=TRUE), .getPipPaths(env))) # package must already be installed to get to this point.

                if (destroyOldVersions()) {
                    clearObsoleteDir(envdir)
                }
            }
        }
    }

    envpath
}
