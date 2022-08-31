#' Obtain the environment path
#'
#' Obtain a path to a Conda environment, lazily installing it if necessary.
#'
#' @param env A \linkS4class{BasiliskEnvironment} object specifying the environment.
#' Alternatively a string containing a path to an existing environment.
#'
#' @return String containing the path to an instantiated Conda environment.
#' 
#' For a BasiliskEnvironment \code{env}, the function will also lazily create the environment if \code{useSystemDir()} returns \code{FALSE}
#' and the environment does not already exist.
#'
#' @author Aaron Lun
#'
#' @examples
#' tmploc <- file.path(tempdir(), "my_package_A")
#' if (!file.exists(tmploc)) {
#'     setupBasiliskEnv(tmploc, c('pandas=1.4.3'))
#' }
#' obtainEnvironmentPath(tmploc)
#'
#' env <- BasiliskEnvironment("test_env", "basilisk", 
#'     packages=c("scikit-learn=1.1.1", "pandas=1.43.1"))
#' \dontrun{obtainEnvironmentPath(env)}
#' @export
#' @importFrom dir.expiry lockDirectory unlockDirectory touchDirectory
#' @importFrom utils packageVersion
obtainEnvironmentPath <- function(env) {
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
            # Make sure that conda is installed - do this first. This also
            # applies a lock to ensure that we wait for any concurrently
            # running Conda installations to finish.
            installConda()

            # Decide whether we want to destroy things.
            do.destroy <- destroyOldVersions()

            # Applying a new shared lock to protect the current use-case from
            # deletion by other processes clearing out stale installations.
            exdir <- getExternalDir()
            exlck <- lockExternalDir(exdir, exclusive=FALSE)
            on.exit(unlockExternalDir(exlck, clear=do.destroy))

            envdir <- file.path(exdir, pkgname, packageVersion(pkgname))
            envpath <- file.path(envdir, envname)

            # Locking the environment directory; this ensures we will wait for
            # any concurrently running installations to finish. Do NOT assign
            # the existence of envpath to a variable for re-use in the
            # conditional below. We want to recheck existance just in case the
            # directory was created after waiting to acquire the lock.
            lck <- lockDirectory(envdir, exclusive=!file.exists(envpath))
            on.exit(unlockDirectory(lck, clear=do.destroy), add=TRUE, after=FALSE)

            if (!file.exists(envpath)) {
                setupBasiliskEnv(envpath, 
                    packages=.getPackages(env), 
                    channels=.getChannels(env),
                    pip=.getPipPackages(env),
                    paths=file.path(getSystemDir(pkgname, installed=TRUE), .getPipPaths(env))) # package must already be installed to get to this point.
            }

            # Touching both the individual package directory _and_ the conda
            # directory on successful acquisition of the path.
            touchDirectory(envdir)
            touchDirectory(exdir)
        }
    }

    envpath
}
