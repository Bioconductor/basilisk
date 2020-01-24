#' @importFrom basilisk.utils isWindows
.get_py_cmd <- function(loc) {
    suffix <- if (isWindows()) "python.exe" else "bin/python"
    file.path(loc, suffix)
}

#' @importFrom basilisk.utils isWindows
.retrieve_conda <- function() {
    if (isWindows()) {
        "Scripts/conda.exe"
    } else {
        "bin/conda"
    }
}

.env_dir <- "basilisk"

#' @importFrom utils packageVersion
#' @importFrom basilisk.utils isWindows isMacOSX getExternalDir
.choose_env_dir <- function(pkgname, mustWork=FALSE) {
    if (isWindows() || isMacOSX()) {
        vdir <- file.path(getExternalDir(), 
            paste0(pkgname, "-", packageVersion(pkgname)))
    } else {
        # As this is run in configure, system.file() will not
        # work, as pkgname isn't even installled yet!
        vdir <- file.path(.libPaths()[1], pkgname)
    }

    vdir <- file.path(vdir, .env_dir)
    if (mustWork && !file.exists(vdir)) {
        stop("basilisk environment directory does not exist")
    }
    vdir
}
