.get_py_cmd <- function(loc) {
    # Ripped out of reticulate::use_virtualenv.
    suffix <- if (.Platform$OS.type=="windows") "python.exe" else "bin/python"
    file.path(loc, suffix)
}

.core_dir <- "anaconda"

.get_basilisk_dir <- function(mustWork=TRUE, conda.dir=TRUE) {
    if (.is_windows()) {
        # Because, y'know, of course windows has to be different in a painful
        # way. In this case, it is something to do with the paths being too
        # long if we ask for system.file(), so we'll just dump it in a user
        # directory instead. Windows users will then have to sit through the
        # installation process... too bad for them, I guess.
        xpath <- rappdirs::user_data_dir(appname="basilisk")

        if (conda.dir) {
            out <- file.path(xpath, .core_dir)
            if (mustWork && !file.exists(out)) {
                stop("'", out, "' does not exist")
            }
            out
        } else {
            xpath 
        }
    } else {
        if (conda.dir) {
            system.file(.core_dir, package="basilisk", mustWork=mustWork)
        } else {
            system.file(package="basilisk")
        }
    }
}

.detect_os <- function() {
    if (.is_windows()) {
        paste0("win", ifelse(.Machine$sizeof.pointer == 8, "64", "32"))
    } else {
        if (Sys.info()[["sysname"]] == "Darwin") {
            "macosx"
        } else {
            "linux"
        }
    }
}

.is_windows <- function() {
    .Platform$OS.type=="windows" 
}

.retrieve_conda <- function() {
    if (.is_windows()) {
        "Scripts/conda.exe"
    } else {
        "bin/conda"
    }
}

.env_dir <- "basilisk"

.choose_env_dir <- function(pkgname, mustWork=FALSE) {
    if (!is.null(pkgname)) {
        if (.is_windows()) {
            vdir <- file.path(rappdirs::user_data_dir(appname="basilisk"), pkgname, .env_dir)
        } else {
            vdir <- file.path(system.file(package=pkgname), .env_dir)
        }
    } else {
        vdir <- Sys.getenv("BASILISK_NONPKG_DIR", unset=getwd())
    }
    if (mustWork && !file.exists(vdir)) {
        stop("basilisk environment directory does not exist")
    }
    vdir
}

.is_roxygen_running <- function(pkgname) {
    basename(system.file(package=pkgname))!=pkgname
}
