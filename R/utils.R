.get_py_cmd <- function(loc) {
    # Ripped out of reticulate::use_virtualenv.
    suffix <- if (.is_windows()) "python.exe" else "bin/python"
    file.path(loc, suffix)
}

.core_dir <- "anaconda"

#' @importFrom utils packageVersion
.get_basilisk_dir <- function(mustWork=TRUE) {
    if (.is_windows()) {
        # Because, y'know, of course windows has to be different in a painful
        # way. In this case, it is something to do with the paths being too
        # long if we ask for system.file(), so we'll just dump it in a user
        # directory instead. Windows users will then have to sit through the
        # installation process... too bad for them, I guess.
        inst_path <- rappdirs::user_data_dir(appname="basilisk")
        inst_path <- gsub("\\\\", "/", inst_path)
        inst_path <- file.path(inst_path, paste0("basilisk-", packageVersion("basilisk")))
    } else {
        inst_path <- system.file(package="basilisk")
    }

    inst_path <- file.path(inst_path, .core_dir)
    if (mustWork && !file.exists(inst_path)) {
        stop("basilisk installation directory does not exist")
    }

    inst_path
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

#' @importFrom utils packageVersion
.choose_env_dir <- function(pkgname, mustWork=FALSE) {
    if (!is.null(pkgname)) {
        if (.is_windows()) {
            vdir <- file.path(rappdirs::user_data_dir(appname="basilisk"), 
                paste0(pkgname, "-", packageVersion(pkgname)), .env_dir)
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
