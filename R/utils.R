.get_py_cmd <- function(loc) {
    # Ripped out of reticulate::use_virtualenv.
    suffix <- if (.Platform$OS.type=="windows") "python.exe" else "bin/python"
    file.path(loc, suffix)
}

.core_dir <- "anaconda"

.get_basilisk_dir <- function() {
    system.file(.core_dir, package="basilisk", mustWork=TRUE)
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
        vdir <- file.path(system.file(package=pkgname), .env_dir)
    } else {
        vdir <- Sys.getenv("BASILISK_NONPKG_DIR", unset=getwd())
    }
    if (mustWork && !file.exists(vdir)) {
        stop("basilisk environment directory does not exist")
    }
    vdir
}
