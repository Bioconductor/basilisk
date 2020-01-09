.common_env <- "common"

.get_py_cmd <- function(loc) {
    # Ripped out of reticulate::use_virtualenv.
    suffix <- if (.Platform$OS.type=="windows") "python.exe" else "bin/python"
    file.path(loc, suffix)
}

.env_dir <- "basilisk"

.get_env_root <- function(pkgname) {
    system.file(.env_dir, package=pkgname, mustWork=TRUE)
}

.get_common_env <- function() {
    loc <- Sys.getenv("BASILISK_TEST_COMMON", .get_env_root("basilisk"))
    file.path(loc, .common_env)
}

.core_dir <- "anaconda"

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
