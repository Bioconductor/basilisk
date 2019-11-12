.common_env <- "common"

.get_py_cmd <- function(loc) {
    # Ripped out of reticulate::use_virtualenv.
    suffix <- if (.Platform$OS.type=="windows") "Scripts/python.exe" else "bin/python"
    file.path(loc, suffix)
}

.get_env_root <- function(pkgname) {
    system.file("basilisk", package=pkgname, mustWork=TRUE)
}

.get_common_env <- function() {
    loc <- Sys.getenv("BASILISK_TEST_COMMON", .get_env_root("basilisk"))
    file.path(loc, .common_env)
}
