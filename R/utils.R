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
