#' Use \pkg{basilisk}'s Python instance
#'
#' Use \pkg{reticulate} to set up the Bioconductor-owned instance of Python provided by \pkg{basilisk}.
#'
#' @return
#' A string containing the path to the \pkg{basilisk} Python executable, invisibly.
#'
#' @author Aaron Lun
#'
#' @details
#' This function is intended for use within other Bioconductor packages.
#' It uses \code{\link{use_python}} to register a consistent Python version that is isolated from the system installation.
#' 
#' @examples
#' useBasilisk()
#' os <- reticulate::import("os")
#' os$listdir()
#'
#' @export
#' @importFrom reticulate use_python
useBasilisk <- function() {
    old <- Sys.getenv("RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")
    if (old!="") {
        on.exit(Sys.setenv(RETICULATE_PYTHON=old))
    }

    py.cmd <- system.file("miniconda", "bin", "python3", package="basilisk", mustWork=TRUE)
    use_python(py.cmd, required=TRUE)
    py.cmd
}
