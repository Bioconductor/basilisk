#' Use \pkg{basilisk}'s Python instance
#'
#' Use \pkg{reticulate} to set up the Bioconductor-owned instance of Python provided by \pkg{basilisk}.
#' This function is primarily intended for developer intended for use within other Bioconductor packages.
#'
#' @return
#' A string containing the path to the \pkg{basilisk} Python executable, invisibly.
#'
#' @author Aaron Lun
#'
#' @details
#' This uses \code{\link{use_python}} to register a consistent Python version that is isolated from the system installation.
#' The \pkg{basilisk}-owned Python is installed using miniconda 4.7.10 (\url{https://docs.conda.io/en/latest/miniconda.html}),
#' with additional packages being lazily installed to eventually reach a full Anaconda installation.
#'
#' Developers should generally not use this function for anything other than testing.
#' Clients of \pkg{basilisk} should interact with its Python instance via \code{\link{basiliskRun}} and related functions,
#' while installation of additional packages should be performed via \code{\link{setupVirtualEnv}}.
#' 
#' @seealso
#' \code{\link{basiliskRun}}, to run Python code on the \pkg{basilisk}-managed Python instance.
#'
#' \code{\link{setupVirtualEnv}}, to install Python packages.
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

    py.cmd <- .get_basilisk_py()
    use_python(py.cmd, required=TRUE)
    py.cmd
}

.get_basilisk_py <- function() {
    .get_py_cmd(system.file(.mc_dir, package="basilisk", mustWork=TRUE))
}
