#' Install Python packages
#' 
#' Install packages for the Bioconductor instance of Python provided by \pkg{basilisk}.
#'
#' @param packages Character vector of the names of packages to install.
#'
#' @return \code{NULL}, invisibly.
#' An installation failure for any package in \code{packages} will raise an error.
#'
#' @details
#' Modules are installed in the home directory of the self-contained Bioconductor-managed instance of Python.
#' This ensures that there are no spill-over effects to or from the system installation of Python and its set of packages.
#'
#' The version of each package is set to the latest version uploaded to PyPi as of 5th October, 2019.
#' This aims to guarantee that all R packages are installing a consistent cohort of Python packages.
#' By doing so, we ensure that all R installations are running the same Python code,
#' improving portability and simplifying maintenance and debugging.
#' 
#' @author
#' Aaron Lun
#'
#' @seealso
#' \code{\link{findVersionUpTo}}, to obtain the latest version up to a given date.
#'
#' @examples
#' installPythonPackages(c("pandas", "scikit-learn"))
#' @export 
installPythonPackages <- function(modules) {
    X <- useBiocPython()
    tmp <- tempfile()

    for (m in modules) {
        v <- findVersionUpTo(m, date="2019-10-05")
        mv <- paste0(m, "==", v)

        out <- system2(X, args=c("-m", "pip", "download", mv, "-d", tmp))
        if (out!=0) {
            stop("downloading of Python package '", m, "' failed")
        }

        # Check that ONE of the wheels has the name of the package.
        # This should error out for, e.g., sklearn, as it redirects to scikit-learn.
        if (!gsub("-", "_", m) %in% sub("-.*", "", list.files(tmp, pattern="\\.whl$"))) {
            stop("installed name of Python package is different from '", m, "'")
        }

        out <- system2(X, args=c("-m", "pip", "install", mv, "--find-links", tmp))
        if (out!=0) {
            stop("installation of Python package '", m, "' failed")
        }
    }
    invisible(NULL)    
}
