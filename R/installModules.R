#' Install Python modules
#' 
#' Install modules for the Bioconductor instance of Python provided by \pkg{basilisk}.
#'
#' @param modules Character vector of the names of modules to install.
#'
#' @return \code{NULL}, invisibly.
#' An installation failure for any module in \code{modules} will raise an error.
#'
#' @details
#' Modules are installed in the home directory of the self-contained Bioconductor-managed instance of Python.
#' This ensures that there are no spill-over effects to or from the system installation of Python and its set of modules.
#'
#' The version of each module is set to the latest version uploaded to PyPi as of 5th October, 2019.
#' This aims to guarantee that all R packages are installing a consistent cohort of Python modules.
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
#' installModules(c("pandas", "scikit-learn"))
#' @export 
installModules <- function(modules) {
    X <- useBiocPython()
    for (m in modules) {
        v <- findVersionUpTo(m, date="2019-10-05")
        out <- system2(X, args=c("-m", "pip", "install", paste0(m, "==", v)))
        if (out!=0) {
            stop("installation of Python module '", m, "' failed")
        }
    }
    invisible(NULL)    
}
