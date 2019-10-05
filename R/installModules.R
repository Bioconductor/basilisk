#' Install Python modules
#' 
#' Install modules for the Bioconductor instance of Python provided by \pkg{jormungandR}.
#'
#' @param modules Character vector of the names of modules to install.
#'
#' @return \code{NULL}, invisibly.
#'
#' @details
#' If the module is already present, this function will attempt to upgrade it.
#'
#' An installation failure for any module in \code{modules} will raise an error.
#' 
#' @export 
installModules <- function(modules) {
    X <- useBiocPython()
    for (m in modules) {
        out <- system2(X, args=c("-m", "pip", "install", "--upgrade", m))
        if (out!=0) {
            stop("installation of Python module '", m, "' failed")
        }
    }
    invisible(NULL)    
}
