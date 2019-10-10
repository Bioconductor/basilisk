#' Safely import a module
#'
#' Safely import a module, loading the Bioconductor-managed instance of Python
#' and installing the corresponding package if it is not already available.
#'
#' @param module String containing the name of the module to import.
#' @param package String containing the name of the package to download containing \code{module}.
#' 
#' @return A Python Module object, as returned by \code{\link[reticulate]{import}}.
#' 
#' @author Aaron Lun
#'
#' @details
#' This function allows developers to safely load Python modules inside R functions
#' by installing the corresponding package if it is not already available.
#' End users can also use this to easily load modules in their own analyses.
#'
#' @examples
#' X <- importModule("sklearn")
#' X
#' 
#' @seealso
#' \code{\link{useBiocPython}}, to load in the correct Python instance.
#'
#' \code{\link{installModules}}, to install modules.
#' 
#' @export
#' @importFrom reticulate import
importModule <- function(module, package=module) {
    useBiocPython()

    x <- try(import(module), silent=TRUE)
    if (!is(x, "try-error")) {
        return(x) 
    }

    installPythonPackages(package)
    import(module)
}
