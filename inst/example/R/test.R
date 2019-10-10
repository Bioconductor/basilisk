#' Test function
#'
#' Does nothing but test that we can load modules from different virtual environments.
#'
#' @return A list of names of objects exposed in each module.
#' @author Aaron Lun
#' 
#' @examples
#' test()
#' @export
#' @importFrom reticulate import
#' @importFrom basilisk callVirtualEnv
test <- function() {
    pandas.names <- callVirtualEnv("env1", pkgname="son.of.basilisk", FUN=function() {
        X <- reticulate::import("pandas")
        names(X) 
    })

    sklearn.names <- callVirtualEnv("env2", pkgname="son.of.basilisk", FUN=function() {
        X <- reticulate::import("sklearn.ensemble")
        names(X)
    })

    list(pandas=pandas.names, sklearn=sklearn.names)
}
