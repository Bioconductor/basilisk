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
#' @importFrom basilisk basiliskStart
#' @importFrom parallel stopCluster clusterCall
test <- function() {
    cl <- basiliskStart("env1", pkgname="son.of.basilisk")
    pandas.names <- clusterCall(cl, function() { 
        X <- reticulate::import("pandas")
        names(X) 
    })
    stopCluster(cl)

    cl <- basiliskStart("env2", pkgname="son.of.basilisk")
    sklearn.names <- clusterCall(cl, function() { 
        X <- reticulate::import("sklearn.ensemble")
        names(X)
    })
    stopCluster(cl)

    list(pandas=pandas.names, sklearn=sklearn.names)
}
