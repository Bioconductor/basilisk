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
#' @importFrom basilisk basiliskStart basiliskRun basiliskStop
test <- function() {
    cl <- basiliskStart(env1)
    pandas.names <- basiliskRun(cl, function() { 
        X <- reticulate::import("pandas")
        names(X) 
    })
    basiliskStop(cl)

    cl <- basiliskStart(env2, testload="scipy.optimize")
    sklearn.names <- basiliskRun(cl, function() { 
        X <- reticulate::import("sklearn.ensemble")
        names(X)
    })
    basiliskStop(cl)

    cl <- basiliskStart(env3)
    dummy <- basiliskRun(cl, function() { 
        X <- reticulate::import("test_dummy")
        X$whoami()
    })
    basiliskStop(cl)

    list(pandas=pandas.names, sklearn=sklearn.names, dummy=dummy)
}
