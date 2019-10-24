#' Start and stop \pkg{basilisk}-related processes
#'
#' Creates a separate persistent R process to load a \pkg{basilisk}-controlled virtual Python environment.
#'
#' @inheritParams setupVirtualEnv
#' @param proc A cluster object pointing to an active R process.
#'
#' @return 
#' \code{basiliskStart} returns a cluster object (same as \code{\link{makePSOCKCluster}}),
#' pointing to a separate R process in which the specified virtual environment \code{envname} has been loaded.
#' 
#' @details
#' The use of a separate process is motivated by the desire to avoid affecting (and being affected by) other Python instances or environments loaded by other packages or by the user.
#' Thus, any client package using this system is guaranteed to have access to the specified versions of packages from \code{envname}.
#' Note that this comes at the cost of some efficiency as any data must be serialized to the separate process.
#'
#' The cluster object returned by \code{basiliskStart} points to a R process in which arbitrary functions can be executed via \code{\link{clusterCall}}.
#' Objects in R or Python can persist across calls, though doing so in R requires modification of the process' global namespace, e.g., via \code{<<-}.
#' It is usually good practice to call \code{\link{stopCluster}} once computation is finished.
#' 
#' @author Aaron Lun
#'
#' @examples
#' # Loading one virtual environment into our R session:
#' setupVirtualEnv('my_package_A', 'pandas==0.25.1')
#' useVirtualEnv("my_package_A")
#' X <- reticulate::import("pandas")
#' X$`__version__` 
#'
#' # Co-exists with our other virtual environment in a separate process:
#' setupVirtualEnv('my_package_B', 'pandas==0.24.1')
#' cl <- basiliskStart('my_package_B')
#' parallel::clusterCall(cl, function() { 
#'     X <- reticulate::import("pandas"); X$`__version__` 
#' })
#' parallel::stopCluster(cl)
#' 
#' @export
#' @importFrom parallel makePSOCKcluster clusterCall
basiliskStart <- function(envname, pkgname=NULL) {
    proc <- makePSOCKcluster(1)
    clusterCall(proc, useVirtualEnv, envname=envname, pkgname=pkgname)
    proc 
}
