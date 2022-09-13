#' Find the persistent environment
#'
#' Previously used to find a persistent environment inside a \code{\link{basiliskRun}} call, to allow variables to be passed across calls.
#' This is deprecated in favor of setting \code{persist=TRUE} in \code{\link{basiliskRun}},
#' which will explicitly pass an environment in which to store persistent variables.
#'
#' @return A defunct error message is raised.
#'
#' @author Aaron Lun
#'
#' @export
findPersistentEnv <- function() {
    .Defunct(msg="'findPersistentEnv' is defunct.\nSet 'persist=TRUE' in basiliskRun instead.")
}
