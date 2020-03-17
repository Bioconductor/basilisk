#' Find the persistent environment
#'
#' Find the persistent environment inside a \code{\link{basiliskRun}} call,
#' to allow variables to be passed across calls.
#'
#' @details
#' The persistent environment is where variables can be stored across \code{\link{basiliskRun}} calls.
#' When \code{proc} is an environment, it serves as the persistent environment;
#' otherwise, if \code{proc} is a process, the global environment of the process is the persistent environment.
#'
#' It is very important that this function be called inside the frame of the \code{FUN} passed to \code{\link{basiliskRun}}.
#' It should not be called inside nested functions within \code{FUN},
#' otherwise the look-up of the \code{\link{parent.frame}} will not work correctly.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{basiliskRun}}, where this function can be used.
#' 
#' @examples
#' cl <- basiliskStart(NULL)
#' basiliskRun(proc=cl, function() {
#'     assign(x="snake.in.my.shoes", 1, envir=basilisk::findPersistentEnv())
#' })
#' basiliskRun(proc=cl, function() {
#'     get("snake.in.my.shoes", envir=basilisk::findPersistentEnv())
#' })
#' basiliskStop(cl)
#' 
#' @export
findPersistentEnv <- function() {
    # This check avoids conflicts with any '.basilisk.fun' variable 
    # in the function passed to basiliskRun when 'cl' is an environment.
    possible <- parent.frame(2)
    if (exists(".basilisk.fun", envir=possible, inherits=FALSE)) {
        possible
    } else {
        .GlobalEnv 
    }
}
