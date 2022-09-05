#' Find the persistent environment
#'
#' Find the persistent environment inside a \code{\link{basiliskRun}} call,
#' to allow variables to be passed across calls.
#' This is deprecated in favor of directly assigning variables to the \code{.basilisk.store} environment,
#' which avoids the need to load \pkg{basilisk} into the target process (e.g., for the minimalistic fallback).
#'
#' @details
#' The persistent environment is where variables can be stored across \code{\link{basiliskRun}} calls.
#' When \code{proc} is an environment, it serves as the persistent environment;
#' otherwise, if \code{proc} is a process, the global environment of the process is the persistent environment.
#'
#' Developers should avoid naming persistent variables with the \code{.basilisk} prefix.
#' These are reserved for internal use and may be overwritten by later calls to \code{\link{basiliskRun}}.
#'
#' @return
#' An environment to which persistent variables can be assigned,
#' for use in later \code{\link{basiliskRun}} calls on the same \code{proc}.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{basiliskRun}}, where this function can be used.
#' 
#' @examples
#' # Using the base environment for brevity.
#' cl <- basiliskStart(NULL)
#' basiliskRun(proc=cl, function() {
#'     assign(x="snake.in.my.shoes", 1, envir=.basilisk.store)
#' })
#' basiliskRun(proc=cl, function() {
#'     get("snake.in.my.shoes", envir=.basilisk.store)
#' })
#' basiliskStop(cl)
#' 
#' @export
findPersistentEnv <- function() {
    .Deprecated(new=".basilisk.store")
    get(".basilisk.store")
}
