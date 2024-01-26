#' Use \pkg{basilisk} environments
#'
#' Use \pkg{basilisk} environments for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the \pkg{basilisk} environment to use. 
#' @param full.activation Logical scalar, see \code{\link{activateEnvironment}} for details.
#' 
#' @return 
#' The function will attempt to load the specified \pkg{basilisk} environment into the R session,
#' possibly with the modification of some environment variables (see Details).
#' A \code{NULL} is invisibly returned.
#'
#' @details
#' It is unlikely that developers should ever need to call \code{\link{useBasiliskEnv}} directly.
#' Rather, this interaction should be automatically handled by \code{\link{basiliskStart}}.
#' 
#' This function will modify a suite of environment variables as a side effect
#' - see \dQuote{Persistence of environment variables} in \code{?\link{basiliskStart}} for the rationale.
#'
#' @author Aaron Lun
#' 
#' @examples
#' \dontshow{basilisk.utils::installConda()}
#'
#' tmploc <- file.path(tempdir(), "my_package_A")
#' if (!file.exists(tmploc)) {
#'     setupBasiliskEnv(tmploc, c(pandas_spec()))
#' }
#'
#' # This may or may not work, depending on whether a Python instance
#' # has already been loaded into this R session.
#' try(useBasiliskEnv(tmploc))
#'
#' # This will definitely not work, as the available Python is already set.
#' baseloc <- basilisk.utils::getCondaDir()
#' status <- try(useBasiliskEnv(baseloc))
#'
#' # ... except on Windows, which somehow avoids tripping the error.
#' stopifnot(is(status, "try-error") || basilisk.utils::isWindows())
#'
#' @seealso
#' \code{\link{basiliskStart}}, for how these \pkg{basilisk} environments should be used.
#'
#' @export
#' @import basilisk.utils
#' @importFrom reticulate use_condaenv py_config
useBasiliskEnv <- function(envpath, full.activation=NA) {
    envpath <- normalizePath(envpath, mustWork=TRUE)

    activateEnvironment(envpath, full.activation=full.activation)
    use_condaenv(envpath, required=TRUE)

    # use_condaenv doesn't actually cause Python to be loaded immediately, 
    # so we force the issue to seal the deal.
    py_config() 

    invisible(NULL)
}

#' @importFrom reticulate py_config 
.same_as_loaded <- function(envpath) 
# Checking whether we're the same as the existing python instance,
# which would indicate that we correctly loaded `envpath`.
{
    expected <- normalizePath(getPythonBinary(envpath)) 
    actual <- normalizePath(py_config()$python)
    identical(expected, actual)
}
