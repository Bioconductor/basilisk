#' Find the module version up to a given date
#'
#' Find the version of a Python module in PyPi up to a given date.
#'
#' @param module String containing the name of the module.
#' @param date String containing the date.
#'
#' @details
#' Only final release versions of the form \code{X.Y[.Z]} are considered.
#' Alpha, beta, pre- and post-release candidates are not considered in this search.
#'
#' This function will hit the REST API at \url{https://pypi.org/pypi},
#' so some discretion is required with the use of this function to avoid getting blacklisted.
#'
#' @return A string containing the latest package up to a given date.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{installModules}}, which uses this function to determine package versions.
#'
#' @examples
#' try(findVersionUpTo("pandas", "2009-05-02"))
#' findVersionUpTo("pandas", "2010-05-02")
#' findVersionUpTo("pandas", "2012-05-02")
#' findVersionUpTo("pandas", "2015-05-02")
#' 
#' @export
#' @importFrom httr GET content stop_for_status
findVersionUpTo <- function(module, date) {
    out <- GET(sprintf("https://pypi.org/pypi/%s/json", module))
    stop_for_status(out)
    releases <- content(out)$releases

    # Only considering final releases.
    keep <- grep("[0-9]+\\.[0-9]+$", names(releases))
    releases <- releases[keep]

    required <- Inf
    candidate <- NULL
    limit <- as.Date(date)

    for (n in names(releases)) {
        uptime <- releases[[n]][[1]]$upload_time
        diff <- as.numeric(limit - as.Date(uptime))
        if (diff >= 0 && required > diff) {
            required <- diff
            candidate <- n
        }
    }

    if (is.null(candidate)) {
        stop("no '", module, "' version is available before '", date, "'")
    }
    
    candidate 
}
