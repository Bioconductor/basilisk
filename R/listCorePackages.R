#' List core packages
#'
#' List the set of core Python packages (and their version numbers) that are provided by \pkg{basilisk}.
#'
#' @details
#' Core Python packages are lazily installed into the \pkg{basilisk} Python instance.
#' These are usually infrastructure packages that are required by many other Python packages,
#' so maintaining a core installation avoids redundancy and reduces the installation footprint.
#'
#' Please contact the \pkg{basilisk} maintainers if you feel that a Python package should be added or an existing version bumped.
#' This requires that (i) multiple Bioconductor packages depend on the proposed Python package and
#' (ii) the proposed package does not have any version conflicts with other core packages.
#'
#' @author Aaron Lun
#'
#' @return A data.frame containing the \code{full}, a versioned package string, and \code{package}, the package name.
#' 
#' @examples
#' listCorePackages()
#' 
#' @export
listCorePackages <- function() {
    pkgs <- readLines(system.file("core_list", package="basilisk"))
    names <- .full2pkg(pkgs)

    # For testing purposes, we remove a few packages.
    if (Sys.getenv("BASILISK_TEST_CORE", FALSE)) {
        discard <- names %in% c("pandas", "python-dateutil", "pytz")
        pkgs <- pkgs[!discard]
        names <- names[!discard]
    }

    data.frame(full=pkgs, name=names, stringsAsFactors=FALSE)
}

.full2pkg <- function(packages) {
    sub("[><=]+.*", "", packages)
}
