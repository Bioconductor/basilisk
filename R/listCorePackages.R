#' List core packages
#'
#' List the set of core Python packages (and their version numbers) that are provided by \pkg{basilisk}.
#'
#' @details
#' Core Python packages are lazily installed into the \pkg{basilisk} Python instance.
#' These are usually infrastructure packages that are required by many other Python packages,
#' so maintaining a core installation avoids redundancy and reduces the installation footprint.
#'
#' Identities and versions of the core packages are scraped from the Anaconda package lists for Python 3.7 (\url{https://docs.anaconda.com/anaconda/packages/pkg-docs/}).
#' Note that there are subtle differences between the package lists for different operating systems;
#' developers of clients of \pkg{basilisk} should avoid such OS-specific core packages.
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
    pkgs <- readLines(.get_core_list_file())
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

.get_core_list_file <- function() {
    system.file("core_lists", .detect_os(), package="basilisk", mustWork=TRUE)
}
