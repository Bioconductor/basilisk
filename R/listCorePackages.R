#' List core packages
#'
#' List the set of core Python packages (and their version numbers) that are provided by \pkg{basilisk}.
#'
#' @details
#' Core Python packages are lazily installed into the \pkg{basilisk} Python instance.
#' These are usually infrastructure packages that are required by many other Python packages,
#' so maintaining a core installation avoids redundancy and reduces the installation footprint.
#'
#' Please contact the \pkg{basilisk} maintainers if you feel that a Python package should be added to this list.
#' This requires that (i) multiple Bioconductor packages depend on the proposed Python package and
#' (ii) the proposed package does not have any version conflicts with existing core packages.
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
    data.frame(full=pkgs, name=names)
}

.full2pkg <- function(packages) {
    sub("[><=]+.*", "", packages)
}
