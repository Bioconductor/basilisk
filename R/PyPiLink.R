#' Link to PyPi
#'
#' Helper function to create a Markdown link to the PyPi landing page for a Python package.
#' Intended primarily for use inside vignettes.
#'
#' @param package String containing the name of the Python package.
#' 
#' @return String containing a Markdown link to the package's landing page.
#' @author Aaron Lun
#'
#' @examples
#' PyPiLink("pandas")
#' PyPiLink("scikit-learn")
#' 
#' @export
PyPiLink <- function(package) {
    sprintf("[%s](https://pypi.org/project/%s)", package, package)
}
