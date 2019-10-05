#' Link to PyPi
#'
#' Helper function to create a Markdown link to the PyPi landing package for a module.
#' Intended primarily for use inside vignettes.
#'
#' @param module String containing the name of the Python module.
#' 
#' @return String containing a Markdown link to the module's landing page.
#' @author Aaron Lun
#'
#' @examples
#' PyPiLink("pandas")
#' PyPiLink("scikit-learn")
#' 
#' @export
PyPiLink <- function(module) {
    sprintf("[%s](https://pypi.org/project/%s)", module, module)
}
