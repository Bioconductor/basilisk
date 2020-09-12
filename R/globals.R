.make_globals <- function () {
    current <- list(
        envir=NULL,
        fork=TRUE,
        shared=TRUE,
        no.version=FALSE
    )

    list(
        set=function(...) {
            replacements <- list(...)
            common <- intersect(names(current), names(replacements))
            current[common] <<- replacements[common]
            invisible(NULL)
        },
        get=function(name) {
            current[[name]]
        }
    )
}

globals <- .make_globals()
