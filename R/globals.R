.make_globals <- function () {
    current <- list(
        fork=TRUE,
        shared=TRUE,
        force.fallback=FALSE,
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
