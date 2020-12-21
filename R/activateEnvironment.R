#' Activate a conda environment
#'
#' Mimic the (de)activation of a conda environment by modifying environment variables in the current R process.
#'
#' @param envpath String containing the path to the conda environment to activate.
#' If \code{NULL}, the base conda instance at \code{\link{getCondaDir}()} is activated.
#' @param listing Named list of strings containing name:value pairs for environment variables.
#'
#' @details
#' Conda environments generally need to be activated to function properly.
#' This is especially relevant on Windows where the \code{"PATH"} variable needs to be modified for the DLL search.
#' The \code{.activateEnvironment} function mimics the effect of activation
#' by modifying environment variables in the current R session.
#' This can be reversed by \code{.deactivateEnvironment} once the conda environment is no longer in use.
#'
#' The \code{.activateEnvironment} function will also unset a few bothersome environment variables:
#' \itemize{
#' \item \code{"PYTHONPATH"}: to avoid compromising the version guarantees 
#' if \pkg{reticulate}'s \code{import} is allowed to search other locations beyond the specified conda environment.
#' \item \code{"RETICULATE_PYTHON"}: this would otherwise override any choice of Python, 
#' even after explicit specification via \pkg{reticulate}'s \code{use_condaenv}!
#' \item \code{"RETICULATE_PYTHON_ENV"}: for similar reasons.
#' }
#'
#' @return
#' \code{.activateEnvironment} will modify environment variables to mimic activation of the conda environment.
#' It returns a named list of the previous values of all variables modified in this manner.
#' (\code{NA} values indicate that the corresponding variable was not previously set.)
#'
#' \code{.deactivateEnvironment} restores the environment variables to their pre-activation state.
#' It returns \code{NULL} invisibly.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_activateEnvironment
.activateEnvironment <- function(envpath=NULL) {
    ADD <- function(listing, var) {
        previous <- Sys.getenv(var, unset=NA)
        if (!is.na(previous)) {
            listing[[var]] <- previous
        }
        listing
    }

    output <- list()
    output <- ADD(output, "PYTHONPATH")
    Sys.unsetenv("PYTHONPATH")

    # This also needs to be unset otherwise it seems to take priority over
    # everything, even if you explicitly request to use a specific conda env's
    # Python (see LTLA/basilisk#1).
    output <- ADD(output, "RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")

    output <- ADD(output, "RETICULATE_PYTHON_ENV")
    Sys.unsetenv("RETICULATE_PYTHON_ENV")

    # Isolating from any user-specific site-packages, see conda/conda#394.
    output <- ADD(output, "PYTHONNOUSERSITE")
    Sys.setenv(PYTHONNOUSERSITE=1)

    # Activating the conda environment.
    output <- .activate_condaenv(output, envpath)

    output
}

#' @importFrom utils getFromNamespace
#' @importFrom basilisk.utils getCondaDir
.activate_condaenv <- function(listing, envpath) {
    if (isWindows()) {
        act.bat <- file.path(getCondaDir(), "condabin", "conda.bat")
        act.cmd <- c(shQuote(act.bat), "activate")
        if (!is.null(envpath)) {
            act.cmd <- c(act.cmd, shQuote(envpath))
        }
    } else {
        profile.sh <- file.path(getCondaDir(), "etc", "profile.d", "conda.sh")
        act.cmd <- c(".", shQuote(profile.sh), "&&", "conda", "activate")
        if (!is.null(envpath)) {
            act.cmd <- c(act.cmd, shQuote(envpath))
        }
    }

    # Couldn't be bothered to reimplement the stuff in
    # initDefaultClusterOptions, so here we are.
    p <- getFromNamespace(x="defaultClusterOptions", "parallel")$port

    # Identifying all environment variables after activation.
    con.cmd <- paste(sprintf("con <- socketConnection(port=%i, open='wb', blocking=TRUE)", p),
        "serialize(Sys.getenv(), con)", "close(con)", sep=";")
    act.cmd <- c(act.cmd, "&&", file.path(R.home("bin"), "Rscript"), 
        "--default-packages=NULL", "-e", deparse(con.cmd))

    soc <- serverSocket(p)
    on.exit(close(soc), add=TRUE)
    system(paste(act.cmd, collapse=" "), intern=TRUE)
    listener <- socketAccept(soc, blocking=TRUE, open = "a+b")
    on.exit(close(listener), add=TRUE)

    activated <- unserialize(listener)
    actvar <- activated 
    names(actvar) <- names(activated)

    existing <- Sys.getenv()
    extvar <- existing
    names(extvar) <- names(existing)

    if (isWindows()) {
        # Case insensitive on Windows. Hey, I don't make the rules.
        names(extvar) <- toupper(names(extvar))
        names(actvar) <- toupper(names(actvar))
    }

    # Manually applying changes to the environment variables while recording 
    # their previous state so that we can unset them appropriately.
    needs.setting <- setdiff(names(actvar), names(extvar))
    for (i in needs.setting) {
        listing[[i]] <- NA
    }

    needs.replacing <- intersect(names(extvar), names(actvar))
    needs.replacing <- needs.replacing[extvar[needs.replacing]!=actvar[needs.replacing]]
    for (i in needs.replacing) {
        listing[[i]] <- extvar[[i]]
    }

    to.change <- union(needs.replacing, needs.setting)
    if (length(to.change)) {
        do.call(Sys.setenv, as.list(actvar[to.change]))
    }

    needs.unsetting <- setdiff(names(extvar), names(actvar))
    for (i in needs.unsetting) {
        listing[[i]] <- extvar[[i]]
    }
    Sys.unsetenv(needs.unsetting)

    listing
}

#' @rdname INTERNAL_activateEnvironment
.deactivateEnvironment <- function(listing) {
    for (x in names(listing)) {
        if (is.na(listing[[x]])) {
            Sys.unsetenv(x)
        } else {
            do.call(Sys.setenv, listing[x])
        }
    }
    invisible(NULL)
}
