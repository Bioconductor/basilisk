.coerce_env_vars <- function(envpath=NULL) {
    ADD <- function(listing, var) {
        previous <- Sys.getenv(var, unset=NA)
        if (!is.na(previous)) {
            listing[[var]] <- previous
        }
        listing
    }

    output <- list()
    output <- ADD(output, "PYTHONPATH")

    # Don't even try to be nice and add an on.exit() clause to protect the
    # global session. This is deliberate; if we're using a virtual environment,
    # and someone tries to import package in the global session, and Python
    # looks somewhere else other than our virtual environment via the
    # PYTHONPATH, we can get the wrong package loaded. 
    Sys.unsetenv("PYTHONPATH")

    # This also needs to be unset otherwise it seems to take priority over
    # everything, even if you explicitly request to use a specific conda env's
    # Python (see LTLA/basilisk#1).
    output <- ADD(output, "RETICULATE_PYTHON")
    Sys.unsetenv("RETICULATE_PYTHON")

    output <- ADD(output, "RETICULATE_PYTHON_ENV")
    Sys.unsetenv("RETICULATE_PYTHON_ENV")

    # Activating the conda environment, to set up the proper PATH (especially
    # on Windows, where this is used for DLL look-up).
    output <- .activate_condaenv(output, envpath)

    output
}

#' @importFrom utils getFromNamespace
#' @importFrom basilisk.utils isWindows getBasiliskDir
.activate_condaenv <- function(listing, envpath) {
    if (isWindows()) {
        act.bat <- file.path(getBasiliskDir(), "condabin", "conda.bat")
        act.cmd <- c(shQuote(act.bat), "activate")
        if (!is.null(envpath)) {
            act.cmd <- c(act.cmd, shQuote(envpath))
        }
    } else {
        profile.sh <- file.path(getBasiliskDir(), "etc", "profile.d", "conda.sh")
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

.restore_env_vars <- function(listing) {
    for (x in names(listing)) {
        if (is.na(listing[[x]])) {
            Sys.unsetenv(x)
        } else {
            do.call(Sys.setenv, listing[x])
        }
    }
}
