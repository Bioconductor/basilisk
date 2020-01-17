.onLoad <- function(libname, pkgname) {
    if (.is_roxygen_running(pkgname)) {
        # Hack to avoid installing into 'inst'
        # when devtools::document() is run.
        return(NULL)
    }

    dest_path <- .get_basilisk_dir(mustWork=FALSE)
    if (dir.exists(dest_path)) {
        # No-op if it is already installed.
        return(NULL)
    }

    .minstaller(dest_path, testing=FALSE)
}

.minstaller <- function(dest_path, testing=FALSE) {
    os <- .detect_os()
    version <- "2019.10"
    base_url <- "https://repo.anaconda.com/archive"

    if (os %in% c("win64", "win32")) {
        # DESTROYING the previous destination path. This avoids accumulating
        # multiple Anaconda installations when basilisk is reinstalled. This
        # also destroys the environments of the client packages, so we rely on
        # the clients to auto-repopulate with their environments based on their
        # .onLoad statements. Note, this isn't a problem on Linux and Mac where
        # the previous installation is in the R installation directory and
        # automatically removed on R package installation. 
        unlink(dirname(dest_path), recursive=TRUE)

        arch <- if (os=="win64") "x86_64" else "x86"
        inst_file <- sprintf("Anaconda3-%s-Windows-%s.exe", version, arch)
        tmploc <- .expedient_download(file.path(base_url, inst_file))

        # Using the same code as reticulate:::miniconda_installer_run.
        dir.create(dest_path, recursive = TRUE, showWarnings = FALSE)
        inst_args <- sprintf("/InstallationType=JustMe /RegisterPython=0 /S /D=%s", utils::shortPathName(dest_path))
        Sys.chmod(tmploc, mode = "0755")
        status <- system2(tmploc, inst_args)

    } else {
        # Stripped from https://github.com/hafen/rminiconda a long time ago...
        sysname <- if (os=="macosx") "MacOSX" else "Linux"
        inst_file <- sprintf("Anaconda3-%s-%s-x86_64.sh", version, sysname)

        if (testing) {
            # FOR INTERNAL TESTING ONLY, avoid re-installing anaconda every 
            # time we recompile the R package.
            dest_path2 <- file.path(path.expand("~/"), ".anaconda")
            if (!file.exists(dest_path2)) {
                tmploc <- .expedient_download(file.path(base_url, inst_file))
                inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path2)
                status <- system2("bash", inst_args)
            } else {
                status <- 0L
            }
            file.symlink(dest_path2, dest_path)

        } else {
            tmploc <- .expedient_download(file.path(base_url, inst_file))
            inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path)

            if (os=="macosx") {
                # The prebuilt R binary for Mac seems to set this variable,
                # which causes default paths for zlib to be ignored and breaks
                # installation. So, we unset it before attempting installation.
                status <- system(paste("unset DYLD_FALLBACK_LIBRARY_PATH; bash", inst_args))
            } else {
                status <- system2("bash", inst_args)
            }
        }
    }

    if (status != 0) {
        stop(sprintf("conda installation failed with status code '%s'", status))
    }

    NULL
}

#' @importFrom utils download.file
#' @importFrom methods is
.expedient_download <- function(url) {
    fname <- try({
        bfc <- BiocFileCache::BiocFileCache(ask=FALSE)
        BiocFileCache::bfcrpath(bfc, url) 
    })

    if (is(fname, "try-error")) {
        tmploc <- file.path(tempdir(), basename(url))
        download.file(url, tmploc)
        fname <- tmploc
    }

    fname
}
