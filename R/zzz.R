.onLoad <- function(libname, pkgname) {
    inst_path <- system.file(package=pkgname)
    if (basename(inst_path)!=pkgname) {
        # Hack to avoid installing into 'inst'
        # when devtools::document() is run.
        return(NULL)
    }

    dest_path <- file.path(inst_path, .core_dir)
    if (dir.exists(dest_path)) {
        # No-op if it is already installed.
        return(NULL)
    }

    .minstaller(dest_path, testing=FALSE)

    # Create a common virtual environment.
    setupVirtualEnv(.common_env, character(0), pkgname="basilisk")
}

.minstaller <- function(dest_path, testing=FALSE) {
    # Stripped from https://github.com/hafen/rminiconda a long time ago...
    # To be replaced if reticulate offers its own solution.
    version <- "2019.10"
    base_url <- "https://repo.anaconda.com/archive"
    os <- .detect_os()

    if (os %in% c("win64", "win32")) {
        arch <- if (os=="win64") "x86_64" else "x86"

        ### TWILIGHT ZONE START ###

        # Apparently Anaconda files get quarantined by some security check on Windows,
        # so we try installing an older version.
        inst_file <- sprintf("Anaconda3-2019.03-Windows-%s.exe", arch)
        tmploc <- .expedient_download(file.path(base_url, inst_file))

        # Apparently installer requires backslashes.
        backpath <- gsub("/", "\\\\", dest_path)
        inst_args <- sprintf("/InstallationType=JustMe /AddToPath=0 /RegisterPython=0 /S /D=%s", backpath)
        system2(tmploc, inst_args)

        ### TWILIGHT ZONE END ###

    } else {
        sysname <- if (os=="macosx") "MacOSX" else "Linux"
        inst_file <- sprintf("Anaconda3-%s-%s-x86_64.sh", version, sysname)

        if (testing) {
            # FOR INTERNAL TESTING ONLY, avoid re-downloading and 
            # re-installing miniconda everytime we update the R package.
            dest_path2 <- file.path(path.expand("~/"), ".anaconda")
            if (!file.exists(dest_path2)) {
                tmploc <- .expedient_download(file.path(base_url, inst_file))
                inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path2)
                system2("bash", inst_args)
            }
            file.symlink(dest_path2, dest_path)

        } else {
            tmploc <- .expedient_download(file.path(base_url, inst_file))
            inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path)

            if (os=="macosx") {
                # The prebuilt R binary for Mac seems to set this variable,
                # which causes default paths for zlib to be ignored and breaks
                # installation. So, we unset it before attempting installation.
                system(paste("unset DYLD_FALLBACK_LIBRARY_PATH; bash", inst_args))
            } else {
                system2("bash", inst_args)
            }
        }
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
