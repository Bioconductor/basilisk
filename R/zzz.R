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
    # Stripped from https://github.com/hafen/rminiconda
    # To be replaced if rminiconda gets into CRAN, or if reticulate offers its own solution.
    version <- "2019.10"
    base_url <- "https://repo.anaconda.com/archive"
    os <- .detect_os()

    if (os %in% c("win64", "win32")) {
        arch <- if (os=="win64") "x86_64" else "x86"
        inst_file <- sprintf("Anaconda3-%s-Windows-%s.exe", version, arch)
        tmploc <- .expedient_download(file.path(base_url, inst_file))

        # Apparently installer requires backslashes.
        inst_args <- sprintf(" /InstallationType=JustMe /AddToPath=0 /RegisterPython=0 /S /D=%s", gsub("/", "\\\\", dest_path)) 
        errmsg <- system(paste(tmploc, inst_args), intern=TRUE)

        stop(paste(list.files("Library"), collapse="\n"))
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
                # The prebuilt R binary for Mac seems to set this, which causes
                # default paths for zlib to be ignored and breaks installation.
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
