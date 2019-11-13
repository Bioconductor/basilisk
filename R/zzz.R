.onLoad <- function(libname, pkgname) {
    inst_path <- system.file(package=pkgname)
    if (basename(inst_path)!=pkgname) {
        # Hack to avoid installing into 'inst'
        # when devtools::document() is run.
        return(NULL)
    }

    dest_path <- file.path(inst_path, .mc_dir)
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
    arch <- paste0("x86", ifelse(.Machine$sizeof.pointer == 8, "_64", ""))
    version <- "4.7.10"
    base_url <- "https://repo.anaconda.com/miniconda/"

    if (.Platform$OS.type=="windows") {
        inst_file <- sprintf("Miniconda%s-latest-Windows-%s.exe", version, arch)
        tmploc <- .expedient_download(paste0(base_url, inst_file))
        inst_args <- sprintf(" /InstallationType=JustMe /RegisterPython=0 /S /D=%s", dest_path)
        system2(tmploc, inst_args)

    } else {
        sysname <- if (Sys.info()[["sysname"]] == "Darwin") {
            "MacOSX"
        } else {
            "Linux"
        }

        inst_file <- sprintf("Miniconda3-%s-%s-%s.sh", version, sysname, arch)

        if (testing) {
            # FOR INTERNAL TESTING ONLY, avoid re-downloading and 
            # re-installing miniconda everytime we update the R package.
            dest_path2 <- file.path(path.expand("~/"), ".miniconda")
            if (!file.exists(dest_path2)) {
                tmploc <- .expedient_download(paste0(base_url, inst_file))
                inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path2)
                system2("bash", inst_args)
            }

            file.symlink(dest_path2, dest_path)
        } else {
            tmploc <- .expedient_download(paste0(base_url, inst_file))
            inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path)
            system2("bash", inst_args)
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
