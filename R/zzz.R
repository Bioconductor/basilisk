#' @importFrom utils download.file
.onLoad <- function(libname, pkgname) {
    inst_path <- system.file(package=pkgname)
    dest_path <- file.path(inst_path, "inst", "miniconda")
    if (dir.exists(dest_path)) {
        return(NULL)
    }
    dir.create(dirname(dest_path), recursive=TRUE)

    # Stripped from https://github.com/hafen/rminiconda
    # To be replaced if rminiconda gets into CRAN.
    arch <- paste0("x86", ifelse(.Machine$sizeof.pointer == 8, "_64", ""))
    version <- "4.7.10"
    base_url <- "https://repo.anaconda.com/miniconda/"

    if (.Platform$OS.type=="windows") {
        inst_file <- sprintf("Miniconda%s-latest-Windows-%s.exe", version, arch)
        tmploc <- file.path(tempdir(), inst_file)
        download.file(paste0(base_url, inst_file), tmploc)
        inst_args <- sprintf(" /InstallationType=JustMe /RegisterPython=0 /S /D=%s", dest_path)
        system2(tmploc, inst_args)

    } else {
        sysname <- if (Sys.info()[["sysname"]] == "Darwin") {
            "MacOSX"
        } else {
            "Linux"
        }

        inst_file <- sprintf("Miniconda3-%s-%s-%s.sh", version, sysname, arch)
        tmploc <- file.path(tempdir(), inst_file)

        if (FALSE) {
            # FOR INTERNAL TESTING ONLY, avoid re-downloading and 
            # re-installing miniconda everytime we update the R package.
            dest_path2 <- file.path(path.expand("~/"), ".miniconda")
            if (!file.exists(dest_path2)) {
                download.file(paste0(base_url, inst_file), tmploc)
                inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path2)
                system2("bash", inst_args)
            }

            file.symlink(dest_path2, dest_path)
        } else {
            download.file(paste0(base_url, inst_file), tmploc)
            inst_args <- sprintf(" %s -b -p %s", tmploc, dest_path)
            system2("bash", inst_args)
        }
    }

    NULL
}
