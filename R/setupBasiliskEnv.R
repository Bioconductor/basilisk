#' Set up a \pkg{basilisk} environments
#'
#' Set up a Python conda environment for isolated execution of Python code with appropriate versions of all Python packages.
#' 
#' @param envpath String containing the path to the environment to use. 
#' @param packages Character vector containing the names of conda packages to install into the environment.
#' Version numbers must be included.
#' @param pip Character vector containing the names of additional packages to install from PyPi using pip.
#' Version numbers must be included.
#' 
#' @return 
#' A conda environment is created at \code{envpath} containing the specified \code{packages}.
#' The function will return a logical scalar indicating whether creation was performed,
#' which will be \code{FALSE} if the environment already exists.
#'
#' @details
#' \pkg{basilisk} environments are simply Python conda environments that are created and managed by \pkg{basilisk}.
#' Each \pkg{basilisk} environment can contain different Python packages with different versions,
#' allowing us to avoid version conflicts within an R session when different Bioconductor packages (or even different functions within a single package) require incompatible versions of Python packages.
#' 
#' Developers of client packages should never need to call this function directly.
#' For typical usage, \code{setupBasiliskEnv} is automatically called by \code{\link{basiliskStart}} to perform lazy installation.
#' Developers should also create \code{configure(.win)} files to call \code{\link{configureBasiliskEnv}},
#' which will call \code{setupBasiliskEnv} during R package installation when \code{BASILISK_USE_SYSTEM_DIR} is set.
#'
#' If a \pkg{basilisk} environment is already present at \code{envpath}, \code{setupBasiliskEnv} is a no-op.
#' This ensures that the function only installs the packages once.
#'
#' @section Versioning:
#' Pinned version numbers must be present for all requested conda packages in \code{packages}.
#' This improved predictability makes debugging much easier when the R package is installed and executed on different systems.
#' Note that this refers to conda packages, not Python packages, where the version notation for the former uses a single \code{=};
#' any \code{==} will be coerced to \code{=} automatically.
#'
#' It is possible to use the \code{pip} argument to install additional packages from PyPi after all the conda packages are installed.
#' All packages listed here are also expected to have pinned versions, this time using the \code{==} notation.
#' However, some caution is required when mixing packages from conda and pip,
#' see \url{https://www.anaconda.com/using-pip-in-a-conda-environment} for more details.
#'
#' It is also good practice to explicitly list the versions of the dependencies of all desired packages.
#' This protects against future changes in the behavior of your code if conda's dependency resolver defaults to a different version of a required package.
#' We suggest using \code{conda env export} to identify relevant dependencies and include them in \code{packages};
#' the only reason that pinned dependencies are not mandatory is because some dependencies are OS-specific,
#' requiring some manual pruning of the output of \code{conda env export}.
#'
#' It is possible to specify a different version of Python in \code{packages} by supplying, e.g., \code{"python=2.7.10"}.
#' If no Python version is listed, the version in the Anaconda installation is used by default.
#'
#' @examples
#' tmploc <- file.path(tempdir(), "my_package_A")
#' setupBasiliskEnv(tmploc, c('pandas=0.25.3',
#'     "python-dateutil=2.8.1", "pytz=2019.3"))
#'
#' @seealso
#' \code{\link{listCorePackages}}, for a list of core Python packages with pinned versions.
#'
#' @export
#' @importFrom basilisk.utils getBasiliskDir installAnaconda getCondaBinary getPythonBinary isWindows
#' @importFrom reticulate conda_install
setupBasiliskEnv <- function(envpath, packages, pip=NULL) {
    if (file.exists(envpath)) {
        return(FALSE)
    }

    packages <- sub("==", "=", packages)
    .check_versions(packages, "=")

    installAnaconda() # no-ops if it's already there.

    # Unsetting this variable, otherwise it seems to override the python=
    # argument in virtualenv_create() (see LTLA/basilisk#1).
    old.retpy <- Sys.getenv("RETICULATE_PYTHON")
    if (old.retpy!="") {
        Sys.unsetenv("RETICULATE_PYTHON")
        on.exit(Sys.setenv(RETICULATE_PYTHON=old.retpy), add=TRUE)
    }

    old.pypath <- Sys.getenv("PYTHONPATH")
    if (old.pypath!="") {
        Sys.unsetenv("PYTHONPATH")
        on.exit(Sys.setenv(PYTHONPATH=old.pypath), add=TRUE)
    }

    base.dir <- getBasiliskDir()
    conda.cmd <- getCondaBinary(base.dir)
    py.cmd <- getPythonBinary(base.dir)

    # Checking if a Python version was specified in 'packages'.
    if (any(is.py <- grepl("^python=", packages))) {
        version <- sub("^python=+", "", packages[is.py][1])
    } else {
        version <- sub("^Python ", "", system2(py.cmd, "--version", stdout=TRUE))
    }

    # We ensure that it exists and installs to the specified location,
    # rather than being tucked away in Anaconda's 'envs' directory.
    dir.create(envpath, recursive=TRUE) 
    conda_install(envname=normalizePath(envpath), conda=conda.cmd, 
        python_version=version, packages=packages)

    if (length(pip)) {
        if (isWindows()) {
            # Motivated by ContinuumIO/anaconda-issues#10576
            old.val <- Sys.getenv("CONDA_DLL_SEARCH_MODIFICATION_ENABLE")
            on.exit(Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE=old.val), add=TRUE)
            Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE=1)
        }

        .check_versions(pip, "==")
        env.py <- getPythonBinary(envpath)
        result <- system2(env.py, c("-m", "pip", "install", pip))
        if (result!=0L) {
            stop("failed to install additional packages via pip")
        }
    }
    
    TRUE 
}

.basilisk_freeze <- function(py.cmd) {
    # Unsetting the PYTHONPATH to avoid freezing other versions.
    old.pypath <- Sys.getenv("PYTHONPATH")
    if (old.pypath!="") {
        Sys.unsetenv("PYTHONPATH")
        on.exit(Sys.setenv(PYTHONPATH=old.pypath), add=TRUE)
    }
    system2(py.cmd, c("-m", "pip", "freeze"), stdout=TRUE)
}

.check_versions <- function(packages, pattern) {
    if (any(failed <- !grepl(pattern, packages))) {
        stop(paste("versions must be explicitly specified for",
            paste(sprintf("'%s'", packages[failed]), collapse=", ")))
    }
}
