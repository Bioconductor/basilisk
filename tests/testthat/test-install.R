# This checks that the version-controlling behavior of setupVirtualEnv is correct.
# library(testthat); library(basilisk); source("setup.R"); source("test-install.R"); 

client.dir <- "install-test-client"
dir.create(client.dir)
Sys.setenv(WORKON_HOME=client.dir)

# Installing a fresh version of Python.
basilisk.dir <- "install-test-basilisk" 
basilisk:::.minstaller(basilisk.dir)

test.py <- basilisk:::.get_py_cmd(basilisk.dir)
Sys.setenv(BASILISK_TEST_PYTHON=test.py)

Sys.setenv(BASILISK_TEST_COMMON=normalizePath(client.dir)) # normalization required for correct symlinks.
reticulate::virtualenv_create(basilisk:::.common_env, python=test.py)

#############################

test_that("setupVirtualEnv refuses to work without all specified versions", {
    expect_error(setupVirtualEnv("thingo", "pandas"), "must be explicitly specified")
    expect_error(setupVirtualEnv("thingo", test.pandas), "need to list dependency")
    expect_error(setupVirtualEnv("thingo", c(test.pandas, old.pandas)), "redundant listing")
})

test_that("setupVirtualEnv switches to a core installation when possible", {
    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_false(any(grepl("numpy==", incoming)))

    unlink(file.path(client.dir, "thingo"), recursive=TRUE)
    expect_error(setupVirtualEnv("thingo", c(test.pandas, test.pandas.deps)), NA)

    core <- listCorePackages()$full
    expected.numpy <- core[grep("numpy==", core)]
    incoming <- basilisk:::.basilisk_freeze(test.py)
    observed.numpy <- incoming[grep("numpy==", incoming)]
    expect_identical(expected.numpy, observed.numpy)
})

all.core <- listCorePackages()
test.numpy <- all.core$full[all.core$name=="numpy"]
old.numpy <- "numpy==1.15.1"

test_that("setupVirtualEnv overrides an incompatible core installation", {
    unlink(file.path(client.dir, "thingo"), recursive=TRUE)
    expect_error(setupVirtualEnv("thingo", old.numpy), NA)

    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true(test.numpy %in% incoming)

    env.py <- basilisk:::.get_py_cmd(file.path(client.dir, "thingo"))
    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(old.numpy %in% incoming)
})

test_that("setupVirtualEnv allows core packages to have unspecified versions", {
    unlink(file.path(client.dir, "thingo"), recursive=TRUE)
    system2(test.py, c("-m", "pip", "uninstall", "-y", "numpy"))

    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_false(test.numpy %in% incoming)

    expect_error(setupVirtualEnv("thingo", "numpy"), NA)
    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true(test.numpy %in% incoming)
})

test_that("setupVirtualEnv skips virtual environment creation for pure core packages", {
    system2(test.py, c("-m", "pip", "uninstall", "-y", "numpy"))

    test.scipy <- all.core$full[all.core$name=="scipy"]
    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_false(test.scipy %in% incoming)
    expect_false(test.numpy %in% incoming)

    expect_error(setupVirtualEnv("thingo1", "scipy"), NA)
    expect_error(setupVirtualEnv("thingo2", "scipy"), NA)

    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true(test.scipy %in% incoming)
    expect_true(test.numpy %in% incoming)
    
    # Makes links rather than new environments.
    s1 <- Sys.readlink(file.path(client.dir, "thingo1"))
    s2 <- Sys.readlink(file.path(client.dir, "thingo2"))
    expect_false(identical(s1, ""))
    expect_identical(s1, s2)

    FUN <- function(envir1, envir2, test.py, client.dir) {
        library(basilisk)
        library(testthat)
        Sys.setenv(BASILISK_TEST_PYTHON=test.py)
        Sys.setenv(WORKON_HOME=client.dir)

        proc <- basiliskStart(envir1)
        expect_true(is.environment(proc))
        test.version <- basiliskRun(proc, fun=function() {
            reticulate::import("scipy")$`__version__`
        })
        basiliskStop(proc)

        # Avoid creating a new process as the Python versions are not contradicting.
        proc2 <- basiliskStart(envir2)
        expect_true(is.environment(proc2))
        test.version2 <- basiliskRun(proc2, fun=function() {
            reticulate::import("scipy")$`__version__`
        })
        basiliskStop(proc2)

        expect_identical(test.version, test.version2)

        TRUE
    }

    library(callr)
    expect_true(r(FUN, args=list(envir1="thingo1", envir2="thingo2", test.py=test.py, client.dir=client.dir), show=TRUE))
})

#############################

unlink(client.dir, recursive=TRUE)
unlink(basilisk.dir, recursive=TRUE)
Sys.unsetenv("BASILISK_TEST_PYTHON")
Sys.unsetenv("BASILISK_TEST_COMMON")
