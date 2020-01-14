# Testing the start and stopping abilities of basilisk.
# All operations are performed via callr to avoid loading Python into the test session.
#
# library(basilisk); library(testthat); source("setup.R"); source("test-start.R")

#################################################################

library(callr)

new.version <- sub(".*==", "", test.pandas)
old.version <- sub(".*==", "", old.pandas)

setupBasiliskEnv('my_package_A', test.pandas) # This doesn't do anything, as it just uses the common installation.
setupBasiliskEnv('my_package_B', c(old.pandas, old.pandas.deps))
setupBasiliskEnv('my_package_C', c(old.pandas, old.pandas.deps), conda=TRUE)

setupBasiliskEnv('occupier', c(old.pandas, old.pandas.deps)) # for use in preloaded_check.

#################################################################
# Defining a helper function to check for correct persistence
# (of variables, not of the process, hence the persist=FALSE).

persistence_check <- function(version, envir, persist=FALSE, ...) {
    library(basilisk)
    library(testthat)
    Sys.setenv(BASILISK_NONPKG_DIR="install-test-client")

    cl <- basiliskStart(envir, ...)

    basiliskRun(proc=cl, function() {
        # For R:
        X <- reticulate::import("pandas")
        assign(x="snake.in.my.shoes", X$`__version__`, envir=parent.frame())

        # For Python:
        reticulate::py_run_string(sprintf("greeting = 'howdy, %s'", X$`__version__`))

        NULL
    })

    # Doesn't contaminate the parent session.
    expect_false(exists("snake.in.my.shoes"))

    # Variable persists to the next call.
    out <- basiliskRun(proc=cl, function() {
        get("snake.in.my.shoes", envir=parent.frame())
    })
    expect_identical(out, version)

    out <- basiliskRun(proc=cl, function() {
        reticulate::py_run_string("greeting")$greeting
    })
    expect_identical(out, sprintf("howdy, %s", version))

    basiliskStop(cl)

    TRUE
}

# Defining helper functions to check new process creation.

process_check <- function(version, envir, ..., persist=FALSE) {
    # Check code copied from related functions. Do NOT put into a separate function,
    # as otherwise r() will not find it in its new namespace.
    library(basilisk)
    library(testthat)
    Sys.setenv(BASILISK_NONPKG_DIR="install-test-client")

    proc <- basiliskStart(envir, shared=FALSE, ..., persist=persist)
    test.version <- basiliskRun(proc, fun=function() {
        reticulate::import("pandas")$`__version__`
    })
    expect_identical(version, test.version)

    expect_false(is.environment(proc))
    expect_false(reticulate::py_available())

    basiliskStop(proc)

    TRUE
}

preloaded_check <- function(version, envir, ..., persist=FALSE) {
    # Checking what happens when Python is already loaded.
    library(basilisk)
    library(testthat)
    useBasiliskEnv("occupier")
    Sys.setenv(BASILISK_NONPKG_DIR="install-test-client")

    proc <- basiliskStart(envir, ..., persist=persist)
    test.version <- basiliskRun(proc, fun=function() {
        reticulate::import("pandas")$`__version__`
    })
    expect_identical(version, test.version)

    expect_false(is.environment(proc))

    basiliskStop(proc)

    TRUE
}

#################################################################

test_that("basilisk directly loads Python when possible", {
    skip_on_os('windows') # don't know why this doesn't load Python directly... don't care.

    FUN <- function(version, envir, persist=FALSE) {
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="install-test-client")

        proc <- basiliskStart(envir, persist=persist)
        expect_true(is.environment(proc))

        test.version <- basiliskRun(proc, fun=function() {
            reticulate::import("pandas")$`__version__`
        })
        expect_identical(version, test.version)

        basiliskStop(proc)

        # Python and the same version of pandas also exist in the parent session.
        expect_true(reticulate::py_available())
        X <- reticulate::import("pandas")
        expect_identical(X$`__version__`, version)

        # Avoid creating a new process as the Python versions are not contradicting.
        proc2 <- basiliskStart(envir)
        expect_true(is.environment(proc2))
        basiliskStop(proc2)

        TRUE
    }

    expect_true(r(FUN, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(FUN, args=list(version=old.version, envir="my_package_B")))
    expect_true(r(FUN, args=list(version=old.version, envir="my_package_C")))

    # Unaffected by persist=TRUE.
    expect_true(r(FUN, args=list(version=new.version, envir="my_package_A", persist=TRUE)))
    expect_true(r(FUN, args=list(version=old.version, envir="my_package_B", persist=TRUE)))
    expect_true(r(FUN, args=list(version=old.version, envir="my_package_C", persist=TRUE)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B")))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_C")))
})

###########################################################

test_that("basilisk forks when possible", { # ... though on windows, this just uses sockets.
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_B")))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_C")))

    # Forcing basilisk to use sockets by loading another virtual environment in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(preloaded_check, args=list(version=old.version, envir="my_package_B")))
    expect_true(r(preloaded_check, args=list(version=old.version, envir="my_package_C")))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A", shared=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B", shared=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_C", shared=FALSE)))
})

###########################################################

test_that("basilisk uses sockets as a fallback", {
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A", fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_B", fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_C", fork=FALSE)))

    # Forcing basilisk to use sockets by loading another virtual environment in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir="my_package_A", fork=FALSE)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir="my_package_B", fork=FALSE)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir="my_package_C", fork=FALSE)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A", shared=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B", shared=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_C", shared=FALSE, fork=FALSE)))
})

###########################################################

test_that("basilisk works with persistent processes", {
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A", persist=TRUE, fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_B", persist=TRUE, fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_C", persist=TRUE, fork=FALSE)))

    out <- r(function(version) {
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="install-test-client")

        envname <- "my_package_A"
        pkgname <- NULL
        proc <- basiliskStart(envname, shared=FALSE)
        old.pid <- parallel::clusterCall(basilisk:::.get_persist(envname, pkgname), Sys.getpid)[[1]]

        # Same process is re-used.
        proc2 <- basiliskStart(envname, shared=FALSE)
        new.pid <- parallel::clusterCall(basilisk:::.get_persist(envname, pkgname), Sys.getpid)[[1]]
        expect_identical(old.pid, new.pid)

        test.version <- basiliskRun(proc2, fun=function() {
            reticulate::import("pandas")$`__version__`
        })
        expect_identical(version, test.version)

        # Reboots persistent process if it's been closed.
        actual_proc <- basilisk:::.get_persist(envname, pkgname)
        parallel::stopCluster(actual_proc)
        basilisk:::.set_persist(envname, pkgname, actual_proc)
        expect_error(parallel::clusterCall(basilisk:::.get_persist(envname, pkgname), Sys.getpid))

        reproc <- basiliskStart(envname, shared=FALSE)
        expect_error(parallel::clusterCall(basilisk:::.get_persist(envname, pkgname), Sys.getpid), NA)

        # Running basiliskStop doesn't have any effect unless persist=FALSE.
        basiliskStop(reproc)
        expect_error(parallel::clusterCall(basilisk:::.get_persist(envname, pkgname), Sys.getpid), NA)

        basiliskStop(reproc, persist=FALSE)
        expect_error(parallel::clusterCall(basilisk:::.get_persist(envname, pkgname), Sys.getpid))

        TRUE
    }, args=list(version=new.version))
    expect_true(out)
})
