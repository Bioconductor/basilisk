# Testing the start and stopping abilities of basilisk.
# All operations are performed via callr to avoid loading Python into the test session.
#
# library(basilisk); library(testthat); source("setup.R"); source("test-start.R")

library(callr)

new.version <- sub(".*==", "", test.pandas)
old.version <- sub(".*==", "", old.pandas)

###########################################################

test_that("basilisk directly loads Python when possible", {
    FUN <- function(version, envir, persist=FALSE) {
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="whee")

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

    # Unaffected by persist=TRUE.
    expect_true(r(FUN, args=list(version=new.version, envir="my_package_A", persist=TRUE)))
    expect_true(r(FUN, args=list(version=old.version, envir="my_package_B", persist=TRUE)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B")))
})

###########################################################

test_that("basilisk forks when possible", { # ... though on windows, this just uses sockets.
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_B")))

    # Forcing basilisk to fork by loading another version of Python in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir="my_package_A")))
    expect_true(r(preloaded_check, args=list(version=old.version, envir="my_package_B")))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A", shared=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B", shared=FALSE)))
})

###########################################################

test_that("basilisk uses sockets as a fallback", {
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A", fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_B", fork=FALSE)))

    # Forcing basilisk to use sockets by loading another version of Python in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir="my_package_A", fork=FALSE)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir="my_package_B", fork=FALSE)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A", shared=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B", shared=FALSE, fork=FALSE)))
})

###########################################################

test_that("basilisk works with persistent processes", {
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A", persist=TRUE, fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_B", persist=TRUE, fork=FALSE)))

    out <- r(function(version) {
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="whee")

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
