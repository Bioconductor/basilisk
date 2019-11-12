# Testing the start and stopping abilities of basilisk.
# All operations are performed via callr to avoid loading Python into the test session.
#
# library(basilisk); library(testthat); source("setup.R"); source("test-start.R")

library(callr)

new.version <- sub(".*==", "", test.pandas)
old.version <- sub(".*==", "", old.pandas)

###########################################################

test_that("basilisk directly loads Python when possible", {
    FUN <- function(version, envir) {
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="whee")

        proc <- basiliskStart(envir)
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

    # Respects persistence.
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

    # Respects persistence.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A", global=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B", global=FALSE)))
})

###########################################################

test_that("basilisk uses sockets as a fallback", {
    expect_true(r(process_check, args=list(version=new.version, envir="my_package_A", fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir="my_package_B", fork=FALSE)))

    # Forcing basilisk to use sockets by loading another version of Python in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir="my_package_A", fork=FALSE)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir="my_package_B", fork=FALSE)))

    # Respects persistence.
    expect_true(r(persistence_check, args=list(version=new.version, envir="my_package_A", global=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir="my_package_B", global=FALSE, fork=FALSE)))
})
