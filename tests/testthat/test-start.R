# Testing the start and stopping abilities of basilisk.
# All operations are performed via callr to avoid loading Python into the test session.
#
# library(testthat); source("setup.R"); source("test-start.R")

library(callr)

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

    expect_true(r(FUN, args=list(version="0.25.1", envir="my_package_A")))
    expect_true(r(FUN, args=list(version="0.24.1", envir="my_package_B")))

    # Respects persistence.
    expect_true(r(persistence_check, args=list(version="0.25.1", envir="my_package_A")))
    expect_true(r(persistence_check, args=list(version="0.24.1", envir="my_package_B")))
})

###########################################################

test_that("basilisk forks when possible", { # ... though on windows, this creates a new process.
    FUN <- function(version, envir) {
        # Copied from above. Do NOT put into a separate function,
        # as otherwise r() will not find it in its new namespace.
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="whee")

        proc <- basiliskStart(envir, global=FALSE)
        test.version <- basiliskRun(proc, fun=function() {
            reticulate::import("pandas")$`__version__`
        })
        expect_identical(version, test.version)

        expect_false(is.environment(proc))
        expect_false(reticulate::py_available())

        basiliskStop(proc)

        TRUE
    }

    expect_true(r(FUN, args=list(version="0.25.1", envir="my_package_A")))
    expect_true(r(FUN, args=list(version="0.24.1", envir="my_package_B")))

    # Forcing basilisk to fork by loading another version of Python in advance.
    expect_true(r(process_check, args=list(version="0.25.1", envir="my_package_A")))
    expect_true(r(process_check, args=list(version="0.24.1", envir="my_package_B")))

    # Respects persistence.
    expect_true(r(persistence_check, args=list(version="0.25.1", envir="my_package_A", global=FALSE)))
    expect_true(r(persistence_check, args=list(version="0.24.1", envir="my_package_B", global=FALSE)))
})

###########################################################

test_that("basilisk uses sockets as a fallback", {
    FUN <- function(version, envir) {
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="whee")

        proc <- basiliskStart(envir, global=FALSE)
        test.version <- basiliskRun(proc, fun=function() {
            reticulate::import("pandas")$`__version__`
        })
        expect_identical(version, test.version)

        expect_false(is.environment(proc))
        expect_false(reticulate::py_available())

        basiliskStop(proc)

        TRUE
    }

    expect_true(r(FUN, args=list(version="0.25.1", envir="my_package_A")))
    expect_true(r(FUN, args=list(version="0.24.1", envir="my_package_B")))

    # Forcing basilisk to use sockets by loading another version of Python in advance.
    expect_true(r(process_check, args=list(version="0.25.1", envir="my_package_A", fork=FALSE)))
    expect_true(r(process_check, args=list(version="0.24.1", envir="my_package_B", fork=FALSE)))

    # Respects persistence.
    expect_true(r(persistence_check, args=list(version="0.25.1", envir="my_package_A", global=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version="0.24.1", envir="my_package_B", global=FALSE, fork=FALSE)))
})
