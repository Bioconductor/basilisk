# Testing the start and stopping abilities of basilisk.
# All operations are performed via callr to avoid loading Python into the test session.
#
# library(basilisk); library(testthat); source("setup.R"); source("test-start.R")

#################################################################

library(callr)

new.version <- sub(".*==", "", test.pandas)
old.version <- sub(".*==", "", old.pandas)
basilisk.utils::installConda()

tA <- file.path(client.dir, "my_package_A") 
setupBasiliskEnv(tA, c(test.pandas, test.pandas.deps))

tB <- file.path(client.dir, 'my_package_B')
setupBasiliskEnv(tB, c(old.pandas, old.pandas.deps))

#################################################################
# Defining a helper function to check for correct persistence.

persistence_check <- function(version, envir, ...) {
    library(basilisk)
    library(testthat)

    cl <- basiliskStart(envir, ...)

    basiliskRun(proc=cl, function(store) {
        # For R:
        X <- reticulate::import("pandas")
        assign(x="snake.in.my.shoes", X$`__version__`, envir=store)

        # For Python:
        reticulate::py_run_string(sprintf("greeting = 'howdy, %s'", X$`__version__`))

        NULL
    }, persist=TRUE)

    # Doesn't contaminate the parent session.
    expect_false(exists("snake.in.my.shoes"))

    # Variable persists to the next call.
    out <- basiliskRun(proc=cl, function(store) {
        get("snake.in.my.shoes", envir=store)
    }, persist=TRUE)
    expect_identical(out, version)

    # You can still call other stuff without persist=.
    out <- basiliskRun(proc=cl, function() {
        reticulate::py_run_string("greeting")$greeting
    })
    expect_identical(out, sprintf("howdy, %s", version))

    # Check that extra arguments are passed along correctly to another process.
    test.version <- basiliskRun(cl, fun=function(str, store) {
        paste0(str, "_", get("snake.in.my.shoes", envir=store))
    }, str="FOO", persist=TRUE)
    expect_identical(paste0("FOO_", version), test.version)

    basiliskStop(cl)

    TRUE
}

# Defining helper functions to check new process creation.

process_check <- function(version, envir, ...) {
    # Check code copied from related functions. Do NOT put into a separate function,
    # as otherwise r() will not find it in its new namespace.
    library(basilisk)
    library(testthat)

    proc <- basiliskStart(envir, shared=FALSE, ...)
    test.version <- basiliskRun(proc, fun=function() {
        reticulate::import("pandas")$`__version__`
    })
    expect_identical(version, test.version)

    # Check that extra arguments are passed along correctly to another process.
    test.version <- basiliskRun(proc, fun=function(str) {
        paste0(str, "_", reticulate::import("pandas")$`__version__`)
    }, str="FOO")
    expect_identical(paste0("FOO_", version), test.version)

    expect_false(is.environment(proc))
    expect_false(reticulate::py_available())

    basiliskStop(proc)

    TRUE
}

preloaded_check <- function(version, envir, ...) {
    # Checking what happens when Python is already loaded.
    library(basilisk)
    library(testthat)
    useBasiliskEnv(basilisk.utils::getCondaDir())

    proc <- basiliskStart(envir, ...)
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
    FUN <- function(version, envir) {
        library(basilisk)
        library(testthat)

        proc <- basiliskStart(envir)
        expect_true(is.environment(proc))

        test.version <- basiliskRun(proc, fun=function() {
            reticulate::import("pandas")$`__version__`
        })
        expect_identical(version, test.version)

        # Check that extra arguments are passed along correctly to the env.
        test.version <- basiliskRun(proc, fun=function(str) {
            paste0(str, "_", reticulate::import("pandas")$`__version__`)
        }, str="FOO")
        expect_identical(paste0("FOO_", version), test.version)

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

    expect_true(r(FUN, args=list(version=new.version, envir=tA)))
    expect_true(r(FUN, args=list(version=old.version, envir=tB)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir=tA)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tB)))
})

###########################################################

test_that("basilisk forks when possible", { # ... though on windows, this just uses sockets.
    expect_true(r(process_check, args=list(version=new.version, envir=tA)))
    expect_true(r(process_check, args=list(version=old.version, envir=tB)))

    # Forcing basilisk to use sockets by loading another virtual environment in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir=tA)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir=tB)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir=tA, shared=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tB, shared=FALSE)))
})

###########################################################

test_that("basilisk uses sockets correctly", {
    expect_true(r(process_check, args=list(version=new.version, envir=tA, fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir=tB, fork=FALSE)))

    # Forcing basilisk to use sockets by loading another virtual environment in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir=tA, fork=FALSE)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir=tB, fork=FALSE)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir=tA, shared=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tB, shared=FALSE, fork=FALSE)))
})

###########################################################

test_that("basilisk hits the fallback R", {
    setBasiliskForceFallback(TRUE)
    on.exit(setBasiliskForceFallback(FALSE))

    expect_true(r(process_check, args=list(version=new.version, envir=tA, fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir=tB, fork=FALSE)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir=tA, shared=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tB, shared=FALSE, fork=FALSE)))
})
