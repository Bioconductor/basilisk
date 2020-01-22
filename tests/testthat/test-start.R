# Testing the start and stopping abilities of basilisk.
# All operations are performed via callr to avoid loading Python into the test session.
#
# library(basilisk); library(testthat); source("setup.R"); source("test-start.R")

#################################################################

library(callr)

new.version <- sub(".*==", "", test.pandas)
old.version <- sub(".*==", "", old.pandas)

tA <- file.path(client.dir, 'my_package_A')
tB <- file.path(client.dir, 'my_package_B')
tC <- file.path(client.dir, 'my_package_C')

setupBasiliskEnv(tA, test.pandas) # This doesn't do anything, as it just uses the common installation.
setupBasiliskEnv(tB, c(old.pandas, old.pandas.deps))
setupBasiliskEnv(tC, c(old.pandas, old.pandas.deps), conda=TRUE)

setupBasiliskEnv(file.path(client.dir, 'occupier'), c(old.pandas, old.pandas.deps)) # for use in preloaded_check.

#################################################################
# Defining a helper function to check for correct persistence.

persistence_check <- function(version, envir, ...) {
    library(basilisk)
    library(testthat)

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

    expect_false(is.environment(proc))
    expect_false(reticulate::py_available())

    basiliskStop(proc)

    TRUE
}

preloaded_check <- function(version, envir, ...) {
    # Checking what happens when Python is already loaded.
    library(basilisk)
    library(testthat)
    useBasiliskEnv("install-test-client/occupier")

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
    skip_on_os('windows') # don't know why this doesn't load Python directly... don't care.

    FUN <- function(version, envir) {
        library(basilisk)
        library(testthat)
        Sys.setenv(WORKON_HOME="install-test-client")

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

    expect_true(r(FUN, args=list(version=new.version, envir=tA)))
    expect_true(r(FUN, args=list(version=old.version, envir=tB)))
    expect_true(r(FUN, args=list(version=old.version, envir=tC)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir=tA)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tB)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tC)))
})

###########################################################

test_that("basilisk forks when possible", { # ... though on windows, this just uses sockets.
    expect_true(r(process_check, args=list(version=new.version, envir=tA)))
    expect_true(r(process_check, args=list(version=new.version, envir=tA)))
    expect_true(r(process_check, args=list(version=old.version, envir=tB)))
    expect_true(r(process_check, args=list(version=old.version, envir=tC)))

    # Forcing basilisk to use sockets by loading another virtual environment in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir=tA)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir=tB)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir=tC)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir=tA, shared=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tB, shared=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tC, shared=FALSE)))
})

###########################################################

test_that("basilisk uses sockets as a fallback", {
    expect_true(r(process_check, args=list(version=new.version, envir=tA, fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir=tB, fork=FALSE)))
    expect_true(r(process_check, args=list(version=old.version, envir=tC, fork=FALSE)))

    # Forcing basilisk to use sockets by loading another virtual environment in advance.
    expect_true(r(preloaded_check, args=list(version=new.version, envir=tA, fork=FALSE)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir=tB, fork=FALSE)))
    expect_true(r(preloaded_check, args=list(version=old.version, envir=tC, fork=FALSE)))

    # Respects persistence of variables.
    expect_true(r(persistence_check, args=list(version=new.version, envir=tA, shared=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tB, shared=FALSE, fork=FALSE)))
    expect_true(r(persistence_check, args=list(version=old.version, envir=tC, shared=FALSE, fork=FALSE)))
})
