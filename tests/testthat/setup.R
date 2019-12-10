#################################################################
# Setting up the virtual environments.

unlink("whee", recursive=TRUE)
Sys.setenv(WORKON_HOME="whee")

core.set <- listCorePackages()
test.pandas <- core.set$full[core.set$package=="pandas"]
setupVirtualEnv('my_package_A', test.pandas)

old.pandas <- "pandas==0.24.1"
old.pandas.deps <- c("python-dateutil==2.7.1", "pytz==2017.2")
setupVirtualEnv('my_package_B', c(old.pandas, old.pandas.deps))

#################################################################
# Defining a helper function to check for correct persistence 
# (of variables, not of the process, hence the persist=FALSE).

persistence_check <- function(version, envir, persist=FALSE, ...) {
    library(basilisk)
    library(testthat)
    Sys.setenv(WORKON_HOME="whee")

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

#################################################################
# Defining helper functions to check new process creation.

process_check <- function(version, envir, ..., persist=FALSE) {
    # Check code copied from related functions. Do NOT put into a separate function,
    # as otherwise r() will not find it in its new namespace.
    library(basilisk)
    library(testthat)
    Sys.setenv(WORKON_HOME="whee")

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
    useBasilisk()
    Sys.setenv(WORKON_HOME="whee")

    proc <- basiliskStart(envir, ..., persist=persist)
    test.version <- basiliskRun(proc, fun=function() {
        reticulate::import("pandas")$`__version__`
    })
    expect_identical(version, test.version)

    expect_false(is.environment(proc))

    basiliskStop(proc)

    TRUE
}
