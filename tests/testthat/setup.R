#################################################################
# Setting up the virtual environments.

Sys.setenv(WORKON_HOME="whee")
setupVirtualEnv('my_package_A', 'pandas==0.25.1')
setupVirtualEnv('my_package_B', 'pandas==0.24.1')

#################################################################
# Defining a helper function to check for correct persistence.

persistence_check <- function(version, envir, ...) {
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

process_check <- function(version, envir, ...) {
    library(basilisk)
    library(testthat)
    useBasilisk()
    Sys.setenv(WORKON_HOME="whee")

    proc <- basiliskStart(envir, global=FALSE)
    test.version <- basiliskRun(proc, fun=function() {
        reticulate::import("pandas")$`__version__`
    })
    expect_identical(version, test.version)

    expect_false(is.environment(proc))

    basiliskStop(proc)

    TRUE
}
