# This tests that we can install basilisk-dependent packages.
# library(testthat); source("test-package.R")

test_that("internal test package installs correctly", {
    lib.path <- tempfile('Rlib.')
    dir.create(lib.path, showWarnings=FALSE)
    old <- .libPaths()
    .libPaths(c(lib.path, old))

    install.packages(system.file("example", package="basilisk"),
        repos=NULL, type="source")

    library(son.of.basilisk)
    output <- son.of.basilisk::test()

    expect_type(output$pandas, "character")
    expect_true(length(output$pandas) > 0L)

    expect_type(output$sklearn, "character")
    expect_true(length(output$sklearn) > 0L)

    .libPaths(old)
})
