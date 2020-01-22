# This tests that we can install basilisk-dependent packages.
# library(testthat); source("test-package.R")

test_that("internal test package installs correctly", {
    stuff <- devtools::check(system.file("example", package="basilisk"), document=FALSE, error_on="error")
    expect_identical(stuff$status, 0L)
})
