# This tests that we can install basilisk-dependent packages.
# library(testthat); source("test-package.R")

old <- Sys.getenv("BASILISK_NONPKG_DIR")
Sys.unsetenv("BASILISK_NONPKG_DIR")

test_that("internal test package installs correctly", {
    expect_error(devtools::check(system.file("example", package="basilisk"), document=FALSE, error_on="error"), NA)
})

if (old!="") {
    Sys.setenv(BASILISK_NONPKG_DIR=old)
}
