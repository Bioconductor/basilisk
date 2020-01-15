# This tests that we can install basilisk-dependent packages.
# library(testthat); source("test-package.R")

old <- Sys.getenv("BASILISK_NONPKG_DIR")
Sys.unsetenv("BASILISK_NONPKG_DIR")

test_that("internal test package installs correctly", {
    expect_error(devtools::install_local(system.file("example", package="basilisk"), force=TRUE), NA)

    xpath <- basilisk:::.choose_env_dir("son.of.basilisk")
    expect_true(file.exists(file.path(xpath, "env1")))
    expect_false(file.exists(file.path(xpath, "env2"))) # defaults to the core installation.

    output <- son.of.basilisk::test()

    expect_type(output$pandas, "character")
    expect_true(length(output$pandas) > 0L)

    expect_type(output$sklearn, "character")
    expect_true(length(output$sklearn) > 0L)
})

if (old!="") {
    Sys.setenv(BASILISK_NONPKG_DIR=old)
}
