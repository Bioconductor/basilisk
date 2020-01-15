# This tests that we can install basilisk-dependent packages.
# library(testthat); source("test-package.R")

test_that("internal test package installs correctly", {
    expect_error(devtools::install_local(system.file("example", package="basilisk"), force=TRUE), NA)

    xpath <- system.file(package="son.of.basilisk")
    expect_true(file.exists(file.path(xpath, "basilisk", "env1")))
    expect_false(file.exists(file.path(xpath, "basilisk", "env2")))

    output <- son.of.basilisk::test()

    expect_type(output$pandas, "character")
    expect_true(length(output$pandas) > 0L)

    expect_type(output$sklearn, "character")
    expect_true(length(output$sklearn) > 0L)
})
