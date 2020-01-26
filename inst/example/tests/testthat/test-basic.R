# This tests that we can install basilisk-dependent packages.
# library(testthat); source("test-basic.R")

test_that("internal test package installs correctly", {
    xpath <- basilisk.utils::getEnvironmentDir("son.of.basilisk")

    output <- son.of.basilisk::test()
    expect_true(file.exists(file.path(xpath, "env1")))
    expect_true(file.exists(file.path(xpath, "env2")))

    expect_type(output$pandas, "character")
    expect_true(length(output$pandas) > 0L)

    expect_type(output$sklearn, "character")
    expect_true(length(output$sklearn) > 0L)
})
