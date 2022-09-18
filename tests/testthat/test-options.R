# Tests that the global options work as expected.
# library(testthat); library(basilisk); source("test-options.R")

test_that("global sharing option setting/getting works as expected", {
    out <- getBasiliskShared()
    expect_true(length(out)==1L && !is.na(out))

    setBasiliskShared(FALSE)
    expect_false(getBasiliskShared())

    setBasiliskShared(TRUE)
    expect_true(getBasiliskShared())

    setBasiliskShared(out)
})

test_that("global fork option setting/getting works as expected", {
    out <- getBasiliskFork()
    expect_true(length(out)==1L && !is.na(out))

    setBasiliskFork(FALSE)
    expect_false(getBasiliskFork())

    setBasiliskFork(TRUE)
    expect_true(getBasiliskFork())

    setBasiliskFork(out)
})

test_that("check version setting/getting works as expected", {
    out <- getBasiliskCheckVersions()
    expect_true(out)

    setBasiliskCheckVersions(FALSE)
    expect_false(getBasiliskCheckVersions())

    setBasiliskCheckVersions(TRUE)
    expect_true(getBasiliskCheckVersions())

    setBasiliskCheckVersions(out)
})
