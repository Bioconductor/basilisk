# This tests the lazy installation of an Anaconda installation via installAnaconda().
# We put this here as it depends on an existing installation of Anaconda.
# library(testthat); library(basilisk); source("test-lazy.R")

test_that("lazy Anaconda installation works as expected", {
    skip_on_os("windows") # avoid problems with long paths on Windows.

    tmp <- tempfile()
    dir.create(tmp)

    old <- Sys.getenv("BASILISK_USE_SYSTEM_DIR")
    Sys.unsetenv("BASILISK_USE_SYSTEM_DIR")
    old.2 <- Sys.getenv("BASILISK_EXTERNAL_DIR")
    Sys.setenv(BASILISK_EXTERNAL_DIR=tmp)

    version <- as.character(packageVersion("basilisk"))
    placeholder <- file.path(tmp, paste0(version, "000"))
    dir.create(placeholder, showWarnings=FALSE)
    expect_true(file.exists(placeholder))

    expect_true(basilisk.utils::installAnaconda())
    target <- file.path(tmp, packageVersion("basilisk"), "0")
    expect_true(file.exists(target))
    expect_false(file.exists(placeholder))

    # Skips the next evaluation, as we've already created it.
    expect_false(basilisk.utils::installAnaconda())
    expect_true(file.exists(target))

    Sys.setenv(BASILISK_EXTERNAL_DIR=old.2)
    Sys.setenv(BASILISK_USE_SYSTEM_DIR=old)
})

