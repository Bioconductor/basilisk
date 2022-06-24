# This tests the lazy installation of conda. 
# library(testthat); library(basilisk); source("setup.R"); source("test-lazy.R")

tmp <- tempfile()
dir.create(tmp)

old <- basilisk.utils::setVariable("BASILISK_USE_SYSTEM_DIR", NA)
old.2 <- basilisk.utils::setVariable("BASILISK_EXTERNAL_DIR", tmp)

test_that("lazy conda installation works as expected", {
    skip_on_os("windows") # avoid problems with long paths on Windows.

    # Creating a placeholder to check if it gets deleted.
    placeholder <- file.path(tmp, "0.0.1")
    dir.create(placeholder, showWarnings=FALSE)
    dir.expiry::touchDirectory(placeholder, date=Sys.Date() - 1000)
    expect_true(file.exists(placeholder))

    expect_true(basilisk.utils::installConda())
    target <- file.path(tmp, packageVersion("basilisk"), "0")
    expect_true(file.exists(target))
    expect_false(file.exists(placeholder))

    # Skips the next evaluation, as we've already created it.
    expect_false(basilisk.utils::installConda())
    expect_true(file.exists(target))
})

test_that("obtainEnvironmentPath works as expected", {
    skip_on_os("windows") # see above.

    testpkg <- "basilisk.utils"
    env <- BasiliskEnvironment(envname="test", pkgname=testpkg, packages=test.pandas)
    dummy <- file.path(basilisk.utils::getExternalDir(), testpkg, "0.0.1")
    dir.create(dummy, recursive=TRUE)
    dir.expiry::touchDirectory(dummy, date=Sys.Date()-100)

    # Omits destruction with NO_DESTROY=1.
    old.d <- basilisk.utils::setVariable("BASILISK_NO_DESTROY", "1")
    out <- obtainEnvironmentPath(env)
    expect_true(file.exists(out))
    expect_true(file.exists(dummy))
    basilisk.utils::setVariable("BASILISK_NO_DESTROY", old.d)

    # Otherwise, destruction of older versions works properly.
    old.d <- basilisk.utils::setVariable("BASILISK_NO_DESTROY", NA)
    out <- obtainEnvironmentPath(env)
    expect_true(file.exists(out))
    expect_false(file.exists(dummy))
    basilisk.utils::setVariable("BASILISK_NO_DESTROY", old.d)
})

basilisk.utils::setVariable("BASILISK_USE_SYSTEM_DIR", old)
basilisk.utils::setVariable("BASILISK_EXTERNAL_DIR", old.2)
