# This tests the lazy installation of conda. 
# library(testthat); library(basilisk); source("setup.R"); source("test-lazy.R")

tmp <- tempfile()
dir.create(tmp)

old <- basilisk.utils::setVariable("BASILISK_USE_SYSTEM_DIR", NA)
old.2 <- basilisk.utils::setVariable("BASILISK_EXTERNAL_DIR", tmp)

test_that("lazy conda installation works as expected", {
    skip_on_os("windows") # avoid problems with long paths on Windows.

    version <- as.character(packageVersion("basilisk"))
    placeholder <- file.path(tmp, paste0(version, "000"))
    dir.create(placeholder, showWarnings=FALSE)
    expect_true(file.exists(placeholder))

    expect_true(basilisk.utils::installConda())
    target <- file.path(tmp, packageVersion("basilisk"), "0")
    expect_true(file.exists(target))
    expect_false(file.exists(placeholder))

    # Skips the next evaluation, as we've already created it.
    expect_false(basilisk.utils::installConda())
    expect_true(file.exists(target))
})

test_that(".obtainEnvironmentPath works as expected", {
    testpkg <- "basilisk.utils"
    env <- BasiliskEnvironment(envname="test", pkgname=testpkg, packages=test.pandas)
    dummy <- file.path(basilisk.utils::getExternalDir(), paste0(testpkg, "-", packageVersion(testpkg), "0"))
    dir.create(dummy, recursive=TRUE)

    # Destruction of alternative versions works properly.
    old.d <- basilisk.utils::setVariable("BASILISK_NO_DESTROY", NA)
    out <- basilisk:::.obtainEnvironmentPath(env)
    expect_true(file.exists(out))
    expect_false(file.exists(dummy))

    # No-ops if the thing already exists.
    dir.create(dummy)
    out <- basilisk:::.obtainEnvironmentPath(env)
    expect_true(file.exists(out))
    expect_true(file.exists(dummy))

    # Also behaves with NO_DESTROY=1.
    basilisk.utils::unlink2(out)
    basilisk.utils::setVariable("BASILISK_NO_DESTROY", "1")

    out <- basilisk:::.obtainEnvironmentPath(env)
    expect_true(file.exists(out))
    expect_true(file.exists(dummy))

    basilisk.utils::setVariable("BASILISK_NO_DESTROY", old.d)
})

basilisk.utils::setVariable("BASILISK_USE_SYSTEM_DIR", old)
basilisk.utils::setVariable("BASILISK_EXTERNAL_DIR", old.2)
