# This checks that the version-controlling behavior of setupBasiliskEnv is correct.
# library(testthat); library(basilisk); source("setup.R"); source("test-install.R"); 

basilisk.utils::installConda()
target <- file.path(client.dir, "thingo")

test_that("setupBasiliskEnv refuses to work without all specified versions", {
    unlink(target, recursive=TRUE)
    expect_error(setupBasiliskEnv(target, "numpy"), "versions must be explicitly specified")
})

test_that("setupBasiliskEnv obtains the correct version of the packages", {
    unlink(target, recursive=TRUE, force=TRUE)
    setupBasiliskEnv(target, c(test.pandas, test.pandas.deps))
    incoming <- basilisk:::.basilisk_freeze(target)
    expect_true(test.pandas %in% incoming)
    expect_true(all(test.pandas.deps %in% incoming))

    unlink(target, recursive=TRUE, force=TRUE)
    setupBasiliskEnv(target, c(old.pandas, old.pandas.deps))
    incoming <- basilisk:::.basilisk_freeze(target)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))
})

test_that("setupBasiliskEnv will install Python 2.7 if requested", {
    unlink(target, recursive=TRUE, force=TRUE)
    setupBasiliskEnv(target, "python=2.7")
    env.py <- basilisk.utils::getPythonBinary(target)
    py.ver <- system2(env.py, "--version", stderr=TRUE, stdout=TRUE)
    expect_match(py.ver, "2\\.7")
})

test_that("setupBasiliskEnv works with pip-hosted packages", {
    unlink(target, recursive=TRUE, force=TRUE)
    setupBasiliskEnv(target, old.pandas.deps, pip=old.pandas)

    incoming <- basilisk:::.basilisk_freeze(target)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))
})

test_that("setupBasiliskEnv destroys directory on error", {
    basilisk.utils::unlink2(target)
    expect_error(setupBasiliskEnv(target, package="WHHEEEEEEEEEEEEEEEEEE==0.0.1"), 'failed to install')
    expect_false(file.exists(target))
})
