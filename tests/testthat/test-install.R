# This checks that the version-controlling behavior of setupBasiliskEnv is correct.
# library(testthat); library(basilisk); source("setup.R"); source("test-install.R"); 

target <- file.path(client.dir, "thingo")
env.py <- basilisk.utils::getPythonBinary(target)

test_that("setupBasiliskEnv refuses to work without all specified versions", {
    unlink(target, recursive=TRUE)
    expect_error(setupBasiliskEnv(target, "numpy"), "versions must be explicitly specified")
})

test_that("setupBasiliskEnv obtains the correct version of the packages", {
    unlink(target, recursive=TRUE, force=TRUE)
    expect_true(setupBasiliskEnv(target, c(test.pandas, test.pandas.deps)))
    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(test.pandas %in% incoming)
    expect_true(all(test.pandas.deps %in% incoming))

    unlink(target, recursive=TRUE, force=TRUE)
    expect_true(setupBasiliskEnv(target, c(old.pandas, old.pandas.deps)))
    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))
    
    # Trying it again is a no-op.
    expect_false(setupBasiliskEnv(target, c(old.pandas, old.pandas.deps)))
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
    expect_true(setupBasiliskEnv(target, old.pandas.deps, pip=old.pandas))

    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))
})
