# This checks that the version-controlling behavior of setupBasiliskEnv is correct.
# library(testthat); library(basilisk); source("setup.R"); source("test-install.R"); 

#############################

base.py <- basilisk:::.get_py_cmd(basilisk.utils::getBasiliskDir())

# Turning off these damn envvars, otherwise .basilisk_freeze doesn't behave.
old.retpy <- Sys.getenv("RETICULATE_PYTHON")
Sys.unsetenv("RETICULATE_PYTHON")

old.pypath <- Sys.getenv("PYTHONPATH")
Sys.unsetenv("PYTHONPATH")

#############################

target <- file.path(client.dir, "thingo")
env.py <- basilisk:::.get_py_cmd(target)

test_that("setupBasiliskEnv refuses to work without all specified versions", {
    unlink(target, recursive=TRUE)
    expect_error(setupBasiliskEnv(target, "numpy"), "versions must be explicitly specified")
})

test_that("setupBasiliskEnv obtains the correct version of the packages", {
    unlink(target, recursive=TRUE)
    expect_true(setupBasiliskEnv(target, c(old.pandas, old.pandas.deps)))

    incoming <- basilisk:::.basilisk_freeze(base.py)
    expect_true(test.pandas %in% incoming)
    expect_true(!any(old.pandas.deps %in% incoming))

    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))
    
    # Trying it again is a no-op.
    expect_false(setupBasiliskEnv(target, c(old.pandas, old.pandas.deps)))
})

test_that("setupBasiliskEnv will install Python 2.7 if requested", {
    unlink(target, recursive=TRUE)
    setupBasiliskEnv(target, "python=2.7")
    env.py <- basilisk:::.get_py_cmd(target)
    py.ver <- system2(env.py, "--version", stderr=TRUE, stdout=TRUE)
    expect_match(py.ver, "2\\.7")
})

#############################

if (old.retpy!="") {
    Sys.setenv(RETICULATE_PYTHON=old.retpy)
}
if (old.pypath!="") {
    Sys.setenv(PYTHONPATH=old.pypath)
}
