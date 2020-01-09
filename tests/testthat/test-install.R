# This checks that the version-controlling behavior of setupBasiliskEnv is correct.
# library(testthat); library(basilisk); source("setup.R"); source("test-install.R"); 

client.dir <- "install-test-client"
dir.create(client.dir)
Sys.setenv(WORKON_HOME=client.dir)

# Installing a fresh version of Python.
basilisk.dir <- "install-test-basilisk" 
basilisk:::.minstaller(basilisk.dir)

Sys.setenv(BASILISK_TEST_CORE=basilisk.dir)
test.py <- basilisk:::.get_py_cmd(basilisk.dir)

Sys.setenv(BASILISK_TEST_COMMON=normalizePath(client.dir)) # normalization required for correct symlinks.
reticulate::virtualenv_create(basilisk:::.common_env, python=test.py)

#############################

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
    expect_error(setupBasiliskEnv("thingo", "pillow"), "version must be explicitly specified")

    unlink(target, recursive=TRUE)
    expect_error(setupBasiliskEnv("thingo", c(test.pandas, old.pandas)), "redundant listing")
})

test_that("setupBasiliskEnv uses the core installation when possible", {
    unlink(target, recursive=TRUE)
    
    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true(test.pandas %in% incoming)
    expect_error(setupBasiliskEnv("thingo", test.pandas), NA)
    expect_true(Sys.readlink(file.path(client.dir, "thingo"))!="") # i.e., is a link.

    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(test.pandas %in% incoming)
})

test_that("setupBasiliskEnv overrides an incompatible core installation", {
    unlink(target, recursive=TRUE)
    expect_error(setupBasiliskEnv("thingo", c(old.pandas, old.pandas.deps)), NA)

    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true(test.pandas %in% incoming)
    expect_true(!any(old.pandas.deps %in% incoming))

    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))
})

test_that("setupBasiliskEnv allows core packages to have unspecified versions", {
    unlink(target, recursive=TRUE)
    expect_error(setupBasiliskEnv("thingo", "numpy"), NA)

    test.numpy <- core.set$full[core.set$package=="numpy"]
    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true(test.numpy %in% incoming)

    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(test.numpy %in% incoming)
})

#############################

unlink(client.dir, recursive=TRUE)
unlink(basilisk.dir, recursive=TRUE)

Sys.unsetenv("BASILISK_TEST_CORE")
Sys.unsetenv("BASILISK_TEST_COMMON")

if (old.retpy!="") {
    Sys.setenv(RETICULATE_PYTHON=old.retpy)
}
if (old.pypath!="") {
    Sys.setenv(PYTHONPATH=old.pypath)
}
