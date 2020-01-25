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
    expect_error(setupBasiliskEnv(target, "pillow"), "version must be explicitly specified")

    unlink(target, recursive=TRUE)
    expect_error(setupBasiliskEnv(target, c(test.pandas, old.pandas)), "redundant listing")
})

test_that("setupBasiliskEnv uses the core installation when possible", {
    unlink(target, recursive=TRUE)
    
    incoming <- basilisk:::.basilisk_freeze(base.py)
    expect_true(test.pandas %in% incoming)
    expect_false(setupBasiliskEnv(target, test.pandas))
    expect_identical(list.files(target), character(0))
})

test_that("setupBasiliskEnv overrides an incompatible core installation", {
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

test_that("setupBasiliskEnv allows core packages to have unspecified versions", {
    unlink(target, recursive=TRUE)
    expect_true(setupBasiliskEnv(target, c("numpy", old.pandas))) # adding old pandas to force it to make a venv.

    test.numpy <- core.set$full[core.set$package=="numpy"]
    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(test.numpy %in% incoming)
})

#############################

target <- file.path(client.dir, "thingo")
env.py <- basilisk:::.get_py_cmd(target)

test_that("setupBasiliskEnv uses the core installation when possible (for conda)", {
    skip_on_os("windows") # conda is the default anyway.
    unlink(target, recursive=TRUE)

    incoming <- basilisk:::.basilisk_freeze(base.py)
    expect_true(test.pandas %in% incoming)
    expect_false(setupBasiliskEnv(target, test.pandas, conda=TRUE))
    expect_identical(list.files(target), character(0))
})

test_that("setupBasiliskEnv overrides an incompatible core installation (for conda)", {
    skip_on_os("windows") # conda is the default anyway.
    unlink(target, recursive=TRUE)
    expect_true(setupBasiliskEnv(target, c(old.pandas, old.pandas.deps), conda=TRUE))

    incoming <- basilisk:::.basilisk_freeze(base.py)
    expect_true(test.pandas %in% incoming)
    expect_true(!any(old.pandas.deps %in% incoming))

    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(old.pandas %in% incoming)
    expect_true(all(old.pandas.deps %in% incoming))

    # Trying it again is a no-op.
    expect_false(setupBasiliskEnv(target, c(old.pandas, old.pandas.deps), conda=TRUE))
})

test_that("setupBasiliskEnv allows core packages to have unspecified versions (for conda)", {
    skip_on_os("windows") # conda is the default anyway.
    unlink(target, recursive=TRUE)
    expect_true(setupBasiliskEnv(target, c("numpy", old.pandas), conda=TRUE), NA) # forcing it to install rather than gloss over.

    test.numpy <- core.set$full[core.set$package=="numpy"]
    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true(test.numpy %in% incoming)
})

#############################

if (old.retpy!="") {
    Sys.setenv(RETICULATE_PYTHON=old.retpy)
}
if (old.pypath!="") {
    Sys.setenv(PYTHONPATH=old.pypath)
}
