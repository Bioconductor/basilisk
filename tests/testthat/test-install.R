# This checks that the version-controlling behavior of setupVirtualEnv is correct.
# library(testthat); library(basilisk); source("test-install.R")

# Installing a fresh version of Python.
basilisk.dir <- "install-test-basilisk" 
basilisk:::.minstaller(basilisk.dir)
client.dir <- "install-test-client"
dir.create(client.dir)

test.py <- file.path(basilisk.dir, 'bin', 'python3')
Sys.setenv(BASILISK_TEST_PYTHON=test.py)
Sys.setenv(BASILISK_TEST_CORE=TRUE)

test_that("setupVirtualEnv refuses to work without all specified versions", {
    expect_error(setupVirtualEnv("thingo", "pandas", pkgpath=client.dir), "must be explicitly specified")
    expect_error(setupVirtualEnv("thingo", "pandas==0.25.1", pkgpath=client.dir), "need to list dependency")
    expect_error(setupVirtualEnv("thingo", c("pandas==0.24.1", "pandas==0.25.1"), pkgpath=client.dir), "redundant listing")
})

test_that("setupVirtualEnv switches to a core installation when possible", {
    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_false(any(grepl("numpy==", incoming)))

    unlink(file.path(client.dir, "basilisk"), recursive=TRUE)
    expect_error(setupVirtualEnv("thingo", c("pandas==0.25.1", "python-dateutil==2.8.1", "pytz==2019.3"), pkgpath=client.dir), NA)

    core <- readLines(system.file("core_list", package="basilisk"))
    expected.numpy <- core[grep("numpy==", core)]
    incoming <- basilisk:::.basilisk_freeze(test.py)
    observed.numpy <- incoming[grep("numpy==", incoming)]
    expect_identical(expected.numpy, observed.numpy)
})

test_that("setupVirtualEnv overrides an incompatible core installation", {
    unlink(file.path(client.dir, "basilisk"), recursive=TRUE)
    expect_error(setupVirtualEnv("thingo", c("numpy==1.15.1"), pkgpath=client.dir), NA)

    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true("numpy==1.17.3" %in% incoming)

    env.py <- file.path(client.dir, 'basilisk', 'thingo', 'bin', 'python3')
    incoming <- basilisk:::.basilisk_freeze(env.py)
    expect_true("numpy==1.15.1" %in% incoming)
})

test_that("setupVirtualEnv allows core packages to have unspecified versions", {
    unlink(file.path(client.dir, "basilisk"), recursive=TRUE)
    system2(test.py, c("-m", "pip", "uninstall", "-y", "numpy"))

    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_false("numpy==1.17.3" %in% incoming)

    expect_error(setupVirtualEnv("thingo", "numpy", pkgpath=client.dir), NA)
    incoming <- basilisk:::.basilisk_freeze(test.py)
    expect_true("numpy==1.17.3" %in% incoming)
})

unlink(client.dir, recursive=TRUE)
unlink(basilisk.dir, recursive=TRUE)
Sys.unsetenv("BASILISK_TEST_PYTHON")
Sys.unsetenv("BASILISK_TEST_CORE")
