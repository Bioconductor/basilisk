# Testing the createLocalBasiliskEnv function.
# library(testthat); library(basilisk); source("setup.R"); source("test-local.R"); 

target <- file.path(client.dir, "locality")

test_that("createLocalBasiliskEnv obtains the correct version of the packages", {
    tmp <- createLocalBasiliskEnv(target, packages=c(test.pandas, test.pandas.deps))
    expect_true(file.exists(file.path(target, packageVersion("basilisk"))))

    # Lock directories should strictly live inside the target directory.
    expect_true(any(grepl("LOCK", list.files(target))))
    expect_false(any(grepl("LOCK", list.files(client.dir))))

    incoming <- basilisk:::.basilisk_freeze(tmp)
    expect_true(test.pandas %in% incoming)
    expect_true(all(test.pandas.deps %in% incoming))

    # Works with a path.
    pandas.ver <- basiliskRun(env=tmp, fun=function() { 
        X <- reticulate::import("pandas"); X$`__version__` 
    })
    expect_identical(pandas.ver, sub("pandas=*", "", test.pandas))

    # Next creation attempt should be a no-op.
    sentinel <- file.path(tmp, "FOOBAR_test")
    write(file=sentinel, character(0))
    tmp2 <- createLocalBasiliskEnv(target, packages=c(test.pandas, test.pandas.deps))
    expect_identical(tmp, tmp2)
    expect_true(file.exists(sentinel))
})
