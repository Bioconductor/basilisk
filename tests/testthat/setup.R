# Setting up common variables.

BASILISK_PANDAS_CURRENT <- "2.1.4"
BASILISK_PANDAS_OLD <- "2.0.3"
BASILISK_PYARROW_CURRENT <- "14.0.2"
BASILISK_PYARROW_OLD <- "14.0.1"

test.pandas <- sprintf("pandas==%s", BASILISK_PANDAS_CURRENT)
test.pandas.deps <- sprintf("pyarrow==%s", BASILISK_PYARROW_CURRENT)
old.pandas <- sprintf("pandas==%s", BASILISK_PANDAS_OLD)
old.pandas.deps <- sprintf("pyarrow==%s", BASILISK_PYARROW_OLD)


client.dir <- "install-test-client"
unlink(client.dir, recursive=TRUE)
dir.create(client.dir)

# See reticulate:::check_forbidden_install().
Sys.setenv(`_RETICULATE_I_KNOW_WHAT_IM_DOING_`="true")
