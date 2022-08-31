# Setting up common variables.

test.pandas <- "pandas==1.4.3"
test.pandas.deps <- c("python-dateutil==2.8.2", "pytz==2022.2.1")

old.pandas <- "pandas==1.4.2"
old.pandas.deps <- c("python-dateutil==2.8.1", "pytz==2022.1")

client.dir <- "install-test-client"
unlink(client.dir, recursive=TRUE)
dir.create(client.dir)

# See reticulate:::check_forbidden_install().
Sys.setenv(`_RETICULATE_I_KNOW_WHAT_IM_DOING_`="true")
