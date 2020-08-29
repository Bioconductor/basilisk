# Setting up common variables.

test.pandas <- "pandas==0.25.1"
test.pandas.deps <- c("python-dateutil==2.8.0", "pytz==2019.3")

old.pandas <- "pandas==0.24.1"
old.pandas.deps <- c("python-dateutil==2.7.1", "pytz==2018.7")

client.dir <- "install-test-client"
unlink(client.dir, recursive=TRUE)
dir.create(client.dir)

reset_env <- function(var, val) {
    if (is.na(val)) {
        Sys.unsetenv(var)
    } else {
        X <- list(val)
        names(X) <- var
        do.call(Sys.setenv, X)
    }     
}
