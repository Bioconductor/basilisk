# Setting up common variables.

core.set <- listCorePackages()
test.pandas <- core.set$full[core.set$package=="pandas"]

old.pandas <- "pandas==0.24.1"
old.pandas.deps <- c("python-dateutil==2.7.1", "pytz==2018.7")

client.dir <- "install-test-client"
unlink(client.dir, recursive=TRUE)
dir.create(client.dir)
