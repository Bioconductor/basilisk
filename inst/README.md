# Regenerating the core lists

Running `generate_core_list.R` will update the core list so that it uses the latest version of all packages (given a date to pin those packages).
This generates a `replacement_core` file that, if satisfactory, should be used to overwrite `core_list`.
However, it requires some care to resolve conflicting dependencies (especially for **tensorflow**!), which we check using `pipdeptree` as shown below.

```r
library(basilisk)
dir.create("delete_me")

# Code ripped more or less out of tests/testthat/test-install.R.
client.dir <- "delete_me/basilisk"
dir.create(client.dir)
Sys.setenv(WORKON_HOME=client.dir)

# Setting up a fresh miniconda install for testing.
basilisk.dir <- "delete_me/miniconda"
basilisk:::.minstaller(basilisk.dir)

test.py <- basilisk:::.get_py_cmd(basilisk.dir)
Sys.setenv(BASILISK_TEST_PYTHON=test.py)

Sys.setenv(BASILISK_TEST_COMMON=normalizePath(client.dir)) 
reticulate::virtualenv_create(basilisk:::.common_env, python=test.py)

# Checking what we have before install.
previous <- basilisk:::.basilisk_freeze(test.py)

# Installing ALL of the core packages to see if they place nice with each other.
fullset <- readLines("core_list") 
setupVirtualEnv('tester', c(fullset, "pipdeptree==0.13.2"))

env.py <- basilisk:::.get_py_cmd(file.path(client.dir, "tester"))
system2(env.py, c('-m', 'pipdeptree'))

# Checking what we have after install... there should be nothing here 
# if all dependencies of core packages are listed in 'core_list'.
updated <- basilisk:::.basilisk_freeze(test.py)
setdiff(updated, c(previous, fullset))
```

Once we are happy with this, we can deparse the tree to record the dependencies.

```r
client.dir <- "delete_me/basilisk"
env.py <- basilisk:::.get_py_cmd(file.path(client.dir, "tester"))
out <- system2(env.py, c('-m', 'pipdeptree', '--json'), stdout=TRUE)

library(jsonlite)
deps <- fromJSON(txt=out, simplifyVector=FALSE)

collated <- list()
for (i in seq_along(deps)) {
    current <- deps[[i]]
    curdeps <- vapply(current$dependencies, function(y) y$package_name, "")
    if (length(curdeps)) {
        collated[[i]] <- cbind(current$package$package_name, curdeps)
    }
}

collated <- do.call(rbind, collated)
fullset <- readLines("core_list") 
core.pkgs <- basilisk:::.full2pkg(fullset)

# Some effort required to deal with the fact that dependencies are 
# allowed to have variable names.
collated0 <- tolower(collated)
collated0[] <- sub("_", "-", collated0)
core.pkgs0 <- tolower(core.pkgs)
core.pkgs0 <- sub("_", "-", core.pkgs0)

# Ignore packages that got installed with miniconda, assuming that
# all other dependencies are listed in 'core_list' (see above).
keep <- collated0[,1] %in% core.pkgs0 & collated0[,2] %in% core.pkgs0
collated0 <- collated0[keep,]

# Restoring the package names.
m <- match(collated0, core.pkgs0)
stopifnot(all(!is.na(m)))

final <- collated0
final[] <- core.pkgs[m]

write(t(final), sep="\t", "replacement_deps", ncolumns=2)
```

