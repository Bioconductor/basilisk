# This updates the core list to use the latest versions of all dependencies.
library(basilisk)
pkg <- sub("==.*", "", readLines("core_list"))

# The target date.
date <- "2019-11-08"

# Collecting all the latest versions.
library(httr)
collated <- NULL
for (module in pkg) {
    out <- GET(sprintf("https://pypi.org/pypi/%s/json", module))
    stop_for_status(out)
    releases <- content(out)$releases

    # Only considering final releases.
    keep <- grep("[0-9]+\\.[0-9]+$", names(releases))
    releases <- releases[keep]

    required <- Inf
    candidate <- NULL
    limit <- as.Date(date)

    for (n in names(releases)) {
        if (length(releases[[n]])==0L) next

        uptime <- releases[[n]][[1]]$upload_time
        diff <- as.numeric(limit - as.Date(uptime))
        if (diff >= 0 && required >= diff) {
            required <- diff
            candidate <- n
        }
    }

    if (is.null(candidate)) {
        stop("no '", module, "' version is available before '", date, "'")
    }

    collated <- c(collated, paste0(module, "==", candidate))
}

write(sort(collated), file="replacement_core")

# Conflicts can be checked by reinstalling basilisk and running:
#
# library(basilisk)
# setupVirtualEnv('whee', c(listCorePackages()$full, "pipdeptree==0.13.2"), pkgpath="stuff")
# system2('stuff/basilisk/whee/bin/python3', c('-m', 'pipdeptree'))
