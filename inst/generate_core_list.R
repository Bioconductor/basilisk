# This updates the core list to use the latest versions of all dependencies.
# It requires us to define a target date to which we pin all versions.
date <- "2019-11-08"

###################
# Querying PyPi versions and finding the latest final release
# that matches up with the specified date.

library(basilisk)
pkg <- sub("==.*", "", readLines("core_list"))

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
