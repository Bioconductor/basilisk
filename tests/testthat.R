library(testthat)
library(basilisk)
if (.Platform$OS.type != "windows") test_check("basilisk")
