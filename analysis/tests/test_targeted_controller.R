#!/usr/bin/env Rscript
# These failures occur before any fit, write, or private input is requested.
source("analysis/targeted/run.R")
library(testthat)
test_that("paper modes and private input contracts are explicit", {
  expect_error(targeted_main(character()), "Supply --mode")
  expect_error(targeted_main("--mode=all"), "Supply --mode")
  expect_error(targeted_main(c("--mode=aggregate", "--mode=synthetic")), "Duplicate")
  expect_error(targeted_main("--mode=protected_extension"), "Private extensions require")
  expect_error(targeted_main("--mode=human_analysis"), "Supply --mode")
  expect_error(targeted_main(c("--mode=protected_extension", "--authorized-workspace=.",
    "--private-output=./unused_private_output")), "outside the public source")
  expect_false(dir.exists("unused_private_output"))
  expect_error(targeted_main(c("--mode=protected_extension", "--authorized-workspace=.",
    paste0("--private-output=", getwd()))), "outside the public source")
})
