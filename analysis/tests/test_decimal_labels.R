library(testthat)
library(ggplot2)
source(if (file.exists("analysis/shared/decimal_labels.R")) "analysis/shared/decimal_labels.R" else "../shared/decimal_labels.R")

test_that("leading-zero formatting preserves signs, precision, and percentages", {
  before = c("0.934", "-0.00637", "+0.00", "0.5%", "1.00", "p50 = 0.0115", NA_character_)
  after = c(".934", "-.00637", "+.00", "0.5%", "1.00", "p50 = .0115", NA_character_)
  expect_identical(.analysis_omit_decimal_zero(before), after)
  expect_identical(.analysis_omit_decimal_zero(after), after)
  expect_identical(.analysis_omit_decimal_zero(c("0.1.4", "x0.5", "0e+00")), c("0.1.4", "x0.5", "0e+00"))
})

test_that("decimal formatting leaves drawing instructions and panel dimensions intact", {
  plot = ggplot(data.frame(x = c(0.1, 0.5), y = c(0.2, 0.7)), aes(x, y)) +
    geom_line() + geom_point() + scale_x_continuous(breaks = c(0.1, 0.5)) +
    scale_y_continuous(breaks = c(0.2, 0.7))
  result = .analysis_decimal_grob(plot, axes = c("x", "y"))
  expect_length(result$changes, 2L)
  expect_identical(result$original$widths, result$revised$widths)
  expect_identical(result$original$heights, result$revised$heights)
  repeated = .analysis_decimal_grob(result$revised, axes = c("x", "y"))
  expect_length(repeated$changes, 0L)
})
