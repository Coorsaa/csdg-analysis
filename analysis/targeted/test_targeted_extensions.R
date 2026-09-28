# Run from the canonical source root with Rscript; fixtures contain no study records.

suppressPackageStartupMessages({ library(data.table); library(testthat) })
source("analysis/targeted/local_helpers.R")
source("analysis/targeted/controlled_examples.R")
source("analysis/targeted/shild_contribution_stability.R")
source("analysis/targeted/shild_convergence.R")
library(mlr3autoiml)

test_that("redundant indicator PFI distinguishes three estimands", {
  for (model in 1:2) {
    expect_equal(controlled_pfi_expectation(1, 1, model, "marginal"), 2)
    expect_equal(controlled_pfi_expectation(1, 1, model, "conditional"), 0)
    expect_equal(controlled_pfi_expectation(1, 1, model, "joint_block"), 2)
    expect_equal(controlled_pfi_expectation(1, 1, model, "marginal", 3L - model), 0)
  }
  expect_equal(controlled_pfi_expectation(.9, .4, 1L, "conditional"), 2 * sqrt(.9) * .6)
  expect_equal(controlled_conditional_draw(c(-2, 0, 2), 1, 1, c(10, 10, 10)), c(-2, 0, 2))
})

test_that("best single-predictor models have PFI 2 r_j and conditional PFI 2 r_j (1 - r_k)", {
  for (intervention in c("marginal", "joint_block")) {
    expect_equal(controlled_pfi_expectation(.9, .4, 1L, intervention, model_form = "optimal"), 2 * .9)
    expect_equal(controlled_pfi_expectation(.9, .4, 2L, intervention, model_form = "optimal"), 2 * .4)
  }
  expect_equal(controlled_pfi_expectation(.9, .4, 1L, "conditional", model_form = "optimal"), 2 * .9 * .6)
  expect_equal(controlled_pfi_expectation(.9, .4, 2L, "conditional", model_form = "optimal"), 2 * .4 * .1)
  expect_equal(controlled_pfi_expectation(.9, .4, 1L, "marginal", 2L, model_form = "optimal"), 0)
  expect_equal(controlled_pfi_expectation(1, 1, 1L, "marginal", model_form = "optimal"),
    controlled_pfi_expectation(1, 1, 1L, "marginal"))
})

test_that("the conditional draw has the declared Gaussian law", {
  set.seed(32L)
  other = rnorm(100000L)
  draw = controlled_conditional_draw(other, .9, .4, rnorm(length(other)))
  expect_lt(abs(var(draw) - 1), .02)
  expect_lt(abs(cov(draw, other) - .6), .02)
  expect_lt(abs(mean(draw - .6 * other)), .01)
})

test_that("all completed controlled estimates agree with the analytical reference", {
  results = fread("analysis/targeted/results/controlled/controlled_pfi_results.csv")
  expect_equal(nrow(results), 80L)
  expect_equal(results[, .N, by = model_form][order(model_form)]$N, c(40L, 40L))
  expect_setequal(unique(results$model_form), c("identity", "optimal"))
  expect_equal(results[, .N, by = scenario]$N, rep(20L, 4L))
  expect_true(all(abs(results$monte_carlo_pfi - results$analytical_population_pfi) <=
    6 * results$monte_carlo_se + 1e-12))
  expect_true(all(results$passed_six_se_check))
  expect_true(all(results$n_monte_carlo == 200000L))
  expected = mapply(function(r1, r2, model, intervention, feature, form) {
    controlled_pfi_expectation(r1, r2, model, intervention,
      if (feature == "X1 + X2") model else as.integer(sub("X", "", feature)), form)
  }, results$r1, results$r2, results$model_index, results$intervention, results$queried_feature,
    results$model_form)
  expect_equal(results$analytical_population_pfi, unname(expected), tolerance = 1e-12)
  metadata = jsonlite::read_json("analysis/targeted/results/controlled/controlled_run_metadata.json")
  expect_equal(metadata$result_rows, nrow(results))
})

test_that("weighted probability scoring retains out-of-range predictions", {
  metrics = targeted_score(c(.1, .9), c(-.1, 1.1), c(1, 3), .5, .6)
  expect_equal(metrics[["weighted_rmse"]], .2)
  expect_equal(metrics[["out_of_probability_range_fraction"]], 1)
  expect_equal(metrics[["target_absolute_error"]], .1)
  expect_error(targeted_score(c(.1, .9), .1, c(1, 3), .5, .6))
  expect_error(targeted_score(c(.1, .9), c(.1, .9), c(-1, 3), .5, .6))
})

test_that("backtransformed pointwise logits are evaluated on a genuinely common scale", {
  training = cbind("(Intercept)" = 1, x = seq(-1, 1, length.out = 200L))
  evaluation = cbind("(Intercept)" = 1, x = seq(-.95, .95, length.out = 99L))
  truth = plogis(.5 + 2 * training[, "x"])
  p_truth = plogis(.5 + 2 * evaluation[, "x"])
  response = targeted_fixed_ridge(training, truth, rep(1, 200), 1e-10)
  link = targeted_fixed_ridge(training, qlogis(truth), rep(1, 200), 1e-10)
  p_response = as.numeric(evaluation %*% response$coefficients)
  p_link = plogis(as.numeric(evaluation %*% link$coefficients))
  expect_lt(sqrt(mean((p_truth - p_link)^2)), 1e-8)
  expect_gt(sqrt(mean((p_truth - p_response)^2)), .01)
})

test_that("normalized ridge strength is invariant to duplicating weighted training rows", {
  design = cbind("(Intercept)" = 1, x = -2:2)
  outcome = c(1, .7, .4, .3, .1)
  weights = c(1, 2, 3, 2, 1)
  fit = targeted_fixed_ridge(design, outcome, weights, .01)
  repeated = targeted_fixed_ridge(rbind(design, design), rep(outcome, 2), rep(weights, 2), .01)
  expect_equal(fit$coefficients, repeated$coefficients, tolerance = 1e-12)
  intercept = targeted_fixed_ridge(matrix(1, 5L, 1L, dimnames = list(NULL, "(Intercept)")),
    outcome, weights, .01)
  expect_equal(unname(intercept$coefficients), weighted.mean(outcome, weights))
})

test_that("convergence selection is balanced and independent of fidelity outcomes", {
  frame = CJ(fold = 1:10, prediction_score_decile = 1:10, selection_draw = 1:3)
  frame[, case_label := sprintf("synthetic_%03d", .I)]
  selected = targeted_convergence_frame(frame)
  frame[, arbitrary_fidelity := rev(seq_len(.N))]
  expect_identical(selected$case_label, targeted_convergence_frame(frame)$case_label)
  expect_equal(nrow(selected), 30L)
  expect_true(all(selected[, .N, by = fold]$N == 3L))
  expect_true(all(selected[, .N, by = prediction_score_decile]$N == 3L))
})

test_that("grouped contributions preserve centered predictions including factor contrasts", {
  data = data.frame(x = 1:6, category = factor(rep(c("a", "b", "c"), 2L)))
  design = model.matrix(~ ., data)
  coefficients = setNames(c(2, .3, -.5, .2), colnames(design))
  weights = 1:6
  contribution = targeted_contributions(design, coefficients, attr(design, "assign"), names(data), weights)
  prediction = as.numeric(design %*% coefficients)
  expect_equal(unname(rowSums(contribution)), prediction - weighted.mean(prediction, weights), tolerance = 1e-12)
  expect_identical(colnames(contribution), c("x", "category"))
})

test_that("paired differences are formed before across-case aggregation", {
  x = data.table(case_label = rep(c("a", "b"), each = 3L), difference = c(-2, -1, 50, -3, 2, 99))
  result = targeted_aggregate_cases(x, character(), "difference")
  expect_equal(result$median, .5)
  expect_equal(result$n_cases, 2L)
})

test_that("public SHILD summaries preserve scale, sample size, and aggregation contracts", {
  root = "analysis/targeted/results/shild"
  common = fread(file.path(root, "shild_common_scale_summary.csv"))
  paired = fread(file.path(root, "shild_common_scale_paired_differences.csv"))
  convergence = fread(file.path(root, "shild_convergence_summary.csv"))
  contribution = fread(file.path(root, "shild_contribution_stability_summary.csv"))
  expect_equal(nrow(common), 18L)
  expect_true(all(common$n_cases == 100L))
  expect_false("native_weighted_r2" %in% common$metric)
  expect_false("native_weighted_r2" %in% paired$metric)
  expect_true(all(convergence$n_cases == 30L))
  expect_equal(sort(unique(convergence[design_role == "size", n_training])), c(250L, 500L, 1000L, 2000L))
  expect_true(all(contribution[, .N, by = surrogate_scale]$N == 27L))
  expect_true(all(common$q25 <= common$median & common$median <= common$q75))
  expect_true(all(convergence$q25 <= convergence$median & convergence$median <= convergence$q75))
  csv_paths = list.files(root, pattern = "\\.csv$", full.names = TRUE)
  for (path in csv_paths) {
    expect_false(any(names(fread(path)) %in% c("case_label", "row_id", "neighbor_id", "prob.discontent")))
  }
})
