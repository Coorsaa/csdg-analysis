#!/usr/bin/env Rscript

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/tests/test_diagnostics.R"
}
analysis_dir = dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
project_root = normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)

required = c("devtools", "mlr3", "mlr3learners", "mlr3pipelines", "rpart")
missing = required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) {
  stop("Missing model-diagnostics self-test packages: ", paste(missing, collapse = ", "), ".", call. = FALSE)
}

library(mlr3autoiml)
suppressPackageStartupMessages(library(mlr3learners))

assert_labeled = function(table) {
  stopifnot(
    data.table::is.data.table(table),
    all(c("estimand", "limitation", "privacy_scope", "interpretation_scope") %in% names(table)),
    all(nzchar(table$estimand)),
    all(nzchar(table$limitation))
  )
}

mask_1d_probe = csdg_mask_effect_support(data.table::data.table(
  ale = c(0.1, 0.2, NA_real_),
  n_interval = c(2L, 8L, NA_integer_),
  limitation = "unsuppressed"
), 5L)
stopifnot(
  is.na(mask_1d_probe$ale[[1L]]),
  mask_1d_probe$n_interval[[1L]] == 2L,
  !mask_1d_probe$supported[[1L]],
  mask_1d_probe$supported[[2L]],
  mask_1d_probe$ale[[2L]] == 0.2,
  !mask_1d_probe$supported[[3L]],
  all(mask_1d_probe$support_threshold == 5L),
  all(grepl("below 5 sampled observations", mask_1d_probe$limitation, fixed = TRUE))
)
mask_probe = csdg_mask_effect_support(data.table::data.table(
  ale2d = c(0.1, 0.2),
  n_cell = c(2L, 8L),
  limitation = "unsuppressed"
), 5L)
stopifnot(
  is.na(mask_probe$ale2d[[1L]]),
  is.na(mask_probe$n_cell[[1L]]),
  !mask_probe$supported[[1L]],
  mask_probe$supported[[2L]],
  mask_probe$ale2d[[2L]] == 0.2
)

set.seed(1001L)
n = 160L
interaction_data = data.frame(
  x1 = stats::runif(n, -2, 2),
  x2 = stats::runif(n, -2, 2),
  x3 = stats::rnorm(n)
)
interaction_data$y = 5 * interaction_data$x1 * interaction_data$x2 + stats::rnorm(n, sd = 0.05)
interaction_task = mlr3::TaskRegr$new(
  "rich_interaction_test", backend = interaction_data, target = "y"
)
interaction_fit = mlr3::lrn("regr.rpart", cp = 0, minsplit = 5L, maxdepth = 12L)
interaction_fit$train(interaction_task)

effects = csdg_effect_diagnostics(
  interaction_task,
  interaction_fit,
  features = c("x1", "x2"),
  sample_n = 100L,
  ale_bins = 5L,
  grid_n = 6L,
  ice_keep_n = 8L,
  seed = 1002L
)
interaction = csdg_interaction_diagnostics(
  interaction_task,
  interaction_fit,
  features = c("x1", "x2", "x3"),
  sample_n = 120L,
  h_grid_n = 5L,
  ale_bins = 5L,
  seed = 1003L
)
shap = csdg_shapley_diagnostics(
  interaction_task,
  interaction_fit,
  case_rows = interaction_task$row_ids[seq_len(5L)],
  background_n = 30L,
  sample_size = 8L,
  seed = 1004L,
  case_selection = "post_hoc_communication"
)
shap_small = csdg_shapley_diagnostics(
  interaction_task,
  interaction_fit,
  case_rows = interaction_task$row_ids[seq_len(2L)],
  background_n = 30L,
  sample_size = 8L,
  seed = 1011L,
  case_selection = "post_hoc_communication"
)
candidate_learners = list(
  shallow = mlr3::lrn("regr.rpart", cp = 0.03),
  deep = mlr3::lrn("regr.rpart", cp = 0.001)
)
multiplicity = csdg_prediction_multiplicity(
  interaction_task,
  candidate_learners = candidate_learners,
  near_equivalent_ids = names(candidate_learners),
  row_ids = interaction_task$row_ids[seq_len(80L)],
  seed = 1005L,
  range_thresholds = c(0.1, 0.5)
)

invisible(lapply(
  c(effects[c("ale_1d", "pdp", "ice_private", "ice_quantiles", "ice_spread")],
    interaction[c("interaction_screen", "ale_2d")],
    shap[c("local_private", "global", "additivity_private")],
    multiplicity[c(
      "private_predictions", "private_row_dispersion", "distribution", "model_summary", "pairwise",
      "threshold_shares"
    )]),
  assert_labeled
))
stopifnot(
  identical(unique(effects$ice_private$privacy_scope), "private_row_level"),
  identical(effects$settings$ale_trim, c(0.05, 0.95)),
  all(grepl("5th-95th percentiles", effects$ale_1d$estimand, fixed = TRUE)),
  interaction$interaction_screen[feature_1 == "x1" & feature_2 == "x2", h_statistic] > 0.8,
  identical(unique(shap$global$case_selection), "post_hoc_communication"),
  all(is.finite(shap$local_private$phi_monte_carlo_se)),
  all(grepl("fixed fitted model and sampled background", shap$local_private$phi_monte_carlo_se_definition)),
  nrow(shap$additivity_private) == 5L,
  all(is.finite(shap$additivity_private$additivity_residual)),
  all(grepl("No standard error is estimated for the residual", shap$additivity_private$limitation)),
  identical(unique(shap_small$global$privacy_scope), "private_small_n_aggregate"),
  identical(unique(shap_small$local_private$privacy_scope), "private_case_level"),
  identical(unique(multiplicity$distribution$privacy_scope), "shareable_aggregate")
)

set.seed(1006L)
additive_data = data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n))
additive_data$y = 2 * additive_data$x1 - 3 * additive_data$x2 + 0.5 * additive_data$x3
additive_task = mlr3::TaskRegr$new("rich_additive_test", backend = additive_data, target = "y")
additive_fit = mlr3::lrn("regr.lm")
additive_fit$train(additive_task)
additive_interaction = csdg_interaction_diagnostics(
  additive_task,
  additive_fit,
  features = c("x1", "x2", "x3"),
  sample_n = 120L,
  h_grid_n = 5L,
  ale_bins = 5L,
  seed = 1007L
)
stopifnot(
  max(additive_interaction$interaction_screen$h_statistic) < 1e-10,
  identical(
    additive_interaction$ale_2d$surface_status[[1L]],
    "not_computed_no_h_above_numerical_tolerance"
  ),
  is.na(additive_interaction$ale_2d$ale2d[[1L]])
)

set.seed(1008L)
classification_data = data.frame(
  x1 = stats::rnorm(120L),
  x2 = stats::runif(120L),
  x3 = stats::rnorm(120L)
)
classification_data$x1[sample.int(120L, 8L)] = NA_real_
linear_predictor = ifelse(is.na(classification_data$x1), 0, classification_data$x1) +
  2 * classification_data$x2 * classification_data$x3
classification_data$y = factor(
  ifelse(stats::runif(120L) < stats::plogis(linear_predictor), "yes", "no"),
  levels = c("no", "yes")
)
classification_task = mlr3::TaskClassif$new(
  "rich_graph_test", backend = classification_data, target = "y", positive = "yes"
)
graph = mlr3pipelines::`%>>%`(
  mlr3pipelines::po("imputemedian"),
  mlr3pipelines::po(
    "learner",
    learner = mlr3::lrn("classif.rpart", predict_type = "prob", cp = 0.001)
  )
)
graph_fit = mlr3::as_learner(graph)
graph_fit$predict_type = "prob"
graph_fit$train(classification_task)
graph_effects = csdg_effect_diagnostics(
  classification_task,
  graph_fit,
  features = c("x1", "x2"),
  sample_n = 80L,
  ale_bins = 5L,
  grid_n = 5L,
  ice_keep_n = 6L,
  seed = 1009L
)
graph_shap_1 = csdg_shapley_diagnostics(
  classification_task,
  graph_fit,
  case_rows = classification_task$row_ids[seq_len(5L)],
  background_n = 25L,
  sample_size = 6L,
  seed = 1010L,
  case_selection = "random_analysis_sample"
)
graph_shap_2 = csdg_shapley_diagnostics(
  classification_task,
  graph_fit,
  case_rows = classification_task$row_ids[seq_len(5L)],
  background_n = 25L,
  sample_size = 6L,
  seed = 1010L,
  case_selection = "random_analysis_sample"
)
stopifnot(
  all(graph_effects$ale_1d$class_label == "yes"),
  identical(graph_shap_1$local_private$phi, graph_shap_2$local_private$phi),
  identical(unique(graph_shap_1$global$privacy_scope), "shareable_aggregate")
)

message("model diagnostics self-test passed")
