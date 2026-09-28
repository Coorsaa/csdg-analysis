#!/usr/bin/env Rscript

source("analysis/shared/study_helpers.R", local = FALSE)
source("analysis/shared/evidence_helpers.R", local = FALSE)
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

assert_synthesis_error = function(code, pattern) {
  observed = tryCatch(
    {
      force(code)
      NULL
    },
    error = identity
  )
  stopifnot(
    inherits(observed, "error"),
    grepl(pattern, conditionMessage(observed), ignore.case = TRUE)
  )
  invisible(TRUE)
}

project_root = .analysis_project_root()
library(mlr3autoiml)
protocol = yaml::read_yaml(file.path(project_root, "analysis", "protocol.yml"))

# The coverage-oriented case sample must be deterministic and balanced over the
# frozen fold-by-prediction-score grid.
synthetic_predictions = CJ(fold = 1:10, row_in_fold = 1:500)
synthetic_predictions[, `:=`(
  row_id = seq_len(.N),
  prob.discontent = (row_in_fold - 0.5) / 500
)]
sample_one = .evidence_select_local_cases(synthetic_predictions)
sample_two = .evidence_select_local_cases(synthetic_predictions)
stopifnot(
  identical(sample_one, sample_two),
  nrow(sample_one$private) == 300L,
  sample_one$private[, uniqueN(row_id)] == 300L,
  all(sample_one$private[, .N, by = .(fold, prediction_score_decile)]$N == 3L),
  !"row_id" %in% names(sample_one$public),
  !"prob.discontent" %in% names(sample_one$public)
)

# Perturbation and cross-fit randomness use reproducible, nonoverlapping seed
# namespaces.
perturbation_seeds = .evidence_perturbation_seed(
  202608120L,
  c("case_001", "case_300"),
  c(1L, 20L),
  1000000L
)
crossfit_seeds = .evidence_perturbation_seed(
  302608120L,
  c("case_001", "case_300"),
  c(1L, 20L),
  1000000L
)
stopifnot(
  identical(perturbation_seeds, c(203608121L, 222608420L)),
  identical(crossfit_seeds, c(303608121L, 322608420L)),
  !length(intersect(perturbation_seeds, crossfit_seeds))
)

# Parent readiness is a structural release check, not an inferential claim.
eligible_parent = data.table(
  structural_status = "PASS",
  release_eligibility = "ELIGIBLE",
  eligibility_blockers = "none"
)
blocked_parent = data.table(
  structural_status = "PASS",
  release_eligibility = "NOT_ELIGIBLE",
  eligibility_blockers = "analyst verification not signed"
)
readiness = .evidence_parent_readiness(eligible_parent, blocked_parent)
stopifnot(
  identical(readiness$manuscript_ready, FALSE),
  grepl("PISA=NOT_ELIGIBLE", readiness$eligibility_blocker, fixed = TRUE)
)
assert_synthesis_error(
  .evidence_parent_readiness(transform(eligible_parent, structural_status = "FAIL"), eligible_parent),
  "structural_status PASS"
)

# Build a complete public local-fidelity grid with the publication design:
# 20 independent seed pairs and four locality widths.
n_cases = 20L
n_replicates = 20L
kernel_widths = c(0.25, 0.50, 0.75, 1.00)
synthetic_replicates = CJ(
  case_number = seq_len(n_cases),
  perturbation_replicate = seq_len(n_replicates),
  kernel_width = kernel_widths
)
synthetic_replicates[, `:=`(
  case_label = sprintf("case_%03d", case_number),
  iteration = 1L + (case_number - 1L) %% 10L,
  repetition = 1L,
  fold = 1L + (case_number - 1L) %% 10L,
  prediction_score_decile = 1L + (case_number - 1L) %% 10L,
  perturbation_seed = as.integer(1000000L + 1000L * perturbation_replicate + case_number),
  crossfit_seed = as.integer(2000000L + 1000L * perturbation_replicate + case_number),
  analysis_role = ifelse(abs(kernel_width - 0.75) < 1e-12, "primary", "sensitivity"),
  target_scale = "response",
  neighborhood_method = "synthetic"
)]
synthetic_replicates[, `:=`(
  n_perturb = 500L,
  n_crossfit_folds = 5L,
  weighted_r2 = 0.70 + 0.01 * prediction_score_decile + 0.0005 * perturbation_replicate -
    0.015 * abs(kernel_width - 0.75),
  weighted_rmse = 0.025 + 0.0015 * prediction_score_decile + 0.004 * abs(kernel_width - 0.75),
  weighted_mae = 0.020 + 0.001 * prediction_score_decile + 0.003 * abs(kernel_width - 0.75),
  maximum_absolute_error = 0.050 + 0.002 * prediction_score_decile,
  target_case_model_prediction = 0.10 + 0.02 * prediction_score_decile,
  target_case_surrogate_prediction = 0.102 + 0.02 * prediction_score_decile,
  target_case_error = 0.002,
  target_case_absolute_error = 0.002,
  perturbation_effective_n = 350,
  crossfit_effective_n_min = 250,
  crossfit_effective_n_max = 300,
  n_design_terms = 5L,
  n_active_design_terms = 4L,
  n_case_values_imputed = 0L,
  fidelity_threshold = 0.80
)]
synthetic_replicates[, `:=`(
  apparent_weighted_r2 = pmin(0.99, weighted_r2 + 0.08),
  apparent_weighted_rmse = pmax(0, weighted_rmse - 0.006),
  meets_threshold = weighted_r2 >= 0.80
)]

synthetic_support = unique(synthetic_replicates[, .(
  case_label,
  iteration,
  repetition,
  fold,
  prediction_score_decile
)])
synthetic_support[, `:=`(
  n_case_values_imputed = 0L,
  case_imputed_features = "",
  support_kernel_width = 0.75,
  nearest_training_distance = 0.05 + 0.002 * prediction_score_decile,
  tenth_nearest_training_distance = 0.10 + 0.003 * prediction_score_decile,
  fiftieth_nearest_training_distance = 0.20 + 0.004 * prediction_score_decile,
  proportion_training_within_kernel = 0.70 - 0.02 * prediction_score_decile,
  kernel_weight_effective_n = 200 + 5 * prediction_score_decile,
  n_training_background = 1000L
)]

synthetic_coefficients = CJ(
  case_number = seq_len(n_cases),
  perturbation_replicate = seq_len(n_replicates),
  term = c("x1", "x2")
)
synthetic_coefficients[, `:=`(
  case_label = sprintf("case_%03d", case_number),
  iteration = 1L + (case_number - 1L) %% 10L,
  repetition = 1L,
  fold = 1L + (case_number - 1L) %% 10L,
  target_scale = "response",
  neighborhood_method = "synthetic",
  coefficient = ifelse(term == "x1", 0.5, -0.25) + 0.001 * perturbation_replicate,
  absolute_rank = ifelse(term == "x1", 1, 2)
)]
synthetic_coefficients[, case_number := NULL]

synthetic_sampling = data.table(
  case_label = sprintf("case_%03d", seq_len(n_cases)),
  row_id = seq_len(n_cases),
  fold = 1L + (seq_len(n_cases) - 1L) %% 10L,
  prediction_score_decile = 1L + (seq_len(n_cases) - 1L) %% 10L
)
synthetic_multiplicity = data.table(
  row_id = seq_len(n_cases),
  prediction_range = seq(0.001, 0.020, length.out = n_cases)
)

synthetic_summary = .evidence_summarize_local_fidelity(
  synthetic_replicates,
  synthetic_support,
  synthetic_sampling,
  synthetic_multiplicity,
  coefficients = synthetic_coefficients,
  expected_replicates = n_replicates,
  expected_widths = kernel_widths
)
direct_summary = csdg_summarize_local_fidelity(
  synthetic_replicates,
  synthetic_support,
  coefficients = synthetic_coefficients,
  primary_kernel_width = 0.75,
  fidelity_threshold = 0.80
)
stopifnot(
  nrow(synthetic_summary$cases) == n_cases,
  nrow(synthetic_summary$by_prediction_score) == 10L,
  nrow(synthetic_summary$by_disagreement) == 10L,
  nrow(synthetic_summary$by_support) == 4L,
  nrow(synthetic_summary$coefficient_stability) == n_cases * 2L,
  all(c(
    "median_weighted_rmse", "median_weighted_mae", "median_maximum_absolute_error",
    "median_target_case_absolute_error", "median_r2_optimism", "median_rmse_optimism"
  ) %in% names(synthetic_summary$cases)),
  identical(
    synthetic_summary$cases$median_weighted_r2,
    direct_summary$cases$median_weighted_r2
  ),
  all(grepl("prediction-score decile", synthetic_summary$by_disagreement$inference, fixed = TRUE))
)

fold_cases = synthetic_sampling[fold == 1L]
fold_result = list(
  replicates = synthetic_replicates[case_label %in% fold_cases$case_label],
  support = synthetic_support[case_label %in% fold_cases$case_label],
  coefficients = synthetic_coefficients[case_label %in% fold_cases$case_label]
)
stopifnot(isTRUE(.evidence_validate_fold_result(
  fold_result,
  fold_cases,
  fold = 1L,
  n_replicates = n_replicates,
  kernel_widths = kernel_widths
)))
duplicate_result = fold_result
duplicate_result$replicates = rbind(fold_result$replicates, fold_result$replicates[1L])
assert_synthesis_error(
  .evidence_validate_fold_result(
    duplicate_result,
    fold_cases,
    fold = 1L,
    n_replicates = n_replicates,
    kernel_widths = kernel_widths
  ),
  "exact common case-by-replicate-by-width grid"
)

# All-PV recurrence is defined on atomic features only; the overlapping joint
# query is deliberately excluded from the rank grid.
pisa_pfi = CJ(
  outcome_id = paste0("PV", seq_len(10L), "READ"),
  feature_number = seq_len(41L)
)
pisa_pfi[, `:=`(
  feature_group = sprintf("feature_%02d", feature_number),
  rank = as.numeric(feature_number)
)]
pisa_pfi[outcome_id == "PV10READ" & feature_number %in% c(10L, 11L), rank := c(11, 10)]
pisa_pfi = rbind(
  pisa_pfi,
  data.table(
    outcome_id = paste0("PV", seq_len(10L), "READ"),
    feature_number = NA_integer_,
    feature_group = "socioeconomic_joint_block",
    rank = NA_real_
  ),
  fill = TRUE
)
recurrence = .evidence_pisa_pv_importance_recurrence(pisa_pfi)
stopifnot(
  nrow(recurrence) == 10L,
  all(recurrence$n_ranked_features == 41L),
  all(recurrence$leading_set_jaccard_vs_pv1 == 1),
  sum(recurrence$full_rank_matches_pv1) == 9L
)

# PISA-C1 is a literal narrowing of PISA-C0: it removes only the unresolved
# calibration-adequacy clause and retains the observed prediction and PFI propositions.
pisa_claim_registry = .analysis_pisa_claim_registry()
pisa_claim_report = .analysis_pisa_claim_report_card()
pisa_c0 = pisa_claim_registry[claim_id == "PISA-C0"]
pisa_c1 = pisa_claim_registry[claim_id == "PISA-C1"]
stopifnot(
  identical(pisa_claim_registry$claim_id, c("PISA-C0", "PISA-C1")),
  identical(pisa_claim_registry$decision, c("unresolved", "met")),
  identical(pisa_claim_report$decision, pisa_claim_registry$decision),
  identical(pisa_c1$parent_claim_id, "PISA-C0"),
  identical(pisa_c1$revision_relation, "narrower"),
  identical(
    unname(unlist(pisa_c1[, .(
      target_relation,
      model_scope_relation,
      semantics_relation,
      analytic_distribution_relation,
      scientific_use_relation,
      explanation_design_relation
    )])),
    c("narrower", "same", "narrower", "same", "narrower", "same")
  ),
  identical(
    pisa_c0$statement,
    paste(
      pisa_c1$statement,
      "The pipeline is also sufficiently calibrated across the prediction range for the intended scientific",
      "interpretation."
    )
  )
)

# Plot helpers must retain only publication layers and expose complete labels.
systematic_panels = .evidence_systematic_g4_panels(
  synthetic_summary$cases,
  synthetic_summary$bandwidth_cases
)
stopifnot(
  identical(names(systematic_panels), c(
    "apparent_vs_cross_fitted", "relative_fidelity_by_prediction",
    "absolute_fidelity_by_prediction", "locality_curve"
  )),
  all(vapply(systematic_panels, inherits, logical(1L), what = "ggplot")),
  all(vapply(systematic_panels, function(plot) is.null(plot$labels$title), logical(1L)))
)

sensitivity_source = data.table(
  study = rep(c("SHILD", "PISA"), each = 3L),
  learner_name = c(
    "xgboost", "ridge_logistic", "random_forest",
    "xgboost", "ridge", "random_forest"
  ),
  focal_learner = "xgboost",
  n_total_pairs = c(351L, 351L, 351L, 820L, 820L, 820L),
  n_practical_ties_excluded = c(0L, 104L, 87L, 0L, 291L, 286L),
  n_comparable_pairs = c(351L, 247L, 264L, 820L, 529L, 534L),
  n_practical_reversals = c(0L, 30L, 32L, 0L, 87L, 5L),
  fraction_of_prespecified_tolerance_from_focal = c(0, 0.25, 0.35, 0, 0.24, 0.12),
  practical_reversal_fraction = c(0, 0.18, 0.25, 0, 0.21, 0.11),
  importance_weighted_reversal_fraction = c(0, 0.13, 0.20, 0, 0.16, 0.09),
  top_k_jaccard_practical = c(1, 0.67, 0.82, 1, 0.67, 1)
)
synthetic_pisa_performance = CJ(
  learner_id = c("xgboost", "featureless"),
  outcome_id = paste0("PV", seq_len(10L), "READ")
)
synthetic_pisa_performance[, `:=`(
  measure_id = "regr.rmse",
  estimate = ifelse(learner_id == "xgboost", 70, 75)
)]
synthetic_pisa_evidence = .analysis_pisa_claim_evidence(
  synthetic_pisa_performance,
  data.table(estimand = "calibration_slope", mean_estimate = 1.01),
  recurrence,
  sensitivity_source[study == "PISA"]
)
stopifnot(
  identical(synthetic_pisa_evidence$evidence_id, sprintf("PISA-E%02d", 0:5)),
  identical(synthetic_pisa_evidence$claim_id, c(rep("PISA-C0", 3L), rep("PISA-C1", 3L))),
  identical(synthetic_pisa_evidence[evidence_id == "PISA-E01", availability], "unavailable"),
  identical(synthetic_pisa_evidence[evidence_id == "PISA-E01", claim_consequence], "unresolved"),
  all(synthetic_pisa_evidence[evidence_id %in% c("PISA-E03", "PISA-E04"), result_direction] == "supports")
)
nonrecurring_pisa = copy(recurrence)
nonrecurring_pisa[outcome_id == "PV10READ", leading_set_jaccard_vs_pv1 := 0.8]
assert_synthesis_error(
  .analysis_pisa_claim_evidence(
    synthetic_pisa_performance,
    data.table(estimand = "calibration_slope", mean_estimate = 1.01),
    nonrecurring_pisa,
    sensitivity_source[study == "PISA"]
  ),
  "leading-PFI-set recurrence proposition"
)
sensitivity_plot = .evidence_performance_explanation_plot(sensitivity_source, "SHILD")
pisa_sensitivity_plot = .evidence_performance_explanation_plot(sensitivity_source, "PISA")
geom_names = vapply(sensitivity_plot$layers, function(layer) class(layer$geom)[[1L]], character(1L))
stopifnot(
  inherits(sensitivity_plot, "ggplot"),
  "GeomCol" %in% geom_names,
  !"GeomPoint" %in% geom_names,
  identical(systematic_panels$apparent_vs_cross_fitted$coordinates$ratio, 1),
  identical(
    systematic_panels$apparent_vs_cross_fitted$coordinates$limits$x,
    systematic_panels$apparent_vs_cross_fitted$coordinates$limits$y
  ),
  all(sensitivity_plot$data$n_comparable_pairs + sensitivity_plot$data$n_practical_ties_excluded ==
    sensitivity_plot$data$n_total_pairs),
  identical(levels(sensitivity_plot$data$learner_label), c("Random forest", "Ridge")),
  identical(levels(pisa_sensitivity_plot$data$learner_label), c("Random forest", "Ridge"))
)

# Protocol assertions keep the executable design synchronized with the written
# analysis specification.
local_protocol = protocol$SHILD$systematic_local_fidelity
pisa_protocol_claims = protocol$PISA$canonical_claims
stopifnot(
  local_protocol$case_sampling$total_cases == 300L,
  local_protocol$local_evaluation$perturbation_replicates == 20L,
  local_protocol$secondary_sensitivity$randomness_seeds_per_source == 5L,
  grepl("Complete 5-by-5 crossing", local_protocol$secondary_sensitivity$randomness_decomposition, fixed = TRUE),
  identical(
    sort(as.numeric(local_protocol$local_evaluation$sensitivity_kernel_widths)),
    c(0.25, 0.50, 1.00)
  ),
  local_protocol$local_evaluation$historical_weighted_r2_reference == 0.80,
  grepl("focal XGBoost", protocol$SHILD$model_sensitivity$near_equivalence, fixed = TRUE),
  grepl("ten aligned plausible values", protocol$PISA$near_equivalence, fixed = TRUE),
  identical(vapply(pisa_protocol_claims, `[[`, character(1L), "id"), pisa_claim_registry$claim_id),
  identical(vapply(pisa_protocol_claims, `[[`, character(1L), "decision"), pisa_claim_registry$decision),
  identical(pisa_protocol_claims[[1L]]$statement, pisa_c0$statement),
  identical(pisa_protocol_claims[[2L]]$statement, pisa_c1$statement),
  identical(
    unname(unlist(pisa_protocol_claims[[2L]]$coordinate_relations)),
    c("narrower", "same", "narrower", "same", "narrower", "same")
  )
)

cat("Evidence-synthesis helper tests passed.\n")
