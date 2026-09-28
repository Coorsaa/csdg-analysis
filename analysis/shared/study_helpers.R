# Shared helpers for the manuscript analyses.

.canonical_analysis_seed = 20260714L

`%or%` = function(x, y) {
  if (is.null(x) || !length(x) || (length(x) == 1L && is.na(x))) y else x
}

.assert_packages = function(packages) {
  missing = packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Install the required analysis packages: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.analysis_project_root = function() {
  args = commandArgs(trailingOnly = FALSE)
  file_arg = grep("^--file=", args, value = TRUE)
  script_path = if (length(file_arg)) {
    normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = FALSE)
  } else {
    ""
  }
  candidates = c(
    if (nzchar(script_path)) dirname(dirname(script_path)) else character(),
    ".", "..", "../.."
  )
  candidates = unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  is_root = function(path) {
    file.exists(file.path(path, "DESCRIPTION")) &&
      file.exists(file.path(path, "analysis", "shared", "study_helpers.R"))
  }
  hits = candidates[vapply(candidates, is_root, logical(1L))]
  if (!length(hits)) {
    stop("Could not locate the repository root from the script path or working directory.", call. = FALSE)
  }
  hits[[1L]]
}

.env_integer = function(name, default, lower = 0L) {
  raw = Sys.getenv(name, unset = "")
  if (!nzchar(raw)) {
    return(as.integer(default))
  }
  value = suppressWarnings(as.integer(raw))
  if (length(value) != 1L || is.na(value) || value < lower) {
    stop(sprintf("%s must be one integer >= %d.", name, lower), call. = FALSE)
  }
  value
}

.env_quick = function() {
  raw = Sys.getenv("CSDG_QUICK", unset = "0")
  if (!raw %in% c("0", "1")) {
    stop("CSDG_QUICK must be exactly 0 or 1.", call. = FALSE)
  }
  identical(raw, "1")
}

.analysis_settings = function(study = c("SHILD", "PISA")) {
  study = match.arg(study)
  quick = .env_quick()
  seed = .env_integer("CSDG_ANALYSIS_SEED", .canonical_analysis_seed)
  if (!quick && seed != .canonical_analysis_seed) {
    stop(
      "Publication-settings runs require CSDG_ANALYSIS_SEED=20260714; use CSDG_QUICK=1 for exploratory seeds.",
      call. = FALSE
    )
  }
  common = list(
    study = study,
    seed = seed,
    quick = quick,
    mode = if (quick) "NONPUBLICATION_QUICK" else "PUBLICATION",
    threads = .env_integer("CSDG_THREADS", 1L, lower = 1L),
    run_id = Sys.getenv("CSDG_RUN_ID", unset = "")
  )
  study_settings = if (identical(study, "SHILD")) {
    list(
      folds = if (quick) 3L else 10L,
      xgb_rounds = if (quick) 25L else 200L,
      forest_trees = if (quick) 60L else 400L,
      pfi_repetitions = if (quick) 2L else 10L,
      calibration_bootstrap_replicates = if (quick) 20L else 500L,
      local_perturbations = if (quick) 100L else 500L,
      effect_rows = if (quick) 150L else 1500L,
      effect_grid = if (quick) 4L else 10L,
      ale_bootstrap_replicates = if (quick) 20L else 500L,
      ale_bootstrap_workers = if (quick) 1L else .env_integer("CSDG_ALE_BOOTSTRAP_WORKERS", 4L)
    )
  } else {
    list(
      folds = if (quick) 3L else 5L,
      n_per_country = if (quick) 100L else 1500L,
      xgb_rounds = if (quick) 15L else 150L,
      forest_trees = if (quick) 50L else 300L,
      pfi_repetitions = if (quick) 2L else 10L,
      calibration_bootstrap_replicates = if (quick) 20L else 500L,
      matched_setting_repetitions = if (quick) 2L else 50L,
      effect_rows = if (quick) 120L else 1500L,
      effect_grid = if (quick) 4L else 10L,
      ale_bootstrap_replicates = if (quick) 20L else 500L,
      ale_bootstrap_workers = if (quick) 1L else .env_integer("CSDG_ALE_BOOTSTRAP_WORKERS", 4L)
    )
  }
  c(common, study_settings)
}

.analysis_calibration_metric_aliases = function() {
  data.table::data.table(
    canonical_metric = "uniform_grid_mean_absolute_calibration_error",
    alias = c(
      "uniform_grid_mean_absolute_calibration_error",
      "integrated_absolute_calibration_error"
    ),
    alias_status = c("canonical", "deprecated_alias"),
    definition = paste(
      "Arithmetic mean of the absolute flexible-calibration-curve deviation from the identity line at",
      "equally spaced points on the declared prediction grid; every grid point receives equal weight"
    ),
    grid_definition = paste(
      "The grid spans the 0.01 and 0.99 empirical prediction quantiles for classification, or the 0.005 and",
      "0.995 empirical prediction quantiles for regression, with the configured number of equally spaced points"
    ),
    weighting_measure = "Uniform discrete weighting over the equally spaced prediction-grid points",
    mathematical_integration = FALSE
  )
}

.analysis_shild_claim_registry = function() {
  data.table::data.table(
    claim_order = 0:3,
    claim_id = paste0("SHILD-C", 0:3),
    claim_version = paste0("C", 0:3),
    parent_claim_id = c(NA_character_, "SHILD-C0", "SHILD-C0", "SHILD-C0"),
    revision_relation = c(
      "original", "alternative_or_incomparable", "narrower", "alternative_or_incomparable"
    ),
    statement = c(
      paste(
        "The selected SHILD workflow supports reliable person-specific local explanations and the related",
        "non-diagnostic follow-up interpretation for the supplied analytic sample."
      ),
      paste(
        "Under the declared additive ridge-surrogate, neighborhood, weighting, output-scale, case-sampling, and",
        "cross-fitting design, the observed fidelity, absolute-error, locality-sensitivity, and encoded-term",
        "stability distributions are as reported."
      ),
      paste(
        "The evaluated local-surrogate procedure is sufficiently accurate and stable for a specified",
        "individual-level use."
      ),
      paste(
        "In the supplied unweighted two-extremes SHILD analytic sample, and under held-out marginal permutation",
        "of the selected XGBoost model, A2 and D2 produce much larger log-loss increases than the remaining",
        "predictors."
      )
    ),
    target = c(
      "Reliable person-specific local explanations and related follow-up interpretation",
      "Observed fidelity, error, locality-sensitivity, and encoded-term-stability distributions",
      "Accuracy and stability sufficient for a specified individual-level use",
      "Feature-wise held-out log-loss increase under marginal permutation"
    ),
    model_scope = c(
      "Selected SHILD workflow",
      "Frozen held-out-fold XGBoost models and cross-fitted additive ridge surrogates",
      "Evaluated cross-fitted additive ridge-surrogate procedure",
      "Selected XGBoost model"
    ),
    semantics = c(
      "Person-specific explanation reliability and use adequacy",
      "Descriptive evaluated-procedure result",
      "Use-adequacy judgment",
      "Global associational selected-model permutation importance"
    ),
    analytic_distribution = c(
      "Supplied unweighted Danish two-extremes analytic sample, ages 16-64",
      "Coverage-oriented 300-case sample from the same analytic distribution",
      "Same analytic distribution and evaluated local neighborhoods",
      "Supplied unweighted Danish two-extremes analytic sample, ages 16-64"
    ),
    scientific_use = c(
      paste(
        "Non-diagnostic person-level follow-up interpretation; no diagnosis, causal effect, or autonomous action"
      ),
      "Methods description only; no person-level adequacy, diagnosis, causal effect, or recourse",
      "Specified individual-level use requiring a defensible use-linked error criterion",
      "Global selected-model description only; no causal or person-specific interpretation"
    ),
    explanation_design = c(
      "Broad person-specific explanation class and follow-up interpretation",
      paste(
        "Additive ridge surrogate; declared synthetic and empirical neighborhoods, weights, response/link scales,",
        "case sampling, perturbation seeds, and surrogate cross-fitting"
      ),
      "The evaluated additive ridge-surrogate design with an explicit use-linked adequacy criterion",
      "Held-out fold-specific marginal permutation importance with log loss and ten repetitions"
    ),
    target_relation = c("same", "alternative_or_incomparable", "narrower", "alternative_or_incomparable"),
    model_scope_relation = c("same", "narrower", "narrower", "narrower"),
    semantics_relation = c("same", "alternative_or_incomparable", "narrower", "alternative_or_incomparable"),
    analytic_distribution_relation = c("same", "narrower", "narrower", "same"),
    scientific_use_relation = c("same", "narrower", "narrower", "alternative_or_incomparable"),
    explanation_design_relation = c(
      "same", "alternative_or_incomparable", "narrower", "alternative_or_incomparable"
    ),
    decision = c("not_met", "met", "unresolved", "met"),
    decision_scope = c(
      "Exact broad compound claim",
      "Descriptive result only",
      "No use-linked criterion available",
      "Selected model, supplied analytic sample, and declared perturbation"
    ),
    relation_rationale = c(
      "Original claim.",
      "The target and semantics change from use adequacy to a descriptive evaluated-procedure result.",
      "This restricts the broad claim to the evaluated procedure and a specified use.",
      paste(
        "The target, semantics, scientific use, and explanation design change from person-specific local",
        "interpretation to global selected-model marginal PFI."
      )
    ),
    decision_rationale = c(
      paste(
        "The prespecified joint local-reliability evidence pattern is a materialized potential defeater for the",
        "exact person-specific claim; it challenges that component without using an aggregate score or universal",
        "threshold, and the claim also exceeds the available follow-up-use evidence."
      ),
      "The declared procedure was executed, and its descriptive evidence distributions were exported.",
      paste(
        "No defensible use-linked error or stability criterion was supplied; the historical weighted-R-squared",
        "reference of 0.800 cannot adjudicate this claim."
      ),
      paste(
        "A2 and D2 rank first and second in held-out marginal PFI and are plainly separated from the remaining",
        "predictors for the selected model and perturbation."
      )
    ),
    linked_evidence_ids = c(
      paste(sprintf("SHILD-E%02d", 0:10), collapse = "|"),
      "SHILD-E11",
      "SHILD-E12",
      "SHILD-E13"
    ),
    linked_evidence_sources = c(
      "SHILD_EVIDENCE_SYNTHESIS.csv",
      "SHILD_EVIDENCE_SYNTHESIS.csv",
      "SHILD_EVIDENCE_SYNTHESIS.csv",
      "SHILD_EVIDENCE_SYNTHESIS.csv|shild_gate_g5__pfi__summary.csv"
    )
  )
}

.analysis_pisa_claim_registry = function() {
  data.table::data.table(
    claim_order = 4:5,
    claim_id = paste0("PISA-C", 0:1),
    claim_version = paste0("C", 0:1),
    parent_claim_id = c(NA_character_, "PISA-C0"),
    revision_relation = c("original", "narrower"),
    statement = c(
      paste(
        "Within the seeded, unweighted equal-country PISA 2018 analytic sample prepared and verified by the",
        "responsible co-authors, the selected XGBoost pipeline achieved lower held-out RMSE than the featureless",
        "learner for each of the ten reading plausible values, and within-country, school-aware held-out marginal",
        "PFI placed the same five atomic feature groups in the leading set for every plausible value; importance",
        "values were aggregated on the RMSE-increase scale for the selected-model summary. The pipeline is also",
        "sufficiently calibrated across the prediction range for the intended scientific interpretation."
      ),
      paste(
        "Within the seeded, unweighted equal-country PISA 2018 analytic sample prepared and verified by the",
        "responsible co-authors, the selected XGBoost pipeline achieved lower held-out RMSE than the featureless",
        "learner for each of the ten reading plausible values, and within-country, school-aware held-out marginal",
        "PFI placed the same five atomic feature groups in the leading set for every plausible value; importance",
        "values were aggregated on the RMSE-increase scale for the selected-model summary."
      )
    ),
    target = c(
      "Observed held-out RMSE contrast and selected-model PFI recurrence plus calibration adequacy",
      "Observed per-value held-out RMSE contrast and selected-model top-five PFI-set recurrence"
    ),
    model_scope = rep("Selected XGBoost regression pipeline", 2L),
    semantics = c(
      "Descriptive held-out prediction and selected-model PFI plus calibration-adequacy judgment",
      "Descriptive held-out prediction and global associational selected-model permutation importance"
    ),
    analytic_distribution = rep(
      "Seeded, unweighted equal-country PISA 2018 analytic sample from 32 observed countries",
      2L
    ),
    scientific_use = c(
      paste(
        "Calibration adequacy and global fitted-model description within the constructed distribution; no causal,",
        "individual, design-weighted population, future-cycle, or new-country interpretation"
      ),
      paste(
        "Bounded selected-model description only; no causal, individual, design-weighted population,",
        "future-cycle, or new-country interpretation"
      )
    ),
    explanation_design = c(
      paste(
        "Within-country, school-aware held-out marginal PFI computed separately by plausible value and aggregated",
        "on the RMSE-increase scale; the overlapping socioeconomic block is reported separately"
      ),
      paste(
        "Within-country, school-aware held-out marginal PFI computed separately by plausible value and aggregated",
        "on the RMSE-increase scale; the overlapping socioeconomic block is reported separately"
      )
    ),
    target_relation = c("same", "narrower"),
    model_scope_relation = c("same", "same"),
    semantics_relation = c("same", "narrower"),
    analytic_distribution_relation = c("same", "same"),
    scientific_use_relation = c("same", "narrower"),
    explanation_design_relation = c("same", "same"),
    decision = c("unresolved", "met"),
    decision_scope = c(
      "No claim-specific calibration-adequacy criterion available",
      "Selected model, constructed analytic sample, and declared held-out perturbation design"
    ),
    relation_rationale = c(
      "Original broad compound claim.",
      paste(
        "PISA-C1 retains the observed prediction comparison and PFI recurrence while removing the unadjudicated",
        "calibration-adequacy clause."
      )
    ),
    decision_rationale = c(
      paste(
        "Prediction, calibration, and PFI estimates are available, but no claim-specific criterion establishes",
        "calibration adequacy for the intended scientific interpretation; the favorable descriptive evidence",
        "cannot compensate for that unresolved necessary requirement."
      ),
      paste(
        "For every plausible value, selected XGBoost has lower held-out RMSE than the featureless learner, and the",
        "same five atomic feature groups form the leading held-out marginal-PFI set under the declared design."
      )
    ),
    linked_evidence_ids = c(
      "PISA-E00|PISA-E01|PISA-E02",
      "PISA-E03|PISA-E04|PISA-E05"
    ),
    linked_evidence_sources = rep("PISA_EVIDENCE_SYNTHESIS.csv", 2L)
  )
}

.analysis_pisa_claim_report_card = function() {
  report = data.table::data.table(
    claim_id = paste0("PISA-C", 0:1),
    decision = c("unresolved", "met"),
    decision_basis = c(
      paste(
        "Held-out prediction, calibration, and selected-model PFI estimates are available, but no external",
        "claim-specific calibration-adequacy criterion was supplied for the broad compound claim."
      ),
      paste(
        "Selected XGBoost has lower held-out RMSE than the featureless learner for every plausible value, and the",
        "same five atomic feature groups form the leading selected-model marginal-PFI set across all ten values."
      )
    ),
    permissible_interpretation = c(
      paste(
        "Report prediction, calibration, and PFI descriptively, but leave the broad compound claim unresolved",
        "until a claim-specific calibration criterion is supplied."
      ),
      paste(
        "Report the bounded held-out performance comparison and recurring leading selected-model PFI pattern in",
        "the constructed equal-country analytic sample."
      )
    ),
    prohibited_interpretation = c(
      "Do not infer calibration adequacy from descriptive estimates alone.",
      paste(
        "No exact model-invariant ranking, causal explanation, individual interpretation, design-weighted",
        "population result, future-cycle result, or transport claim."
      )
    )
  )
  registry = .analysis_pisa_claim_registry()
  registry_rows = match(report$claim_id, registry$claim_id)
  if (anyNA(registry_rows) || !identical(report$decision, registry$decision[registry_rows])) {
    stop("The PISA claim report card does not match the canonical claim registry.", call. = FALSE)
  }
  report[]
}

.analysis_pisa_claim_evidence = function(performance, calibration, recurrence, sensitivity) {
  require_columns = function(data, columns, label) {
    missing = setdiff(columns, names(data))
    if (length(missing)) {
      stop(label, " is missing required columns: ", paste(missing, collapse = ", "), ".", call. = FALSE)
    }
  }
  expected_outcomes = paste0("PV", seq_len(10L), "READ")
  performance = data.table::copy(data.table::as.data.table(performance))
  calibration = .analysis_standardize_calibration_metric(calibration)
  recurrence = data.table::copy(data.table::as.data.table(recurrence))
  sensitivity = data.table::copy(data.table::as.data.table(sensitivity))
  require_columns(
    performance,
    c("learner_id", "measure_id", "outcome_id", "estimate"),
    "PISA all-plausible-value performance"
  )
  require_columns(
    calibration,
    c("estimand", "mean_estimate"),
    "PISA all-plausible-value calibration summary"
  )
  require_columns(
    recurrence,
    c("outcome_id", "leading_set_jaccard_vs_pv1"),
    "PISA all-plausible-value PFI recurrence"
  )
  require_columns(
    sensitivity,
    c("learner_name", "focal_learner", "practical_reversal_fraction"),
    "PISA performance-explanation sensitivity"
  )

  performance = performance[
    learner_id %in% c("xgboost", "featureless") & measure_id == "regr.rmse",
    .(learner_id, outcome_id, estimate)
  ]
  if (nrow(performance) != 20L ||
      any(performance[, .N, by = .(learner_id, outcome_id)]$N != 1L) ||
      !setequal(performance$outcome_id, expected_outcomes) ||
      any(performance[, data.table::uniqueN(outcome_id), by = learner_id]$V1 != 10L) ||
      any(!is.finite(performance$estimate))) {
    stop("PISA performance does not contain one finite RMSE per learner and plausible value.", call. = FALSE)
  }
  xgboost_rmse = performance[learner_id == "xgboost", .(outcome_id, xgboost_rmse = estimate)]
  featureless_rmse = performance[learner_id == "featureless", .(outcome_id, featureless_rmse = estimate)]
  performance_comparison = merge(xgboost_rmse, featureless_rmse, by = "outcome_id", all = TRUE)
  performance_comparison[, outcome_order := match(outcome_id, expected_outcomes)]
  data.table::setorder(performance_comparison, outcome_order)
  performance_comparison[, outcome_order := NULL]
  if (nrow(performance_comparison) != 10L || anyNA(performance_comparison) ||
      !all(performance_comparison$xgboost_rmse < performance_comparison$featureless_rmse)) {
    stop("The bounded PISA held-out performance proposition is not reproduced.", call. = FALSE)
  }

  calibration_slope = calibration[estimand == "calibration_slope", mean_estimate]
  if (length(calibration_slope) != 1L || !is.finite(calibration_slope)) {
    stop("PISA calibration must contain one finite across-value calibration-slope summary.", call. = FALSE)
  }
  if (nrow(recurrence) != 10L || anyDuplicated(recurrence$outcome_id) ||
      !setequal(recurrence$outcome_id, expected_outcomes) ||
      any(!is.finite(recurrence$leading_set_jaccard_vs_pv1)) ||
      !all(recurrence$leading_set_jaccard_vs_pv1 == 1)) {
    stop("The bounded PISA leading-PFI-set recurrence proposition is not reproduced.", call. = FALSE)
  }
  if ("top_k" %in% names(recurrence) && any(recurrence$top_k != 5L)) {
    stop("PISA PFI recurrence must use the declared five-feature leading set.", call. = FALSE)
  }
  if ("n_ranked_features" %in% names(recurrence) && any(recurrence$n_ranked_features != 41L)) {
    stop("PISA PFI recurrence must use all 41 atomic feature groups.", call. = FALSE)
  }
  focal_learners = unique(sensitivity$focal_learner)
  alternative_sensitivity = sensitivity[learner_name != focal_learner]
  if (length(focal_learners) != 1L || is.na(focal_learners) || focal_learners != "xgboost" ||
      !nrow(alternative_sensitivity) ||
      any(!is.finite(alternative_sensitivity$practical_reversal_fraction)) ||
      any(alternative_sensitivity$practical_reversal_fraction < 0 |
        alternative_sensitivity$practical_reversal_fraction > 1)) {
    stop("PISA model-class sensitivity does not contain valid accepted alternatives.", call. = FALSE)
  }

  mean_xgboost_rmse = mean(performance_comparison$xgboost_rmse)
  mean_rmse_advantage = mean(
    performance_comparison$featureless_rmse - performance_comparison$xgboost_rmse
  )
  mean_recurrence = mean(recurrence$leading_set_jaccard_vs_pv1)
  maximum_practical_reversal = max(alternative_sensitivity$practical_reversal_fraction)
  data.table::data.table(
    evidence_id = sprintf("PISA-E%02d", 0:5),
    claim_id = c(rep("PISA-C0", 3L), rep("PISA-C1", 3L)),
    gate_id = c("G1", "G3a", "G5", "G1", "G5", "G6a"),
    applicable = TRUE,
    evidence_component = c(
      "Observed all-plausible-value prediction description",
      "Calibration-adequacy criterion",
      "Selected-model all-plausible-value PFI recurrence",
      "Held-out performance relative to a featureless learner",
      "Selected-model all-plausible-value PFI recurrence",
      "Model-class explanation sensitivity"
    ),
    role = c(
      "necessary_requirement", "necessary_requirement", "necessary_requirement",
      "necessary_requirement", "necessary_requirement", "descriptive_context"
    ),
    availability = c("complete", "unavailable", rep("complete", 4L)),
    result_direction = c("supports", "not_evaluated", "supports", "supports", "supports", "mixed"),
    estimate = c(
      mean_xgboost_rmse,
      NA_real_,
      mean_recurrence,
      mean_rmse_advantage,
      mean_recurrence,
      maximum_practical_reversal
    ),
    estimate_detail = c(
      paste0(
        "Selected-XGBoost mean held-out RMSE=", signif(mean_xgboost_rmse, 7L),
        "; all ten outcome-specific estimates are reported descriptively."
      ),
      paste0(
        "Mean calibration slope=", signif(calibration_slope, 7L),
        "; descriptive calibration evidence is available, but no adequacy criterion was supplied."
      ),
      "The same five atomic feature groups form the leading PFI set for all ten plausible values.",
      paste0(
        "Mean RMSE advantage over the featureless learner=", signif(mean_rmse_advantage, 7L),
        " score points; XGBoost has lower RMSE for every plausible value."
      ),
      "The same five atomic feature groups form the leading PFI set for all ten plausible values.",
      paste0(
        "Largest practical-pair reversal fraction among accepted alternatives=",
        signif(maximum_practical_reversal, 7L), "; this bounds model-general interpretation."
      )
    ),
    criterion = c(
      "Selected-XGBoost held-out RMSE is lower than featureless held-out RMSE for every plausible value.",
      NA_character_,
      "The same five atomic feature groups form the leading selected-model PFI set for all ten plausible values.",
      "Selected-XGBoost held-out RMSE is lower than featureless held-out RMSE for every plausible value.",
      "The same five atomic feature groups form the leading selected-model PFI set for all ten plausible values.",
      NA_character_
    ),
    criterion_direction = c(
      "qualitative", NA_character_, "qualitative", "qualitative", "qualitative", NA_character_
    ),
    criterion_source = c(
      "Canonical PISA-C0 compound proposition",
      NA_character_,
      "Canonical PISA-C0 compound proposition",
      "Canonical PISA-C1 descriptive proposition",
      "Canonical PISA-C1 descriptive proposition",
      NA_character_
    ),
    criterion_rationale = c(
      "This exact comparison is a descriptive conjunct of PISA-C0, not a universal adequacy threshold.",
      NA_character_,
      "This exact recurrence is the bounded PFI conjunct of PISA-C0, not a model-invariance criterion.",
      "This exact comparison is the bounded predictive content of PISA-C1, not a universal adequacy threshold.",
      "This exact recurrence is the bounded PFI content of PISA-C1, not a model-invariance criterion.",
      NA_character_
    ),
    materiality = "not_applicable",
    adjudication_basis = NA_character_,
    claim_consequence = c("none", "unresolved", "none", "retain_exact_claim", "retain_exact_claim", "none"),
    source_table = c(
      "pisa_all_pv_pooled_performance.csv",
      "pisa_all_pv_calibration_descriptive_summary.csv",
      "pisa_all_pv_leading_importance_recurrence.csv",
      "pisa_all_pv_pooled_performance.csv",
      "pisa_all_pv_leading_importance_recurrence.csv",
      "pisa_performance_explanation_sensitivity.csv"
    ),
    rationale = c(
      "The observed all-plausible-value prediction result supports the descriptive component of PISA-C0.",
      "Observed calibration cannot adjudicate adequacy without a claim-specific criterion.",
      "The recurring selected-model PFI pattern supports only the declared perturbation-specific description.",
      "The complete ten-value comparison directly matches the bounded predictive proposition.",
      "The complete ten-value leading-set recurrence directly matches the bounded PFI proposition.",
      "Accepted-model disagreement constrains any model-general ranking but does not contradict PISA-C1."
    )
  )[]
}

.analysis_standardize_calibration_metric = function(value) {
  old = "integrated_absolute_calibration_error"
  new = "uniform_grid_mean_absolute_calibration_error"
  if (data.table::is.data.table(value) || is.data.frame(value)) {
    result = data.table::copy(data.table::as.data.table(value))
    if (old %in% names(result)) {
      if (new %in% names(result)) {
        result[, (old) := NULL]
      } else {
        data.table::setnames(result, old, new)
      }
    }
    identifier_columns = intersect(c("estimand", "metric", "metric_id"), names(result))
    for (column in identifier_columns) {
      identifier = as.character(result[[column]])
      alias_rows = !is.na(identifier) & identifier == old
      canonical_rows = !is.na(identifier) & identifier == new
      alias_indices = which(alias_rows)
      if (any(alias_rows) && any(canonical_rows)) {
        result = result[-alias_indices]
      } else if (any(alias_rows)) {
        result[alias_indices, (column) := new]
        if ("canonical_estimand" %in% names(result)) {
          result[alias_indices, canonical_estimand := new]
        }
        if ("estimand_status" %in% names(result)) {
          result[alias_indices, estimand_status := "canonical"]
        }
      }
    }
    if (all(c("estimand", "definition") %in% names(result))) {
      result[estimand == new, definition := .analysis_calibration_metric_aliases()[
        alias_status == "canonical",
        definition
      ]]
    }
    return(result[])
  }
  if (is.list(value)) {
    return(lapply(value, .analysis_standardize_calibration_metric))
  }
  value
}

.analysis_add_practical_tie_denominators = function(data) {
  result = data.table::copy(data.table::as.data.table(data))
  required = c("n_pairs", "n_comparable_pairs", "n_practical_reversals")
  missing = setdiff(required, names(result))
  if (length(missing)) {
    stop(
      "Practical-pair table is missing denominator columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  result[, `:=`(
    n_total_pairs = as.integer(n_pairs),
    n_practical_ties_excluded = as.integer(n_pairs - n_comparable_pairs),
    n_comparable_pairs = as.integer(n_comparable_pairs),
    n_practical_reversals = as.integer(n_practical_reversals),
    practical_reversal_percent_among_comparable = data.table::fifelse(
      n_comparable_pairs > 0L,
      100 * n_practical_reversals / n_comparable_pairs,
      NA_real_
    )
  )]
  if (any(result$n_practical_ties_excluded < 0L) ||
      any(result$n_comparable_pairs + result$n_practical_ties_excluded != result$n_total_pairs) ||
      any(result$n_practical_reversals > result$n_comparable_pairs)) {
    stop("Practical-pair denominators are internally inconsistent.", call. = FALSE)
  }
  result[]
}

.assert_canonical_environment = function() {
  supported = c(
    "CSDG_ANALYSIS_SEED", "CSDG_QUICK", "CSDG_THREADS", "CSDG_ALE_BOOTSTRAP_WORKERS", "CSDG_RUN_ID"
  )
  unsupported = c(
    "CSDG_SEED", "CSDG_PROFILE", "CSDG_SHILD_FOLDS", "CSDG_PISA_FOLDS",
    "CSDG_PISA_N_PER_COUNTRY", "AUTOIML_SEED"
  )
  set = unsupported[nzchar(Sys.getenv(unsupported, unset = ""))]
  if (length(set)) {
    stop(
      "Unsupported analysis environment variable(s): ",
      paste(set, collapse = ", "),
      ". Supported variables are: ",
      paste(supported, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.require_files = function(paths) {
  missing = paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Required input file(s) are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(normalizePath(paths, winslash = "/", mustWork = TRUE))
}

.write_table = function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fwrite(as.data.table(x), path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.analysis_figure_metadata_columns = c(
  "study", "figure_id", "figure_group_id", "is_grouped_figure", "latex_group_id",
  "panel_id", "panel_order", "panel_heading", "pdf_path", "png_path", "title", "subtitle", "caption",
  "group_title", "group_subtitle", "group_caption", "source_tables", "generator_script", "generator_function",
  "width_in", "height_in", "role", "estimand_or_scope", "uncertainty_semantics", "privacy_scope",
  "latex_assembly_note"
)

.analysis_figure_registry = new.env(parent = emptyenv())
.analysis_figure_registry$records = list()

.analysis_empty_figure_metadata = function() {
  data.table(
    study = character(),
    figure_id = character(),
    figure_group_id = character(),
    is_grouped_figure = logical(),
    latex_group_id = character(),
    panel_id = character(),
    panel_order = integer(),
    panel_heading = character(),
    pdf_path = character(),
    png_path = character(),
    title = character(),
    subtitle = character(),
    caption = character(),
    group_title = character(),
    group_subtitle = character(),
    group_caption = character(),
    source_tables = character(),
    generator_script = character(),
    generator_function = character(),
    width_in = numeric(),
    height_in = numeric(),
    role = character(),
    estimand_or_scope = character(),
    uncertainty_semantics = character(),
    privacy_scope = character(),
    latex_assembly_note = character()
  )
}

.analysis_reset_figure_registry = function() {
  .analysis_figure_registry$records = list()
  invisible(TRUE)
}

.analysis_figure_registry_table = function() {
  records = .analysis_figure_registry$records
  if (!length(records)) {
    return(.analysis_empty_figure_metadata())
  }
  result = rbindlist(records, use.names = TRUE, fill = TRUE)
  setcolorder(result, .analysis_figure_metadata_columns)
  result[]
}

.analysis_clear_figure_files = function(figures_dir) {
  if (!is.character(figures_dir) || length(figures_dir) != 1L || is.na(figures_dir) || !nzchar(figures_dir)) {
    stop("`figures_dir` must be one explicit, non-empty directory path.", call. = FALSE)
  }
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(figures_dir)) {
    stop("Could not create the explicit figures directory: ", figures_dir, call. = FALSE)
  }
  normalized_dir = normalizePath(figures_dir, winslash = "/", mustWork = TRUE)
  if (!identical(basename(normalized_dir), "figures")) {
    stop("`figures_dir` must resolve to a directory named exactly `figures`.", call. = FALSE)
  }
  files = list.files(
    figures_dir,
    pattern = "\\.(?:pdf|png)$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  files = files[file.info(files)$isdir %in% FALSE]
  if (length(files) && any(!file.remove(files))) {
    stop("Could not clear every PDF/PNG from the explicit figures directory.", call. = FALSE)
  }
  invisible(gsub("\\\\", "/", files))
}

.analysis_plot_text = function(value) {
  if (is.null(value) || !length(value) || inherits(value, "waiver")) {
    return(NA_character_)
  }
  value = paste(as.character(value), collapse = " ")
  if (is.na(value) || !nzchar(value)) NA_character_ else value
}

.analysis_plot_outer_text = function(plot) {
  annotation = if (inherits(plot, "patchwork") && !is.null(plot$patches$annotation)) {
    plot$patches$annotation
  } else {
    list()
  }
  labels = plot$labels %or% list()
  list(
    title = .analysis_plot_text(annotation$title %or% labels$title),
    subtitle = .analysis_plot_text(annotation$subtitle %or% labels$subtitle),
    caption = .analysis_plot_text(annotation$caption %or% labels$caption)
  )
}

.analysis_strip_plot_outer_text = function(plot, suppress_strip = FALSE) {
  if (!is.null(plot$labels)) {
    plot$labels$title = NULL
    plot$labels$subtitle = NULL
    plot$labels$caption = NULL
  }
  if (inherits(plot, "patchwork") && !is.null(plot$patches$annotation)) {
    plot$patches$annotation$title = NULL
    plot$patches$annotation$subtitle = NULL
    plot$patches$annotation$caption = NULL
  }
  plot + ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank(),
    strip.text = if (suppress_strip) ggplot2::element_blank() else NULL,
    strip.background = if (suppress_strip) ggplot2::element_blank() else NULL
  )
}

.analysis_figure_slug = function(value) {
  value = iconv(as.character(value), from = "", to = "ASCII//TRANSLIT", sub = "")
  value = tolower(value)
  value = gsub("[^a-z0-9]+", "_", value)
  value = gsub("^_+|_+$", "", value)
  value[!nzchar(value)] = "panel"
  value
}

.analysis_facet_layout = function(plot) {
  if (!inherits(plot, "ggplot") || inherits(plot, "patchwork")) {
    return(list(layout = data.table(), facet_names = character()))
  }
  built = ggplot2::ggplot_build(plot)
  layout = as.data.table(built$layout$layout)
  administrative = c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD", "AXIS_X", "AXIS_Y")
  facet_names = setdiff(names(layout), administrative)
  if (!length(facet_names)) {
    return(list(layout = data.table(), facet_names = character()))
  }
  keep = c(intersect(c("PANEL", "ROW", "COL"), names(layout)), facet_names)
  layout = unique(layout[, ..keep])
  order_columns = intersect(c("ROW", "COL", "PANEL"), names(layout))
  if (length(order_columns)) setorderv(layout, order_columns)
  list(layout = layout, facet_names = facet_names)
}

.analysis_subset_facet_data = function(data, facet_values) {
  if (!is.data.frame(data)) return(data)
  result = data
  for (name in intersect(names(facet_values), names(result))) {
    target = facet_values[[name]]
    keep = if (length(target) != 1L || is.na(target)) {
      is.na(result[[name]])
    } else {
      as.character(result[[name]]) == as.character(target)
    }
    result = result[keep, , drop = FALSE]
  }
  result
}

.analysis_facet_heading = function(facet_values) {
  values = vapply(facet_values, function(value) {
    if (!length(value) || is.na(value)) "NA" else as.character(value)
  }, character(1L))
  if (length(values) == 1L) values[[1L]] else paste(paste(names(values), values, sep = " = "), collapse = " | ")
}

.analysis_panel_source_rows = function(plot, facet_values) {
  sources = c(list(plot$data), lapply(plot$layers, function(layer) layer$data))
  sources = sources[vapply(sources, is.data.frame, logical(1L))]
  if (!length(sources)) return(data.table())
  rows = lapply(sources, .analysis_subset_facet_data, facet_values = facet_values)
  rows = rows[vapply(rows, nrow, integer(1L)) > 0L]
  if (!length(rows)) data.table() else as.data.table(rows[[1L]])
}

.analysis_unique_semantic_value = function(rows, column) {
  if (!column %in% names(rows)) return(character())
  values = unique(as.character(rows[[column]]))
  values = values[!is.na(values) & nzchar(values)]
  if (length(values) == 1L) values else character()
}

.analysis_panel_suffix = function(plot, facet_values) {
  rows = .analysis_panel_source_rows(plot, facet_values)
  pairs = list(c("feature_1", "feature_2"), c("feature1", "feature2"))
  for (pair in pairs) {
    values = lapply(pair, function(column) .analysis_unique_semantic_value(rows, column))
    if (all(lengths(values) == 1L)) {
      return(paste(.analysis_figure_slug(unlist(values, use.names = FALSE)), collapse = "_x_"))
    }
  }
  semantic_columns = c("measure", "measure_id", "audit_variable", "study", "feature", "feature_group")
  for (column in semantic_columns) {
    value = .analysis_unique_semantic_value(rows, column)
    if (length(value)) return(.analysis_figure_slug(value))
  }
  .analysis_figure_slug(paste(unlist(facet_values, use.names = FALSE), collapse = "_"))
}

.analysis_disambiguate_panel_suffixes = function(suffixes, headings) {
  result = suffixes
  duplicates = duplicated(result) | duplicated(result, fromLast = TRUE)
  result[duplicates] = paste(result[duplicates], .analysis_figure_slug(headings[duplicates]), sep = "_")
  if (anyDuplicated(result)) {
    occurrence = ave(seq_along(result), result, FUN = seq_along)
    repeated = duplicated(result) | duplicated(result, fromLast = TRUE)
    result[repeated] = paste(result[repeated], occurrence[repeated], sep = "_")
  }
  result
}

.analysis_plot_components = function(plot, stem) {
  if (!inherits(plot, c("gg", "ggplot", "patchwork"))) {
    stop("`plot` must be a ggplot or patchwork object.", call. = FALSE)
  }
  if (inherits(plot, "patchwork")) {
    stop(
      "Combined patchwork figures are not allowed; save and register each component as a separate figure.",
      call. = FALSE
    )
  }
  if (!is.character(stem) || length(stem) != 1L || is.na(stem) || !nzchar(stem)) {
    stop("`stem` must be one non-empty path without a file extension.", call. = FALSE)
  }
  if (grepl("\\.(?:pdf|png)$", stem, ignore.case = TRUE)) {
    stop("`stem` must not include a PDF or PNG extension.", call. = FALSE)
  }
  outer_text = .analysis_plot_outer_text(plot)
  facet = .analysis_facet_layout(plot)
  if (!nrow(facet$layout)) {
    return(list(list(
      plot = .analysis_strip_plot_outer_text(plot),
      stem = stem,
      figure_id = basename(stem),
      figure_group_id = basename(stem),
      is_grouped_figure = FALSE,
      panel_id = NA_character_,
      panel_order = 1L,
      panel_heading = NA_character_,
      title = outer_text$title,
      subtitle = outer_text$subtitle,
      caption = outer_text$caption,
      group_title = NA_character_,
      group_subtitle = NA_character_,
      group_caption = NA_character_,
      latex_assembly_note = NA_character_,
      width_fraction = 1,
      height_fraction = 1
    )))
  }

  facet_values = lapply(seq_len(nrow(facet$layout)), function(index) {
    as.list(facet$layout[index, facet$facet_names, with = FALSE])
  })
  plot_sources = c(list(plot$data), lapply(plot$layers, function(layer) layer$data))
  supports_facet_split = any(vapply(plot_sources, function(source) {
    is.data.frame(source) && all(facet$facet_names %in% names(source))
  }, logical(1L)))
  if (length(facet_values) > 1L && !supports_facet_split) {
    stop(
      "Faceted plots must expose their facet variables as data columns before they can be split safely.",
      call. = FALSE
    )
  }
  headings = vapply(facet_values, .analysis_facet_heading, character(1L))
  suffixes = vapply(facet_values, function(values) .analysis_panel_suffix(plot, values), character(1L))
  suffixes = .analysis_disambiguate_panel_suffixes(suffixes, headings)
  multipanel = length(facet_values) > 1L
  panel_rows = max(as.integer(facet$layout$ROW), na.rm = TRUE)
  panel_columns = max(as.integer(facet$layout$COL), na.rm = TRUE)
  assembly_note = if (multipanel) {
    paste0(
      "Grouped ", panel_rows, "-row by ", panel_columns,
      "-column figure; assemble the component files in panel_order with LaTeX."
    )
  } else {
    "The facet-strip heading is recorded in panel_heading and should be supplied through LaTeX."
  }

  lapply(seq_along(facet_values), function(index) {
    values = facet_values[[index]]
    panel_plot = plot
    panel_plot$data = .analysis_subset_facet_data(panel_plot$data, values)
    panel_plot$layers = lapply(plot$layers, function(layer) {
      cloned_layer = ggplot2::ggproto(NULL, layer)
      cloned_layer$data = .analysis_subset_facet_data(layer$data, values)
      cloned_layer
    })
    panel_plot$facet = ggplot2::facet_null()
    component_stem = if (multipanel) paste0(stem, "__", suffixes[[index]]) else stem
    list(
      plot = .analysis_strip_plot_outer_text(panel_plot, suppress_strip = TRUE),
      stem = component_stem,
      figure_id = basename(component_stem),
      figure_group_id = basename(stem),
      is_grouped_figure = multipanel,
      panel_id = suffixes[[index]],
      panel_order = as.integer(index),
      panel_heading = headings[[index]],
      title = if (multipanel) NA_character_ else outer_text$title,
      subtitle = if (multipanel) NA_character_ else outer_text$subtitle,
      caption = if (multipanel) NA_character_ else outer_text$caption,
      group_title = if (multipanel) outer_text$title else NA_character_,
      group_subtitle = if (multipanel) outer_text$subtitle else NA_character_,
      group_caption = if (multipanel) outer_text$caption else NA_character_,
      latex_assembly_note = assembly_note,
      width_fraction = if (multipanel) 1 / panel_columns else 1,
      height_fraction = if (multipanel) 1 / panel_rows else 1
    )
  })
}

.analysis_metadata_value = function(metadata, name, default = NA_character_) {
  value = metadata[[name]]
  if (is.null(value) || !length(value)) default else value[[1L]]
}

.analysis_register_figure_component = function(component, width, height, metadata) {
  existing = .analysis_figure_registry_table()
  if (component$figure_id %in% existing$figure_id) {
    stop("A figure component is already registered: ", component$figure_id, call. = FALSE)
  }
  figure_group_id = as.character(.analysis_metadata_value(
    metadata,
    "figure_group_id",
    component$figure_group_id
  ))
  is_grouped_figure = as.logical(.analysis_metadata_value(
    metadata,
    "is_grouped_figure",
    component$is_grouped_figure
  ))
  panel_id = as.character(.analysis_metadata_value(metadata, "panel_id", component$panel_id))
  panel_order = as.integer(.analysis_metadata_value(metadata, "panel_order", component$panel_order))
  panel_heading = as.character(.analysis_metadata_value(metadata, "panel_heading", component$panel_heading))
  title = as.character(.analysis_metadata_value(metadata, "title", component$title))
  subtitle = as.character(.analysis_metadata_value(metadata, "subtitle", component$subtitle))
  caption = as.character(.analysis_metadata_value(metadata, "caption", component$caption))
  group_title = as.character(.analysis_metadata_value(metadata, "group_title", component$group_title))
  group_subtitle = as.character(.analysis_metadata_value(metadata, "group_subtitle", component$group_subtitle))
  group_caption = as.character(.analysis_metadata_value(metadata, "group_caption", component$group_caption))
  if (length(figure_group_id) != 1L || is.na(figure_group_id) || !nzchar(figure_group_id) ||
      length(is_grouped_figure) != 1L || is.na(is_grouped_figure) ||
      length(panel_order) != 1L || is.na(panel_order) || panel_order < 1L) {
    stop(
      "Explicit figure-component metadata has an invalid group ID, grouped-figure flag, or panel order.",
      call. = FALSE
    )
  }
  note = .analysis_metadata_value(metadata, "latex_assembly_note", component$latex_assembly_note)
  if (is.na(note) || !nzchar(note)) {
    note = if (is_grouped_figure) {
      "Grouped figure; assemble the component files in panel_order with LaTeX."
    } else if (!is.na(panel_heading)) {
      "The facet-strip heading is recorded in panel_heading and should be supplied through LaTeX."
    } else {
      "Outer title, subtitle, and caption are recorded here and should be supplied through LaTeX."
    }
  }
  record = data.table(
    study = as.character(.analysis_metadata_value(metadata, "study")),
    figure_id = component$figure_id,
    figure_group_id = figure_group_id,
    is_grouped_figure = is_grouped_figure,
    latex_group_id = as.character(.analysis_metadata_value(
      metadata,
      "latex_group_id",
      figure_group_id
    )),
    panel_id = panel_id,
    panel_order = panel_order,
    panel_heading = panel_heading,
    pdf_path = gsub("\\\\", "/", paste0(component$stem, ".pdf")),
    png_path = gsub("\\\\", "/", paste0(component$stem, ".png")),
    title = title,
    subtitle = subtitle,
    caption = caption,
    group_title = group_title,
    group_subtitle = group_subtitle,
    group_caption = group_caption,
    source_tables = as.character(.analysis_metadata_value(metadata, "source_tables")),
    generator_script = as.character(.analysis_metadata_value(metadata, "generator_script")),
    generator_function = as.character(.analysis_metadata_value(metadata, "generator_function")),
    width_in = as.numeric(width),
    height_in = as.numeric(height),
    role = as.character(.analysis_metadata_value(metadata, "role")),
    estimand_or_scope = as.character(.analysis_metadata_value(metadata, "estimand_or_scope")),
    uncertainty_semantics = as.character(.analysis_metadata_value(metadata, "uncertainty_semantics")),
    privacy_scope = as.character(.analysis_metadata_value(metadata, "privacy_scope")),
    latex_assembly_note = note
  )
  .analysis_figure_registry$records[[length(.analysis_figure_registry$records) + 1L]] = record
  invisible(record)
}

.analysis_align_component_widths = function(components) {
  if (length(components) < 2L) return(components)

  grobs = lapply(components, function(component) ggplot2::ggplotGrob(component$plot))
  width_counts = vapply(grobs, function(grob) length(grob$widths), integer(1L))
  if (length(unique(width_counts)) != 1L) {
    stop("Grouped plot components do not have compatible layout columns for width alignment.", call. = FALSE)
  }

  shared_widths = do.call(grid::unit.pmax, lapply(grobs, function(grob) grob$widths))
  lapply(seq_along(components), function(index) {
    grobs[[index]]$widths = shared_widths
    components[[index]]$plot = grobs[[index]]
    components[[index]]
  })
}

.save_plot = function(
    plot,
    stem,
    width = 8,
    height = 5.5,
    metadata = list(),
    min_component_width = 5,
    min_component_height = 4,
    align_component_widths = FALSE) {
  if (!is.list(metadata) || (length(metadata) && (is.null(names(metadata)) || any(!nzchar(names(metadata)))))) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0 ||
      !is.numeric(height) || length(height) != 1L || !is.finite(height) || height <= 0) {
    stop("Figure width and height must be positive finite numbers.", call. = FALSE)
  }
  if (!is.numeric(min_component_width) || length(min_component_width) != 1L ||
      !is.finite(min_component_width) || min_component_width <= 0 ||
      !is.numeric(min_component_height) || length(min_component_height) != 1L ||
      !is.finite(min_component_height) || min_component_height <= 0) {
    stop("Minimum component width and height must be positive finite numbers.", call. = FALSE)
  }
  if (!is.logical(align_component_widths) || length(align_component_widths) != 1L ||
      is.na(align_component_widths)) {
    stop("`align_component_widths` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  components = .analysis_plot_components(plot, stem)
  components = lapply(components, function(component) {
    component$plot = .analysis_raise_point_layers(component$plot)
    component
  })
  if (align_component_widths) components = .analysis_align_component_widths(components)
  cache = Sys.getenv("XDG_CACHE_HOME", unset = "")
  if (!nzchar(cache) || !dir.exists(cache) || file.access(cache, 2L) != 0L) {
    cache = file.path(tempdir(), "csdg-cache")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    Sys.setenv(XDG_CACHE_HOME = cache)
  }
  paths = unlist(lapply(components, function(component) {
    dir.create(dirname(component$stem), recursive = TRUE, showWarnings = FALSE)
    pdf_path = paste0(component$stem, ".pdf")
    png_path = paste0(component$stem, ".png")
    component_width = if (component$is_grouped_figure) {
      max(min_component_width, width * component$width_fraction)
    } else {
      width
    }
    component_height = if (component$is_grouped_figure) {
      max(min_component_height, height * component$height_fraction)
    } else {
      height
    }
    component_dpi = ceiling(max(300, 1500 / component_width, 1500 / component_height))
    ggsave(
      pdf_path, plot = component$plot, width = component_width, height = component_height,
      units = "in", device = grDevices::cairo_pdf, bg = "white"
    )
    ggsave(
      png_path, plot = component$plot, width = component_width, height = component_height,
      units = "in", dpi = component_dpi, bg = "white"
    )
    .analysis_register_figure_component(component, component_width, component_height, metadata)
    c(pdf_path, png_path)
  }), use.names = FALSE)
  invisible(paths)
}

.analysis_scalar_missing = function(value) {
  is.null(value) || !length(value) || is.na(value[[1L]]) ||
    (is.character(value[[1L]]) && !nzchar(value[[1L]]))
}

.analysis_catalog_value = function(row, candidates) {
  column = candidates[candidates %in% names(row)][1L]
  if (is.na(column)) return(NULL)
  row[[column]][[1L]]
}

.analysis_relative_figure_path = function(path, run_dir) {
  if (is.null(run_dir) || !length(run_dir) || is.na(run_dir) || !nzchar(run_dir)) return(path)
  normalized_path = normalizePath(path, winslash = "/", mustWork = FALSE)
  normalized_root = normalizePath(run_dir, winslash = "/", mustWork = FALSE)
  prefix = paste0(normalized_root, "/")
  if (startsWith(normalized_path, prefix)) substring(normalized_path, nchar(prefix) + 1L) else path
}

.analysis_expand_figure_catalog = function(
    catalog,
    study = NULL,
    generator_script = NULL,
    generator_function = NULL,
    registry = .analysis_figure_registry_table(),
    run_dir = NULL,
    strict = TRUE) {
  catalog = copy(as.data.table(catalog))
  registry = copy(as.data.table(registry))
  if (!"figure_id" %in% names(catalog) || !nrow(catalog) || anyNA(catalog$figure_id) ||
      any(!nzchar(catalog$figure_id)) || anyDuplicated(catalog$figure_id)) {
    stop("The base figure catalog must have one unique non-empty `figure_id` per figure group.", call. = FALSE)
  }
  if (!all(.analysis_figure_metadata_columns %in% names(registry))) {
    stop("The figure registry does not have the required metadata schema.", call. = FALSE)
  }
  missing_catalog = setdiff(unique(registry$figure_group_id), catalog$figure_id)
  missing_registry = setdiff(catalog$figure_id, unique(registry$figure_group_id))
  if (length(missing_catalog) || (isTRUE(strict) && length(missing_registry))) {
    stop(
      "The figure registry and base catalog do not match: missing catalog IDs [",
      paste(missing_catalog, collapse = ", "),
      "]; missing registry IDs [",
      paste(missing_registry, collapse = ", "),
      "].",
      call. = FALSE
    )
  }
  if (!nrow(registry)) return(.analysis_empty_figure_metadata())

  field_candidates = list(
    study = "study",
    source_tables = "source_tables",
    generator_script = "generator_script",
    generator_function = "generator_function",
    role = "role",
    estimand_or_scope = c("estimand_or_scope", "estimand_scope"),
    uncertainty_semantics = "uncertainty_semantics",
    privacy_scope = c("privacy_scope", "privacy"),
    latex_assembly_note = "latex_assembly_note"
  )
  for (index in seq_len(nrow(registry))) {
    base = catalog[figure_id == registry$figure_group_id[[index]]]
    for (field in names(field_candidates)) {
      if (.analysis_scalar_missing(registry[[field]][index])) {
        value = .analysis_catalog_value(base, field_candidates[[field]])
        if (!.analysis_scalar_missing(value)) set(registry, index, field, value)
      }
    }
  }
  study_value = study
  generator_script_value = generator_script
  generator_function_value = generator_function
  if (!is.null(study_value)) {
    registry[is.na(study) | !nzchar(study), study := as.character(study_value)]
  }
  if (!is.null(generator_script_value)) {
    registry[
      is.na(generator_script) | !nzchar(generator_script),
      generator_script := as.character(generator_script_value)
    ]
  }
  if (!is.null(generator_function_value)) {
    registry[
      is.na(generator_function) | !nzchar(generator_function),
      generator_function := as.character(generator_function_value)
    ]
  }
  registry[, `:=`(
    pdf_path = vapply(pdf_path, .analysis_relative_figure_path, character(1L), run_dir = run_dir),
    png_path = vapply(png_path, .analysis_relative_figure_path, character(1L), run_dir = run_dir)
  )]
  setcolorder(registry, .analysis_figure_metadata_columns)
  registry[]
}

.analysis_catalog_source_paths = function(catalog) {
  paths = trimws(unlist(strsplit(catalog$source_tables, "|", fixed = TRUE), use.names = FALSE))
  paths = trimws(sub(";.*$", "", paths))
  valid = grepl("^tables/[A-Za-z0-9._-]+\\.csv$", paths)
  if (!length(paths) || any(!valid)) {
    stop("The figure catalog contains an invalid source-table path.", call. = FALSE)
  }
  sort(unique(paths))
}

.analysis_write_figure_latex_metadata = function(metadata, path, run_dir = NULL) {
  metadata = copy(as.data.table(metadata))
  missing = setdiff(.analysis_figure_metadata_columns, names(metadata))
  if (length(missing)) {
    stop("Figure LaTeX metadata is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyNA(metadata$figure_id) || any(!nzchar(metadata$figure_id)) || anyDuplicated(metadata$figure_id)) {
    stop("Figure LaTeX metadata must have one unique non-empty actual figure ID per row.", call. = FALSE)
  }
  metadata[, `:=`(
    pdf_path = vapply(pdf_path, .analysis_relative_figure_path, character(1L), run_dir = run_dir),
    png_path = vapply(png_path, .analysis_relative_figure_path, character(1L), run_dir = run_dir)
  )]
  setcolorder(metadata, .analysis_figure_metadata_columns)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fwrite(metadata, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.analysis_palette = function() {
  autoiml_palette()
}

.analysis_plot_font_family = function() {
  if (!"Arial" %in% names(grDevices::pdfFonts()) && "ArialMT" %in% names(grDevices::pdfFonts())) {
    font = grDevices::pdfFonts("ArialMT")[[1L]]
    font$family = "Arial"
    try(do.call(grDevices::pdfFonts, setNames(list(font), "Arial")), silent = TRUE)
  }
  if (!"Arial" %in% names(grDevices::postscriptFonts()) &&
      "ArialMT" %in% names(grDevices::postscriptFonts())) {
    font = grDevices::postscriptFonts("ArialMT")[[1L]]
    font$family = "Arial"
    try(do.call(grDevices::postscriptFonts, setNames(list(font), "Arial")), silent = TRUE)
  }
  if ("Arial" %in% names(grDevices::pdfFonts())) "Arial" else "sans"
}

.analysis_theme = function(base_size = 11, line_base_size = 11) {
  font_family = .analysis_plot_font_family()
  theme_minimal(base_size = line_base_size, base_family = font_family) +
    theme(
      text = element_text(family = font_family, size = base_size),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.title.position = "plot",
      plot.title = element_text(face = "plain"),
      axis.title = element_text(face = "plain"),
      plot.margin = margin(10, 18, 12, 18)
    )
}

.analysis_ale_semantics = function(study, dimension = c("1d", "2d"), support_threshold = 20L,
    reference_n = 1500L, bootstrap_replicates = 500L, available = TRUE) {
  study = match.arg(study, c("SHILD", "PISA"))
  dimension = match.arg(dimension)
  if (!is.numeric(support_threshold) || length(support_threshold) != 1L || is.na(support_threshold) ||
      support_threshold < 1 || support_threshold != as.integer(support_threshold)) {
    stop("`support_threshold` must be one positive integer.", call. = FALSE)
  }
  if (!is.logical(available) || length(available) != 1L || is.na(available)) {
    stop("`available` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.numeric(reference_n) || length(reference_n) != 1L || is.na(reference_n) || reference_n < 1 ||
      reference_n != as.integer(reference_n) || !is.numeric(bootstrap_replicates) ||
      length(bootstrap_replicates) != 1L || is.na(bootstrap_replicates) || bootstrap_replicates < 20 ||
      bootstrap_replicates != as.integer(bootstrap_replicates)) {
    stop("`reference_n` and `bootstrap_replicates` must be positive integer counts.", call. = FALSE)
  }
  reference_sample_label = paste0(format(reference_n, big.mark = ","), "-row")

  study_semantics = if (study == "SHILD") {
    list(
      selected_learner = "Selected focal XGBoost classification pipeline",
      plausible_value_scope = "Not applicable; SHILD uses one observed binary outcome",
      analytic_distribution = paste(
        "Target: unweighted two-extremes SHILD analytic sample of 16,971 respondents aged 16-64 with an observed",
        "content or discontent outcome. Curves use a seeded", reference_sample_label,
        "reference sample and empirical intervals trimmed to the 5th-95th percentile support"
      ),
      bootstrap_unit = "Respondent rows sampled with replacement",
      omitted_uncertainty = paste(
        "Learner-selection and hyperparameter-search variation, preprocessing and source-measurement uncertainty,",
        "and uncertainty for future populations are omitted"
      )
    )
  } else {
    list(
      selected_learner = "Selected focal XGBoost regression pipeline",
      plausible_value_scope = paste(
        "First reading plausible value only (PV1READ); no pooling over the ten reading plausible values"
      ),
      analytic_distribution = paste(
        "Target: unweighted 48,000-student equal-country analytic sample across 32 observed countries, conditional",
        "on the aligned selected features and fixed analysis specification. Curves use a seeded",
        reference_sample_label, "reference sample and empirical intervals trimmed to the 5th-95th percentile support"
      ),
      bootstrap_unit = "Schools sampled with replacement independently within country",
      omitted_uncertainty = paste(
        "Across-plausible-value variation, survey weights and survey-design uncertainty, learner-selection and",
        "hyperparameter-search variation, preprocessing uncertainty, and future-cycle or new-country uncertainty",
        "are omitted"
      )
    )
  }

  if (dimension == "1d") {
    fit_scope = paste(
      "The point curve uses one full-analytic-sample fit of the fixed selected pipeline; each bootstrap replicate",
      "refits that complete fixed pipeline and recomputes ALE on a seeded", reference_sample_label,
      "bootstrap reference sample",
      "without repeating learner selection or hyperparameter search"
    )
    uncertainty_semantics = paste0(
      "Pointwise 95% percentile intervals from ", bootstrap_replicates,
      " requested refits; bootstrap unit: ",
      study_semantics$bootstrap_unit,
      "; refit status: complete fixed-pipeline refit in every successful replicate; not a simultaneous band. ",
      study_semantics$omitted_uncertainty, "."
    )
    estimand_or_scope = paste(
      "One-dimensional associational ALE for", study_semantics$selected_learner, "; fit:", fit_scope,
      "; plausible-value scope:", study_semantics$plausible_value_scope, "; analytic distribution:",
      study_semantics$analytic_distribution
    )
  } else if (available) {
    fit_scope = paste(
      "One full-analytic-sample fit of the fixed selected pipeline supplies the descriptive surface; the top",
      "screened pair is conditional on the preceding interaction screen"
    )
    uncertainty_semantics = paste0(
      "No bootstrap interval and no uncertainty refit; cells with fewer than ", as.integer(support_threshold),
      " sampled observations are suppressed. All sampling and model-fitting uncertainty is omitted; ",
      study_semantics$omitted_uncertainty, "."
    )
    estimand_or_scope = paste(
      "Second-order associational ALE for", study_semantics$selected_learner, "; fit:", fit_scope,
      "; plausible-value scope:", study_semantics$plausible_value_scope, "; analytic distribution:",
      study_semantics$analytic_distribution
    )
  } else {
    fit_scope = "No second-order ALE fit was produced because the interaction screen did not select a pair"
    uncertainty_semantics = paste(
      "No effect estimate, bootstrap interval, or uncertainty refit was produced.",
      study_semantics$omitted_uncertainty
    )
    estimand_or_scope = paste(
      "Interaction-screen status only for", study_semantics$selected_learner, "; fit:", fit_scope,
      "; plausible-value scope:", study_semantics$plausible_value_scope, "; analytic distribution:",
      study_semantics$analytic_distribution
    )
  }

  c(
    study_semantics,
    list(
      dimension = dimension,
      fit_scope = fit_scope,
      bootstrap_refit_status = if (dimension == "1d") {
        "Complete fixed learner pipeline refitted in every successful bootstrap replicate"
      } else {
        "No uncertainty refit"
      },
      estimand_or_scope = estimand_or_scope,
      uncertainty_semantics = uncertainty_semantics
    )
  )
}

.analysis_ale_semantics_table = function(
    support_threshold = 20L, reference_n = 1500L, bootstrap_replicates = 500L) {
  specifications = data.table::data.table(
    study = c("SHILD", "SHILD", "PISA", "PISA"),
    dimension = c("1d", "2d", "1d", "2d"),
    figure_group_id = c(
      "shild_rich_ale_1d", "shild_rich_ale_2d", "pisa_rich_ale_1d", "pisa_rich_ale_2d"
    )
  )
  rows = lapply(seq_len(nrow(specifications)), function(index) {
    semantics = .analysis_ale_semantics(
      specifications$study[[index]],
      specifications$dimension[[index]],
      support_threshold = support_threshold,
      reference_n = reference_n,
      bootstrap_replicates = bootstrap_replicates
    )
    data.table::data.table(
      study = specifications$study[[index]],
      figure_group_id = specifications$figure_group_id[[index]],
      ale_dimension = semantics$dimension,
      selected_learner = semantics$selected_learner,
      fit_scope = semantics$fit_scope,
      plausible_value_scope = semantics$plausible_value_scope,
      analytic_distribution = semantics$analytic_distribution,
      bootstrap_unit = if (semantics$dimension == "1d") semantics$bootstrap_unit else "Not applicable",
      bootstrap_refit_status = semantics$bootstrap_refit_status,
      bootstrap_replicates_requested = if (semantics$dimension == "1d") bootstrap_replicates else NA_integer_,
      bootstrap_refits_successful = if (semantics$dimension == "1d") bootstrap_replicates else NA_integer_,
      confidence_level = if (semantics$dimension == "1d") 0.95 else NA_real_,
      interval_type = if (semantics$dimension == "1d") "Pointwise percentile bootstrap" else "None",
      simultaneous_interval = if (semantics$dimension == "1d") FALSE else NA,
      minimum_success_fraction = if (semantics$dimension == "1d") 0.80 else NA_real_,
      support_threshold = as.integer(support_threshold),
      grid_scope = if (semantics$dimension == "1d") {
        "Fixed point-reference-sample empirical ALE boundaries over 5th-95th percentile support"
      } else {
        "Empirical second-order ALE cells over 5th-95th percentile support"
      },
      reference_scope = if (semantics$dimension == "1d") {
        "Seeded point reference; seeded reference sample drawn anew from each bootstrap task"
      } else {
        "Seeded reference sample from the analytic task"
      },
      uncertainty_semantics = semantics$uncertainty_semantics,
      omitted_uncertainty = semantics$omitted_uncertainty,
      plot_outer_text = "Title, subtitle, and caption are metadata only and are stripped from PDF and PNG files"
    )
  })
  data.table::rbindlist(rows)
}

.analysis_raise_point_layers = function(plot) {
  if (!inherits(plot, c("gg", "ggplot")) || !length(plot$layers)) return(plot)
  is_point = vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1L))
  if (any(is_point) && !all(tail(is_point, sum(is_point)))) {
    plot$layers = c(plot$layers[!is_point], plot$layers[is_point])
  }
  plot
}

.analysis_typography_theme = function(
    axis_text_size = NULL,
    axis_title_size = NULL,
    legend_text_size = NULL,
    legend_title_size = NULL,
    strip_text_size = NULL) {
  elements = list(
    axis.text = if (is.null(axis_text_size)) NULL else element_text(size = axis_text_size),
    axis.title = if (is.null(axis_title_size)) NULL else element_text(size = axis_title_size),
    legend.text = if (is.null(legend_text_size)) NULL else element_text(size = legend_text_size),
    legend.title = if (is.null(legend_title_size)) NULL else element_text(size = legend_title_size),
    strip.text = if (is.null(strip_text_size)) NULL else element_text(size = strip_text_size)
  )
  elements = elements[!vapply(elements, is.null, logical(1L))]
  do.call(theme, elements)
}

.analysis_wrap_text = function(value, width = 105L) {
  if (is.null(value) || !length(value)) {
    return(value)
  }
  lines = strsplit(paste(value, collapse = " "), "\n", fixed = TRUE)[[1L]]
  paste(
    vapply(lines, function(line) paste(strwrap(line, width = width), collapse = "\n"), character(1L)),
    collapse = "\n"
  )
}

.analysis_model_labels = function(values) {
  labels = c(
    xgboost = "XGBoost",
    logistic = "Logistic regression",
    ridge_logistic = "Ridge logistic regression",
    random_forest = "Random forest",
    ridge = "Ridge regression",
    decision_tree = "Decision tree",
    featureless = "Featureless baseline"
  )
  result = unname(labels[as.character(values)])
  result[is.na(result)] = tools::toTitleCase(gsub("_", " ", as.character(values)[is.na(result)]))
  result
}

.analysis_feature_labels = function(values, feature_labels = NULL) {
  values = as.character(values)
  result = rep(NA_character_, length(values))
  if (!is.null(feature_labels)) {
    result = unname(feature_labels[values])
  }
  missing = is.na(result) | !nzchar(result)
  fallback = gsub("_", " ", values[missing])
  fallback = gsub("\\b([A-Za-z]+)([0-9]+)\\b", "\\1 \\2", fallback, perl = TRUE)
  result[missing] = tools::toTitleCase(fallback)
  result
}

.analysis_require_columns = function(data, required, label) {
  missing = setdiff(required, names(data))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.missingness_table = function(data, variables, stage) {
  .analysis_require_columns(data, variables, "Missingness source")
  data.table(
    stage = stage,
    variable = variables,
    n = nrow(data),
    n_missing = vapply(data[, ..variables], function(value) sum(is.na(value)), integer(1L)),
    missing_fraction = vapply(data[, ..variables], function(value) mean(is.na(value)), numeric(1L))
  )[order(-missing_fraction, variable)]
}

.analysis_first_column = function(data, candidates, label) {
  found = candidates[candidates %in% names(data)]
  if (!length(found)) {
    stop(label, " is unavailable; expected one of: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  found[[1L]]
}

.analysis_calibration_source = function(curve, confidence_level = 0.95) {
  curve = copy(as.data.table(curve))
  checkmate::assert_number(confidence_level, lower = 0, upper = 1, finite = TRUE)
  if (confidence_level <= 0 || confidence_level >= 1) {
    stop("`confidence_level` must lie strictly between zero and one.", call. = FALSE)
  }
  prediction_column = if ("predicted_value" %in% names(curve)) {
    "predicted_value"
  } else {
    .analysis_first_column(curve, c("predicted", "prediction", "mean_prediction"), "Calibration prediction")
  }
  observed_column = if ("observed_value" %in% names(curve)) {
    "observed_value"
  } else {
    .analysis_first_column(curve, c("observed", "truth", "event_rate"), "Calibration outcome")
  }
  curve[, `:=`(
    predicted_value = as.numeric(get(prediction_column)),
    observed_value = as.numeric(get(observed_column))
  )]
  if ("n" %in% names(curve)) curve[, n := as.numeric(n)]
  curve = curve[is.finite(predicted_value) & is.finite(observed_value)]
  if (!nrow(curve)) {
    stop("Calibration curve has no finite bins.", call. = FALSE)
  }
  binary_curve = all(curve$predicted_value >= 0 & curve$predicted_value <= 1) &&
    all(curve$observed_value >= 0 & curve$observed_value <= 1) &&
    "n" %in% names(curve) && all(is.finite(curve$n) & curve$n > 0 & curve$n == floor(curve$n))
  precomputed_columns = c("ci_low", "ci_high", "interval_method", "confidence_level")
  has_precomputed = all(precomputed_columns %in% names(curve))
  if (has_precomputed) {
    if (binary_curve && (any(!is.finite(curve$ci_low)) || any(!is.finite(curve$ci_high)) ||
        any(curve$ci_low < 0 | curve$ci_high > 1 | curve$ci_low > curve$observed_value |
          curve$ci_high < curve$observed_value) ||
        any(!is.finite(curve$confidence_level)) || any(curve$confidence_level <= 0 | curve$confidence_level >= 1) ||
        uniqueN(curve$confidence_level) != 1L || anyNA(curve$interval_method) ||
        any(!nzchar(as.character(curve$interval_method))))) {
      stop("Precomputed binary calibration intervals are incomplete or invalid.", call. = FALSE)
    }
    if (!binary_curve && (any(is.finite(curve$ci_low)) || any(is.finite(curve$ci_high)) ||
        any(is.finite(curve$confidence_level)) || anyNA(curve$interval_method) ||
        any(!nzchar(as.character(curve$interval_method))))) {
      stop("Regression calibration bins must not contain fabricated confidence intervals.", call. = FALSE)
    }
  } else if (binary_curve) {
    z = stats::qnorm(1 - (1 - confidence_level) / 2)
    denominator = 1 + z^2 / curve$n
    curve[, `:=`(
      ci_low = pmax(0, (observed_value + z^2 / (2 * n)) / denominator -
        z * sqrt(observed_value * (1 - observed_value) / n + z^2 / (4 * n^2)) / denominator),
      ci_high = pmin(1, (observed_value + z^2 / (2 * n)) / denominator +
        z * sqrt(observed_value * (1 - observed_value) / n + z^2 / (4 * n^2)) / denominator),
      interval_method = "Wilson score interval for a binomial proportion",
      confidence_level = confidence_level
    )]
  } else {
    curve[, `:=`(
      ci_low = NA_real_,
      ci_high = NA_real_,
      interval_method = "not applicable to descriptive regression bin means",
      confidence_level = NA_real_
    )]
  }
  bin_n_relative_range = if (binary_curve) diff(range(curve$n)) / mean(curve$n) else NA_real_
  use_bin_n_size = binary_curve && uniqueN(curve$n) > 1L && bin_n_relative_range >= 0.10
  curve[, point_size_semantics := if (use_bin_n_size) {
    "point area proportional to bin n"
  } else if (binary_curve && uniqueN(n) == 1L) {
    "fixed point size because all bin n values are equal"
  } else if (binary_curve) {
    "fixed point size because the relative range of bin n is below 10%"
  } else {
    "fixed point size"
  }]
  if (!"bin" %in% names(curve)) {
    curve[, bin := if ("bin_label" %in% names(curve)) bin_label else as.character(seq_len(.N))]
  }
  if (!"n" %in% names(curve)) curve[, n := NA_integer_]
  curve[, .(
    bin_index = if ("bin_index" %in% names(curve)) as.integer(bin_index) else seq_len(.N),
    bin_label = as.character(bin),
    n = as.integer(n),
    predicted_value,
    observed_value,
    ci_low,
    ci_high,
    interval_method,
    confidence_level,
    point_size_semantics
  )]
}

.analysis_shap_additivity_quality = function(
    additivity,
    absolute_tolerance = 0.01,
    relative_tolerance = 0.25) {
  additivity = copy(as.data.table(additivity))
  .analysis_require_columns(
    additivity,
    c("case_id", "background_baseline", "case_prediction", "sum_phi", "additivity_residual"),
    "Conditional Shapley additivity"
  )
  checkmate::assert_number(absolute_tolerance, lower = 0, finite = TRUE)
  checkmate::assert_number(relative_tolerance, lower = 0, finite = TRUE)
  required_values = as.matrix(additivity[, .(
    background_baseline, case_prediction, sum_phi, additivity_residual
  )])
  if (!nrow(additivity) || any(!is.finite(required_values)) || anyNA(additivity$case_id) ||
      anyDuplicated(additivity$case_id)) {
    stop("Conditional Shapley additivity evidence is incomplete.", call. = FALSE)
  }
  recomputed_residual = additivity$case_prediction - additivity$background_baseline - additivity$sum_phi
  residual_tolerance = 1e-12 * pmax(1, abs(recomputed_residual), abs(additivity$additivity_residual))
  if (any(abs(recomputed_residual - additivity$additivity_residual) > residual_tolerance)) {
    stop("Conditional Shapley additivity residuals do not match their defining identity.", call. = FALSE)
  }
  additivity[, `:=`(
    prediction_contrast = case_prediction - background_baseline,
    absolute_additivity_residual = abs(additivity_residual),
    absolute_prediction_contrast = abs(case_prediction - background_baseline),
    additivity_tolerance = absolute_tolerance + relative_tolerance *
      abs(case_prediction - background_baseline),
    absolute_tolerance = absolute_tolerance,
    relative_tolerance = relative_tolerance,
    quality_rule = paste0(
      "abs(additivity_residual) <= ", formatC(absolute_tolerance, digits = 3L, format = "f"),
      " + ", formatC(relative_tolerance, digits = 2L, format = "f"),
      " * abs(case_prediction - background_baseline)"
    )
  )]
  additivity[, `:=`(
    residual_to_prediction_contrast = fifelse(
      absolute_prediction_contrast > 0,
      absolute_additivity_residual / absolute_prediction_contrast,
      fifelse(absolute_additivity_residual == 0, 0, Inf)
    ),
    meets_additivity_quality = absolute_additivity_residual <= additivity_tolerance
  )]
  additivity[, quality_status := ifelse(meets_additivity_quality, "pass", "suppress_communication_figure")]
  additivity[]
}

.atomic_save_rds = function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary = tempfile(pattern = paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(object, temporary, version = 3)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically write checkpoint: ", path, call. = FALSE)
  }
  invisible(path)
}

.checkpoint = function(path, signature, compute) {
  if (file.exists(path)) {
    cached = readRDS(path)
    if (identical(cached$signature, signature)) {
      message("Reusing checkpoint ", basename(path))
      return(cached$value)
    }
    stop("Checkpoint signature mismatch: ", path, call. = FALSE)
  }
  value = compute()
  .atomic_save_rds(list(signature = signature, value = value), path)
  value
}

.make_pipeline_learner = function(base_learner, id, scale_numeric = FALSE) {
  numeric_selector = selector_type(c("integer", "numeric"))
  factor_selector = selector_type(c("factor", "ordered", "character"))
  operators = list(
    po("imputemedian", id = paste0(id, "_impute_numeric"), affect_columns = numeric_selector),
    po("imputemode", id = paste0(id, "_impute_factor"), affect_columns = factor_selector),
    po("encode", id = paste0(id, "_encode"), method = "one-hot", affect_columns = factor_selector),
    po("removeconstants", id = paste0(id, "_removeconstants"))
  )
  if (isTRUE(scale_numeric)) {
    operators[[length(operators) + 1L]] = po(
      "scale",
      id = paste0(id, "_scale"),
      affect_columns = numeric_selector
    )
  }
  operators[[length(operators) + 1L]] = po(
    "learner",
    id = paste0(id, "_learner"),
    learner = base_learner
  )
  graph = Reduce(function(left, right) mlr3pipelines::`%>>%`(left, right), operators)
  learner = as_learner(graph)
  learner$id = id
  learner
}

.copy_resampling = function(reference, task) {
  grouped = inherits(reference, "CSDGGroupedResampling")
  source = if (grouped) reference$resampling else reference
  if (!isTRUE(source$is_instantiated)) {
    stop("The reference resampling must be instantiated.", call. = FALSE)
  }
  train_sets = lapply(seq_len(source$iters), source$train_set)
  test_sets = lapply(seq_len(source$iters), source$test_set)
  copied = rsmp("custom")
  copied$instantiate(task, train_sets = train_sets, test_sets = test_sets)
  if (!grouped) {
    return(copied)
  }
  result = reference
  result$resampling = copied
  result$train_sets = train_sets
  result$test_sets = test_sets
  result
}

.resampling_from_fold = function(task, fold) {
  if (length(fold) != task$nrow || anyNA(fold)) {
    stop("Fold assignment must contain one non-missing value per task row.", call. = FALSE)
  }
  fold = as.integer(factor(fold, levels = sort(unique(fold))))
  row_ids = task$row_ids
  test_sets = lapply(sort(unique(fold)), function(value) row_ids[fold == value])
  if (any(lengths(test_sets) == 0L)) {
    stop("At least one requested assessment fold is empty.", call. = FALSE)
  }
  train_sets = lapply(test_sets, function(test) setdiff(row_ids, test))
  result = rsmp("custom")
  result$instantiate(task, train_sets = train_sets, test_sets = test_sets)
  result
}

.stratified_resampling = function(task, strata, folds, seed) {
  if (length(strata) != task$nrow || anyNA(strata)) {
    stop("Strata must be complete and row-aligned.", call. = FALSE)
  }
  assignment = integer(task$nrow)
  set.seed(seed)
  for (level in levels(factor(strata))) {
    rows = which(as.character(strata) == level)
    rows = sample(rows, length(rows), replace = FALSE)
    assignment[rows] = rep(seq_len(folds), length.out = length(rows))
  }
  list(
    resampling = .resampling_from_fold(task, assignment),
    assignment = data.table(
      row_id = task$row_ids,
      fold = assignment,
      stratum = as.character(strata)
    )
  )
}

.grouped_fold_assignment = function(grouped, ids) {
  assignments = copy(grouped$assignments)
  row_map = copy(grouped$row_map)
  setnames(assignments, "group", "school_id")
  setnames(row_map, "group", "school_id")
  rows = merge(row_map, assignments, by = c("school_id", "stratum"), allow.cartesian = TRUE)
  rows = merge(rows, ids, by = "row_id", all.x = TRUE, sort = FALSE)
  setorder(rows, repetition, fold, row_id)
  rows[]
}

.assert_school_split_integrity = function(fold_assignment) {
  crossing = fold_assignment[, uniqueN(fold), by = .(repetition, school_id)][V1 != 1L]
  if (nrow(crossing)) {
    stop("At least one school appears in multiple folds within a repetition.", call. = FALSE)
  }
  invisible(TRUE)
}

.prediction_value = function(prediction, task, positive = NULL) {
  if (identical(task$task_type, "classif")) {
    positive = positive %or% task$positive
    as.numeric(prediction$prob[, positive])
  } else {
    as.numeric(prediction$response)
  }
}

.select_probability_cases = function(oof, probabilities = c(0.10, 0.90)) {
  predictions = csdg_oof_predictions(oof, collapse_repeats = TRUE)
  positive = oof$positive
  probability_candidates = c(
    paste0("prob.", positive),
    paste0("prob_", positive),
    paste0("prob.", make.names(positive)),
    paste0("prob_", make.names(positive))
  )
  probability_column = probability_candidates[probability_candidates %in% names(predictions)][1L]
  if (is.na(probability_column)) {
    candidates = grep("^prob_", names(predictions), value = TRUE)
    candidates = c(candidates, grep("^prob\\.", names(predictions), value = TRUE))
    if (!length(candidates)) {
      stop("Could not identify an out-of-fold probability column.", call. = FALSE)
    }
    probability_column = candidates[[length(candidates)]]
  }
  predictions[, probability := get(probability_column)]
  predictions = predictions[is.finite(probability)]
  targets = quantile(predictions$probability, probabilities, names = FALSE, type = 8)
  selected = rbindlist(lapply(seq_along(targets), function(index) {
    row = predictions[which.min(abs(probability - targets[[index]]))]
    row[, `:=`(
      selection_quantile = probabilities[[index]],
      quantile_target = targets[[index]],
      selection_rule = "post-hoc nearest focal OOF prediction to declared quantile"
    )]
    row
  }))
  if (uniqueN(selected$row_id) != nrow(selected)) {
    selected = predictions[order(probability)][c(1L, .N)]
    selected[, `:=`(
      selection_quantile = range(probabilities),
      quantile_target = probability,
      selection_rule = "post-hoc OOF extremes because declared quantiles selected one row"
    )]
  }
  selected[]
}

.fold_performance_long = function(oof, outcome_id, learner_id) {
  id_columns = intersect(c("iteration", "repetition", "fold"), names(oof$fold_scores))
  measure_columns = setdiff(names(oof$fold_scores), id_columns)
  result = melt(
    copy(oof$fold_scores),
    id.vars = id_columns,
    measure.vars = measure_columns,
    variable.name = "measure_id",
    value.name = "estimate"
  )
  result[, `:=`(
    outcome_id = outcome_id,
    learner_id = learner_id,
    uncertainty = "descriptive fold variation; not independent-sample inference"
  )]
  setcolorder(result, c("outcome_id", "learner_id", id_columns, "measure_id", "estimate", "uncertainty"))
  result[]
}

.pooled_regression_performance = function(oof, outcome_id, learner_id) {
  predictions = csdg_oof_predictions(oof, collapse_repeats = TRUE)
  truth = as.numeric(predictions$truth)
  response = as.numeric(predictions$response)
  denominator = sum((truth - mean(truth))^2)
  data.table(
    outcome_id = outcome_id,
    learner_id = learner_id,
    measure_id = c("regr.rmse", "regr.mae", "regr.rsq"),
    estimate = c(
      sqrt(mean((truth - response)^2)),
      mean(abs(truth - response)),
      if (denominator > 0) 1 - sum((truth - response)^2) / denominator else NA_real_
    ),
    aggregation = "one pooled OOF prediction per sampled student",
    uncertainty = "point estimate conditional on the prespecified sample and splits"
  )
}

.descriptive_pv_summary = function(performance) {
  performance[, .(
    n_pv = uniqueN(outcome_id),
    mean_across_pv = mean(estimate, na.rm = TRUE),
    sd_across_pv = sd(estimate, na.rm = TRUE),
    min_across_pv = min(estimate, na.rm = TRUE),
    max_across_pv = max(estimate, na.rm = TRUE),
    interpretation = "descriptive across-PV variation; not Rubin-pooled uncertainty"
  ), by = .(learner_id, measure_id)]
}

.complete_case_resample = function(data, target, audit_columns, fold, learner, measures, seed) {
  feature_columns = setdiff(names(data), c(target, audit_columns))
  complete = complete.cases(data[, c(target, feature_columns), with = FALSE])
  retained = data[complete]
  retained_fold = fold[complete]
  if (nrow(retained) < length(unique(fold)) * 2L) {
    stop("Too few complete cases for the requested sensitivity split.", call. = FALSE)
  }
  task = if (is.factor(retained[[target]])) {
    TaskClassif$new(
      paste0(target, "_complete_case"),
      backend = retained,
      target = target,
      positive = levels(retained[[target]])[[2L]]
    )
  } else {
    TaskRegr$new(paste0(target, "_complete_case"), backend = retained, target = target)
  }
  for (column in intersect(audit_columns, task$feature_names)) {
    task$set_col_roles(column, remove_from = "feature")
  }
  resampling = .resampling_from_fold(task, retained_fold)
  oof = csdg_resample(
    task = task,
    learner = learner$clone(deep = TRUE),
    resampling = resampling,
    measures = measures,
    store_models = FALSE,
    seed = seed
  )
  membership = data.table(
    complete_case_row_id = as.integer(task$row_ids),
    source_row_id = as.integer(which(complete)),
    fold = as.integer(retained_fold)
  )
  prediction_row_ids = sort(unique(as.integer(oof$predictions$row_id)))
  if (anyDuplicated(membership$complete_case_row_id) || anyDuplicated(membership$source_row_id) ||
      !identical(prediction_row_ids, sort(membership$complete_case_row_id))) {
    stop("The complete-case resample did not preserve an auditable source-row bridge.", call. = FALSE)
  }
  prediction_membership = unique(copy(oof$predictions)[, .(
    complete_case_row_id = as.integer(row_id),
    prediction_fold = as.integer(fold)
  )])
  fold_check = merge(
    membership[, .(complete_case_row_id, fold)],
    prediction_membership,
    by = "complete_case_row_id",
    all = TRUE
  )
  if (nrow(fold_check) != nrow(membership) || anyNA(fold_check) ||
      any(fold_check$fold != fold_check$prediction_fold)) {
    stop("The complete-case OOF prediction folds disagree with the source-row bridge.", call. = FALSE)
  }
  list(oof = oof, retained = complete, membership = membership)
}

.effect_grid = function(values, grid_size) {
  if (is.numeric(values)) {
    unique(as.numeric(quantile(
      values,
      probs = seq(0.05, 0.95, length.out = grid_size),
      na.rm = TRUE,
      names = FALSE,
      type = 8
    )))
  } else {
    observed = unique(as.character(values[!is.na(values)]))
    sort(observed)[seq_len(min(length(observed), max(grid_size, 12L)))]
  }
}

.set_feature_value = function(data, feature, value, prototype) {
  if (is.factor(prototype)) {
    data[[feature]] = factor(
      rep(as.character(value), nrow(data)),
      levels = levels(prototype),
      ordered = is.ordered(prototype)
    )
  } else if (is.integer(prototype)) {
    data[[feature]] = rep(as.integer(round(value)), nrow(data))
  } else if (is.numeric(prototype)) {
    data[[feature]] = rep(as.numeric(value), nrow(data))
  } else {
    data[[feature]] = rep(as.character(value), nrow(data))
  }
  data
}

.fitted_effect_profiles = function(task, learner, features, sample_n, grid_size, seed, positive = NULL) {
  selected = intersect(features, task$feature_names)
  if (!length(selected)) {
    stop("None of the requested fitted-effect features are task features.", call. = FALSE)
  }
  set.seed(seed)
  rows = sample(task$row_ids, min(task$nrow, sample_n), replace = FALSE)
  reference = as.data.table(task$data(rows = rows, cols = task$feature_names))
  fit = learner$clone(deep = TRUE)
  fit$train(task)
  profiles = rbindlist(lapply(selected, function(feature) {
    prototype = task$data(cols = feature)[[feature]]
    grid = .effect_grid(prototype, grid_size)
    rbindlist(lapply(grid, function(value) {
      modified = .set_feature_value(copy(reference), feature, value, prototype)
      prediction = fit$predict_newdata(modified)
      response = .prediction_value(prediction, task, positive)
      data.table(
        feature = feature,
        value = as.character(value),
        value_numeric = if (is.numeric(prototype)) as.numeric(value) else NA_real_,
        mean_prediction = mean(response),
        median_prediction = stats::median(response),
        q10_prediction = quantile(response, 0.10, names = FALSE),
        q90_prediction = quantile(response, 0.90, names = FALSE),
        n_reference = length(response),
        method = "fitted-model partial-dependence query; not a causal effect"
      )
    }))
  }))
  list(model = fit, profiles = profiles, reference_rows = rows)
}

.fitted_interaction_profiles = function(task, fitted_learner, pairs, sample_n, grid_size, seed, positive = NULL) {
  set.seed(seed)
  rows = sample(task$row_ids, min(task$nrow, sample_n), replace = FALSE)
  reference = as.data.table(task$data(rows = rows, cols = task$feature_names))
  rbindlist(lapply(pairs, function(pair) {
    if (length(pair) != 2L || any(!pair %in% task$feature_names)) {
      return(NULL)
    }
    first = task$data(cols = pair[[1L]])[[pair[[1L]]]]
    second = task$data(cols = pair[[2L]])[[pair[[2L]]]]
    first_grid = .effect_grid(first, min(grid_size, 6L))
    second_grid = .effect_grid(second, min(grid_size, 6L))
    combinations = CJ(first_value = first_grid, second_value = second_grid, unique = TRUE)
    combinations[, {
      modified = .set_feature_value(copy(reference), pair[[1L]], first_value[[1L]], first)
      modified = .set_feature_value(modified, pair[[2L]], second_value[[1L]], second)
      prediction = fitted_learner$predict_newdata(modified)
      response = .prediction_value(prediction, task, positive)
      .(
        mean_prediction = mean(response),
        n_reference = length(response)
      )
    }, by = .(first_value, second_value)][, `:=`(
      feature_1 = pair[[1L]],
      feature_2 = pair[[2L]],
      method = "two-feature fitted-model partial-dependence query; not a causal interaction"
    )]
  }), fill = TRUE)
}

.write_gate_tables = function(result, directory, prefix) {
  flatten = function(value, path) {
    output = list()
    if (is.data.frame(value) || is.data.table(value)) {
      if (ncol(value)) output[[path]] = as.data.table(value)
    } else if (is.list(value)) {
      for (name in names(value)) {
        child = if (nzchar(path)) paste(path, name, sep = "__") else name
        output = c(output, flatten(value[[name]], child))
      }
    }
    output
  }
  for (gate_id in names(result$gates)) {
    tables = flatten(result$gates[[gate_id]]$evidence, tolower(gate_id))
    for (name in names(tables)) {
      .write_table(tables[[name]], file.path(directory, paste0(prefix, "_", name, ".csv")))
    }
  }
  invisible(TRUE)
}

.assert_expected_columns = function(data, required, label) {
  missing = setdiff(required, names(data))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.input_record = function(path, role) {
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  data.table(
    role = role,
    path = path,
    bytes = as.numeric(file.info(path)$size),
    md5 = unname(tools::md5sum(path)),
    sha256 = digest::digest(file = path, algo = "sha256")
  )
}

.package_source_manifest = function(project_root) {
  project_root = normalizePath(project_root, winslash = "/", mustWork = TRUE)
  roots = c("R", "inst", "tests/testthat", "analysis")
  files = c("DESCRIPTION", "NAMESPACE", "_pkgdown.yml")
  for (root in roots) {
    directory = file.path(project_root, root)
    if (dir.exists(directory)) {
      relative = list.files(
        directory,
        recursive = TRUE,
        full.names = FALSE,
        all.files = TRUE,
        no.. = TRUE
      )
      files = c(files, file.path(root, relative))
    }
  }
  files = unique(files)
  files = files[!basename(files) %in% c(".DS_Store", "Rplots.pdf")]
  absolute = file.path(project_root, files)
  keep = file.exists(absolute) & !file.info(absolute)$isdir
  files = files[keep]
  absolute = absolute[keep]
  order_index = order(files)
  files = files[order_index]
  absolute = absolute[order_index]
  manifest = data.table::data.table(
    path = gsub("\\\\", "/", files),
    bytes = as.numeric(file.info(absolute)$size),
    md5 = unname(tools::md5sum(absolute)),
    sha256 = vapply(absolute, digest::digest, character(1L), file = TRUE, algo = "sha256")
  )
  canonical_records = enc2utf8(paste(
    manifest$path,
    format(manifest$bytes, scientific = FALSE, trim = TRUE),
    manifest$md5,
    manifest$sha256,
    sep = "\t"
  ))
  list(
    manifest = manifest,
    tree_hash = digest::digest(
      paste0(paste(canonical_records, collapse = "\n"), "\n"),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

.assert_package_source_snapshot = function(snapshot, project_root, context = "analysis run") {
  if (!is.list(snapshot) || !all(c("manifest", "tree_hash") %in% names(snapshot)) ||
      !is.character(snapshot$tree_hash) || length(snapshot$tree_hash) != 1L || !nzchar(snapshot$tree_hash)) {
    stop("`snapshot` must be returned by .package_source_manifest().", call. = FALSE)
  }
  current = .package_source_manifest(project_root)
  if (identical(snapshot$tree_hash, current$tree_hash)) {
    return(invisible(TRUE))
  }

  before = copy(as.data.table(snapshot$manifest))
  after = copy(as.data.table(current$manifest))
  changed = merge(
    before[, .(path, sha256_before = sha256)],
    after[, .(path, sha256_after = sha256)],
    by = "path",
    all = TRUE,
    sort = TRUE
  )
  changed = changed[
    is.na(sha256_before) | is.na(sha256_after) | sha256_before != sha256_after,
    path
  ]
  detail = if (length(changed)) paste(head(changed, 10L), collapse = ", ") else "unknown source change"
  if (length(changed) > 10L) {
    detail = paste0(detail, sprintf(", and %d more", length(changed) - 10L))
  }
  stop(
    "Package source changed during ", context, "; refusing to finalize mixed-source results: ", detail, ".",
    call. = FALSE
  )
}

.analysis_run_signature = function(package_source, source_files, input_files, settings, study) {
  source_files = normalizePath(source_files, winslash = "/", mustWork = TRUE)
  input_files = normalizePath(input_files, winslash = "/", mustWork = TRUE)
  source_hashes = data.table(
    path = basename(source_files),
    md5 = unname(tools::md5sum(source_files)),
    sha256 = vapply(source_files, digest::digest, character(1L), file = TRUE, algo = "sha256")
  )
  input_hashes = data.table(
    path = basename(input_files),
    md5 = unname(tools::md5sum(input_files)),
    sha256 = vapply(input_files, digest::digest, character(1L), file = TRUE, algo = "sha256")
  )
  runtime_packages = c(
    "data.table", "digest", "mlr3", "mlr3learners", "mlr3measures", "mlr3misc", "mlr3pipelines",
    "ranger", "rpart", "xgboost"
  )
  runtime_versions = vapply(runtime_packages, function(package) {
    if (requireNamespace(package, quietly = TRUE)) {
      as.character(utils::packageVersion(package))
    } else {
      NA_character_
    }
  }, character(1L))
  runtime = list(
    r_version = R.version.string,
    platform = R.version$platform,
    os = R.version$os,
    package_versions = runtime_versions
  )
  digest::digest(
    list(
      study = study,
      settings = settings,
      runtime = runtime,
      package_tree_hash = package_source$tree_hash,
      source_hashes = source_hashes,
      input_hashes = input_hashes
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

.checkpoint_directory = function(project_root, run, settings) {
  if (!nzchar(settings$run_id)) {
    return(run$dirs$objects)
  }
  safe_id = gsub("[^A-Za-z0-9._-]+", "_", settings$run_id)
  if (!nzchar(safe_id)) {
    stop("CSDG_RUN_ID does not contain a safe checkpoint identifier.", call. = FALSE)
  }
  directory = file.path(project_root, "paper_artifacts", ".checkpoint_cache", safe_id, tolower(settings$study))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  normalizePath(directory, winslash = "/", mustWork = TRUE)
}

.run_checkpoint = function(directory, name, run_signature, stage_signature, compute) {
  safe_name = gsub("[^A-Za-z0-9._-]+", "_", name)
  signature = digest::digest(
    list(run_signature = run_signature, stage_signature = stage_signature),
    algo = "sha256",
    serialize = TRUE
  )
  value = .checkpoint(file.path(directory, paste0(safe_name, ".rds")), signature, compute)
  list(value = value, signature = signature)
}

.prediction_column = function(data, positive = NULL) {
  if ("response" %in% names(data) && is.numeric(data$response)) {
    return("response")
  }
  candidates = c(
    paste0("prob.", positive), paste0("prob_", positive),
    paste0("prob.", make.names(positive)), paste0("prob_", make.names(positive))
  )
  found = candidates[candidates %in% names(data)]
  if (length(found)) {
    return(found[[1L]])
  }
  found = grep("^prob[._]", names(data), value = TRUE)
  if (!length(found)) {
    stop("Could not identify a prediction column.", call. = FALSE)
  }
  found[[length(found)]]
}

.analysis_fixed_width_bins = function(values, minimum_bins = 12L, maximum_bins = 40L) {
  values = as.numeric(values)
  values = values[is.finite(values)]
  if (!length(values)) {
    stop("Fixed-width bins require at least one finite value.", call. = FALSE)
  }
  minimum_bins = as.integer(minimum_bins)
  maximum_bins = as.integer(maximum_bins)
  if (minimum_bins < 1L || maximum_bins < minimum_bins) {
    stop("Fixed-width bin bounds are invalid.", call. = FALSE)
  }
  lower = min(0, min(values))
  upper = max(values)
  if (upper <= lower) {
    upper = lower + max(.Machine$double.eps, abs(lower) * 1e-8)
  }
  width = 2 * IQR(values) / length(values)^(1 / 3)
  suggested = if (is.finite(width) && width > 0) ceiling((upper - lower) / width) else nclass.Sturges(values)
  n_bins = max(minimum_bins, min(maximum_bins, as.integer(suggested)))
  breaks = seq(lower, upper, length.out = n_bins + 1L)
  bin_index = findInterval(values, breaks, all.inside = TRUE, rightmost.closed = TRUE)
  result = data.table(bin_index = seq_len(n_bins))
  result[, `:=`(
    bin_left = breaks[bin_index],
    bin_right = breaks[bin_index + 1L]
  )]
  counts = tabulate(bin_index, nbins = n_bins)
  result[, `:=`(
    bin_midpoint = (bin_left + bin_right) / 2,
    N = counts,
    proportion = counts / length(values),
    density = counts / length(values) / (bin_right - bin_left),
    range_bin = factor(
      sprintf("[%s, %s%s", formatC(bin_left, digits = 3L, format = "fg"),
        formatC(bin_right, digits = 3L, format = "fg"), ifelse(bin_index == n_bins, "]", ")")),
      levels = sprintf("[%s, %s%s", formatC(bin_left, digits = 3L, format = "fg"),
        formatC(bin_right, digits = 3L, format = "fg"), ifelse(bin_index == n_bins, "]", ")"))
    ),
    binning = paste0("fixed-width; ", n_bins, " bins; Freedman-Diaconis count capped to ", minimum_bins, "-",
      maximum_bins)
  )]
  result[]
}

.rashomon_prediction_multiplicity = function(rashomon, positive = NULL) {
  if (is.null(rashomon) || is.null(rashomon$candidates) || is.null(rashomon$resamples)) {
    stop("Rashomon evidence is unavailable for prediction multiplicity.", call. = FALSE)
  }
  accepted = rashomon$candidates[accepted == TRUE, learner_name]
  if (!length(accepted)) {
    stop("No near-equivalent learner is available for prediction multiplicity.", call. = FALSE)
  }
  long = rbindlist(lapply(accepted, function(learner_name) {
    predictions = csdg_oof_predictions(rashomon$resamples[[learner_name]], collapse_repeats = TRUE)
    column = .prediction_column(predictions, positive)
    data.table(
      row_id = predictions$row_id,
      learner_name = learner_name,
      prediction = as.numeric(predictions[[column]])
    )
  }))
  row_level = long[, .(
    n_models = .N,
    minimum_prediction = min(prediction),
    mean_prediction = mean(prediction),
    maximum_prediction = max(prediction),
    sd_prediction = if (.N > 1L) sd(prediction) else 0,
    prediction_range = max(prediction) - min(prediction)
  ), by = row_id]
  quantiles = data.table(
    n_rows = nrow(row_level),
    n_accepted_models = length(accepted),
    statistic = c("mean", "sd", "p50", "p90", "p95", "p99", "maximum"),
    prediction_range = c(
      mean(row_level$prediction_range),
      sd(row_level$prediction_range),
      quantile(row_level$prediction_range, c(0.50, 0.90, 0.95, 0.99), names = FALSE),
      max(row_level$prediction_range)
    ),
    interpretation = "Descriptive OOF prediction dispersion across prespecified near-equivalent models"
  )
  bins = .analysis_fixed_width_bins(row_level$prediction_range)
  list(row_level = row_level, quantiles = quantiles, bins = bins, accepted = accepted)
}
