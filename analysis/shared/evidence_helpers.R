# Helpers for the systematic SHILD local-fidelity audit and cross-study evidence synthesis.

.evidence_require_columns = function(data, columns, label) {
  missing = setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

.evidence_quantile = function(x, probability) {
  as.numeric(stats::quantile(x, probability, names = FALSE, type = 8, na.rm = TRUE))
}

.evidence_safe_cor = function(x, y, method = "spearman") {
  keep = is.finite(x) & is.finite(y)
  if (sum(keep) < 3L || uniqueN(x[keep]) < 2L || uniqueN(y[keep]) < 2L) {
    return(NA_real_)
  }
  stats::cor(x[keep], y[keep], method = method)
}

.evidence_with_seed = function(seed, code, environment = parent.frame()) {
  had_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    previous_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      remove(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  eval(substitute(code), envir = environment)
}

.evidence_perturbation_seed = function(base, case_label, replicate, replicate_stride = 1000000L) {
  case_number = suppressWarnings(as.integer(sub("^case_", "", as.character(case_label))))
  replicate = as.integer(replicate)
  replicate_stride = as.numeric(replicate_stride)
  if (anyNA(case_number) || any(case_number < 1L) || anyNA(replicate) || any(replicate < 1L) ||
      length(replicate_stride) != 1L || !is.finite(replicate_stride) || replicate_stride <= 100000) {
    stop("Perturbation seeds require positive public case numbers and replicate numbers.", call. = FALSE)
  }
  seed = as.numeric(base) + replicate_stride * replicate + case_number
  if (any(!is.finite(seed)) || any(seed < 0) || any(seed > .Machine$integer.max)) {
    stop("A derived perturbation seed lies outside the supported integer range.", call. = FALSE)
  }
  as.integer(seed)
}

.evidence_canonical_signature = function(package_tree_hash, source_hashes, input_hashes, parameters) {
  values = list(source_hashes = source_hashes, input_hashes = input_hashes, parameters = parameters)
  if (!is.character(package_tree_hash) || length(package_tree_hash) != 1L || !nzchar(package_tree_hash) ||
      any(!vapply(values, function(value) !is.null(names(value)) && all(nzchar(names(value))), logical(1L)))) {
    stop("Synthesis signature inputs must be named, and the package tree hash must be scalar.", call. = FALSE)
  }
  records = c(paste0("package_tree_hash=", package_tree_hash))
  for (group in names(values)) {
    value = values[[group]]
    order_index = order(names(value))
    records = c(records, paste0(group, ":", names(value)[order_index], "=", value[order_index]))
  }
  digest::digest(paste(enc2utf8(records), collapse = "\n"), algo = "sha256", serialize = FALSE)
}

.evidence_parent_audit_status = function(x, study) {
  if (!is.character(study) || length(study) != 1L || is.na(study) || !nzchar(study)) {
    stop("Parent-audit study labels must be scalar non-empty strings.", call. = FALSE)
  }
  source = if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    if (!file.exists(x)) stop(study, " parent output-audit summary is missing.", call. = FALSE)
    fread(x)
  } else if (is.data.frame(x)) {
    copy(as.data.table(x))
  } else {
    stop(study, " parent output-audit summary must be a CSV path or data frame.", call. = FALSE)
  }
  .evidence_require_columns(
    source,
    c("structural_status", "release_eligibility", "eligibility_blockers"),
    paste(study, "parent output-audit summary")
  )
  if (nrow(source) != 1L) {
    stop(study, " parent output-audit summary must contain exactly one row.", call. = FALSE)
  }
  structural_status = trimws(as.character(source$structural_status[[1L]]))
  release_eligibility = trimws(as.character(source$release_eligibility[[1L]]))
  eligibility_blockers = trimws(as.character(source$eligibility_blockers[[1L]]))
  if (!identical(structural_status, "PASS")) {
    stop(study, " parent output-audit summary must have structural_status PASS.", call. = FALSE)
  }
  if (!release_eligibility %in% c("ELIGIBLE", "NOT_ELIGIBLE")) {
    stop(study, " parent output-audit summary has an invalid release_eligibility value.", call. = FALSE)
  }
  blocker_missing = is.na(eligibility_blockers) || !nzchar(eligibility_blockers) ||
    identical(tolower(eligibility_blockers), "none")
  if (identical(release_eligibility, "NOT_ELIGIBLE") && blocker_missing) {
    stop(study, " parent NOT_ELIGIBLE status must include explicit eligibility blockers.", call. = FALSE)
  }
  if (identical(release_eligibility, "ELIGIBLE") && !blocker_missing) {
    stop(study, " parent ELIGIBLE status cannot include eligibility blockers.", call. = FALSE)
  }
  data.table(
    study = study,
    structural_status = structural_status,
    release_eligibility = release_eligibility,
    eligibility_blockers = if (blocker_missing) NA_character_ else eligibility_blockers
  )
}

.evidence_parent_readiness = function(shild_summary, pisa_summary) {
  shild = .evidence_parent_audit_status(shild_summary, "SHILD")
  pisa = .evidence_parent_audit_status(pisa_summary, "PISA")
  statuses = rbindlist(list(shild, pisa))
  manuscript_ready = all(statuses$release_eligibility == "ELIGIBLE")
  blocked = statuses[release_eligibility != "ELIGIBLE"]
  eligibility_blocker = if (manuscript_ready) {
    NA_character_
  } else {
    paste0(
      "Parent release eligibility not satisfied: ",
      paste0(
        blocked$study,
        "=",
        blocked$release_eligibility,
        " (",
        blocked$eligibility_blockers,
        ")",
        collapse = "; "
      )
    )
  }
  data.table(
    manuscript_ready = manuscript_ready,
    eligibility_blocker = eligibility_blocker,
    parent_shild_structural_status = shild$structural_status,
    parent_shild_release_eligibility = shild$release_eligibility,
    parent_shild_eligibility_blockers = shild$eligibility_blockers,
    parent_pisa_structural_status = pisa$structural_status,
    parent_pisa_release_eligibility = pisa$release_eligibility,
    parent_pisa_eligibility_blockers = pisa$eligibility_blockers
  )
}

.evidence_select_local_cases = function(
    predictions,
    folds = 10L,
    deciles = 10L,
    cases_per_cell = 3L,
    seed = 20260812L) {
  predictions = copy(as.data.table(predictions))
  .evidence_require_columns(
    predictions,
    c("row_id", "fold", "prob.discontent"),
    "Frozen SHILD out-of-fold predictions"
  )
  if (nrow(predictions) != uniqueN(predictions$row_id) || anyNA(predictions[, .(row_id, fold, prob.discontent)])) {
    stop("Frozen SHILD predictions must contain one complete row per analytic case.", call. = FALSE)
  }
  if (!identical(sort(unique(predictions$fold)), seq_len(as.integer(folds)))) {
    stop("Frozen SHILD prediction folds do not match the prespecified fold count.", call. = FALSE)
  }
  if (any(!is.finite(predictions$prob.discontent)) ||
      any(predictions$prob.discontent < 0 | predictions$prob.discontent > 1)) {
    stop("Frozen SHILD out-of-fold probabilities must be finite and lie in [0, 1].", call. = FALSE)
  }

  setorder(predictions, fold, prob.discontent, row_id)
  predictions[, within_fold_order := seq_len(.N), by = fold]
  predictions[, prediction_score_decile := pmin(
    as.integer(deciles),
    1L + floor((within_fold_order - 1L) * as.integer(deciles) / .N)
  ), by = fold]
  candidates = predictions[, .(
    n_candidates = .N,
    minimum_oof_probability = min(prob.discontent),
    maximum_oof_probability = max(prob.discontent)
  ), by = .(fold, prediction_score_decile)]
  if (nrow(candidates) != as.integer(folds) * as.integer(deciles) ||
      any(candidates$n_candidates < as.integer(cases_per_cell))) {
    stop("At least one fold-by-prediction-score-decile cell cannot supply the prespecified cases.", call. = FALSE)
  }

  selected = predictions[, {
    cell_seed = as.integer(seed) + 100L * as.integer(fold[[1L]]) + as.integer(prediction_score_decile[[1L]])
    draw = .evidence_with_seed(cell_seed, sample.int(.N, as.integer(cases_per_cell), replace = FALSE))
    .SD[draw][, `:=`(selection_draw = seq_len(.N), selection_seed = cell_seed)]
  }, by = .(fold, prediction_score_decile)]
  setorder(selected, fold, prediction_score_decile, selection_draw)
  selected[, case_label := sprintf("case_%03d", seq_len(.N))]
  selected[, within_fold_order := NULL]
  if (nrow(selected) != as.integer(folds) * as.integer(deciles) * as.integer(cases_per_cell) ||
      anyDuplicated(selected$row_id) || anyDuplicated(selected$case_label)) {
    stop("The systematic SHILD case sample is incomplete or duplicated.", call. = FALSE)
  }

  public_plan = merge(
    selected[, .(case_label, fold, prediction_score_decile, selection_draw, selection_seed)],
    candidates[, .(fold, prediction_score_decile, n_candidates)],
    by = c("fold", "prediction_score_decile"),
    all.x = TRUE,
    sort = FALSE
  )
  setorder(public_plan, fold, prediction_score_decile, selection_draw)
  public_plan[, `:=`(
    sampling_scope = paste(
      "Three cases sampled without replacement in each frozen fold-by-prediction-score-decile cell"
    ),
    tie_rule = "Equal prediction scores ordered by frozen analytic row identifier before balanced decile assignment"
  )]

  list(private = selected[], public = public_plan[])
}

.evidence_validate_fold_result = function(
    result,
    fold_cases,
    fold,
    n_replicates,
    kernel_widths,
    primary_kernel_width = 0.75,
    fidelity_threshold = 0.80) {
  if (!is.list(result) || !all(c("replicates", "support", "coefficients") %in% names(result))) {
    stop("A systematic G4 fold result is malformed.", call. = FALSE)
  }
  replicates = as.data.table(result$replicates)
  support = as.data.table(result$support)
  coefficients = as.data.table(result$coefficients)
  fold_cases = as.data.table(fold_cases)
  csdg_summarize_local_fidelity(
    replicates,
    support,
    coefficients = coefficients,
    primary_kernel_width = primary_kernel_width,
    fidelity_threshold = fidelity_threshold
  )
  .evidence_require_columns(replicates, "prediction_score_decile", "Systematic G4 fold replicates")
  .evidence_require_columns(support, "prediction_score_decile", "Systematic G4 fold support")
  .evidence_require_columns(
    fold_cases,
    c("case_label", "fold", "prediction_score_decile"),
    "Systematic G4 sampled cases"
  )
  observed_replicates = sort(unique(as.integer(replicates$perturbation_replicate)))
  observed_widths = sort(unique(as.numeric(replicates$kernel_width)))
  location = merge(
    unique(replicates[, .(case_label, fold, prediction_score_decile)]),
    fold_cases[, .(case_label, expected_fold = fold, expected_decile = prediction_score_decile)],
    by = "case_label",
    all = TRUE
  )
  support_location = merge(
    support[, .(case_label, fold, prediction_score_decile)],
    fold_cases[, .(case_label, expected_fold = fold, expected_decile = prediction_score_decile)],
    by = "case_label",
    all = TRUE
  )
  if (!identical(observed_replicates, seq_len(as.integer(n_replicates))) ||
      !isTRUE(all.equal(observed_widths, sort(as.numeric(kernel_widths)), tolerance = 0)) ||
      nrow(location) != nrow(fold_cases) || anyNA(location) ||
      any(location$fold != location$expected_fold) ||
      any(location$prediction_score_decile != location$expected_decile) ||
      nrow(support_location) != nrow(fold_cases) || anyNA(support_location) ||
      any(support_location$fold != support_location$expected_fold) ||
      any(support_location$prediction_score_decile != support_location$expected_decile) ||
      any(replicates$fold != as.integer(fold)) || any(support$fold != as.integer(fold))) {
    stop(
      "A systematic G4 fold result differs from the prespecified replicate, width, fold, or prediction-score grid.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.evidence_summarize_local_fidelity = function(
    replicates,
    support,
    sampling_private,
    multiplicity,
    coefficients = NULL,
    primary_kernel_width = 0.75,
    historical_reference = 0.80,
    expected_replicates = 20L,
    expected_widths = c(0.25, 0.50, 0.75, 1.00)) {
  replicates = copy(as.data.table(replicates))
  support = copy(as.data.table(support))
  sampling_private = copy(as.data.table(sampling_private))
  multiplicity = copy(as.data.table(multiplicity))
  coefficients = if (is.null(coefficients)) data.table() else copy(as.data.table(coefficients))
  package_summary = csdg_summarize_local_fidelity(
    replicates,
    support,
    coefficients = coefficients,
    primary_kernel_width = primary_kernel_width,
    fidelity_threshold = historical_reference
  )
  .evidence_require_columns(replicates, "prediction_score_decile", "Systematic SHILD local-fidelity replicates")
  .evidence_require_columns(support, "prediction_score_decile", "SHILD training-set proximity diagnostics")
  .evidence_require_columns(
    sampling_private,
    c("case_label", "row_id", "fold", "prediction_score_decile"),
    "Private SHILD sampling plan"
  )
  .evidence_require_columns(multiplicity, c("row_id", "prediction_range"), "SHILD model disagreement")

  expected_cases = uniqueN(sampling_private$case_label)
  observed_widths = sort(unique(as.numeric(replicates$kernel_width)))
  observed_replicates = sort(unique(as.integer(replicates$perturbation_replicate)))
  if (!isTRUE(all.equal(observed_widths, sort(as.numeric(expected_widths)), tolerance = 0)) ||
      !identical(observed_replicates, seq_len(as.integer(expected_replicates))) ||
      nrow(package_summary$cases) != expected_cases) {
    stop("The systematic SHILD local-fidelity grid is incomplete.", call. = FALSE)
  }
  sampling_key = sampling_private[, .(case_label, fold, prediction_score_decile)]
  if (anyDuplicated(sampling_private$case_label) || anyDuplicated(sampling_private$row_id) ||
      anyDuplicated(support$case_label) || nrow(support) != expected_cases ||
      !setequal(support$case_label, sampling_private$case_label)) {
    stop("The private case sample or local-support table is duplicated or incomplete.", call. = FALSE)
  }
  location_check = merge(
    unique(replicates[, .(case_label, fold, prediction_score_decile)]),
    sampling_key,
    by = "case_label",
    all = TRUE,
    suffixes = c("_replicate", "_sample")
  )
  support_check = merge(
    support[, .(case_label, fold, prediction_score_decile)],
    sampling_key,
    by = "case_label",
    all = TRUE,
    suffixes = c("_support", "_sample")
  )
  if (nrow(location_check) != expected_cases || anyNA(location_check) ||
      any(location_check$fold_replicate != location_check$fold_sample) ||
      any(location_check$prediction_score_decile_replicate != location_check$prediction_score_decile_sample) ||
      nrow(support_check) != expected_cases || anyNA(support_check) ||
      any(support_check$fold_support != support_check$fold_sample) ||
      any(support_check$prediction_score_decile_support != support_check$prediction_score_decile_sample)) {
    stop("A SHILD replicate or local-support row has the wrong sampled fold or prediction-score decile.",
      call. = FALSE)
  }
  case_summary = copy(package_summary$cases)
  summary = copy(package_summary$summary)
  by_support = copy(package_summary$by_support)
  primary = replicates[abs(kernel_width - primary_kernel_width) < 1e-12]
  apparent = primary[, .(
    median_apparent_weighted_r2 = stats::median(apparent_weighted_r2),
    median_apparent_weighted_rmse = stats::median(apparent_weighted_rmse),
    median_r2_optimism = stats::median(apparent_weighted_r2 - weighted_r2),
    median_rmse_optimism = stats::median(weighted_rmse - apparent_weighted_rmse)
  ), by = .(case_label, iteration, repetition, fold)]
  case_summary = merge(
    case_summary,
    apparent,
    by = c("case_label", "iteration", "repetition", "fold"),
    all.x = TRUE,
    sort = FALSE
  )
  case_summary[, case_summary_rule := paste0(
    "Metric medians across ", expected_replicates,
    " independently generated perturbation and cross-fit seed pairs"
  )]
  summary[, uncertainty_semantics := paste(
    "Descriptive distributions only. Variation across independently generated seed pairs reflects neighborhood draws",
    paste(
      "and surrogate cross-fit partitions, not model-fit or population-sampling uncertainty;",
      "no confidence intervals or hypothesis tests."
    )
  )]

  disagreement = merge(
    sampling_private[, .(case_label, row_id)],
    multiplicity[, .(row_id, accepted_model_disagreement = prediction_range)],
    by = "row_id",
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(disagreement) != expected_cases || anyNA(disagreement$accepted_model_disagreement)) {
    stop("Accepted-model disagreement does not cover every sampled SHILD case.", call. = FALSE)
  }
  case_summary = merge(case_summary, disagreement[, .(case_label, accepted_model_disagreement)],
    by = "case_label", all.x = TRUE, sort = FALSE)
  setorder(case_summary, fold, prediction_score_decile, case_label)
  case_summary[, disagreement_quartile := {
    rank = frank(accepted_model_disagreement, ties.method = "first")
    1L + floor((rank - 1L) * 4L / .N)
  }]

  by_prediction_score = case_summary[, .(
    n_cases = .N,
    minimum_case_median_r2 = min(median_weighted_r2),
    first_quartile_case_median_r2 = .evidence_quantile(median_weighted_r2, 0.25),
    median_case_median_r2 = stats::median(median_weighted_r2),
    third_quartile_case_median_r2 = .evidence_quantile(median_weighted_r2, 0.75),
    maximum_case_median_r2 = max(median_weighted_r2),
    median_case_median_weighted_rmse = stats::median(median_weighted_rmse),
    median_case_median_weighted_mae = stats::median(median_weighted_mae),
    median_case_maximum_absolute_error = stats::median(median_maximum_absolute_error),
    median_target_case_absolute_error = stats::median(median_target_case_absolute_error),
    median_apparent_minus_crossfit_r2 = stats::median(median_r2_optimism),
    n_cases_below_threshold = sum(case_below_threshold),
    proportion_cases_below_threshold = mean(case_below_threshold),
    median_monte_carlo_sd = stats::median(monte_carlo_sd)
  ), by = prediction_score_decile]
  by_prediction_score[, `:=`(
    historical_weighted_r2_reference = historical_reference,
    inference = "Descriptive by prespecified within-fold OOF prediction-score decile; no significance tests"
  )]

  by_disagreement = case_summary[, .(
    n_cases = .N,
    minimum_disagreement = min(accepted_model_disagreement),
    median_disagreement = stats::median(accepted_model_disagreement),
    maximum_disagreement = max(accepted_model_disagreement),
    spearman_rho_disagreement_fidelity = .evidence_safe_cor(
      accepted_model_disagreement,
      median_weighted_r2,
      method = "spearman"
    ),
    median_case_median_r2 = stats::median(median_weighted_r2),
    median_case_median_weighted_rmse = stats::median(median_weighted_rmse)
  ), by = prediction_score_decile]
  by_disagreement[, inference := paste(
    "Descriptive within prediction-score decile, so the principal prediction region is held fixed by stratification;",
    "no p-value or confidence interval."
  )]

  by_support[, inference := paste(
    "Quartile 1 has the nearest tenth training neighbor; greater distance means weaker observed training-set",
    paste(
      "proximity under the surrogate mixed-type distance. The association is unadjusted, and prediction-score",
      "region is not"
    ),
    "controlled. Descriptive only, not proof of joint support, with no p-value or confidence interval. Kish",
    "effective N is reported only as kernel-weight concentration."
  )]

  list(
    summary = summary[],
    cases = case_summary[],
    bandwidth_cases = package_summary$bandwidth_cases,
    by_prediction_score = by_prediction_score[],
    by_disagreement = by_disagreement[],
    by_support = by_support[],
    coefficient_stability = package_summary$coefficient_stability[]
  )
}

.evidence_performance_explanation_source = function(
    study,
    claim_id,
    model_ranks,
    importance_ranks,
    focal_learner = "xgboost",
    pairwise_top_k = NULL) {
  model_ranks = copy(as.data.table(model_ranks))
  importance_ranks = copy(as.data.table(importance_ranks))
  .evidence_require_columns(
    model_ranks,
    c(
      "learner_name", "primary_measure", "direction", "mean_score", "best_score", "acceptance_limit",
      "tolerance_absolute", "accepted", "distance_from_best"
    ),
    paste(study, "model ranks")
  )
  accepted = model_ranks[accepted == TRUE]
  learner_column = if ("learner_name" %in% names(importance_ranks)) "learner_name" else "learner_id"
  feature_column = if ("feature_group" %in% names(importance_ranks)) "feature_group" else "feature"
  rank_column = if ("rank" %in% names(importance_ranks)) "rank" else "importance_rank"
  .evidence_require_columns(
    importance_ranks,
    c(learner_column, feature_column, rank_column),
    paste(study, "importance ranks")
  )
  package_ranks = importance_ranks[, .(
    learner_name = as.character(get(learner_column)),
    feature_group = as.character(get(feature_column)),
    rank = as.numeric(get(rank_column))
  )]
  top_k = 5L
  if (!is.null(pairwise_top_k)) {
    pairwise_top_k = copy(as.data.table(pairwise_top_k))
    .evidence_require_columns(pairwise_top_k, c("learner_1", "learner_2", "jaccard"), "Pairwise top-k ranks")
    if ("top_k" %in% names(pairwise_top_k)) {
      top_k = unique(pairwise_top_k$top_k)
      if (length(top_k) != 1L) stop("Pairwise importance rows use inconsistent top-k values.", call. = FALSE)
    } else {
      top_k = 5L
    }
  }
  diagnostics = csdg_explanation_sensitivity(
    model_ranks,
    package_ranks,
    focal_learner = focal_learner,
    top_k = as.integer(top_k)
  )
  diagnostics[, `:=`(
    n_ranked_features = n_ranked_feature_groups,
    performance_tolerance_fraction = fraction_of_prespecified_tolerance,
    rank_discordance_vs_focal = full_pair_reversal_rate_vs_focal
  )]
  diagnostic_columns = c(
    "learner_name", "focal_learner", "focal_mean_score", "raw_performance_difference_from_focal",
    "direction_adjusted_performance_difference_from_focal", "prespecified_tolerance",
    "fraction_of_prespecified_tolerance_from_focal", "performance_tolerance_fraction",
    "n_ranked_features", "kendall_tau_vs_focal", "n_feature_group_pairs",
    "n_reversed_feature_group_pairs", "n_tied_feature_group_pairs", "rank_discordance_vs_focal",
    "top_k", "top_k_intersection", "top_k_union", "top_k_jaccard_vs_focal"
  )
  source = merge(
    accepted,
    diagnostics[, ..diagnostic_columns],
    by = "learner_name",
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(source) != nrow(accepted) || anyNA(source[, .(n_ranked_features, kendall_tau_vs_focal)])) {
    stop(study, " accepted models do not map completely to package-derived explanation sensitivity.", call. = FALSE)
  }
  if (!is.null(pairwise_top_k)) {
    archived_jaccard = source[, .(
      archived_jaccard = if (learner_name == focal_learner) {
        1
      } else {
        values = pairwise_top_k[
          (learner_1 == focal_learner & learner_2 == learner_name) |
            (learner_2 == focal_learner & learner_1 == learner_name),
          jaccard
        ]
        if (length(values) != 1L) NA_real_ else as.numeric(values)
      }
    ), by = learner_name]
    archived_index = match(source$learner_name, archived_jaccard$learner_name)
    if (anyNA(archived_index) || anyNA(archived_jaccard$archived_jaccard) || any(abs(
      source$top_k_jaccard_vs_focal - archived_jaccard$archived_jaccard[archived_index]
    ) > 1e-12)) {
      stop(study, " package-derived top-k agreement does not reproduce the frozen pairwise table.", call. = FALSE)
    }
  }
  source[, `:=`(
    study = study,
    claim_id = claim_id,
    focal_learner = focal_learner,
    learner_label = .analysis_model_labels(learner_name),
    evidence_scope = paste(
      "Accepted prespecified near-equivalent models only; full-rank discordance is the fraction of feature pairs",
      "whose order reverses relative to focal XGBoost"
    ),
    uncertainty_semantics = paste(
      "Deterministic descriptive summaries from archived held-out performance and PFI ranks;",
      "no confidence intervals or hypothesis tests"
    )
  )]
  setcolorder(source, c(
    "study", "claim_id", "focal_learner", "learner_name", "learner_label", "primary_measure", "direction",
    "mean_score", "best_score", "acceptance_limit", "tolerance_absolute", "accepted", "distance_from_best",
    "performance_tolerance_fraction", "n_ranked_features", "kendall_tau_vs_focal",
    "rank_discordance_vs_focal", "top_k", "top_k_jaccard_vs_focal",
    "raw_performance_difference_from_focal", "direction_adjusted_performance_difference_from_focal",
    "focal_mean_score", "prespecified_tolerance", "fraction_of_prespecified_tolerance_from_focal",
    "n_feature_group_pairs", "n_reversed_feature_group_pairs", "n_tied_feature_group_pairs",
    "top_k_intersection", "top_k_union", "evidence_scope",
    "uncertainty_semantics"
  ))
  source[]
}

.evidence_pisa_pv_importance_recurrence = function(all_pv_pfi, top_k = 5L) {
  all_pv_pfi = copy(as.data.table(all_pv_pfi))
  .evidence_require_columns(
    all_pv_pfi,
    c("outcome_id", "feature_group", "rank"),
    "PISA all-plausible-value PFI summary"
  )
  expected_outcomes = paste0("PV", seq_len(10L), "READ")
  all_pv_pfi = all_pv_pfi[feature_group != "socioeconomic_joint_block"]
  top_k = as.integer(top_k)
  rank_grid = all_pv_pfi[, .(
    n_ranked_features = .N,
    finite_ranks = all(is.finite(rank)),
    unique_feature_groups = uniqueN(feature_group) == .N
  ), by = outcome_id]
  if (top_k != 5L || nrow(all_pv_pfi) != 410L ||
      !setequal(rank_grid$outcome_id, expected_outcomes) || any(rank_grid$n_ranked_features != 41L) ||
      !all(rank_grid$finite_ranks) || !all(rank_grid$unique_feature_groups)) {
    stop("PISA all-plausible-value PFI does not match the 10-by-41 atomic-feature grid.", call. = FALSE)
  }
  setorder(all_pv_pfi, outcome_id, rank, feature_group)
  patterns = all_pv_pfi[, .(
    n_ranked_features = .N,
    leading_groups = paste(feature_group[seq_len(top_k)], collapse = " | "),
    full_rank_pattern = paste(feature_group, collapse = " | ")
  ), by = outcome_id]
  reference_top = strsplit(patterns[outcome_id == "PV1READ", leading_groups], " \\| ")[[1L]]
  reference_full = patterns[outcome_id == "PV1READ", full_rank_pattern]
  patterns[, leading_set_jaccard_vs_pv1 := vapply(strsplit(leading_groups, " \\| "), function(groups) {
    length(intersect(groups, reference_top)) / length(union(groups, reference_top))
  }, numeric(1L))]
  patterns[, `:=`(
    top_k = top_k,
    leading_order_matches_pv1 = leading_groups == paste(reference_top, collapse = " | "),
    full_rank_matches_pv1 = full_rank_pattern == reference_full,
    evidence_scope = paste(
      "Deterministic comparison of atomic feature-group ranks after held-out marginal-PFI values were computed",
      "separately for ten plausible values; the overlapping socioeconomic joint query is excluded"
    )
  )]
  patterns[, full_rank_pattern := NULL]
  patterns[, outcome_order := match(outcome_id, expected_outcomes)]
  setorder(patterns, outcome_order)
  patterns[, outcome_order := NULL]
  patterns[]
}

.evidence_pisa_boundary_source = function(all_pv, performance_explanation, loco) {
  all_pv = copy(as.data.table(all_pv))
  performance_explanation = copy(as.data.table(performance_explanation))
  loco = copy(as.data.table(loco))
  rmse = all_pv[learner_id == "xgboost" & measure_id == "regr.rmse"]
  if (nrow(rmse) != 1L || nrow(performance_explanation) < 2L || nrow(loco) != 32L) {
    stop("PISA boundary sources do not match the frozen publication evidence.", call. = FALSE)
  }
  alternative_models = performance_explanation[learner_name != focal_learner]
  if (!nrow(alternative_models)) {
    stop("PISA model-boundary evidence must contain an accepted alternative to the focal model.", call. = FALSE)
  }
  data.table(
    boundary_order = 1:3,
    boundary = c("Outcome", "Model", "Setting"),
    question = c(
      "How stable is the result across ten reading plausible values?",
      "How stable is the explanation across accepted near-equivalent model classes?",
      "How does performance change when each observed country is excluded from training?"
    ),
    observed_range = c(
      sprintf("XGBoost RMSE %.3f\u2013%.3f across 10 PVs", rmse$min_across_pv, rmse$max_across_pv),
      sprintf(
        paste(
          "Practical-tie pair reversals %.1f%%\u2013%.1f%% for %d accepted alternatives vs focal XGBoost",
          "(%d accepted models total)"
        ),
        100 * min(alternative_models$practical_reversal_fraction),
        100 * max(alternative_models$practical_reversal_fraction),
        nrow(alternative_models),
        nrow(performance_explanation)
      ),
      sprintf(
        "Mean country-exclusion penalty %.3f\u2013%.3f RMSE across 32 observed countries",
        min(loco$mean_penalty),
        max(loco$mean_penalty)
      )
    ),
    permissible_interpretation = c(
      "Descriptive across-plausible-value variation; not a generic Rubin-pooled uncertainty estimate",
      "Practical-tie and value-weighted explanation disagreement within the accepted model set",
      "Observed-country boundary stress only; not performance on an unobserved country and not causal transport"
    ),
    inference = "No hypothesis tests or confidence intervals"
  )
}

.evidence_attach_practical_tie_denominators = function(source, agreement, practical_tolerance) {
  source = copy(as.data.table(source))
  agreement = .analysis_add_practical_tie_denominators(agreement)
  .evidence_require_columns(source, c("learner_name", "focal_learner"), "Performance-explanation source")
  .evidence_require_columns(agreement, c("learner_1", "learner_2"), "Importance-agreement table")
  tolerance_value = practical_tolerance
  practical = agreement[abs(practical_tolerance - tolerance_value) < 1e-12]
  practical[, learner_name := fifelse(learner_1 == source$focal_learner[[1L]], learner_2, learner_1)]
  keep = c(
    "learner_name", "n_total_pairs", "n_practical_ties_excluded", "n_comparable_pairs",
    "n_practical_reversals", "practical_reversal_percent_among_comparable"
  )
  practical = practical[, ..keep]
  existing_denominators = intersect(setdiff(keep, "learner_name"), names(source))
  if (length(existing_denominators)) {
    source[, (existing_denominators) := NULL]
  }
  if (anyDuplicated(practical$learner_name)) {
    stop("Importance-agreement rows are duplicated at the selected practical tolerance.", call. = FALSE)
  }
  result = merge(source, practical, by = "learner_name", all.x = TRUE, sort = FALSE)
  focal = result$learner_name == result$focal_learner
  result[focal, `:=`(
    n_total_pairs = as.integer(n_feature_group_pairs),
    n_practical_ties_excluded = 0L,
    n_comparable_pairs = as.integer(n_feature_group_pairs),
    n_practical_reversals = 0L,
    practical_reversal_percent_among_comparable = 0
  )]
  if (anyNA(result[, ..keep])) {
    stop("Practical-tie denominators do not cover every accepted model.", call. = FALSE)
  }
  result[]
}

.evidence_performance_explanation_plot = function(source, study_name) {
  source = copy(as.data.table(source))
  source = source[study == study_name & learner_name != focal_learner]
  if (!nrow(source)) stop(study_name, " has no accepted alternative models.", call. = FALSE)
  source[, learner_label := fifelse(
    grepl("^ridge", learner_name),
    "Ridge",
    .analysis_model_labels(learner_name)
  )]
  required_denominators = c(
    "n_total_pairs", "n_practical_ties_excluded", "n_comparable_pairs", "n_practical_reversals"
  )
  .evidence_require_columns(source, required_denominators, paste(study_name, "practical-tie source"))
  long = melt(
    source,
    id.vars = c("learner_name", "learner_label", required_denominators),
    measure.vars = c(
      "fraction_of_prespecified_tolerance_from_focal",
      "practical_reversal_fraction",
      "importance_weighted_reversal_fraction",
      "top_k_jaccard_practical"
    ),
    variable.name = "quantity",
    value.name = "value"
  )
  long[, measure_id := fcase(
    quantity == "fraction_of_prespecified_tolerance_from_focal", "performance_tolerance_used",
    quantity == "practical_reversal_fraction", "practical_pair_reversals",
    quantity == "importance_weighted_reversal_fraction", "importance_weighted_reversals",
    quantity == "top_k_jaccard_practical", "top_five_overlap"
  )]
  long[, quantity := factor(quantity, levels = c(
    "fraction_of_prespecified_tolerance_from_focal",
    "practical_reversal_fraction",
    "importance_weighted_reversal_fraction",
    "top_k_jaccard_practical"
  ), labels = c(
    "Performance\ntolerance used",
    "Practical pair\nreversals\n(lower is less)",
    "Importance-weighted\nreversals",
    "Top-five\noverlap\n(higher is more)"
  ))]
  long[, value_label := scales::percent(value, accuracy = 0.1)]
  labels_top_to_bottom = c("Ridge", "Random forest")
  labels_top_to_bottom = c(
    labels_top_to_bottom[labels_top_to_bottom %in% unique(long$learner_label)],
    sort(setdiff(unique(long$learner_label), labels_top_to_bottom))
  )
  long[, learner_label := factor(learner_label, levels = rev(labels_top_to_bottom))]
  ggplot(long, aes(x = value, y = learner_label)) +
    geom_col(width = 0.42, fill = "grey35", color = "black", linewidth = 0.35) +
    geom_text(
      aes(label = value_label),
      hjust = -0.12,
      size = 4.0,
      family = .analysis_plot_font_family()
    ) +
    facet_grid(. ~ quantity) +
    scale_y_discrete(labels = function(value) sub("Random forest", "Random\nforest", value, fixed = TRUE)) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = scales::label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0.30))
    ) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(clip = "off") +
    .analysis_theme(base_size = 11.5, line_base_size = 11.5) +
    .analysis_typography_theme(axis_text_size = 12.5, axis_title_size = 13.5, strip_text_size = 12.5) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing.x = grid::unit(0.8, "lines"),
      strip.text = element_text(face = "plain", margin = margin(b = 4)),
      plot.margin = margin(5.5, 24, 5.5, 5.5)
    )
}

.evidence_systematic_g4_group_text = function() {
  list(
    title = "SHILD held-out additive-surrogate fidelity",
    subtitle = .analysis_wrap_text(paste(
      "Coverage-oriented sample of 300 held-out cases; 20 independently seeded perturbation and cross-fit",
      "replicates with 500 perturbation points per replicate."
    )),
    caption = .analysis_wrap_text(paste(
      "Panels describe one additive ridge-surrogate procedure for frozen fold-specific XGBoost predictions.",
      "The dotted weighted-R-squared line is a historical reference, not a universal adequacy threshold.",
      "In panels b-d, points are medians across within-case replicate medians; whiskers are their 25th-75th",
      "percentiles within prediction-score decile (b-c) or kernel width (d), using quantile type 8.",
      paste(
        "Weighted R-squared depends on local prediction variance and is shown with absolute error. Seed variation is",
        "computational variation, not model-fit or population-sampling uncertainty; no confidence intervals or tests."
      )
    ), width = 125L)
  )
}

.evidence_systematic_g4_panels = function(cases, bandwidth_cases) {
  cases = copy(as.data.table(cases))
  bandwidth_cases = copy(as.data.table(bandwidth_cases))
  threshold = unique(cases$fidelity_threshold)
  if (length(threshold) != 1L) stop("Systematic G4 cases use inconsistent thresholds.", call. = FALSE)
  decile_summary = cases[, .(
    median_r2 = stats::median(median_weighted_r2),
    q25_r2 = .evidence_quantile(median_weighted_r2, 0.25),
    q75_r2 = .evidence_quantile(median_weighted_r2, 0.75),
    median_rmse = stats::median(median_weighted_rmse),
    q25_rmse = .evidence_quantile(median_weighted_rmse, 0.25),
    q75_rmse = .evidence_quantile(median_weighted_rmse, 0.75)
  ), by = prediction_score_decile]
  bandwidth_summary = bandwidth_cases[, .(
    median_rmse = stats::median(median_weighted_rmse),
    q25_rmse = .evidence_quantile(median_weighted_rmse, 0.25),
    q75_rmse = .evidence_quantile(median_weighted_rmse, 0.75)
  ), by = kernel_width]

  equal_limits = range(c(cases$median_apparent_weighted_r2, cases$median_weighted_r2), na.rm = TRUE)
  equal_padding = max(0.02 * diff(equal_limits), 0.01)
  equal_limits = equal_limits + c(-equal_padding, equal_padding)
  equal_breaks = scales::breaks_pretty(n = 5L)(equal_limits)

  panel_optimism = ggplot(cases, aes(x = median_apparent_weighted_r2, y = median_weighted_r2)) +
    geom_abline(slope = 1, intercept = 0, color = "grey55", linetype = 2, linewidth = 0.65) +
    geom_point(shape = 21, fill = "white", color = "black", stroke = 0.35, alpha = 0.75, size = 1.7) +
    scale_x_continuous(breaks = equal_breaks) +
    scale_y_continuous(breaks = equal_breaks) +
    coord_equal(xlim = equal_limits, ylim = equal_limits) +
    labs(
      x = "Apparent weighted\nR-squared",
      y = "Cross-fitted weighted\nR-squared"
    ) +
    .analysis_theme(base_size = 12, line_base_size = 12) +
    .analysis_typography_theme(axis_text_size = 14, axis_title_size = 15.5) +
    theme(legend.position = "none")

  c(
    list(apparent_vs_cross_fitted = panel_optimism),
    .evidence_systematic_g4_summary_panels(decile_summary, bandwidth_summary, threshold)
  )
}

.evidence_systematic_g4_summary_panels = function(decile_summary, bandwidth_summary, threshold = 0.8) {
  decile_summary = copy(as.data.table(decile_summary))
  bandwidth_summary = copy(as.data.table(bandwidth_summary))
  .evidence_require_columns(
    decile_summary,
    c("prediction_score_decile", "median_r2", "q25_r2", "q75_r2", "median_rmse", "q25_rmse", "q75_rmse"),
    "Local-fidelity decile plot summaries"
  )
  .evidence_require_columns(
    bandwidth_summary,
    c("kernel_width", "median_rmse", "q25_rmse", "q75_rmse"),
    "Local-fidelity bandwidth plot summaries"
  )
  setorder(decile_summary, prediction_score_decile)
  setorder(bandwidth_summary, kernel_width)
  panel_relative = ggplot(decile_summary, aes(x = prediction_score_decile, y = median_r2)) +
    geom_hline(yintercept = threshold, linetype = 3, color = "grey35", linewidth = 0.7) +
    geom_errorbar(aes(ymin = q25_r2, ymax = q75_r2), width = 0.16, linewidth = 0.65) +
    geom_line(linewidth = 0.65) +
    geom_point(shape = 21, fill = "white", color = "black", stroke = 0.45, size = 2.3) +
    scale_x_continuous(breaks = seq_len(10L)) +
    labs(
      x = "Within-fold prediction-score\ndecile",
      y = "Case median weighted\nR-squared"
    ) +
    .analysis_theme(base_size = 12, line_base_size = 12) +
    .analysis_typography_theme(axis_text_size = 14, axis_title_size = 15.5) +
    theme(legend.position = "none")

  panel_absolute = ggplot(decile_summary, aes(x = prediction_score_decile, y = median_rmse)) +
    geom_errorbar(aes(ymin = q25_rmse, ymax = q75_rmse), width = 0.16, linewidth = 0.65) +
    geom_line(linewidth = 0.65) +
    geom_point(shape = 21, fill = "white", color = "black", stroke = 0.45, size = 2.3) +
    scale_x_continuous(breaks = seq_len(10L)) +
    labs(
      x = "Within-fold prediction-score\ndecile",
      y = "Case median weighted\nRMSE"
    ) +
    .analysis_theme(base_size = 12, line_base_size = 12) +
    .analysis_typography_theme(axis_text_size = 14, axis_title_size = 15.5) +
    theme(legend.position = "none")

  panel_locality = ggplot(bandwidth_summary, aes(x = kernel_width, y = median_rmse)) +
    geom_errorbar(aes(ymin = q25_rmse, ymax = q75_rmse), width = 0.035, linewidth = 0.65) +
    geom_line(linewidth = 0.65) +
    geom_point(shape = 21, fill = "white", color = "black", stroke = 0.45, size = 2.3) +
    scale_x_continuous(breaks = sort(unique(bandwidth_summary$kernel_width))) +
    labs(
      x = "Kernel width",
      y = "Case median weighted\nRMSE"
    ) +
    .analysis_theme(base_size = 12, line_base_size = 12) +
    .analysis_typography_theme(axis_text_size = 14, axis_title_size = 15.5) +
    theme(legend.position = "none")

  list(
    relative_fidelity_by_prediction = panel_relative,
    absolute_fidelity_by_prediction = panel_absolute,
    locality_curve = panel_locality
  )
}

.evidence_pisa_boundary_plot = function(boundaries) {
  source = copy(as.data.table(boundaries))
  source[, boundary_label := factor(boundary, levels = rev(boundary))]
  colors = c(Outcome = "#4C72B0", Model = "#6E90C9", Setting = "#C44E52")
  text_colors = c(Outcome = "white", Model = "#202020", Setting = "white")
  source[, `:=`(
    x = 0,
    range_text = vapply(observed_range, .analysis_wrap_text, character(1L), width = 45L),
    interpretation_text = vapply(
      permissible_interpretation,
      .analysis_wrap_text,
      character(1L),
      width = 52L
    )
  )]
  ggplot(source, aes(x = x, y = boundary_label)) +
    geom_tile(aes(fill = boundary), width = 0.12, height = 0.62) +
    geom_text(aes(label = boundary, color = boundary), fontface = "bold", size = 4.2) +
    geom_text(aes(x = 0.08, label = range_text), hjust = 0, color = "grey15", size = 4.0, lineheight = 0.98) +
    geom_text(
      aes(x = 0.58, label = interpretation_text),
      hjust = 0,
      color = "grey30",
      size = 3.9,
      lineheight = 0.98
    ) +
    annotate("text", x = 0.08, y = 3.65, label = "Observed descriptive range", hjust = 0,
      fontface = "bold", size = 4.2) +
    annotate("text", x = 0.58, y = 3.65, label = "Interpretation boundary", hjust = 0,
      fontface = "bold", size = 4.2) +
    scale_fill_manual(values = colors, guide = "none") +
    scale_color_manual(values = text_colors, guide = "none") +
    coord_cartesian(xlim = c(-0.07, 1), ylim = c(0.55, 3.85), clip = "off") +
    labs(
      x = NULL,
      y = NULL,
      title = "PISA evidence is bounded along three distinct dimensions",
      subtitle = .analysis_wrap_text(paste(
        "Outcome, model, and observed-setting sensitivity answer different questions.",
        "None alone establishes invariance or transport."
      )),
      caption = "Descriptive archived results only; no PISA model was retrained."
    ) +
    .analysis_theme(base_size = 12, line_base_size = 12) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(5.5, 12, 5.5, 5.5)
    )
}
