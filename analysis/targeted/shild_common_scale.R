# Targeted paired extension: existing 100 cases and five seeds, with no empirical predictor retraining.

run_shild_common_scale = function(workspace, private_output, project_root = getwd(), workers = 2L) {
  source(file.path(project_root, "analysis/targeted/local_helpers.R"))
  source(file.path(project_root, "analysis/targeted/shild_contribution_stability.R"))
  verified = targeted_verify_inputs(workspace, project_root)
  stopifnot(dir.exists(dirname(private_output)), workers >= 1L, workers <= 2L)
  dir.create(private_output, recursive = TRUE, showWarnings = FALSE)
  stopifnot(!startsWith(normalizePath(private_output), paste0(normalizePath(project_root), "/")))
  source_paths = file.path(project_root, c("analysis/targeted/shild_common_scale.R",
    "analysis/targeted/local_helpers.R", "analysis/targeted/shild_contribution_stability.R",
    "R/csdg_local_faithfulness.R", "R/csdg_local_fidelity_audit.R"))
  source_hashes = setNames(vapply(source_paths, targeted_sha256, character(1L)), basename(source_paths))
  oof = verified$oof
  selected = verified$frame[selection_draw == 1L]
  stopifnot(nrow(selected) == 100L)
  output = file.path(project_root, "analysis/targeted/results/shild")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  previous_metadata_path = file.path(output, "shild_common_scale_metadata.json")
  previous_metadata = if (file.exists(previous_metadata_path)) jsonlite::read_json(previous_metadata_path) else NULL
  signature = digest::digest(list(model = verified$model_sha256, frame = selected$case_label,
    helpers = targeted_sha256(file.path(project_root, "analysis/targeted/local_helpers.R")),
    contribution = targeted_sha256(file.path(project_root,
      "analysis/targeted/shild_contribution_stability.R")),
    n = 500L, replicates = 5L, kernel = .75), algo = "sha256")
  start = proc.time()[["elapsed"]]
  cached_before = sum(file.exists(file.path(private_output, paste0("paired_", selected$case_label, ".rds"))))
  evaluate_case = function(index) {
    case = selected[index]
    cache_path = file.path(private_output, paste0("paired_", case$case_label, ".rds"))
    if (file.exists(cache_path)) {
      cached = readRDS(cache_path)
      stopifnot(identical(cached$signature, signature))
      return(cached$result)
    }
    stopifnot(proc.time()[["elapsed"]] - start < 3600)
    map = mlr3autoiml:::.local_fidelity_prepare_cases(oof, case$row_id, case$case_label,
      data.frame(prediction_score_decile = case$prediction_score_decile))
    fits = lapply(1:5, function(replicate) {
      result = targeted_pair_case(oof, map,
        verified$helper$.evidence_perturbation_seed(202608120L, case$case_label, replicate),
        verified$helper$.evidence_perturbation_seed(302608120L, case$case_label, replicate))
      result$metrics[, `:=`(case_label = case$case_label, fold = case$fold,
        prediction_score_decile = case$prediction_score_decile, replicate = replicate)]
      result
    })
    contribution = targeted_contribution_case(oof, map, fits)
    contribution[, `:=`(case_label = case$case_label, fold = case$fold,
      prediction_score_decile = case$prediction_score_decile)]
    result = list(metrics = rbindlist(lapply(fits, `[[`, "metrics")), contribution = contribution)
    saveRDS(list(signature = signature, result = result,
      pointwise = lapply(fits, `[[`, "points"), coefficients = lapply(fits, `[[`, "coefficients")),
      cache_path, version = 3)
    result
  }
  results = if (.Platform$OS.type != "windows" && workers > 1L) {
    parallel::mclapply(seq_len(nrow(selected)), evaluate_case, mc.cores = workers)
  } else lapply(seq_len(nrow(selected)), evaluate_case)
  stopifnot(!any(vapply(results, inherits, logical(1L), "try-error")))
  metrics = rbindlist(lapply(results, `[[`, "metrics"))
  stopifnot(nrow(metrics) == 1000L)
  variables = setdiff(names(metrics), c("surrogate_scale", "case_label", "fold", "prediction_score_decile",
    "replicate", "native_weighted_r2"))
  summary = targeted_aggregate_cases(metrics, "surrogate_scale", variables)
  summary[, `:=`(aggregation = "Across-case medians of five within-case computational replicates",
    uncertainty = "Descriptive across cases; quartiles are not confidence intervals")]
  wide = dcast(metrics, case_label + fold + prediction_score_decile + replicate ~ surrogate_scale,
    value.var = variables)
  for (variable in variables) {
    wide[, (variable) := get(paste0(variable, "_link")) - get(paste0(variable, "_response"))]
  }
  paired = targeted_aggregate_cases(wide, character(), variables)
  paired[, contrast := "logit-backtransformed minus probability surrogate; difference before aggregation"]
  paired[, improvement_direction := ifelse(metric == "weighted_r2", "positive", "negative")]
  by_decile = targeted_aggregate_cases(wide, "prediction_score_decile", variables)
  original = fread(file.path(workspace,
    "paper_artifacts/current/evidence/private/tables/shild_g4_method_sensitivity_cases.csv"))
  native = metrics[, .(native_r2 = median(native_weighted_r2)), by = .(case_label, surrogate_scale)]
  historical = original[neighborhood_method == "synthetic" & surrogate_family == "additive ridge",
    .(case_label, surrogate_scale = target_scale, historical_r2 = median_weighted_r2)]
  native = merge(native, historical, by = c("case_label", "surrogate_scale"))
  stopifnot(nrow(native) == 200L, max(abs(native$native_r2 - native$historical_r2)) < 1e-10)
  native_summary = native[, .(n_cases = .N, median_native_r2 = median(native_r2),
    maximum_replay_difference = max(abs(native_r2 - historical_r2)),
    interpretation = "Different native target scales; not a common-scale performance contrast"), by = surrogate_scale]
  contribution = rbindlist(lapply(results, `[[`, "contribution"))
  contribution_summary = contribution[, .(n_cases = .N,
    median_contribution_rms = median(mean_contribution_rms),
    q25_contribution_rms = quantile(mean_contribution_rms, .25, type = 8, names = FALSE),
    q75_contribution_rms = quantile(mean_contribution_rms, .75, type = 8, names = FALSE),
    median_pairwise_contribution_rms_difference = median(mean_pairwise_contribution_rms_difference),
    median_relative_contribution_difference = median(relative_contribution_difference, na.rm = TRUE),
    median_magnitude_rank_sd = median(magnitude_rank_sd)), by = .(surrogate_scale, feature)]
  contribution_overall = unique(contribution[, .(case_label, surrogate_scale, magnitude_weighted_total_difference)])[
    , .(n_cases = .N, median_magnitude_weighted_difference = median(magnitude_weighted_total_difference),
      q25 = quantile(magnitude_weighted_total_difference, .25, type = 8, names = FALSE),
      q75 = quantile(magnitude_weighted_total_difference, .75, type = 8, names = FALSE)), by = surrogate_scale]
  fwrite(summary, file.path(output, "shild_common_scale_summary.csv"))
  fwrite(paired, file.path(output, "shild_common_scale_paired_differences.csv"))
  fwrite(by_decile, file.path(output, "shild_common_scale_paired_by_decile.csv"))
  fwrite(native_summary, file.path(output, "shild_native_scale_replay_check.csv"))
  fwrite(contribution_summary, file.path(output, "shild_contribution_stability_summary.csv"))
  fwrite(contribution_overall, file.path(output, "shild_contribution_stability_overall.csv"))
  jsonlite::write_json(list(date = "2026-09-10", status = "executed_on_verified_frozen_models",
    source_sha256 = as.list(source_hashes),
    model_sha256 = verified$model_sha256, cache_signature = signature, n_cases = 100L, replicates = 5L,
    evaluation_neighbors_per_replicate = 500L, outer_folds = 10L, crossfit_folds = 5L, kernel_width = .75,
    paired_keys_and_weights = "Shared by construction; same case/seed/neighborhood/held-out fold in both variants",
    target_excluded_from_target_surrogate_fit = TRUE, probability_predictions_clipped = FALSE,
    link_target_clipping_epsilon = 1e-15, comparison_scale = "probability",
    historical_native_r2_maximum_difference = max(abs(native$native_r2 - native$historical_r2)),
    contribution_scale = "Response contributions and additive logit contributions reported separately",
    contribution_center = "Common within-case weighted mean on 1000 independent reference neighbors",
    contribution_grouping = paste("All encoded terms summed into their 27 original input variables;",
      "intercept excluded"),
    predictors_retrained = FALSE, elapsed_seconds_this_invocation = proc.time()[["elapsed"]] - start,
    initial_local_fit_elapsed_seconds = if (cached_before == 0L) proc.time()[["elapsed"]] - start else {
      previous_metadata$initial_local_fit_elapsed_seconds %||% previous_metadata$elapsed_seconds
    },
    case_caches_reused = cached_before, newly_evaluated_cases = 100L - cached_before,
    paired_frozen_prediction_rows = 250500L, reference_frozen_prediction_rows = 100100L,
    local_ridge_fits = 7000L,
    private_cache_bytes = sum(file.info(list.files(private_output,
      pattern = "^paired_.*rds$", full.names = TRUE))$size),
    inference = "Retrospective paired descriptive local extension; five seeds are not independent people"
  ), file.path(output, "shild_common_scale_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  stopifnot(sum(file.info(list.files(private_output, pattern = "^paired_.*rds$", full.names = TRUE))$size) < 250e6)
  stopifnot(identical(source_hashes,
    setNames(vapply(source_paths, targeted_sha256, character(1L)), basename(source_paths))))
  stopifnot(identical(verified$model_sha256, targeted_sha256(file.path(workspace,
    "paper_artifacts/current/shild/private/objects/shild_csdg_result_private.rds"))))
  message("Completed paired common-scale and contribution extensions for all 100 existing method cases.")
  invisible(summary)
}

if (sys.nframe() == 0L) {
  arguments = commandArgs(trailingOnly = TRUE)
  stopifnot(length(arguments) == 2L)
  run_shild_common_scale(normalizePath(arguments[1L]), arguments[2L])
}
