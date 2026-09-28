# Verify retained author inputs against the canonical evidence before any targeted local extension.

inventory_local_resources = function(workspace, project_root = getwd()) {
  source(file.path(project_root, "analysis/targeted/local_helpers.R"))
  verified = targeted_verify_inputs(workspace, project_root)
  oof = verified$oof
  replay_delta = max(vapply(seq_along(oof$models), function(index) {
    rows = oof$test_sets[[index]]
    data = oof$task$data(rows = rows, cols = oof$task$feature_names)
    prediction = oof$models[[index]]$predict_newdata(data)$prob[, "discontent"]
    archived = oof$predictions$prob.discontent[match(rows, oof$predictions$row_id)]
    max(abs(prediction - archived))
  }, numeric(1L)))
  stopifnot(replay_delta < 1e-12)
  selected = verified$frame[selection_draw == 1L]
  map = mlr3autoiml:::.local_fidelity_prepare_cases(oof, selected$row_id[1L], selected$case_label[1L], NULL)
  elapsed = system.time({ probe = targeted_pair_case(oof, map,
    verified$helper$.evidence_perturbation_seed(202608120L, map$case_label, 1L),
    verified$helper$.evidence_perturbation_seed(302608120L, map$case_label, 1L)) })
  original = fread(file.path(workspace,
    "paper_artifacts/current/evidence/private/tables/shild_g4_local_fidelity_replicates.csv"))
  expected = original[case_label == map$case_label & perturbation_replicate == 1L & kernel_width == .75]
  actual = probe$metrics[surrogate_scale == "response"]
  probe_delta = abs(actual$native_weighted_r2 - expected$weighted_r2)
  stopifnot(nrow(expected) == 1L, probe_delta < 1e-12)
  pfi_path = file.path(workspace, "paper_artifacts/current/shild/upload/tables/shild_gate_g5__pfi__summary.csv")
  pfi = fread(pfi_path)
  stopifnot(nrow(pfi) == 27L)
  importance = setNames(pfi$mean_importance, pfi$feature_group)
  stopifnot(abs(importance[["A2"]] - .04583561) < 1e-8,
    abs(importance[["D2"]] - .03620421) < 1e-8, abs(importance[["M8"]] - .005420838) < 1e-9)
  private_pfi = file.path(workspace, "paper_artifacts/current/shild/private/tables/shild_gate_g5__pfi__summary.csv")
  stopifnot(identical(targeted_sha256(pfi_path), targeted_sha256(private_pfi)))
  artifact = as.data.table(readRDS(file.path(workspace,
    "paper_artifacts/current/shild/private/objects/shild_csdg_result_private.rds"))$artifacts$pfi$summary)
  stopifnot(isTRUE(all.equal(as.data.frame(pfi), as.data.frame(artifact), check.attributes = FALSE)))
  resources = file.path(project_root, "analysis/targeted/resources")
  dir.create(resources, recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(pfi_path, file.path(resources, "shild_selected_model_pfi_summary.csv"), overwrite = TRUE))
  jsonlite::write_json(list(
    date = "2026-09-10", scientific_basis = "Archived study outputs of the primary SHILD run (paper_artifacts/current/)",
    scope = "Authorized retained author caches; no alternate scientific baseline",
    frozen_result = list(path = "paper_artifacts/current/shild/private/objects/shild_csdg_result_private.rds",
      sha256 = verified$model_sha256, n_outer_models = 10L, analytic_rows = 16971L, predictors = 27L,
      maximum_oof_replay_difference = replay_delta, empirical_predictors_retrained = FALSE),
    frame = list(n_coverage_cases = 300L, n_method_cases = 100L,
      deterministic_keys_match_canonical_sampler = TRUE, probability_csv_rounding_tolerance = 1e-12),
    pointwise_surrogate_cache = paste("Not found in authorized workspace;",
      "deterministically regenerate matched neighborhoods"),
    coefficient_cache = paste("Only term-level stability summaries retained;",
      "refit coefficients reused within targeted extension"),
    checkpoint_cache = "Historical .checkpoint_cache directory absent",
    baseline_table_identity = verified$table_checks,
    dry_run = list(probe_cases = 1L, probe_replicates = 1L, neighbors = 500L,
      elapsed_seconds = unname(elapsed[["elapsed"]]), maximum_historical_probability_r2_difference = probe_delta,
      paired_cases = 100L, paired_replicates = 5L, paired_prediction_rows = 250500L,
      paired_local_ridge_fits_upper_bound = 7000L, convergence_cases = 30L, convergence_replicates = 5L,
      training_neighbor_counts = c(250L, 500L, 1000L, 2000L), fixed_evaluation_neighbors = 1000L,
      convergence_prediction_rows_upper_bound = 330330L, convergence_ridge_fits_upper_bound = 1800L,
      contribution_reference_queries_upper_bound = 100100L,
      total_prediction_rows_upper_bound = 680930L,
      local_private_cache_budget_mb = 250L, elapsed_budget_seconds = 3600L,
      concurrency = 2L, no_full_audit_or_predictive_training = TRUE),
    atomic_pfi_resource = list(source = "paper_artifacts/current/shild/upload/tables/shild_gate_g5__pfi__summary.csv",
      sha256 = targeted_sha256(pfi_path), rows = 27L, matches_frozen_result_artifact = TRUE,
      matches_E13_leading_values = TRUE,
      export = "analysis/targeted/resources/shild_selected_model_pfi_summary.csv"),
    status = "verified_inputs_available_targeted_execution_authorized",
    privacy = "Private rows and prediction caches remain local; public outputs are aggregate only"
  ), file.path(project_root, "analysis/targeted/qa/LOCAL_RESOURCE_INVENTORY.json"), pretty = TRUE, auto_unbox = TRUE)
  message("Frozen models, frame, four baseline tables, and atomic PFI verified; dry-run historical R2 reproduced.")
  invisible(verified)
}

if (sys.nframe() == 0L) {
  arguments = commandArgs(trailingOnly = TRUE)
  stopifnot(length(arguments) == 1L)
  inventory_local_resources(normalizePath(arguments[[1L]]))
}
