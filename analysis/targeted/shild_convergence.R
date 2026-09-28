# A bounded learning-curve extension with fixed independent evaluation neighborhoods.

targeted_convergence_frame = function(frame) {
  selected = frame[selection_draw == 1L]
  desired = CJ(prediction_score_decile = 1:10, offset = c(0L, 3L, 6L))
  desired[, fold := 1L + (prediction_score_decile - 1L + offset) %% 10L]
  answer = merge(selected, desired[, .(prediction_score_decile, fold)],
    by = c("prediction_score_decile", "fold"))
  stopifnot(nrow(answer) == 30L, all(answer[, .N, by = fold]$N == 3L),
    all(answer[, .N, by = prediction_score_decile]$N == 3L))
  answer
}

run_shild_convergence = function(workspace, private_output, project_root = getwd(), workers = 2L) {
  source(file.path(project_root, "analysis/targeted/local_helpers.R"))
  verified = targeted_verify_inputs(workspace, project_root)
  oof = verified$oof
  selected = targeted_convergence_frame(verified$frame)
  dir.create(private_output, recursive = TRUE, showWarnings = FALSE)
  stopifnot(workers >= 1L, workers <= 2L,
    !startsWith(normalizePath(private_output), paste0(normalizePath(project_root), "/")))
  source_paths = file.path(project_root, c("analysis/targeted/shild_convergence.R",
    "analysis/targeted/local_helpers.R", "R/csdg_local_faithfulness.R", "R/csdg_local_fidelity_audit.R"))
  source_hashes = setNames(vapply(source_paths, targeted_sha256, character(1L)), basename(source_paths))
  output = file.path(project_root, "analysis/targeted/results/shild")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  previous_path = file.path(output, "shild_convergence_metadata.json")
  previous_metadata = if (file.exists(previous_path)) jsonlite::read_json(previous_path) else NULL
  settings = rbindlist(list(data.table(n = c(250L, 500L, 1000L, 2000L), lambda = .001, role = "size"),
    data.table(n = 1000L, lambda = c(.0001, .01), role = "regularization")))
  signature = digest::digest(list(model = verified$model_sha256, cases = selected$case_label, settings = settings,
    helper = targeted_sha256(file.path(project_root, "analysis/targeted/local_helpers.R"))), algo = "sha256")
  start = proc.time()[["elapsed"]]
  cached_before = sum(file.exists(file.path(private_output, paste0("convergence_", selected$case_label, ".rds"))))
  evaluate_case = function(index) {
    case = selected[index]
    cache_path = file.path(private_output, paste0("convergence_", case$case_label, ".rds"))
    if (file.exists(cache_path)) {
      cache = readRDS(cache_path)
      stopifnot(identical(cache$signature, signature))
      return(cache$result)
    }
    stopifnot(proc.time()[["elapsed"]] - start < 3600)
    map = mlr3autoiml:::.local_fidelity_prepare_cases(oof, case$row_id, case$case_label, NULL)
    case_number = as.integer(sub("case_", "", case$case_label))
    evaluation = targeted_case_design(oof, map, 1000L, 403609100L + case_number)
    eval_design = evaluation$design[-1L, , drop = FALSE]
    eval_probability = evaluation$probability[-1L]
    eval_weights = evaluation$weights[-1L]
    rows = list()
    for (replicate in 1:5) {
      training = targeted_case_design(oof, map, 2000L, 503609100L + 1000000L * replicate + case_number)
      stopifnot(identical(colnames(training$design), colnames(eval_design)))
      # The original case at row 1 is excluded from training and scored separately.
      for (setting_index in seq_len(nrow(settings))) {
        setting = settings[setting_index]
        training_rows = seq.int(2L, setting$n + 1L)
        for (scale in c("response", "link")) {
          target = mlr3autoiml:::.local_prediction_scale(training$probability[training_rows], "classif", scale)
          fit = targeted_fixed_ridge(training$design[training_rows, , drop = FALSE], target,
            training$weights[training_rows], setting$lambda)
          prediction = as.numeric(eval_design %*% fit$coefficients)
          target_prediction = sum(evaluation$design[1L, ] * fit$coefficients)
          if (scale == "link") {
            prediction = plogis(prediction)
            target_prediction = plogis(target_prediction)
          }
          row = as.data.table(as.list(targeted_score(eval_probability, prediction, eval_weights,
            evaluation$probability[1L], target_prediction)))
          row[, `:=`(case_label = case$case_label, fold = case$fold,
            prediction_score_decile = case$prediction_score_decile, replicate = replicate,
            n_training = setting$n, lambda = setting$lambda, design_role = setting$role, surrogate_scale = scale,
            effective_training_n = fit$effective_n, n_active_terms = fit$n_active_terms)]
          rows[[length(rows) + 1L]] = row
        }
      }
    }
    result = rbindlist(rows)
    saveRDS(list(signature = signature, result = result), cache_path, version = 3)
    result
  }
  result = if (.Platform$OS.type != "windows" && workers > 1L) {
    parallel::mclapply(seq_len(nrow(selected)), evaluate_case, mc.cores = workers)
  } else lapply(seq_len(nrow(selected)), evaluate_case)
  stopifnot(!any(vapply(result, inherits, logical(1L), "try-error")))
  result = rbindlist(result)
  stopifnot(nrow(result) == 1800L)
  metrics = c("weighted_r2", "weighted_rmse", "weighted_mae", "target_absolute_error",
    "effective_training_n", "n_active_terms", "out_of_probability_range_fraction")
  summary = targeted_aggregate_cases(result, c("n_training", "lambda", "design_role", "surrogate_scale"), metrics)
  summary[, uncertainty := "Across-case medians and quartiles; five seeds first summarized within case; no CI"]
  fwrite(summary, file.path(output, "shild_convergence_summary.csv"))
  fwrite(selected[, .N, by = .(fold, prediction_score_decile)],
    file.path(output, "shild_convergence_design_cells.csv"))
  jsonlite::write_json(list(status = "executed_on_verified_frozen_models", date = "2026-09-10",
    source_sha256 = as.list(source_hashes),
    model_sha256 = verified$model_sha256, cache_signature = signature, n_cases = 30L, replicates = 5L,
    case_selection = paste("Three fixed cyclic fold offsets per decile;",
      "three cases per fold and decile; no fidelity selection"),
    training_neighbor_counts = c(250L, 500L, 1000L, 2000L), kernel_width = .75,
    evaluation_neighbors = 1000L, evaluation_fixed_across_replicates_and_sizes = TRUE,
    evaluation_seed_base = 403609100L, training_seed_base = 503609100L,
    training_rows_nested_within_replicate = TRUE, target_case_excluded_from_training = TRUE,
    primary_lambda = .001, sensitivity_lambda = c(.0001, .01), sensitivity_training_n = 1000L,
    objective = "Sum(normalized_weights * squared_residual) + lambda * sum(standardized_slope^2)",
    standardization = "Weighted means and variances estimated from training rows only; unpenalized intercept",
    penalty_chosen_using_evaluation = FALSE, predictor_refitted = FALSE,
    frozen_prediction_rows = 330180L, local_ridge_fits = 1800L,
    elapsed_seconds_this_invocation = proc.time()[["elapsed"]] - start,
    initial_local_fit_elapsed_seconds = if (cached_before == 0L) proc.time()[["elapsed"]] - start else {
      previous_metadata$initial_local_fit_elapsed_seconds %||% previous_metadata$elapsed_seconds
    },
    case_caches_reused = cached_before, newly_evaluated_cases = 30L - cached_before,
    interpretation = "Retrospective bounded size and regularization sensitivity; no universal approximation claim"
  ), file.path(output, "shild_convergence_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  stopifnot(identical(source_hashes,
    setNames(vapply(source_paths, targeted_sha256, character(1L)), basename(source_paths))))
  stopifnot(identical(verified$model_sha256, targeted_sha256(file.path(workspace,
    "paper_artifacts/current/shild/private/objects/shild_csdg_result_private.rds"))))
  message("Completed fixed-evaluation convergence and bounded penalty sensitivity for all 30 cases.")
  invisible(summary)
}

if (sys.nframe() == 0L) {
  arguments = commandArgs(trailingOnly = TRUE)
  stopifnot(length(arguments) == 2L)
  run_shild_convergence(normalizePath(arguments[1L]), arguments[2L])
}
