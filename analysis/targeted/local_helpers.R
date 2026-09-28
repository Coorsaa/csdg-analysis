# Shared numerical helpers for the targeted, frozen-model SHILD extensions.

targeted_load_helpers = function(project_root = getwd()) {
  library(mlr3autoiml)
  suppressPackageStartupMessages(library(data.table))
  helper = new.env(parent = globalenv())
  sys.source(file.path(project_root, "analysis/shared/evidence_helpers.R"), envir = helper)
  helper
}

targeted_sha256 = function(path) digest::digest(file = path, algo = "sha256")

targeted_weighted_quantile = function(x, weights, probability) {
  stopifnot(length(x) == length(weights), all(is.finite(x)), all(is.finite(weights)), all(weights > 0))
  order = order(x)
  x[order][which(cumsum(weights[order]) / sum(weights) >= probability)[1L]]
}

targeted_score = function(observed, predicted, weights, target_observed, target_predicted) {
  stopifnot(length(observed) == length(predicted), length(weights) == length(observed))
  stopifnot(all(is.finite(c(observed, predicted, weights))), all(weights > 0))
  metrics = mlr3autoiml:::.local_weighted_metrics(observed, predicted, weights)
  c(
    weighted_r2 = metrics$r2, weighted_rmse = metrics$rmse, weighted_mae = metrics$mae,
    error_q50 = targeted_weighted_quantile(abs(observed - predicted), weights, 0.50),
    error_q90 = targeted_weighted_quantile(abs(observed - predicted), weights, 0.90),
    error_q95 = targeted_weighted_quantile(abs(observed - predicted), weights, 0.95),
    maximum_absolute_error = metrics$maximum_absolute_error,
    target_absolute_error = abs(target_observed - target_predicted),
    out_of_probability_range_fraction = mean(predicted < 0 | predicted > 1)
  )
}

targeted_case_design = function(oof, case_map, n, seed) {
  prepared = mlr3autoiml:::.local_fidelity_case_data(oof, case_map)
  neighbors = mlr3autoiml:::.make_local_neighborhood(prepared$case, prepared$background, n, seed)
  neighbors = rbindlist(list(prepared$case, neighbors), use.names = TRUE)
  design = model.matrix(~ ., data = data.frame(neighbors, check.names = TRUE))
  prediction = mlr3autoiml:::.predict_newdata(oof$models[[case_map$iteration]], neighbors, task = oof$task)
  probability = mlr3autoiml:::.prediction_vector(prediction, oof$task$task_type, oof$task$positive)
  distance = mlr3autoiml:::.local_distance(neighbors, prepared$case, prepared$background)
  list(design = design, probability = probability, weights = exp(-(distance / 0.75)^2),
    feature_names = names(neighbors), prepared = prepared)
}

targeted_pair_case = function(oof, case_map, seed, crossfit_seed, n = 500L) {
  local = targeted_case_design(oof, case_map, n, seed)
  assessment = seq.int(2L, n + 1L)
  folds = c(NA_integer_, mlr3autoiml:::.local_crossfit_folds(n, crossfit_seed, 5L))
  pointwise = data.table(neighbor_id = assessment, evaluation_fold = folds[assessment],
    probability = local$probability[assessment], weight = local$weights[assessment])
  rows = list()
  coefficients = list()
  for (scale in c("response", "link")) {
    target = mlr3autoiml:::.local_prediction_scale(local$probability, "classif", scale)
    fit = mlr3autoiml:::.local_crossfit_surrogate(local$design, target, local$weights, folds)
    target_fit = mlr3autoiml:::.local_weighted_ridge(
      local$design[assessment, , drop = FALSE], target[assessment], local$weights[assessment]
    )
    target_prediction = sum(local$design[1L, ] * target_fit$coefficients)
    scored_prediction = if (scale == "link") plogis(fit$predicted[assessment]) else fit$predicted[assessment]
    scored_target = if (scale == "link") plogis(target_prediction) else target_prediction
    pointwise[, (paste0(scale, "_prediction")) := scored_prediction]
    metrics = targeted_score(local$probability[assessment], scored_prediction, local$weights[assessment],
      local$probability[1L], scored_target)
    rows[[scale]] = as.data.table(as.list(c(metrics, native_weighted_r2 =
      mlr3autoiml:::.local_weighted_metrics(target[assessment], fit$predicted[assessment],
        local$weights[assessment])$r2)))
    rows[[scale]][, surrogate_scale := scale]
    coefficients[[scale]] = mlr3autoiml:::.local_weighted_ridge(
      local$design, target, local$weights
    )$coefficients
  }
  list(metrics = rbindlist(rows), points = pointwise, coefficients = coefficients,
    feature_names = local$feature_names, term_assign = attr(local$design, "assign"),
    design_names = colnames(local$design))
}

targeted_fixed_ridge = function(design, outcome, weights, lambda) {
  stopifnot(is.matrix(design), all(is.finite(design)), length(outcome) == nrow(design),
    length(weights) == nrow(design), all(is.finite(outcome)), all(weights > 0), lambda > 0)
  intercept = match("(Intercept)", colnames(design))
  stopifnot(!is.na(intercept))
  weights = weights / sum(weights)
  features = setdiff(seq_len(ncol(design)), intercept)
  x = design[, features, drop = FALSE]
  center = colSums(x * weights)
  centered = sweep(x, 2L, center)
  variance = colSums(centered^2 * weights)
  active = which(variance > 0)
  standardized = sweep(centered[, active, drop = FALSE], 2L, sqrt(variance[active]), "/")
  scaled_design = cbind(1, standardized)
  penalty = diag(c(0, rep(lambda, length(active))), nrow = length(active) + 1L)
  scaled_beta = solve(crossprod(scaled_design, scaled_design * weights) + penalty,
    crossprod(scaled_design, outcome * weights))[, 1L]
  coefficient = setNames(numeric(ncol(design)), colnames(design))
  coefficient[features[active]] = scaled_beta[-1L] / sqrt(variance[active])
  coefficient[intercept] = scaled_beta[1L] - sum(center * coefficient[features])
  list(coefficients = coefficient, effective_n = 1 / sum(weights^2), n_active_terms = length(active))
}

targeted_aggregate_cases = function(replicates, groups, metrics) {
  case_results = replicates[, lapply(.SD, median), by = c("case_label", groups), .SDcols = metrics]
  long = melt(case_results, id.vars = c("case_label", groups), variable.name = "metric", value.name = "value")
  long[, .(n_cases = .N, median = median(value), q25 = quantile(value, .25, type = 8, names = FALSE),
    q75 = quantile(value, .75, type = 8, names = FALSE)), by = c(groups, "metric")]
}

targeted_verify_inputs = function(workspace, project_root = getwd()) {
  helper = targeted_load_helpers(project_root)
  result_path = file.path(workspace, "paper_artifacts/current/shild/private/objects/shild_csdg_result_private.rds")
  input_provenance = fread(file.path(workspace,
    "paper_artifacts/current/evidence/private/provenance/input_checksums.csv"))
  expected = input_provenance[role == "frozen_shild_result", sha256]
  stopifnot(length(expected) == 1L, identical(targeted_sha256(result_path), expected))
  table_names = c("shild_g4_local_fidelity_summary.csv", "shild_g4_method_sensitivity_summary.csv",
    "shild_g4_coefficient_stability_summary.csv", "shild_g4_fidelity_by_prediction_score_decile.csv")
  table_checks = rbindlist(lapply(table_names, function(name) {
    canonical = file.path(project_root, "baseline_evidence/EVIDENCE_SYNTHESIS/tables", name)
    retained = file.path(workspace, "paper_artifacts/current/evidence/private/tables", name)
    stopifnot(identical(targeted_sha256(canonical), targeted_sha256(retained)))
    data.table(table = name, sha256 = targeted_sha256(canonical), identical_to_canonical = TRUE)
  }))
  result = readRDS(result_path)
  oof = result$artifacts$oof
  stopifnot(length(oof$models) == 10L, oof$task$nrow == 16971L, length(oof$task$feature_names) == 27L)
  frame = fread(file.path(workspace,
    "paper_artifacts/current/evidence/private/objects/shild_g4_case_sampling_plan_private.csv"))
  replay = helper$.evidence_select_local_cases(oof$predictions)$private
  keys = c("fold", "prediction_score_decile", "row_id", "selection_draw", "selection_seed", "case_label")
  stopifnot(nrow(frame) == 300L, all(vapply(keys, function(key) {
    identical(as.character(frame[[key]]), as.character(replay[[key]]))
  }, logical(1L))))
  stopifnot(max(abs(frame$prob.discontent - replay$prob.discontent)) < 1e-12)
  for (index in seq_along(oof$models)) {
    stopifnot(!length(intersect(oof$train_sets[[index]], oof$test_sets[[index]])))
  }
  list(oof = oof, frame = replay, helper = helper, model_sha256 = expected, table_checks = table_checks)
}
