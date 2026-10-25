# SHILD: analyses behind the extended claim records of the article.
#
#   --part=importance   learner-level global importance from the stored PFI of the primary run (no refitting):
#                       fold means, ratios, corrected resampled t intervals (Nadeau & Bengio, 2003), and the
#                       Monte Carlo comparison of self-rated health and depressed mood per fold.
#   --part=loco         population importance: leave-one-covariate-out (LOCO) refits of XGBoost, ridge logistic
#                       regression, and random forest on the primary folds; held-out log loss and Brier score,
#                       unweighted and weighted with the survey weight; the number of held-out respondents whose
#                       observed outcome received probability 0 (log loss then depends on the clipping bound);
#                       corrected intervals and Holm-adjusted comparisons.
#   --part=conditional  conditional PFI of the XGBoost fits on realistic inputs: each item permuted within the
#                       subgroups of a regression or classification tree that predicts it from the other items
#                       on the training fold (conditional subgroup approach, Molnar et al., 2024); held-out
#                       log loss, 20 permutations per item and fold.
#   --part=selection    nested check of the predictor selection: recursive feature elimination (RFE) with XGBoost
#                       inside each training fold, compared with the fixed 27 items on the same folds.
#   --part=local        individual explanations: the 300 sampled cases of the primary local evaluation, 20 repeated
#                       local surrogates per case (same seeds), on the probability and the log-odds scale; recurrence
#                       of the two items with the largest contributions and cross-fitted fidelity.
#   --part=export       aggregate tables behind decisive statements, read from the frozen primary outputs in
#                       paper_artifacts/current/ (no data, no refitting): local fidelity of the 300 evaluated cases,
#                       item missingness, and the fold-level PFI of self-rated health and depressed mood per run.
#
# Aggregates are written to analysis/shild/results/claim_extensions/; case-level tables only to CSDG_PRIVATE_DIR.
# Usage (from the repository root): Rscript analysis/shild/claim_extensions.R --part=loco --workers=6

suppressPackageStartupMessages({
  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3autoiml)
})
lgr::get_logger("mlr3")$set_threshold("warn")

args = commandArgs(trailingOnly = TRUE)
arg_value = function(name, default = NULL) {
  hit = grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}
part = arg_value("part", "importance")
stopifnot(part %in% c("importance", "loco", "conditional", "selection", "local", "export"))
workers = as.integer(arg_value("workers", "1"))
root = normalizePath(arg_value("root", "."), mustWork = TRUE)
output_dir = file.path(root, "analysis/shild/results/claim_extensions")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
private_dir = Sys.getenv("CSDG_PRIVATE_DIR", unset = "")
frozen = file.path(root, "paper_artifacts/current")
seed = 20260714L
folds = 10L

source(file.path(root, "analysis/shared/study_helpers.R"), local = FALSE)
source(file.path(root, "analysis/shild/prepare_data.R"), local = FALSE)
source(file.path(root, "analysis/shared/evidence_helpers.R"), local = FALSE)

# Corrected resampled t interval for a mean over K cross-validation folds (Nadeau & Bengio, 2003).
corrected_interval = function(values, test_fraction = 1 / folds, level = 0.95) {
  k = length(values)
  se = sqrt((1 / k + test_fraction / (1 - test_fraction)) * stats::var(values))
  q = stats::qt(1 - (1 - level) / 2, df = k - 1L)
  estimate = mean(values)
  list(estimate = estimate, se = se, lower = estimate - q * se, upper = estimate + q * se,
    p_greater = stats::pt(estimate / se, df = k - 1L, lower.tail = FALSE))
}
interval_row = function(values, ...) as.data.table(corrected_interval(values, ...))
fwrite_out = function(table, name) fwrite(table, file.path(output_dir, name))
write_session = function() {
  writeLines(capture.output(sessionInfo()), file.path(output_dir, paste0("sessionInfo_", part, ".txt")))
}

# ---- Part: aggregate tables from the frozen primary outputs (no data, no refitting) ----

if (part == "export") {
  # Every exported row summarizes at least 10 cases or respondents.
  minimum_n = 10L
  # Local fidelity of the 300 evaluated cases: primary kernel width, probability scale, per case the median over the
  # 20 repetitions (as in the stored case table); quartiles across cases are type-8 quantiles.
  cases = fread(file.path(frozen, "evidence/private/tables/shild_g4_local_fidelity_cases.csv"))
  stopifnot(nrow(cases) == 300L, uniqueN(cases$case_label) == 300L, all(cases$n_perturbation_replicates == 20L))
  fidelity = function(x) {
    list(n_cases = nrow(x),
      median_apparent_r2 = median(x$median_apparent_weighted_r2),
      q25_apparent_r2 = .evidence_quantile(x$median_apparent_weighted_r2, 0.25),
      q75_apparent_r2 = .evidence_quantile(x$median_apparent_weighted_r2, 0.75),
      median_crossfit_r2 = median(x$median_weighted_r2),
      q25_crossfit_r2 = .evidence_quantile(x$median_weighted_r2, 0.25),
      q75_crossfit_r2 = .evidence_quantile(x$median_weighted_r2, 0.75),
      n_apparent_above_crossfit = sum(x$median_apparent_weighted_r2 > x$median_weighted_r2),
      median_crossfit_rmse = median(x$median_weighted_rmse),
      q25_crossfit_rmse = .evidence_quantile(x$median_weighted_rmse, 0.25),
      q75_crossfit_rmse = .evidence_quantile(x$median_weighted_rmse, 0.75))
  }
  overall = as.data.table(fidelity(cases))
  # Deciles and folds hold 30 cases each; a fold within a decile holds only 3 and is therefore not exported.
  by_decile = cases[, fidelity(.SD), by = .(decile = prediction_score_decile)][order(decile)]
  by_fold = cases[, fidelity(.SD), by = fold][order(fold)]
  stopifnot(all(c(overall$n_cases, by_decile$n_cases, by_fold$n_cases) >= minimum_n))
  fwrite_out(overall, "export_local_fidelity_summary.csv")
  fwrite_out(by_decile, "export_local_fidelity_by_decile.csv")
  fwrite_out(by_fold, "export_local_fidelity_by_fold.csv")
  # Missingness of the 27 items in the analytic sample, before and after the documented recoding.
  missingness = fread(file.path(frozen, "shild/private/tables/shild_missingness.csv"))
  missingness[, stage := ifelse(startsWith(stage, "before"), "before_recoding", "after_recoding")]
  stopifnot(nrow(missingness) == 54L, uniqueN(missingness$variable) == 27L, all(missingness$n >= minimum_n))
  fwrite_out(missingness[order(stage, -missing_fraction, variable),
    .(stage, item = variable, n, n_missing, missing_share = missing_fraction)], "export_item_missingness.csv")
  # PFI of self-rated health (A2) and depressed mood (D2) per fold in each of the two stored runs of 10 permutations
  # (the importance part pools them to 20); each fold holds about 1,700 respondents.
  sources = c("shild_gate_g5__pfi__raw.csv", "shild_gate_g6a__model_generalization__pfi__xgboost__raw.csv")
  runs = rbindlist(lapply(seq_along(sources), function(index) {
    fread(file.path(frozen, "shild/private/tables", sources[[index]]))[, `:=`(run = index, source = sources[[index]])]
  }), fill = TRUE)
  pooled = runs[, .(pfi = mean(importance)), by = .(fold, item = feature_group)]
  stored = fread(file.path(output_dir, "importance_fold_pfi.csv"))
  check = merge(pooled, stored[, .(fold, item, stored_pfi = pfi)], by = c("fold", "item"))
  stopifnot(nrow(check) == nrow(stored), max(abs(check$pfi - check$stored_pfi)) < 1e-12)
  per_run = runs[feature_group %in% c("A2", "D2"), .(n_assessment = n_assessment[[1L]], pfi = mean(importance),
    mc_se = stats::sd(importance) / sqrt(.N), n_permutations = .N), by = .(run, source, fold, item = feature_group)]
  stopifnot(all(per_run$n_assessment >= minimum_n), all(per_run$n_permutations == 10L))
  wide = dcast(per_run, run + source + fold + n_assessment + n_permutations ~ item, value.var = c("pfi", "mc_se"))
  wide[, `:=`(difference_A2_minus_D2 = pfi_A2 - pfi_D2, mc_se_difference = sqrt(mc_se_A2^2 + mc_se_D2^2))]
  fwrite_out(wide[order(run, fold), .(run, source, fold, n_assessment, n_permutations, pfi_A2, mc_se_A2, pfi_D2,
    mc_se_D2, difference_A2_minus_D2, mc_se_difference)], "export_importance_fold_pfi_by_run.csv")
  print(overall); print(by_decile[decile %in% c(1L, 10L)])
  print(missingness[, .(max_share = max(missing_fraction)), by = stage])
  print(wide[, .(run, fold, difference_A2_minus_D2, mc_se_difference)])
  write_session()
  quit(save = "no")
}

# ---- Analytic sample, task, and the primary folds ----

raw = fread(file.path(root, "data/SHILD/archive/Danish_SHILD_draft_data.csv"), na.strings = c("", "NA", "NaN"))
preparation = .prepare_shild_raw(raw)
dt = preparation$data
items = preparation$predictors
for (column in items[vapply(dt[, ..items], is.character, logical(1L))]) set(dt, j = column, value = factor(dt[[column]]))
dt[, survey_weight := raw$pervgt[respondent_id]]
stopifnot(nrow(dt) == 16971L, !anyNA(dt$survey_weight), all(dt$survey_weight > 0))
make_task = function(features) {
  backend = dt[, c(features, "life_satisfaction"), with = FALSE]
  TaskClassif$new("shild", backend = backend, target = "life_satisfaction", positive = "discontent")
}
full_task = make_task(items)
split = .stratified_resampling(full_task, dt$life_satisfaction, folds, seed)
fold_of_row = split$assignment$fold
stored_folds = fread(file.path(frozen, "shild/private/tables/shild_fold_assignment.csv"))
stopifnot(identical(as.integer(stored_folds$fold), as.integer(fold_of_row)))
train_rows = lapply(seq_len(folds), function(fold) which(fold_of_row != fold))
test_rows = lapply(seq_len(folds), function(fold) which(fold_of_row == fold))
log_loss = function(truth_positive, probability) {
  probability = pmin(pmax(probability, 1e-15), 1 - 1e-15)
  -(truth_positive * log(probability) + (1 - truth_positive) * log(1 - probability))
}
positive = as.integer(dt$life_satisfaction == "discontent")

# ---- Part: learner-level global importance from the stored PFI ----

if (part == "importance") {
  # The article pools the two stored PFI runs of the XGBoost learner (10 permutations each, 20 per fold).
  runs = list(
    fread(file.path(frozen, "shild/private/tables/shild_gate_g5__pfi__raw.csv")),
    fread(file.path(frozen, "shild/private/tables/shild_gate_g6a__model_generalization__pfi__xgboost__raw.csv"))
  )
  raw_pfi = rbindlist(lapply(seq_along(runs), function(index) runs[[index]][, run := index]), fill = TRUE)
  stopifnot("importance" %in% names(raw_pfi))
  per_fold = raw_pfi[, .(pfi = mean(importance), mc_sd = sd(importance), n_permutations = .N), by = .(fold, feature_group)]
  per_fold[, mc_se := mc_sd / sqrt(n_permutations)]
  means = per_fold[, .(mean_pfi = mean(pfi), min_fold = min(pfi), max_fold = max(pfi)), by = feature_group][order(-mean_pfi)]
  fwrite_out(per_fold[, .(fold, item = feature_group, pfi, mc_se, n_permutations)], "importance_fold_pfi.csv")
  others = setdiff(means$feature_group, c("A2", "D2"))
  largest_other = means[feature_group %in% others][1L, feature_group]
  wide = dcast(per_fold, fold ~ feature_group, value.var = "pfi")
  max_other = wide[, do.call(pmax, .SD), .SDcols = others]
  ratios = data.table(fold = wide$fold, ratio_A2 = wide$A2 / max_other, ratio_D2 = wide$D2 / max_other,
    difference_A2_minus_D2 = wide$A2 - wide$D2)
  fwrite_out(ratios, "importance_fold_ratios.csv")
  comparisons = rbindlist(list(
    cbind(comparison = paste0("A2 - 2 x ", largest_other), interval_row(wide$A2 - 2 * wide[[largest_other]])),
    cbind(comparison = paste0("D2 - 2 x ", largest_other), interval_row(wide$D2 - 2 * wide[[largest_other]])),
    cbind(comparison = "A2 - 2 x largest other (per fold)", interval_row(wide$A2 - 2 * max_other)),
    cbind(comparison = "D2 - 2 x largest other (per fold)", interval_row(wide$D2 - 2 * max_other)),
    cbind(comparison = "A2 - D2", interval_row(wide$A2 - wide$D2))
  ))
  fwrite_out(comparisons, "importance_learner_intervals.csv")
  mc = dcast(per_fold[feature_group %in% c("A2", "D2")], fold ~ feature_group, value.var = c("pfi", "mc_se", "n_permutations"))
  mc[, `:=`(difference = pfi_A2 - pfi_D2, mc_se_difference = sqrt(mc_se_A2^2 + mc_se_D2^2))]
  mc[, t_threshold := stats::qt(0.975, df = pmin(n_permutations_A2, n_permutations_D2) - 1L) * mc_se_difference]
  mc[, beyond_mc_error := abs(difference) > t_threshold]
  fwrite_out(mc[, .(fold, difference, mc_se_difference, t_threshold, beyond_mc_error)], "importance_fold_A2_vs_D2.csv")
  fwrite_out(means[, .(item = feature_group, mean_pfi, min_fold, max_fold)], "importance_item_means.csv")
  print(means[1:4]); print(ratios[, .(min_A2 = min(ratio_A2), min_D2 = min(ratio_D2))]); print(comparisons); print(mc)
}

# ---- Learners as in analysis/shild/analysis.R ----

make_learner = function(learner_id) {
  switch(learner_id,
    xgboost = .make_pipeline_learner(lrn("classif.xgboost", predict_type = "prob", nrounds = 200L, max_depth = 3L,
      eta = 0.05, nthread = 1L, subsample = 1, colsample_bytree = 1), "shild_xgboost"),
    ridge_logistic = .make_pipeline_learner(lrn("classif.cv_glmnet", predict_type = "prob", alpha = 0, nfolds = 5L,
      s = "lambda.1se"), "shild_ridge_logistic"),
    random_forest = .make_pipeline_learner(lrn("classif.ranger", predict_type = "prob", num.trees = 400L,
      min.node.size = 5L, num.threads = 1L, seed = seed), "shild_ranger")
  )
}
fit_predict = function(learner_id, features, fold) {
  task = make_task(features)
  learner = make_learner(learner_id)
  set.seed(seed + fold)
  learner$train(task, row_ids = train_rows[[fold]])
  learner$predict(task, row_ids = test_rows[[fold]])$prob[, "discontent"]
}

# ---- Part: LOCO refits (population importance) ----

if (part == "loco") {
  jobs = CJ(learner = c("random_forest", "xgboost", "ridge_logistic"), design = c("all_items", items), sorted = FALSE)
  results = parallel::mclapply(seq_len(nrow(jobs)), function(index) {
    job = jobs[index]
    features = if (job$design == "all_items") items else setdiff(items, job$design)
    rbindlist(lapply(seq_len(folds), function(fold) {
      probability = fit_predict(job$learner, features, fold)
      rows = test_rows[[fold]]
      outcome = positive[rows]
      weight = dt$survey_weight[rows]
      loss = log_loss(outcome, probability)
      squared = (probability - outcome)^2
      # Observed outcome predicted with probability 0: its log loss is set by the clipping bound alone.
      at_bound = (outcome == 1L & probability <= 1e-15) | (outcome == 0L & probability >= 1 - 1e-15)
      data.table(learner = job$learner, design = job$design, fold = fold, n_test = length(rows),
        log_loss = mean(loss), weighted_log_loss = sum(loss * weight) / sum(weight),
        brier = mean(squared), weighted_brier = sum(squared * weight) / sum(weight),
        n_outcome_at_bound = sum(at_bound))
    }))
  }, mc.cores = workers, mc.preschedule = FALSE)
  failed = vapply(results, inherits, logical(1L), what = "try-error")
  if (any(failed)) stop("LOCO jobs failed: ", paste(vapply(results[failed], as.character, ""), collapse = " | "))
  losses = rbindlist(results)
  fwrite_out(losses, "loco_fold_losses.csv")
  full = losses[design == "all_items", .(learner, fold, full_log_loss = log_loss, full_weighted = weighted_log_loss,
    full_brier = brier, full_weighted_brier = weighted_brier)]
  loco = merge(losses[design != "all_items"], full, by = c("learner", "fold"))
  loco[, `:=`(loco = log_loss - full_log_loss, loco_weighted = weighted_log_loss - full_weighted,
    loco_brier = brier - full_brier, loco_brier_weighted = weighted_brier - full_weighted_brier)]
  fwrite_out(loco[, .(learner, item = design, fold, loco, loco_weighted, loco_brier, loco_brier_weighted)],
    "loco_fold_importance.csv")
  loco_measures = c("loco", "loco_weighted", "loco_brier", "loco_brier_weighted")
  summary = rbindlist(lapply(loco_measures, function(measure) {
    loco[, cbind(measure = measure, interval_row(get(measure))), by = .(learner, item = design)]
  }))
  summary[, rank := frank(-estimate), by = .(learner, measure)]
  fwrite_out(summary[order(measure, learner, rank)], "loco_summary.csv")
  comparisons = rbindlist(lapply(loco_measures, function(measure) {
    wide = dcast(loco, learner + fold ~ design, value.var = measure)
    rbindlist(lapply(unique(wide$learner), function(model) {
      w = wide[learner == model]
      rows = rbindlist(lapply(c("A2", "D2"), function(focal) {
        rbindlist(lapply(setdiff(items, c("A2", "D2")), function(other) {
          cbind(learner = model, measure = measure, focal = focal, other = other, interval_row(w[[focal]] - w[[other]]))
        }))
      }))
      rows[, p_holm := p.adjust(p_greater, method = "holm")]
      rows
    }))
  }))
  fwrite_out(comparisons, "loco_comparisons.csv")
  print(summary[rank <= 4][order(measure, learner, rank)])
  print(comparisons[, .(min_lower = min(lower), max_p_holm = max(p_holm), closest = other[which.min(estimate)]),
    by = .(learner, measure, focal)])
}

# ---- Part: conditional PFI on realistic inputs (conditional subgroup approach) ----

if (part == "conditional") {
  n_permutations = 20L
  # Subgroups: leaves of a tree that predicts the item from the other 26 items on the training fold; within a leaf,
  # the item is approximately independent of the others, so permuting it there keeps inputs realistic.
  leaf_of = function(tree, newdata) {
    # The predicted value of a tree whose node values are the node numbers is the leaf of each row.
    tree$frame$yval = as.numeric(rownames(tree$frame))
    as.integer(stats::predict(tree, newdata = newdata, type = "vector"))
  }
  results = parallel::mclapply(seq_len(folds), function(fold) {
    task = make_task(items)
    learner = make_learner("xgboost")
    set.seed(seed + fold)
    learner$train(task, row_ids = train_rows[[fold]])
    train = dt[train_rows[[fold]], ..items]
    test = dt[test_rows[[fold]], ..items]
    outcome = positive[test_rows[[fold]]]
    base = mean(log_loss(outcome, learner$predict_newdata(test)$prob[, "discontent"]))
    set.seed(seed + 1000L + fold)
    rbindlist(lapply(items, function(item) {
      formula = stats::as.formula(paste0("`", item, "` ~ ."))
      tree = rpart::rpart(formula, data = train, control = rpart::rpart.control(maxdepth = 4L, minbucket = 50L,
        cp = 0.001, xval = 0L))
      leaves = leaf_of(tree, test)
      groups = split(seq_len(nrow(test)), leaves)
      increases = vapply(seq_len(n_permutations), function(r) {
        permuted = copy(test)
        values = permuted[[item]]
        for (rows in groups) if (length(rows) > 1L) values[rows] = values[rows][sample.int(length(rows))]
        set(permuted, j = item, value = values)
        mean(log_loss(outcome, learner$predict_newdata(permuted)$prob[, "discontent"])) - base
      }, numeric(1L))
      data.table(fold = fold, item = item, cpfi = mean(increases), mc_se = stats::sd(increases) / sqrt(n_permutations),
        n_permutations = n_permutations, n_subgroups = length(groups))
    }))
  }, mc.cores = workers, mc.preschedule = FALSE)
  failed = vapply(results, inherits, logical(1L), what = "try-error")
  if (any(failed)) stop("Conditional PFI jobs failed: ", paste(vapply(results[failed], as.character, ""), collapse = " | "))
  per_fold = rbindlist(results)
  fwrite_out(per_fold[order(fold, -cpfi)], "conditional_fold_pfi.csv")
  means = per_fold[, .(mean_cpfi = mean(cpfi), min_fold = min(cpfi), max_fold = max(cpfi),
    median_subgroups = median(n_subgroups)), by = item][order(-mean_cpfi)]
  wide = dcast(per_fold, fold ~ item, value.var = "cpfi")
  others = setdiff(items, c("A2", "D2"))
  max_other = wide[, do.call(pmax, .SD), .SDcols = others]
  by_fold = per_fold[, {
    ordered = .SD[order(-cpfi)]
    list(top_two_are_A2_D2 = setequal(ordered$item[1:2], c("A2", "D2")),
      margin_holds = setequal(ordered$item[1:2], c("A2", "D2")) && min(ordered$cpfi[1:2]) >= 2 * ordered$cpfi[3L])
  }, by = fold]
  summary = data.table(mean_A2 = means[item == "A2", mean_cpfi], mean_D2 = means[item == "D2", mean_cpfi],
    largest_other_item = means[!item %in% c("A2", "D2")][1L, item],
    largest_other_mean = means[!item %in% c("A2", "D2")][1L, mean_cpfi],
    min_ratio_A2 = min(wide$A2 / max_other), min_ratio_D2 = min(wide$D2 / max_other),
    folds_top_two_A2_D2 = sum(by_fold$top_two_are_A2_D2), folds_margin_holds = sum(by_fold$margin_holds))
  fwrite_out(means, "conditional_item_means.csv")
  fwrite_out(summary, "conditional_summary.csv")
  print(means[1:5]); print(summary)
  write_session()
}

# ---- Part: nested check of the predictor selection ----

if (part == "selection") {
  dictionary = fread(file.path(root, "data/SHILD/archive/Danish_SHILD_dictionnary.csv"))
  excluded = c("respnb", "A1", "pervgt")
  # Candidate pool: variables marked as usable or as selected in the dictionary (the selected item E5_a is not
  # marked as usable), without identifiers, the outcome, and the survey weight.
  candidates = setdiff(unique(c(dictionary[use == 1L | study_selection == 1L, variable], items)), excluded)
  stopifnot(all(items %in% candidates))
  pool = raw[dt$respondent_id, ..candidates]
  # Same simple preprocessing for every candidate, including the 27 items, so that the comparison isolates the
  # selection: variables with at most 15 distinct values are categorical (codes, including nonresponse codes, are
  # categories); others are numeric. XGBoost treats missing values natively.
  for (column in candidates) {
    values = pool[[column]]
    if (is.character(values) || uniqueN(values[!is.na(values)]) <= 15L) set(pool, j = column, value = factor(values))
  }
  item_of_column = function(matrix_names, columns) {
    ordered = columns[order(-nchar(columns))]
    vapply(matrix_names, function(name) ordered[startsWith(name, ordered)][[1L]], character(1L))
  }
  xgb_fit = function(x, y) {
    xgboost::xgb.train(params = list(objective = "binary:logistic", max_depth = 3L, eta = 0.05, subsample = 1,
      colsample_bytree = 1, nthread = 1L, eval_metric = "logloss"), data = xgboost::xgb.DMatrix(x, label = y, missing = NA),
      nrounds = 200L, verbose = 0L)
  }
  auc = function(y, p) {
    ranks = rank(p)
    (sum(ranks[y == 1L]) - sum(y == 1L) * (sum(y == 1L) + 1) / 2) / (sum(y == 1L) * sum(y == 0L))
  }
  full_matrix = stats::model.matrix(~ . - 1, model.frame(~ ., as.data.frame(pool), na.action = na.pass))
  column_item = item_of_column(colnames(full_matrix), candidates)
  run_fold = function(fold) {
    train = train_rows[[fold]]
    test = test_rows[[fold]]
    evaluate = function(selected) {
      columns = which(column_item %in% selected)
      model = xgb_fit(full_matrix[train, columns, drop = FALSE], positive[train])
      probability = predict(model, xgboost::xgb.DMatrix(full_matrix[test, columns, drop = FALSE], missing = NA))
      c(auc = auc(positive[test], probability), log_loss = mean(log_loss(positive[test], probability)))
    }
    selected = candidates
    steps = list()
    while (length(selected) > 27L) {
      columns = which(column_item %in% selected)
      model = xgb_fit(full_matrix[train, columns, drop = FALSE], positive[train])
      gain = xgboost::xgb.importance(model = model)
      item_gain = setNames(numeric(length(selected)), selected)
      if (nrow(gain)) {
        by_item = tapply(gain$Gain, column_item[match(gain$Feature, colnames(full_matrix))], sum)
        item_gain[names(by_item)] = by_item
      }
      n_drop = min(length(selected) - 27L, max(1L, floor(0.2 * length(selected))))
      selected = names(sort(item_gain, decreasing = TRUE))[seq_len(length(selected) - n_drop)]
      steps[[length(steps) + 1L]] = length(selected)
    }
    nested = evaluate(selected)
    fixed = evaluate(items)
    list(summary = data.table(fold = fold, n_candidates = length(candidates), rfe_steps = length(steps),
      auc_nested = nested[["auc"]], log_loss_nested = nested[["log_loss"]], auc_fixed = fixed[["auc"]],
      log_loss_fixed = fixed[["log_loss"]], jaccard_with_27 = length(intersect(selected, items)) /
        length(union(selected, items)), A2_selected = "A2" %in% selected, D2_selected = "D2" %in% selected),
      selected = data.table(fold = fold, item = selected))
  }
  results = parallel::mclapply(seq_len(folds), run_fold, mc.cores = min(workers, folds), mc.preschedule = FALSE)
  failed = vapply(results, inherits, logical(1L), what = "try-error")
  if (any(failed)) stop("Selection folds failed: ", paste(vapply(results[failed], as.character, ""), collapse = " | "))
  summary = rbindlist(lapply(results, `[[`, "summary"))
  selected = rbindlist(lapply(results, `[[`, "selected"))
  fwrite_out(summary, "selection_fold_results.csv")
  fwrite_out(selected[, .(n_folds_selected = .N), by = item][order(-n_folds_selected)], "selection_item_frequency.csv")
  comparison = rbindlist(list(
    cbind(measure = "auc_nested_minus_fixed", interval_row(summary$auc_nested - summary$auc_fixed)),
    cbind(measure = "log_loss_nested_minus_fixed", interval_row(summary$log_loss_nested - summary$log_loss_fixed))
  ))
  comparison[, `:=`(mean_nested = c(mean(summary$auc_nested), mean(summary$log_loss_nested)),
    mean_fixed = c(mean(summary$auc_fixed), mean(summary$log_loss_fixed)))]
  fwrite_out(comparison, "selection_comparison.csv")
  print(summary); print(comparison)
}

# ---- Part: repeated local surrogates of the 300 sampled cases ----

if (part == "local") {
  ns = asNamespace("mlr3autoiml")
  protocol = yaml::read_yaml(file.path(root, "analysis/protocol.yml"))
  find_local = function(x) {
    if (!is.list(x)) return(NULL)
    if (!is.null(x$local_evaluation)) return(x$local_evaluation)
    for (element in x) { hit = find_local(element); if (!is.null(hit)) return(hit) }
    NULL
  }
  local = find_local(protocol)
  stopifnot(!is.null(local$primary_kernel_width))
  kernel_width = as.numeric(local$primary_kernel_width)
  n_perturb = as.integer(local$perturbations_per_case_seed)
  n_replicates = as.integer(local$perturbation_replicates)
  crossfit_folds = as.integer(local$surrogate_folds)
  result = readRDS(file.path(frozen, "shild/private/objects/shild_csdg_result_private.rds"))
  oof = result$artifacts$oof
  rm(result)
  plan = fread(file.path(frozen, "evidence/private/objects/shild_g4_case_sampling_plan_private.csv"))
  stopifnot(nrow(plan) == 300L)
  max_cases = as.integer(arg_value("max-cases", "300"))
  if (max_cases < 300L) {
    plan = plan[seq_len(max_cases)]
    output_dir = file.path(tempdir(), "claim_extensions_test")
    dir.create(output_dir, showWarnings = FALSE)
    private_dir = ""
  }
  stored = fread(file.path(frozen, "evidence/private/tables/shild_g4_local_fidelity_replicates.csv"))[
    abs(kernel_width - local$primary_kernel_width) < 1e-12 & target_scale == "response" &
      neighborhood_method == "synthetic"]
  run_case = function(index) {
    case = plan[index]
    case_map = ns$.local_fidelity_prepare_cases(oof, case$row_id, case$case_label,
      data.frame(prediction_score_decile = case$prediction_score_decile))
    model_index = case_map$iteration[[1L]]
    case_data = ns$.local_fidelity_case_data(oof, case_map)
    perturbation_seeds = .evidence_perturbation_seed(as.integer(local$perturbation_seed_base),
      rep(case$case_label, n_replicates), seq_len(n_replicates), as.integer(local$perturbation_seed_stride))
    crossfit_seeds = .evidence_perturbation_seed(as.integer(local$crossfit_seed_base),
      rep(case$case_label, n_replicates), seq_len(n_replicates), as.integer(local$perturbation_seed_stride))
    rbindlist(lapply(seq_len(n_replicates), function(replicate) {
      neighborhood = ns$.make_local_neighborhood(case_data$case, case_data$background, n_perturb,
        perturbation_seeds[[replicate]])
      neighborhood = rbindlist(list(case_data$case, neighborhood), fill = TRUE)
      prediction = ns$.predict_newdata(oof$models[[model_index]], neighborhood, task = oof$task)
      probability = ns$.prediction_vector(prediction, oof$task$task_type, oof$task$positive)
      distance = ns$.local_distance(neighborhood, case_data$case, case_data$background)
      frame = data.frame(neighborhood, check.names = TRUE)
      design = stats::model.matrix(~ ., data = frame)
      assign = attr(design, "assign")
      term_item = c("(Intercept)", names(frame))[assign + 1L]
      weights = exp(pmax(-(distance^2) / kernel_width^2, log(.Machine$double.xmin)))
      evaluation = seq.int(2L, nrow(neighborhood))
      fold_id = c(NA_integer_, ns$.local_crossfit_folds(length(evaluation), crossfit_seeds[[replicate]],
        folds = crossfit_folds))
      rbindlist(lapply(c("response", "link"), function(scale) {
        target = ns$.local_prediction_scale(probability, oof$task$task_type, scale)
        crossfit = ns$.local_crossfit_surrogate(design, target, weights, fold_id)
        metrics = ns$.local_weighted_metrics(target[evaluation], crossfit$predicted[evaluation], weights[evaluation])
        fit = ns$.local_weighted_ridge(design, target, weights)
        # Contribution of each item to the surrogate prediction, centered at the weighted mean of the neighborhood;
        # its magnitude is the weighted root mean square over the neighborhood (comparable across items).
        w = weights[evaluation] / sum(weights[evaluation])
        x = design[evaluation, , drop = FALSE]
        centered = sweep(x, 2L, colSums(x * w))
        terms = sweep(centered, 2L, fit$coefficients, "*")
        magnitude = vapply(names(frame), function(item) {
          contribution = rowSums(terms[, term_item == item, drop = FALSE])
          sqrt(sum(w * contribution^2))
        }, numeric(1L))
        top = names(sort(magnitude, decreasing = TRUE))[1:2]
        data.table(case_label = case$case_label, fold = case$fold, decile = case$prediction_score_decile,
          replicate = replicate, scale = scale, weighted_r2 = metrics$r2, weighted_rmse = metrics$rmse,
          top1 = top[[1L]], top2 = top[[2L]], top_pair = paste(sort(top), collapse = "+"))
      }))
    }))
  }
  # Forked workers cannot use the loaded XGBoost models safely, so the cases are split into slices that run as
  # separate processes (--slice=k --slices=n); the call with --combine pools the slices.
  slices = as.integer(arg_value("slices", "1"))
  slice = as.integer(arg_value("slice", "1"))
  combine = "--combine" %in% args
  slice_dir = if (nzchar(private_dir)) private_dir else file.path(tempdir(), "shild_local_slices")
  dir.create(slice_dir, recursive = TRUE, showWarnings = FALSE)
  if (!combine) {
    indices = which((seq_len(nrow(plan)) - 1L) %% slices == slice - 1L)
    results = lapply(indices, function(index) tryCatch(run_case(index), error = function(e) e))
    failed = vapply(results, function(value) inherits(value, "error"), logical(1L))
    if (any(failed)) stop("Local cases failed: ", paste(unique(vapply(results[failed], conditionMessage, "")), collapse = " | "))
    fwrite(rbindlist(results), file.path(slice_dir, sprintf("shild_local_slice_%02d_of_%02d.csv", slice, slices)))
    if (slices > 1L) quit(save = "no")
  }
  replicates = rbindlist(lapply(list.files(slice_dir, pattern = sprintf("^shild_local_slice_.*_of_%02d[.]csv$", slices),
    full.names = TRUE), fread))
  stopifnot(uniqueN(replicates$case_label) == nrow(plan))
  identity = merge(replicates[scale == "response", .(case_label, replicate, weighted_r2)],
    stored[, .(case_label, replicate = perturbation_replicate, stored_r2 = weighted_r2)], by = c("case_label", "replicate"))
  identity_check = data.table(n_compared = nrow(identity), max_abs_difference = max(abs(identity$weighted_r2 - identity$stored_r2)))
  fwrite_out(identity_check, "local_identity_check.csv")
  cases = replicates[, {
    counts = table(top_pair)
    modal = names(counts)[which.max(counts)]
    .(modal_pair = modal, modal_count = max(counts), median_r2 = median(weighted_r2),
      median_rmse = median(weighted_rmse))
  }, by = .(case_label, fold, decile, scale)]
  if (nzchar(private_dir)) {
    dir.create(private_dir, recursive = TRUE, showWarnings = FALSE)
    fwrite(replicates, file.path(private_dir, "shild_local_replicates_top_items.csv"))
    fwrite(cases, file.path(private_dir, "shild_local_cases_top_items.csv"))
  }
  by_decile = cases[, .(n_cases = .N, share_at_least_19_of_20 = mean(modal_count >= 19L),
    median_modal_count = median(modal_count), min_modal_count = min(modal_count),
    share_median_r2_at_least_80 = mean(median_r2 >= 0.80), median_r2 = median(median_r2)), by = .(scale, decile)]
  overall = cases[, .(n_cases = .N, share_at_least_19_of_20 = mean(modal_count >= 19L),
    median_modal_count = median(modal_count), share_median_r2_at_least_80 = mean(median_r2 >= 0.80)), by = scale]
  # Most frequent modal pairs in the top decile (aggregated over at least 10 cases per row is not guaranteed for
  # pairs, so only the number of distinct pairs is exported).
  top_decile = cases[decile == 10L, .(n_cases = .N, n_distinct_modal_pairs = uniqueN(modal_pair),
    share_at_least_19_of_20 = mean(modal_count >= 19L), share_median_r2_at_least_80 = mean(median_r2 >= 0.80),
    share_both = mean(modal_count >= 19L & median_r2 >= 0.80)), by = scale]
  fwrite_out(by_decile[order(scale, decile)], "local_top_items_by_decile.csv")
  fwrite_out(overall, "local_top_items_overall.csv")
  fwrite_out(top_decile, "local_top_items_top_decile.csv")
  print(identity_check); print(overall); print(top_decile); print(by_decile[order(scale, decile)])
}
write_session()
