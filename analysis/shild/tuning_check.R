# SHILD: how far the fixed learner settings are from tuned settings, and whether statement (a) holds for a tuned
# XGBoost.
#
# XGBoost and random forest are tuned by random search with stratified inner cross-validation inside each of the
# 10 outer training folds of the primary analysis (nested resampling). The held-out log loss, AUC, and the number of
# held-out respondents whose observed outcome received probability 0 are compared with those of the fixed settings
# used throughout the article. For the fixed and the tuned XGBoost, held-out PFI (marginal permutation, 20
# permutations per item and fold, increase in log loss) is computed on the same folds, and the criterion of
# statement (a) is evaluated with the two leading items selected anew in each fold. Only aggregates are written.
# Usage (from the repository root): Rscript analysis/shild/tuning_check.R --workers=10

suppressPackageStartupMessages({
  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  library(paradox)
})
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

args = commandArgs(trailingOnly = TRUE)
arg_value = function(name, default = NULL) {
  hit = grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}
quick = "--quick" %in% args
workers = as.integer(arg_value("workers", "1"))
root = normalizePath(arg_value("root", "."), mustWork = TRUE)
output_dir = file.path(root, "analysis/shild/results/tuning_check")
if (quick) output_dir = paste0(output_dir, "_quick")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
seed = 20260714L
folds = 10L
n_evals = c(xgboost = if (quick) 2L else 30L, random_forest = if (quick) 2L else 20L)
n_permutations = if (quick) 2L else 20L

source(file.path(root, "analysis/shared/study_helpers.R"), local = FALSE)
source(file.path(root, "analysis/shild/prepare_data.R"), local = FALSE)

# ---- Analytic sample, task, and the primary folds (as in analysis/shild/claim_extensions.R) ----

raw = fread(file.path(root, "data/SHILD/archive/Danish_SHILD_draft_data.csv"), na.strings = c("", "NA", "NaN"))
preparation = .prepare_shild_raw(raw)
dt = preparation$data
items = preparation$predictors
for (column in items[vapply(dt[, ..items], is.character, logical(1L))]) set(dt, j = column, value = factor(dt[[column]]))
stopifnot(nrow(dt) == 16971L)
task = TaskClassif$new("shild", backend = dt[, c(items, "life_satisfaction"), with = FALSE],
  target = "life_satisfaction", positive = "discontent")
task$set_col_roles("life_satisfaction", add_to = "stratum")
split = .stratified_resampling(task, dt$life_satisfaction, folds, seed)
fold_of_row = split$assignment$fold
stored_folds = fread(file.path(root, "paper_artifacts/current/shild/private/tables/shild_fold_assignment.csv"))
stopifnot(identical(as.integer(stored_folds$fold), as.integer(fold_of_row)))
train_rows = lapply(seq_len(folds), function(fold) which(fold_of_row != fold))
test_rows = lapply(seq_len(folds), function(fold) which(fold_of_row == fold))
positive = as.integer(dt$life_satisfaction == "discontent")
log_loss = function(truth_positive, probability) {
  probability = pmin(pmax(probability, 1e-15), 1 - 1e-15)
  -(truth_positive * log(probability) + (1 - truth_positive) * log(1 - probability))
}

# ---- Fixed settings (as in analysis/shild/analysis.R) and search spaces ----

fixed = function(learner_id) switch(learner_id,
  xgboost = .make_pipeline_learner(lrn("classif.xgboost", predict_type = "prob", nrounds = if (quick) 20L else 200L,
    max_depth = 3L, eta = 0.05, nthread = 1L, subsample = 1, colsample_bytree = 1), "shild_xgboost"),
  random_forest = .make_pipeline_learner(lrn("classif.ranger", predict_type = "prob", num.trees = 400L,
    min.node.size = 5L, num.threads = 1L, seed = seed), "shild_ranger"))
tunable = function(learner_id) switch(learner_id,
  xgboost = .make_pipeline_learner(lrn("classif.xgboost", predict_type = "prob", nthread = 1L,
    nrounds = to_tune(50L, if (quick) 60L else 1000L), eta = to_tune(0.01, 0.3, logscale = TRUE),
    max_depth = to_tune(2L, 8L), min_child_weight = to_tune(1, 50, logscale = TRUE),
    subsample = to_tune(0.5, 1), colsample_bytree = to_tune(0.5, 1)), "shild_xgboost_tuned"),
  random_forest = .make_pipeline_learner(lrn("classif.ranger", predict_type = "prob", num.trees = 400L,
    num.threads = 1L, seed = seed, mtry = to_tune(p_int(3L, 40L)),
    min.node.size = to_tune(p_int(5L, 100L, logscale = TRUE)), sample.fraction = to_tune(0.5, 1)), "shild_ranger_tuned"))

# Held-out PFI: increase in mean log loss after permuting one item in the held-out fold.
held_out_pfi = function(learner, fold, job_seed) {
  rows = test_rows[[fold]]
  newdata = dt[rows, ..items]
  outcome = positive[rows]
  base = mean(log_loss(outcome, learner$predict_newdata(newdata)$prob[, "discontent"]))
  set.seed(job_seed)
  rbindlist(lapply(items, function(item) {
    increases = vapply(seq_len(n_permutations), function(r) {
      permuted = copy(newdata)
      set(permuted, j = item, value = permuted[[item]][sample.int(nrow(permuted))])
      mean(log_loss(outcome, learner$predict_newdata(permuted)$prob[, "discontent"])) - base
    }, numeric(1L))
    data.table(fold = fold, item = item, pfi = mean(increases), mc_se = stats::sd(increases) / sqrt(n_permutations),
      n_permutations = n_permutations)
  }))
}

# ---- Nested evaluation, parallel over outer folds ----

jobs = CJ(learner = c("xgboost", "random_forest"), fold = seq_len(folds), version = c("fixed", "tuned"), sorted = FALSE)
run_job = function(index) {
  job = jobs[index]
  set.seed(seed + index)
  learner = if (job$version == "fixed") fixed(job$learner) else {
    AutoTuner$new(tuner = tnr("random_search"), learner = tunable(job$learner), resampling = rsmp("cv", folds = 3L),
      measure = msr("classif.logloss"), terminator = trm("evals", n_evals = n_evals[[job$learner]]))
  }
  learner$train(task, row_ids = train_rows[[job$fold]])
  rows = test_rows[[job$fold]]
  probability = learner$predict(task, row_ids = rows)$prob[, "discontent"]
  outcome = positive[rows]
  chosen = if (job$version == "tuned") {
    values = learner$tuning_result$learner_param_vals[[1L]]
    keep = grepl("nrounds|eta|max_depth|min_child_weight|subsample|colsample|mtry|min.node.size|sample.fraction",
      names(values))
    paste(sprintf("%s=%s", sub("^.*\\.", "", names(values)[keep]), signif(unlist(values[keep]), 3)), collapse = "; ")
  } else NA_character_
  prevalence = mean(positive[train_rows[[job$fold]]])
  performance = cbind(job, n_test = length(rows), log_loss = mean(log_loss(outcome, probability)),
    featureless_log_loss = mean(log_loss(outcome, rep(prevalence, length(rows)))),
    auc = mlr3measures::auc(factor(outcome, levels = c(1L, 0L)), probability, positive = "1"),
    n_outcome_at_bound = sum(outcome == 1L & probability <= 1e-15), chosen = chosen)
  pfi = if (job$learner == "xgboost") cbind(version = job$version, held_out_pfi(learner, job$fold, seed + 100L * index))
  list(performance = performance, pfi = pfi)
}
started = Sys.time()
results = parallel::mclapply(seq_len(nrow(jobs)), run_job, mc.cores = workers, mc.preschedule = FALSE)
failed = vapply(results, inherits, logical(1L), what = "try-error")
if (any(failed)) stop("Jobs failed: ", paste(vapply(results[failed], as.character, ""), collapse = " | "))
message(sprintf("Finished in %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))
performance = rbindlist(lapply(results, `[[`, "performance"))
pfi = rbindlist(lapply(results, `[[`, "pfi"))
fwrite(performance, file.path(output_dir, "tuning_check_folds.csv"))
fwrite(pfi[order(version, fold, -pfi)], file.path(output_dir, "tuning_check_pfi_fold.csv"))

summary = performance[, .(mean_log_loss = mean(log_loss), mean_auc = mean(auc),
  mean_featureless_log_loss = mean(featureless_log_loss), n_outcome_at_bound = sum(n_outcome_at_bound)),
  by = .(learner, version)]
difference = dcast(performance, learner + fold ~ version, value.var = "log_loss")[, .(
  mean_tuned_minus_fixed = mean(tuned - fixed), min_difference = min(tuned - fixed),
  max_difference = max(tuned - fixed)), by = learner]
fwrite(merge(summary, difference, by = "learner"), file.path(output_dir, "tuning_check_summary.csv"))

# ---- Statement (a) for the fixed and the tuned XGBoost ----

statement_a = rbindlist(lapply(c("fixed", "tuned"), function(v) {
  p = pfi[version == v]
  improvement = performance[learner == "xgboost" & version == v, mean(featureless_log_loss - log_loss)]
  means = p[, .(mean_pfi = mean(pfi)), by = item][order(-mean_pfi)]
  others_mean = means[!item %in% c("A2", "D2"), max(mean_pfi)]
  per_fold = p[, {
    ordered = .SD[order(-pfi)]
    top_two = ordered$item[1:2]
    third = ordered$pfi[3L]
    list(top_two_are_A2_D2 = setequal(top_two, c("A2", "D2")),
      margin_holds = setequal(top_two, c("A2", "D2")) && min(ordered$pfi[1:2]) >= 2 * third,
      ratio_A2 = pfi[item == "A2"] / max(pfi[!item %in% c("A2", "D2")]),
      ratio_D2 = pfi[item == "D2"] / max(pfi[!item %in% c("A2", "D2")]))
  }, by = fold]
  data.table(version = v, mean_pfi_A2 = means[item == "A2", mean_pfi], mean_pfi_D2 = means[item == "D2", mean_pfi],
    largest_other_item = means[!item %in% c("A2", "D2")][1L, item], largest_other_mean_pfi = others_mean,
    improvement_over_featureless = improvement,
    share_A2 = means[item == "A2", mean_pfi] / improvement, share_D2 = means[item == "D2", mean_pfi] / improvement,
    folds_top_two_A2_D2 = sum(per_fold$top_two_are_A2_D2), folds_margin_holds = sum(per_fold$margin_holds),
    min_ratio_A2 = min(per_fold$ratio_A2), min_ratio_D2 = min(per_fold$ratio_D2))
}))
fwrite(statement_a, file.path(output_dir, "tuning_check_statement_a.csv"))
print(merge(summary, difference, by = "learner")); print(statement_a)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
