# PISA: how far the fixed learner settings are from tuned settings (PV1, exploratory sample).
#
# XGBoost and random forest are tuned by random search with school-grouped inner cross-validation inside each
# of the five outer training folds of the primary analysis (nested resampling). The held-out RMSE of the tuned
# learners is compared with that of the fixed settings used throughout the article. Only aggregates are written.
# Usage (from the repository root): Rscript analysis/pisa/tuning_check.R --workers=5

suppressPackageStartupMessages({
  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  library(paradox)
  library(mlr3autoiml)
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
threads = as.integer(arg_value("threads", "2"))
root = normalizePath(arg_value("root", "."), mustWork = TRUE)
output_dir = file.path(root, "analysis/pisa/results/ses_reanalysis/tuning_check")
if (quick) output_dir = paste0(output_dir, "_quick")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
seed = 20260714L
n_evals = c(xgboost = if (quick) 2L else 30L, random_forest = if (quick) 2L else 20L)

# ---- Sample, task, and outer splits as in analysis/pisa/analysis.R ----

id_columns = c("CNT", "CNTSCHID", "CNTSTUID")
all_pv = paste0("PV", 1:10, "READ")
categorical = c("ST004D01T", "ST001D01T", "IMMIG", "LANGN", "ISCEDD", "ISCEDL", "ISCEDO")
source_data = fread(file.path(root, "data/PISA/pisa2018_reading_compactv2.csv"), na.strings = c("", "NA", "NaN"))
source_data[LANGN == 999, LANGN := NA_integer_]
set.seed(seed)
dt = source_data[source_data[, .I[sample.int(.N, if (quick) 100L else 1500L)], by = CNT]$V1]
setorder(dt, CNT, CNTSTUID)
predictors = setdiff(names(dt), c(id_columns, all_pv))
for (column in intersect(categorical, predictors)) set(dt, j = column, value = factor(dt[[column]]))
dt[, audit_country := factor(CNT)]
dt[, audit_school := paste(CNT, CNTSCHID, sep = "::")]

backend = dt[, c(predictors, "audit_country", "audit_school"), with = FALSE]
backend[, reading := dt$PV1READ]
task = TaskRegr$new("pisa_PV1READ", backend = backend, target = "reading")
task$set_col_roles("audit_country", remove_from = "feature")
task$set_col_roles("audit_school", roles = "group")
grouped = csdg_grouped_resampling(task, group = dt$audit_school, folds = if (quick) 3L else 5L, repeats = 1L,
  strata = dt$audit_country, seed = seed)
outer_train = lapply(seq_len(grouped$resampling$iters), grouped$resampling$train_set)
outer_test = lapply(seq_len(grouped$resampling$iters), grouped$resampling$test_set)

pipeline_learner = function(base_learner, id) {
  numeric_selector = selector_type(c("integer", "numeric"))
  factor_selector = selector_type(c("factor", "ordered", "character"))
  graph = po("imputemedian", affect_columns = numeric_selector) %>>%
    po("imputemode", affect_columns = factor_selector) %>>%
    po("encode", method = "one-hot", affect_columns = factor_selector) %>>%
    po("removeconstants") %>>% po("learner", learner = base_learner)
  learner = as_learner(graph)
  learner$id = id
  learner
}
fixed = list(
  xgboost = pipeline_learner(lrn("regr.xgboost", nrounds = if (quick) 15L else 150L, max_depth = 3L, eta = 0.05,
    base_score = 500, nthread = threads, subsample = 1, colsample_bytree = 1), "xgboost"),
  random_forest = pipeline_learner(lrn("regr.ranger", num.trees = if (quick) 50L else 300L, min.node.size = 20L,
    num.threads = threads, seed = seed), "random_forest")
)
tunable = list(
  xgboost = pipeline_learner(lrn("regr.xgboost", base_score = 500, nthread = threads,
    nrounds = to_tune(50L, if (quick) 60L else 1000L), eta = to_tune(0.01, 0.3, logscale = TRUE),
    max_depth = to_tune(2L, 8L), min_child_weight = to_tune(1, 50, logscale = TRUE),
    subsample = to_tune(0.5, 1), colsample_bytree = to_tune(0.5, 1)), "xgboost_tuned"),
  random_forest = pipeline_learner(lrn("regr.ranger", num.trees = if (quick) 50L else 300L, num.threads = threads,
    seed = seed, mtry = to_tune(3L, 40L), min.node.size = to_tune(5L, 100L, logscale = TRUE),
    sample.fraction = to_tune(0.5, 1)), "random_forest_tuned")
)

# ---- Nested evaluation, parallel over outer folds ----

jobs = CJ(learner = names(fixed), fold = seq_along(outer_train), version = c("fixed", "tuned"), sorted = FALSE)
run_job = function(index) {
  job = jobs[index]
  set.seed(seed + index)
  train = outer_train[[job$fold]]
  test = outer_test[[job$fold]]
  learner = if (job$version == "fixed") fixed[[job$learner]]$clone(deep = TRUE) else {
    AutoTuner$new(
      tuner = tnr("random_search"),
      learner = tunable[[job$learner]]$clone(deep = TRUE),
      resampling = rsmp("cv", folds = 3L),
      measure = msr("regr.rmse"),
      terminator = trm("evals", n_evals = n_evals[[job$learner]])
    )
  }
  learner$train(task, row_ids = train)
  prediction = learner$predict(task, row_ids = test)
  chosen = if (job$version == "tuned") {
    values = learner$tuning_result$learner_param_vals[[1L]]
    keep = grepl("nrounds|eta|max_depth|min_child_weight|subsample|colsample|mtry|min.node.size|sample.fraction",
      names(values))
    paste(sprintf("%s=%s", sub("^.*\\.", "", names(values)[keep]), signif(unlist(values[keep]), 3)), collapse = "; ")
  } else NA_character_
  cbind(job, rmse = prediction$score(msr("regr.rmse")), n_test = length(test), chosen = chosen)
}
started = Sys.time()
results = rbindlist(parallel::mclapply(seq_len(nrow(jobs)), run_job, mc.cores = workers, mc.preschedule = FALSE))
message(sprintf("Finished in %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))
fwrite(results, file.path(output_dir, "tuning_check_folds.csv"))
summary = results[, .(mean_rmse = mean(rmse), sd_rmse = sd(rmse)), by = .(learner, version)]
difference = dcast(results, learner + fold ~ version, value.var = "rmse")[, .(
  mean_tuned_minus_fixed = mean(tuned - fixed), min_difference = min(tuned - fixed),
  max_difference = max(tuned - fixed)), by = learner]
fwrite(merge(summary, difference, by = "learner"), file.path(output_dir, "tuning_check_summary.csv"))
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
