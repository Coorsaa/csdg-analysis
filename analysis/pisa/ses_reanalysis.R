# PISA: reliance on socioeconomic indices across models, plausible values, and analysis designs.
#
# Stage "exploratory" uses the equal-country sample of the primary PISA analysis (seed 20260714) with the
# same school-grouped, country-stratified folds, learners, and permutation scheme. It adds
#   * grouped PFI for the complete family of socioeconomic indices and for the home-possession family,
#   * a ridge regression whose penalty is chosen by inner cross-validation (as in SHILD),
#   * refit-based designs: leaving out a block of predictors (LOCO) and keeping either the composite index or
#     its components, so that no permutation combines overlapping indices,
#   * accumulated local effects (ALE) of assessing credibility and ESCS on held-out folds.
# Stage "confirmatory" draws 1,500 further students per country who were not in the exploratory sample and
# assesses the claims fixed in analysis/pisa/confirmatory_claims.md before this stage was run.
#
# Only aggregate tables are written. Usage (from the repository root):
#   Rscript analysis/pisa/ses_reanalysis.R --stage=exploratory --workers=12
#   Rscript analysis/pisa/ses_reanalysis.R --stage=confirmatory --workers=12
#   Rscript analysis/pisa/ses_reanalysis.R --stage=exploratory --quick   # smoke test

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
stage = arg_value("stage", "exploratory")
stopifnot(stage %in% c("exploratory", "confirmatory"))
quick = "--quick" %in% args
workers = as.integer(arg_value("workers", "1"))
root = normalizePath(arg_value("root", "."), mustWork = TRUE)
output_dir = arg_value("output-dir", file.path(root, "analysis/pisa/results/ses_reanalysis", stage))
if (quick) output_dir = paste0(output_dir, "_quick")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

settings = list(
  exploratory_seed = 20260714L,
  confirmatory_seed = 20260929L,
  n_per_country = if (quick) 100L else 1500L,
  folds = if (quick) 3L else 5L,
  pvs = if (quick) paste0("PV", 1:2, "READ") else paste0("PV", 1:10, "READ"),
  xgb_rounds = if (quick) 15L else 150L,
  forest_trees = if (quick) 50L else 300L,
  pfi_repetitions = if (quick) 2L else 10L,
  ale_intervals = 20L
)
seed = if (stage == "exploratory") settings$exploratory_seed else settings$confirmatory_seed

# ---- Data: the primary PISA sample or a disjoint confirmatory sample ----

id_columns = c("CNT", "CNTSCHID", "CNTSTUID")
all_pv = paste0("PV", 1:10, "READ")
categorical = c("ST004D01T", "ST001D01T", "IMMIG", "LANGN", "ISCEDD", "ISCEDL", "ISCEDO")
source_data = fread(file.path(root, "data/PISA/pisa2018_reading_compactv2.csv"), na.strings = c("", "NA", "NaN"))
stopifnot(uniqueN(source_data$CNT) == 32L, !anyNA(source_data[, ..all_pv]))
source_data[LANGN == 999, LANGN := NA_integer_]
manifest = fread(file.path(root, "data/PISA/pisa2018_reading_compactv2_manifest.csv"))

draw_sample = function(data, n, sampling_seed) {
  set.seed(sampling_seed)
  index = data[, .I[sample.int(.N, n, replace = FALSE)], by = CNT]$V1
  data[index]
}
# The exploratory sample is drawn exactly as in analysis/pisa/analysis.R (primary n = 1,500 per country).
exploratory = draw_sample(source_data, if (quick) settings$n_per_country else 1500L, settings$exploratory_seed)
if (stage == "exploratory") {
  dt = copy(exploratory)
} else {
  remaining = source_data[!exploratory, on = c("CNT", "CNTSTUID")]
  stopifnot(nrow(remaining) == nrow(source_data) - nrow(exploratory))
  dt = draw_sample(remaining, settings$n_per_country, settings$confirmatory_seed)
  stopifnot(!nrow(dt[exploratory, on = c("CNT", "CNTSTUID"), nomatch = 0L]))
}
setorder(dt, CNT, CNTSTUID)
predictors = setdiff(names(dt), c(id_columns, all_pv))
stopifnot(length(predictors) == 41L)
for (column in intersect(categorical, predictors)) set(dt, j = column, value = factor(dt[[column]]))
dt[, audit_country := factor(CNT)]
dt[, audit_school := paste(CNT, CNTSCHID, sep = "::")]
school_level = intersect(predictors, manifest[level == "School", variable_out])
stopifnot(identical(sort(school_level), sort(grep("^SCH_", predictors, value = TRUE))))

sample_description = data.table(
  stage = stage, n_students = nrow(dt), n_countries = uniqueN(dt$CNT), n_schools = uniqueN(dt$audit_school),
  n_schools_shared_with_exploratory = if (stage == "exploratory") NA_integer_ else
    length(intersect(unique(dt$audit_school), unique(paste(exploratory$CNT, exploratory$CNTSCHID, sep = "::")))),
  seed = seed
)
fwrite(sample_description, file.path(output_dir, "sample_description.csv"))

# ---- Predictor blocks and designs ----

ses_family = c("ESCS", "HISEI", "PARED", "HOMEPOS", "WEALTH", "HEDRES", "CULTPOSS", "ICTRES")
homepos_family = c("HOMEPOS", "WEALTH", "HEDRES", "CULTPOSS", "ICTRES")
partial_block = c("ESCS", "HISEI", "HOMEPOS")
components = c("HISEI", "PARED", "WEALTH", "HEDRES", "CULTPOSS", "ICTRES")
stopifnot(all(ses_family %in% predictors))

designs = list(
  full = predictors,
  without_ses_family = setdiff(predictors, ses_family),
  without_partial_block = setdiff(predictors, partial_block),
  composite_only = setdiff(predictors, setdiff(ses_family, "ESCS")),
  components_only = setdiff(predictors, c("ESCS", "HOMEPOS"))
)
# Individual groups first and in the order of the primary analysis, so that their permutation seeds, which
# depend on the group index, are the same as there; the added blocks come last.
full_groups = c(setNames(lapply(predictors, identity), predictors),
  list(socioeconomic_joint_block = partial_block, ses_family = ses_family, homepos_family = homepos_family))
design_groups = list(
  composite_only = list(ESCS = "ESCS", METASPAM = "METASPAM"),
  components_only = list(ses_components = components, METASPAM = "METASPAM")
)

# ---- Learners (as in analysis/pisa/analysis.R; ridge_cv chooses its penalty by inner cross-validation) ----

pipeline_learner = function(base_learner, id, scale_numeric = FALSE) {
  numeric_selector = selector_type(c("integer", "numeric"))
  factor_selector = selector_type(c("factor", "ordered", "character"))
  operators = list(
    po("imputemedian", id = paste0(id, "_impute_numeric"), affect_columns = numeric_selector),
    po("imputemode", id = paste0(id, "_impute_factor"), affect_columns = factor_selector),
    po("encode", id = paste0(id, "_encode"), method = "one-hot", affect_columns = factor_selector),
    po("removeconstants", id = paste0(id, "_removeconstants"))
  )
  if (scale_numeric) operators[[length(operators) + 1L]] =
    po("scale", id = paste0(id, "_scale"), affect_columns = numeric_selector)
  operators[[length(operators) + 1L]] = po("learner", id = paste0(id, "_learner"), learner = base_learner)
  learner = as_learner(Reduce(function(left, right) mlr3pipelines::`%>>%`(left, right), operators))
  learner$id = id
  learner
}
make_learner = function(learner_id) {
  switch(learner_id,
    xgboost = pipeline_learner(lrn("regr.xgboost", nrounds = settings$xgb_rounds, max_depth = 3L, eta = 0.05,
      base_score = 500, nthread = 1L, subsample = 1, colsample_bytree = 1), "pisa_xgboost"),
    random_forest = pipeline_learner(lrn("regr.ranger", num.trees = settings$forest_trees, min.node.size = 20L,
      num.threads = 1L, seed = settings$exploratory_seed), "pisa_ranger"),
    ridge_fixed = pipeline_learner(lrn("regr.glmnet", alpha = 0, lambda = 0.10), "pisa_ridge", scale_numeric = TRUE),
    ridge_cv = pipeline_learner(lrn("regr.cv_glmnet", alpha = 0, nfolds = 5L, s = "lambda.1se"),
      "pisa_ridge_cv", scale_numeric = TRUE),
    # Median of the configurations chosen by nested tuning in analysis/pisa/tuning_check.R (PV1).
    xgboost_tuned = pipeline_learner(lrn("regr.xgboost", nrounds = if (quick) 15L else 856L, max_depth = 3L,
      eta = 0.0828, min_child_weight = 16.4, subsample = 0.773, colsample_bytree = 0.684, base_score = 500,
      nthread = 1L), "pisa_xgboost_tuned"),
    featureless = { learner = lrn("regr.featureless"); learner$id = "pisa_featureless"; learner }
  )
}
learner_ids = c("xgboost", "random_forest", "ridge_fixed", "ridge_cv", "featureless")
# Seeds of the primary analysis: resampling seed offsets by candidate position and PFI seed offsets.
resample_seed = function(learner_id) seed + switch(learner_id, xgboost = 0L, ridge_fixed = 200000L,
  random_forest = 300000L, featureless = 500000L, ridge_cv = 600000L, xgboost_tuned = 700000L)
pfi_seed = function(learner_id, pv_index) seed + switch(learner_id,
  xgboost = 1000000L, random_forest = 2400000L, ridge_fixed = 2500000L, ridge_cv = 2600000L,
  xgboost_tuned = 2700000L) + pv_index * 10000L

make_task = function(pv, features) {
  backend = dt[, c(features, "audit_country", "audit_school"), with = FALSE]
  backend[, reading := dt[[pv]]]
  task = TaskRegr$new(paste0("pisa_", pv), backend = backend, target = "reading")
  task$set_col_roles("audit_country", remove_from = "feature")
  task$set_col_roles("audit_school", remove_from = "feature")
  stopifnot(setequal(task$feature_names, features))
  task
}
reference_task = make_task(settings$pvs[[1L]], predictors)
grouped = csdg_grouped_resampling(reference_task, group = dt$audit_school, folds = settings$folds, repeats = 1L,
  strata = dt$audit_country, seed = seed)
split_source = grouped$resampling
train_sets = lapply(seq_len(split_source$iters), split_source$train_set)
test_sets = lapply(seq_len(split_source$iters), split_source$test_set)
copy_splits = function(task) {
  resampling = rsmp("custom")
  resampling$instantiate(task, train_sets = train_sets, test_sets = test_sets)
  resampling
}

# ---- Helpers ----

fold_rmse = function(result) {
  predictions = as.data.table(result$predictions)
  truth_column = intersect(c("truth", "target"), names(predictions))[[1L]]
  predictions[, .(rmse = sqrt(mean((get(truth_column) - response)^2)), n = .N), by = .(fold = iteration)]
}
pooled_rmse = function(result) {
  predictions = as.data.table(result$predictions)
  truth_column = intersect(c("truth", "target"), names(predictions))[[1L]]
  sqrt(mean((predictions[[truth_column]] - predictions$response)^2))
}
glmnet_coefficients = function(model, learner_id) {
  fitted = model$graph_model$pipeops[[paste0(if (learner_id == "ridge_cv") "pisa_ridge_cv" else "pisa_ridge",
    "_learner")]]$learner_model$model
  if (inherits(fitted, "cv.glmnet")) {
    coefficients = stats::coef(fitted, s = "lambda.1se")
    lambda = fitted$lambda.1se
  } else {
    coefficients = stats::coef(fitted)
    lambda = fitted$lambda[[1L]]
  }
  data.table(term = rownames(coefficients), coefficient = as.numeric(coefficients[, 1L]), lambda = lambda)
}
# First-order ALE of one numeric predictor on held-out data (quantile intervals; observations with a missing
# value of the predictor are left out; curve centered at its mean over these observations).
ale_curve = function(model, data, feature, intervals) {
  values = data[[feature]]
  keep = !is.na(values)
  data = data[keep]
  values = values[keep]
  breaks = unique(stats::quantile(values, probs = seq(0, 1, length.out = intervals + 1L), names = FALSE, type = 1))
  interval = pmax(1L, findInterval(values, breaks, rightmost.closed = TRUE, left.open = TRUE))
  lower = copy(data); set(lower, j = feature, value = breaks[interval])
  upper = copy(data); set(upper, j = feature, value = breaks[interval + 1L])
  difference = model$predict_newdata(upper)$response - model$predict_newdata(lower)$response
  local = data.table(interval, difference)[, .(effect = mean(difference), n = .N), keyby = interval]
  local = local[data.table(interval = seq_len(length(breaks) - 1L)), on = "interval"]
  local[is.na(effect), `:=`(effect = 0, n = 0L)]
  accumulated = c(0, cumsum(local$effect))
  centered = accumulated - sum(((accumulated[-1L] + accumulated[-length(accumulated)]) / 2) * local$n) / sum(local$n)
  quantile_value = function(probability) stats::approx(breaks, centered, xout = stats::quantile(values, probability,
    names = FALSE, type = 7), rule = 2)$y
  list(curve = data.table(grid = breaks, ale = centered),
    q10_to_q90 = quantile_value(0.90) - quantile_value(0.10),
    nondecreasing_share = mean(diff(centered) >= 0))
}

# ---- One job: one learner, plausible value, and design ----

run_job = function(learner_id, pv, design_id) {
  pv_index = match(pv, all_pv)
  task = make_task(pv, designs[[design_id]])
  learner = make_learner(learner_id)
  result = csdg_resample(task, learner, copy_splits(task), measures = msrs("regr.rmse"),
    store_models = design_id %in% c("full", names(design_groups)) || grepl("^ridge", learner_id),
    seed = resample_seed(learner_id))
  key = data.table(stage = stage, learner = learner_id, outcome_id = pv, design = design_id)
  out = list(
    fold_rmse = cbind(key, fold_rmse(result)),
    pooled_rmse = cbind(key, pooled_rmse = pooled_rmse(result)),
    predictions = if (design_id == "full") cbind(key[rep(1L, nrow(as.data.table(result$predictions)))],
      as.data.table(result$predictions)[, .(row_id, fold = iteration, response)]) else NULL
  )
  if (learner_id == "featureless") return(out)
  groups = if (design_id == "full") full_groups else design_groups[[design_id]]
  if (!is.null(groups)) {
    pfi = csdg_fold_pfi(result, task = task, feature_groups = groups, loss = "rmse",
      repetitions = settings$pfi_repetitions, strata = dt$audit_country, cluster = dt$audit_school,
      cluster_level_groups = intersect(school_level, names(groups)), batch_size = 8L,
      seed = pfi_seed(learner_id, pv_index) + if (design_id == "full") 0L else 50000L)
    out$pfi_fold = cbind(key[rep(1L, nrow(pfi$per_iteration))], pfi$per_iteration)
    out$pfi_summary = cbind(key[rep(1L, nrow(pfi$summary))], pfi$summary)
  }
  if (grepl("^ridge", learner_id) && design_id == "full") {
    out$coefficients = rbindlist(lapply(seq_along(result$models), function(fold) {
      cbind(key, fold = fold, glmnet_coefficients(result$models[[fold]], learner_id))
    }))
  }
  if (design_id == "full") {
    out$ale = rbindlist(lapply(seq_along(result$models), function(fold) {
      newdata = task$data(rows = test_sets[[fold]], cols = task$feature_names)
      rbindlist(lapply(c("METASPAM", "ESCS"), function(feature) {
        ale = ale_curve(result$models[[fold]], newdata, feature, settings$ale_intervals)
        cbind(key, fold = fold, feature = feature, ale$curve,
          q10_to_q90 = ale$q10_to_q90, nondecreasing_share = ale$nondecreasing_share)
      }))
    }))
  }
  out
}

jobs = rbindlist(list(
  CJ(learner = setdiff(learner_ids, "featureless"), outcome_id = settings$pvs, design = names(designs), sorted = FALSE),
  CJ(learner = "featureless", outcome_id = settings$pvs, design = "full", sorted = FALSE)
))
# Optional restriction, e.g. --learners=xgboost_tuned --designs=full (used for the tuned-XGBoost check).
only_learners = arg_value("learners", NULL)
only_designs = arg_value("designs", NULL)
if (!is.null(only_learners)) {
  requested = strsplit(only_learners, ",")[[1L]]
  jobs = CJ(learner = requested, outcome_id = settings$pvs, design = if (is.null(only_designs)) names(designs) else
    strsplit(only_designs, ",")[[1L]], sorted = FALSE)
}
# Longest jobs first.
jobs[, cost := fifelse(learner == "random_forest", 3, fifelse(learner == "xgboost", 2, 1)) *
  fifelse(design == "full", 3, 1)]
setorder(jobs, -cost)
message(sprintf("PISA %s stage: %d students, %d jobs, %d workers", stage, nrow(dt), nrow(jobs), workers))
started = Sys.time()
results = parallel::mclapply(seq_len(nrow(jobs)), function(index) {
  job = jobs[index]
  value = tryCatch(run_job(job$learner, job$outcome_id, job$design), error = function(error) error)
  if (inherits(value, "error")) message("FAILED ", paste(job[, 1:3], collapse = "/"), ": ", conditionMessage(value))
  value
}, mc.cores = workers, mc.preschedule = FALSE)
failed = vapply(results, function(value) inherits(value, "error") || is.null(value), logical(1L))
if (any(failed)) stop(sum(failed), " jobs failed; see messages above.", call. = FALSE)
message(sprintf("Finished in %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))

collect = function(name) rbindlist(lapply(results, `[[`, name), fill = TRUE)
tables = list(
  fold_rmse = collect("fold_rmse"), pooled_rmse = collect("pooled_rmse"), pfi_fold = collect("pfi_fold"),
  pfi_summary = collect("pfi_summary"), coefficients = collect("coefficients"), ale = collect("ale")
)
for (name in names(tables)) if (nrow(tables[[name]])) fwrite(tables[[name]], file.path(output_dir, paste0(name, ".csv")))

# Held-out predictions are row-level; they are kept only for the private identity check against the primary
# run and never written to the public results.
predictions = collect("predictions")
private_dir = Sys.getenv("CSDG_PRIVATE_DIR", unset = "")
if (nzchar(private_dir)) {
  dir.create(private_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(predictions = predictions, row_ids = reference_task$row_ids, student = dt[, .(CNT, CNTSTUID)]),
    file.path(private_dir, paste0("pisa_ses_reanalysis_", stage, if (quick) "_quick", "_predictions.rds")))
}
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
message("Aggregate tables written to ", output_dir)
