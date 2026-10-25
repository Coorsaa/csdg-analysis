# SHILD: statement (a) and the stronger claim of the article rederived with the analysis-first interface of
# mlr3autoiml 0.3.0.
#
# The task and the XGBoost learner are those of analysis/shild/analysis.R: the 27 items, the outcome
# life_satisfaction (positive class discontent), training-fold median and mode imputation and one-hot encoding inside
# the learner pipeline, and nrounds 200, max_depth 3, eta 0.05, subsample 1, colsample_bytree 1. csdg_fit() fits the
# learner and the model without predictors on the 10 retained folds of the primary analysis (a custom resampling,
# checked against the stored fold assignment in paper_artifacts/current/), csdg_importance() computes held-out PFI
# (increase in log loss, 20 permutations per item and fold), and two claims are derived, checked, and assessed:
#   statement (a)   claim_relies_mainly(k = 2): "XGBoost relies mainly on self-rated health (A2) and depressed
#                   mood (D2)";
#   stronger claim  claim_order(A2, D2): "XGBoost relies more on self-rated health (A2) than on depressed mood (D2)".
# G0b (measurement and data) is judged with csdg_judge(); all other properties are computed by csdg_check() with its
# default criteria. The held-out losses are compared with the stored fold scores of the primary run. Because the
# stronger claim lies at the boundary of its criterion, the rule of csdg_check() for it is also applied to the stored
# permutations of the primary run, to 20 further sets of 20 permutations of A2 and D2 on the same fold models, to a
# precise estimate with 500 permutations per item and fold, and to the 22 sets of 20 pooled (440 per fold), the last
# two also under neighboring criteria. A stratified bootstrap of the held-out respondents of each fold, with the fold
# models held fixed, estimates the held-out sampling uncertainty of each fold's difference.
# Only aggregates are written, to analysis/shild/results/package_check/.
# Usage (from the repository root, with mlr3autoiml 0.3.0): Rscript analysis/shild/package_check.R

suppressPackageStartupMessages({
  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3autoiml)
})
lgr::get_logger("mlr3")$set_threshold("warn")
if (packageVersion("mlr3autoiml") < "0.3.0") {
  stop("analysis/shild/package_check.R needs mlr3autoiml 0.3.0 or later; found ", packageVersion("mlr3autoiml"), ".",
    call. = FALSE)
}

args = commandArgs(trailingOnly = TRUE)
arg_value = function(name, default = NULL) {
  hit = grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}
root = normalizePath(arg_value("root", "."), mustWork = TRUE)
output_dir = file.path(root, "analysis/shild/results/package_check")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
frozen = file.path(root, "paper_artifacts/current/shild/private/tables")
started = Sys.time()

source(file.path(root, "analysis/shared/study_helpers.R"), local = FALSE)
source(file.path(root, "analysis/shild/prepare_data.R"), local = FALSE)
settings = .analysis_settings("SHILD")
stopifnot(!settings$quick, settings$folds == 10L, settings$xgb_rounds == 200L)
seed = settings$seed

# ---- Analytic sample, task, and the retained folds (as in analysis/shild/claim_extensions.R) ----

raw = fread(file.path(root, "data/SHILD/archive/Danish_SHILD_draft_data.csv"), na.strings = c("", "NA", "NaN"))
preparation = .prepare_shild_raw(raw)
rm(raw)
dt = preparation$data
items = preparation$predictors
for (column in items[vapply(dt[, ..items], is.character, logical(1L))]) set(dt, j = column, value = factor(dt[[column]]))
task = TaskClassif$new("shild_life_satisfaction", backend = dt[, c(items, "life_satisfaction"), with = FALSE],
  target = "life_satisfaction", positive = "discontent")
stopifnot(task$nrow == 16971L, length(items) == 27L, setequal(task$feature_names, items))
split = .stratified_resampling(task, dt$life_satisfaction, settings$folds, seed)
stored_folds = fread(file.path(frozen, "shild_fold_assignment.csv"))
stopifnot(identical(as.integer(stored_folds$row_id), as.integer(task$row_ids)),
  identical(as.integer(stored_folds$fold), as.integer(split$assignment$fold)),
  identical(stored_folds$stratum, as.character(dt$life_satisfaction)))
resampling = split$resampling

xgboost_learner = .make_pipeline_learner(lrn("classif.xgboost", predict_type = "prob", nrounds = settings$xgb_rounds,
  max_depth = 3L, eta = 0.05, nthread = 1L, subsample = 1, colsample_bytree = 1), "shild_xgboost")

# ---- Analysis-first interface ----

csdg_claim_log_reset()
fits = csdg_fit(task, list(xgboost = xgboost_learner), resampling = resampling, labels = c(xgboost = "XGBoost"),
  seed = seed)
imp = csdg_importance(fits, repetitions = 20L, seed = seed)

labels = c(A2 = "self-rated health (A2)", D2 = "depressed mood (D2)")
g0b = csdg_judge("G0b", supported(),
  note = "Single survey items coded as in the codebook; claims name items, not constructs.")
claim_a = claim_relies_mainly(imp, k = 2, learners = "xgboost", labels = labels)
stopifnot(setequal(claim_a$predictors, c("A2", "D2")))
check_a = csdg_check(claim_a, imp, judgments = g0b)
assessment_a = csdg_assess(check_a)
claim_b = claim_order(imp, "A2", "D2", learners = "xgboost", labels = labels)
check_b = csdg_check(claim_b, imp, judgments = g0b)
assessment_b = csdg_assess(check_b)

# The exports are written now, so that their claim logs hold the derivation, check, and assessment of the two claims
# with the default criteria (the checks of the rule below follow), and copied to the output directory at the end.
export_dir = file.path(tempdir(), "package_check_exports")
unlink(export_dir, recursive = TRUE)
dir.create(export_dir)
exports = c(csdg_export_assessment(assessment_a, export_dir, prefix = "statement_a"),
  csdg_export_assessment(assessment_b, export_dir, prefix = "stronger_claim"))

# ---- Numbers compared with the article ----

improvement = imp$improvement[learner == "xgboost"]
performance = imp$performance[learner == "xgboost", .(fold, n_test, loss, loss_baseline, improvement)]
primary_scores = merge(
  fread(file.path(frozen, "shild_gate_g6a__model_generalization__resamples__xgboost__fold_scores.csv"))[,
    .(fold, primary_loss = classif.logloss)],
  fread(file.path(frozen, "shild_gate_g6a__model_generalization__resamples__featureless__fold_scores.csv"))[,
    .(fold, primary_loss_baseline = classif.logloss)], by = "fold")
performance = merge(performance, primary_scores, by = "fold")

P = check_a$details$fold_pfi$xgboost
others = setdiff(colnames(P), c("A2", "D2"))
largest_per_fold = apply(P[, others, drop = FALSE], 1L, max)
fold_ratios = data.table(fold = as.integer(rownames(P)), ratio_A2 = P[, "A2"] / largest_per_fold,
  ratio_D2 = P[, "D2"] / largest_per_fold, difference_A2_minus_D2 = P[, "A2"] - P[, "D2"])
content_a = check_a$details$content$xgboost
stability_a = check_a$details$stability$xgboost
content_b = check_b$details$content$xgboost
stability_b = check_b$details$stability$xgboost
delta_default = check_b$criteria$min_difference[["xgboost"]]
need_default = check_b$criteria$need
tau = check_b$criteria$tau[["xgboost"]]

# Fold-level PFI of the primary run (two stored runs of 10 permutations, pooled to 20; public aggregate). Item-fold
# pairs without permutation variation in either run (PFI 0 in every permutation) have no z value.
primary_pfi = fread(file.path(root, "analysis/shild/results/claim_extensions/importance_fold_pfi.csv"))
package_pfi = imp$fold_pfi[learner == "xgboost" & type == "marginal", .(fold, item = feature_group, pfi = importance,
  mc_se = monte_carlo_se)]
pfi_both = merge(package_pfi, primary_pfi[, .(fold, item, primary_pfi = pfi, primary_mc_se = mc_se)],
  by = c("fold", "item"))
stopifnot(nrow(pfi_both) == 270L)
pfi_both[, z := fifelse(mc_se + primary_mc_se > 0, (pfi - primary_pfi) / sqrt(mc_se^2 + primary_mc_se^2), NA_real_)]
max_abs = function(x) if (all(is.na(x))) NA_real_ else max(abs(x), na.rm = TRUE)
pfi_items = pfi_both[, .(mean_pfi = mean(pfi), primary_mean_pfi = mean(primary_pfi),
  max_abs_fold_difference = max(abs(pfi - primary_pfi)), max_abs_z = max_abs(z)), by = item][order(-mean_pfi)]

# ---- The rule of csdg_check() for the stronger claim ----

# Content (G2), minimum importance (G2), and stability (G5) of the order claim as csdg_check() decides them, from the
# fold x permutation PFI values of A2 and D2 (exported helpers of mlr3autoiml), and the assessment; the other required
# properties (specification, judged measurement, procedure) do not depend on the permutations, and the dependence
# screen is context. `delta` is the smallest relevant difference and `need` the number of folds required. A fold in
# which A2 does not exceed D2 fails if its difference is beyond Monte Carlo error, t(.975, R - 1) x SE (with the Holm
# step-down of csdg_check() over these folds when every fold is required, two-sided as in Supplement D:
# t(1 - (1 - level) / (2 (K - i + 1)), R - 1) for the i-th largest reversal), and at least `delta`; otherwise it is
# undecided. The rule is checked against csdg_check() and csdg_assess() below.
raw_matrix = function(raw, group) {
  rows = raw[feature_group == group]
  setorderv(rows, intersect(c("fold", "run", "permutation_repetition"), names(rows)))
  stopifnot(nrow(rows) > 0L, nrow(rows) %% fits$K == 0L)
  matrix(rows$importance, nrow = fits$K, byrow = TRUE)
}
order_rule = function(raw_a, raw_b, delta = delta_default, need = need_default, level = 0.95) {
  K = nrow(raw_a)
  R = ncol(raw_a)
  stopifnot(identical(dim(raw_a), dim(raw_b)), K == fits$K, R >= 2L)
  q = stats::qt(1 - (1 - level) / 2, R - 1L)
  interval_of = function(values) {
    csdg_learner_pfi_interval(values, n_train = fits$n_train, n_test = fits$n_test, level = level)
  }
  d = rowMeans(raw_a) - rowMeans(raw_b)
  ci = interval_of(d)
  mc = rbindlist(lapply(seq_len(K), function(k) csdg_pfi_mc_difference(raw_a[k, ], raw_b[k, ], level = level)))
  mc_threshold = q * sqrt(sum(mc$se^2)) / K
  estimate = mean(d)
  beyond = abs(estimate) > mc_threshold
  content = if (estimate > 0 && beyond && ci$lower >= delta) "supported" else
    if (estimate <= 0 && beyond && ci$upper <= -delta) "contradicted" else "open"
  mean_a = mean(raw_a)
  floor_threshold = q * sqrt(sum(apply(raw_a, 1L, stats::var) / R)) / K
  minimum = if (abs(mean_a - tau) <= floor_threshold) "open" else if (mean_a >= tau) "supported" else
    if (interval_of(rowMeans(raw_a))$upper < tau) "contradicted" else "open"
  failing = which(d <= 0)
  rule_threshold = rep(NA_real_, K)
  fold_beyond = rep(FALSE, K)
  if (need < K) {
    rule_threshold[failing] = mc$threshold[failing]
    fold_beyond[failing] = abs(d[failing]) > mc$threshold[failing]
  } else {
    ratio = ifelse(mc$se[failing] > 0, abs(d[failing]) / mc$se[failing], ifelse(abs(d[failing]) > 0, Inf, 0))
    still = TRUE
    for (i in seq_along(failing)) {
      k = failing[order(-ratio)][[i]]
      rule_threshold[k] = stats::qt(1 - (1 - level) / (2 * (K - i + 1L)), R - 1L) * mc$se[k]
      fold_beyond[k] = still && abs(d[k]) > rule_threshold[k]
      still = fold_beyond[k]
    }
  }
  fold_class = fifelse(d > 0, "holds", fifelse(fold_beyond & abs(d) >= delta, "fails", "undecided"))
  n_holds = sum(fold_class == "holds")
  n_undecided = sum(fold_class == "undecided")
  stability = if (n_holds >= need) "supported" else if (n_holds + n_undecided < need) "contradicted" else "open"
  statuses = c(content, minimum, stability)
  list(
    summary = data.table(n_permutations = R, mean_A2 = mean_a, mean_D2 = mean(raw_b), difference = estimate,
      lower = ci$lower, upper = ci$upper, mc_threshold = mc_threshold, sd_fold_differences = stats::sd(d),
      folds_A2_larger = n_holds, folds_failing = sum(fold_class == "fails"), folds_undecided = n_undecided,
      content = content, minimum_importance = minimum, stability = stability,
      assessment = if (any(statuses == "contradicted")) "not met" else if (any(statuses == "open")) "unresolved" else
        "met"),
    folds = data.table(fold = seq_len(K), pfi_A2 = rowMeans(raw_a), pfi_D2 = rowMeans(raw_b), difference = d,
      mc_se = mc$se, rule_threshold = rule_threshold, class = fold_class)
  )
}

# The default criteria and neighboring criteria, one varied at a time.
criteria_grid = data.table(
  criterion = c("defaults (smallest relevant difference 1% of the improvement, 9 of 10 folds)",
    "smallest relevant difference 0", "smallest relevant difference 0.5% of the improvement",
    "smallest relevant difference 2% of the improvement", "8 of 10 folds", "10 of 10 folds"),
  min_difference = c(NA, 0, 0.005, 0.02, NA, NA) * improvement$improvement,
  stability = c(0.9, 0.9, 0.9, 0.9, 0.8, 1)
)
criteria_grid[, `:=`(delta = fifelse(is.na(min_difference), delta_default, min_difference),
  need = as.integer(ceiling(stability * fits$K - 1e-8)))]

# Check of the rule on the permutations of csdg_importance(): each criterion against csdg_check() and csdg_assess().
package_raw = as.data.table(imp$pfi$xgboost$raw)
main = list(A2 = raw_matrix(package_raw, "A2"), D2 = raw_matrix(package_raw, "D2"))
own = order_rule(main$A2, main$D2)$summary
stopifnot(isTRUE(all.equal(unlist(own[, .(difference, lower, upper, mc_threshold)], use.names = FALSE),
  unlist(content_b$margins[, .(estimate, lower, upper, mc_threshold)], use.names = FALSE), tolerance = 1e-12)))
for (i in seq_len(nrow(criteria_grid))) {
  criterion = criteria_grid[i]
  chk = csdg_check(claim_b, imp, judgments = g0b, stability = criterion$stability,
    min_difference = if (is.na(criterion$min_difference)) NULL else criterion$min_difference)
  rule = order_rule(main$A2, main$D2, delta = criterion$delta, need = criterion$need)
  st = chk$details$stability$xgboost
  failing = which(st$folds$class != "holds")
  stopifnot(
    identical(rule$summary$content, chk$details$content$xgboost$status),
    identical(rule$summary$minimum_importance, chk$details$minimum_importance$xgboost$status),
    identical(rule$summary$stability, st$status), identical(rule$summary$folds_A2_larger, st$n_holds),
    identical(rule$summary$folds_failing, st$n_fails), identical(rule$folds$class, st$folds$class),
    isTRUE(all.equal(rule$folds$rule_threshold[failing], st$folds$threshold[failing], tolerance = 1e-12)),
    identical(rule$summary$assessment, gsub("_", " ", csdg_assess(chk)$assessment)))
}

# ---- The stronger claim on other permutations of the same fold models ----

# The permutations of the primary run behind the article (two stored runs of 10 per fold, pooled to 20), those of
# csdg_importance(), and 20 further sets of 20 permutations per fold of A2 and D2. The seeds of the further sets lie
# 1e7 apart, so that no permutation seed is shared between sets.
primary_sources = c("shild_gate_g5__pfi__raw.csv", "shild_gate_g6a__model_generalization__pfi__xgboost__raw.csv")
primary_raw = rbindlist(lapply(seq_along(primary_sources), function(run) {
  fread(file.path(frozen, primary_sources[[run]]))[feature_group %in% c("A2", "D2"),
    .(fold, run = run, feature_group, permutation_repetition, importance)]
}))
sets = list()
sets[["primary run (article), two stored runs of 10"]] = list(A2 = raw_matrix(primary_raw, "A2"),
  D2 = raw_matrix(primary_raw, "D2"))
sets[[sprintf("csdg_importance(), seed %d", seed)]] = main
rerun_seeds = seed + 1e7 * seq_len(20L)
for (s in rerun_seeds) {
  raw = as.data.table(csdg_fold_pfi(fits$resamples$xgboost, feature_groups = list(A2 = "A2", D2 = "D2"),
    loss = fits$loss, repetitions = 20L, batch_size = 20L, seed = s)$raw)
  sets[[sprintf("csdg_fold_pfi(), seed %d", s)]] = list(A2 = raw_matrix(raw, "A2"), D2 = raw_matrix(raw, "D2"))
}
permutation_sets = rbindlist(lapply(names(sets), function(name) {
  cbind(permutations = name, order_rule(sets[[name]]$A2, sets[[name]]$D2)$summary)
}))
primary = permutation_sets[1L]
reruns = permutation_sets[-(1:2)]
stored_difference = fread(file.path(root, "analysis/shild/results/claim_extensions/importance_learner_intervals.csv"))[
  comparison == "A2 - D2", estimate]
stopifnot(abs(primary$difference - stored_difference) < 1e-12)

# ---- The stronger claim with 500 permutations per item and fold ----

# A precise estimate on the same fold models (seed outside the seed ranges of all sets above), and, as a cross-check,
# the 22 sets of 20 pooled (440 per fold). holm_threshold is t(1 - .025/K, R - 1) x SE for the largest reversal, as in
# Supplement D of the article and in the rule of csdg_check() when every fold is required.
precise_seed = seed + 1e7 * (length(rerun_seeds) + 1L)
precise_raw = as.data.table(csdg_fold_pfi(fits$resamples$xgboost, feature_groups = list(A2 = "A2", D2 = "D2"),
  loss = fits$loss, repetitions = 500L, batch_size = 20L, seed = precise_seed)$raw)
estimates = list(
  list(label = sprintf("500 permutations per fold (csdg_fold_pfi(), seed %d)", precise_seed),
    A2 = raw_matrix(precise_raw, "A2"), D2 = raw_matrix(precise_raw, "D2")),
  list(label = "440 permutations per fold (the 22 sets of 20 pooled)",
    A2 = do.call(cbind, lapply(sets, `[[`, "A2")), D2 = do.call(cbind, lapply(sets, `[[`, "D2")))
)
stopifnot(ncol(estimates[[1L]]$A2) == 500L, ncol(estimates[[2L]]$A2) == 440L)
precise_folds = rbindlist(lapply(estimates, function(e) {
  R = ncol(e$A2)
  cbind(estimate = e$label, n_permutations = R, order_rule(e$A2, e$D2)$folds[, .(fold, pfi_A2, pfi_D2, difference,
    mc_se, threshold = stats::qt(0.975, R - 1L) * mc_se, holm_threshold = stats::qt(1 - 0.025 / fits$K, R - 1L) * mc_se,
    class)])
}))
precise_summary = rbindlist(lapply(estimates, function(e) {
  rbindlist(lapply(seq_len(nrow(criteria_grid)), function(i) {
    criterion = criteria_grid[i]
    cbind(estimate = e$label, criterion = criterion$criterion, smallest_relevant_difference = criterion$delta,
      folds_required = criterion$need, order_rule(e$A2, e$D2, delta = criterion$delta, need = criterion$need)$summary)
  }))
}))
precise = precise_summary[1L]
fold9 = precise_folds[n_permutations == 500L & fold == 9L]

# ---- Held-out sampling uncertainty of the fold differences (fold models held fixed) ----

# Permuting one item changes only its level, and the learner pipeline (training-fold imputation, encoding, XGBoost)
# predicts each row on its own. Each permuted prediction is therefore a lookup in a table of the predictions of every
# held-out row with the item set to each of its levels (missing values form a level of their own, which the pipeline
# imputes). The lookup is checked against permutation values of the 500-permutation run.
logloss = function(y, p) {
  p = pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
fold_tables = lapply(seq_len(fits$K), function(k) {
  model = fits$resamples$xgboost$models[[k]]
  rows = fits$resamples$xgboost$test_sets[[k]]
  newdata = task$data(rows = rows, cols = task$feature_names)
  predict_positive = function(data) model$predict_newdata(data, task = task)$prob[, "discontent"]
  lookup = lapply(c(A2 = "A2", D2 = "D2"), function(item) {
    keys = levels(newdata[[item]])
    codes = as.integer(newdata[[item]])
    codes[is.na(codes)] = length(keys) + 1L
    pred = matrix(NA_real_, length(rows), length(keys) + 1L)
    for (code in sort(unique(codes))) {
      altered = copy(newdata)
      set(altered, j = item, value = factor(rep(c(keys, NA_character_)[[code]], length(rows)), levels = keys))
      pred[, code] = predict_positive(altered)
    }
    list(codes = codes, pred = pred)
  })
  list(y = as.integer(task$truth(rows) == "discontent"), base = predict_positive(newdata), lookup = lookup)
})
lookup_check = rbindlist(lapply(seq_len(fits$K), function(k) {
  ft = fold_tables[[k]]
  n = length(ft$y)
  rbindlist(lapply(1:2, function(g) {
    lk = ft$lookup[[g]]
    rbindlist(lapply(1:3, function(b) {
      set.seed(precise_seed + k * 100000L + g * 1000L + b)
      index = sample(seq_len(n))
      data.table(fold = k, feature_group = names(ft$lookup)[[g]], permutation_repetition = b,
        lookup = logloss(ft$y, lk$pred[cbind(seq_len(n), lk$codes[index])]) - logloss(ft$y, ft$base))
    }))
  }))
}))
lookup_check = merge(lookup_check, precise_raw[, .(fold, feature_group, permutation_repetition, importance)],
  by = c("fold", "feature_group", "permutation_repetition"))
stopifnot(nrow(lookup_check) == 60L, max(abs(lookup_check$lookup - lookup_check$importance)) < 1e-12)

# Stratified bootstrap of each fold's held-out respondents (by outcome), B = 200 resamples, PFI difference A2 - D2
# with 50 permutations per item on each resample; one seed per fold and resample, from which the resample and its
# permutations are drawn. The bootstrap SE includes the Monte Carlo error of 50 permutations; bootstrap_se_without_mc
# subtracts its mean variance. z divides the difference of the 500-permutation run by the bootstrap SE.
boot_seed = seed + 1e7 * (length(rerun_seeds) + 2L)
B = 200L
R_boot = 50L
d500 = precise_folds[n_permutations == 500L][order(fold), difference]
fold_bootstrap = rbindlist(lapply(seq_len(fits$K), function(k) {
  ft = fold_tables[[k]]
  n = length(ft$y)
  strata = split(seq_len(n), ft$y)
  draws = rbindlist(lapply(seq_len(B), function(b) {
    set.seed(boot_seed + k * 1000L + b)
    index = unlist(lapply(strata, function(rows) rows[sample.int(length(rows), replace = TRUE)]), use.names = FALSE)
    y = ft$y[index]
    base_loss = logloss(y, ft$base[index])
    pfi = lapply(ft$lookup, function(lk) vapply(seq_len(R_boot), function(r) {
      logloss(y, lk$pred[cbind(index, lk$codes[index[sample.int(n)]])]) - base_loss
    }, numeric(1L)))
    data.table(difference = mean(pfi$A2) - mean(pfi$D2),
      mc_var = stats::var(pfi$A2) / R_boot + stats::var(pfi$D2) / R_boot)
  }))
  se = stats::sd(draws$difference)
  data.table(fold = k, n_assessment = n, n_discontent = sum(ft$y), difference_500 = d500[[k]],
    bootstrap_mean = mean(draws$difference), bootstrap_se = se,
    bootstrap_se_without_mc = sqrt(max(0, se^2 - mean(draws$mc_var))),
    lower = stats::quantile(draws$difference, 0.025, names = FALSE),
    upper = stats::quantile(draws$difference, 0.975, names = FALSE), z = d500[[k]] / se)
}))
fold_bootstrap[, `:=`(beyond_1_96 = abs(z) > stats::qnorm(0.975),
  beyond_holm_2_81 = abs(z) > stats::qnorm(1 - 0.025 / fits$K))]
boot9 = fold_bootstrap[fold == 9L]

num = function(x, digits = 4L) formatC(signif(x, digits), digits = digits, format = "fg", flag = "#")
interval = function(estimate, lower, upper) sprintf("%s [%s, %s]", num(estimate), num(lower), num(upper))
largest_other = content_a$largest_other
comparison = data.table(
  quantity = c(
    "respondents (rows of the task)", "items",
    "held-out log loss, XGBoost", "held-out log loss, model without predictors",
    "minimum importance and smallest relevant difference (1% of the improvement)",
    "statement (a): mean PFI of self-rated health (A2)", "statement (a): mean PFI of depressed mood (D2)",
    "statement (a): mean PFI of the largest other item",
    "statement (a): weakest comparison A2 - 2 x other item (of 25), corrected 95% interval",
    "statement (a): weakest comparison D2 - 2 x other item (of 25), corrected 95% interval",
    "statement (a): smallest ratio to the largest other item of the fold, A2",
    "statement (a): smallest ratio to the largest other item of the fold, D2",
    "statement (a): folds that reproduce the claim (stability, G5)",
    "statement (a): assessment",
    "stronger claim: mean difference A2 - D2, corrected 95% interval",
    "stronger claim: content (G2)",
    "stronger claim: folds with A2 > D2 (stability, G5)",
    "stronger claim: assessment",
    "stronger claim: assessment by the same rule from the stored permutations of the primary run",
    "stronger claim: further sets of 20 permutations per fold with the claim met",
    "stronger claim, 500 permutations per fold: mean difference A2 - D2, corrected 95% interval",
    "stronger claim, 500 permutations per fold: folds with A2 > D2; failing folds",
    "stronger claim, 500 permutations per fold: assessment",
    "stronger claim, every fitted model: uncertainty of the reversal in fold 9"
  ),
  article = c(
    "16,971", "27", "0.122", "0.229", "0.001", "0.046", "0.036", "0.0055 (M8)", "[0.027, 0.043] (M8)", "[0.021, 0.030] (M8)",
    "5.0", "4.0", "10 of 10 (supported)", "met (exploratory)", "0.0097 [0.0005, 0.0188]",
    "depends on the permutation draw (lower bound 0.0005 in the stored draw)", "9 of 10 (supported)",
    "with 20 permutations, depends on the draw", "unresolved (exploratory)",
    "17 of 21 (lower bound 0.0004 to 0.0022)", "0.0103 [0.0014, 0.0192]", "9 of 10; fold 9 fails",
    "met (exploratory)", "0.0024, 0.32 bootstrap standard errors (SE 0.0075)"
  ),
  package = c(
    format(task$nrow, big.mark = ","), as.character(length(task$feature_names)), num(improvement$loss),
    num(improvement$loss_baseline), num(delta_default), num(content_a$averages[["A2"]]),
    num(content_a$averages[["D2"]]), sprintf("%s (%s)", num(content_a$averages[[largest_other]]), largest_other),
    with(content_a$margins[predictor == "A2"], sprintf("[%s, %s] (%s)", num(lower), num(upper), versus)),
    with(content_a$margins[predictor == "D2"], sprintf("[%s, %s] (%s)", num(lower), num(upper), versus)),
    num(min(fold_ratios$ratio_A2)), num(min(fold_ratios$ratio_D2)),
    sprintf("%d of %d (%s)", stability_a$n_holds, stability_a$K, stability_a$status), assessment_a$label,
    with(content_b$margins, interval(estimate, lower, upper)),
    sprintf("%s (lower bound %s, %s %s)", content_b$status, num(content_b$margins$lower),
      if (content_b$margins$lower >= delta_default) "above" else "below", num(delta_default)),
    sprintf("%d of %d (%s)", stability_b$n_holds, stability_b$K, stability_b$status), assessment_b$label,
    sprintf("%s; difference %s; %d of %d folds", primary$assessment, interval(primary$difference, primary$lower,
      primary$upper), primary$folds_A2_larger, fits$K),
    sprintf("%d of %d (lower bound from %s to %s)", sum(reruns$assessment == "met"), nrow(reruns),
      num(min(reruns$lower)), num(max(reruns$lower))),
    interval(precise$difference, precise$lower, precise$upper),
    sprintf("%d of %d; %d failing", precise$folds_A2_larger, fits$K, precise$folds_failing),
    sprintf("%s (content %s, stability %s)", precise$assessment, precise$content, precise$stability),
    sprintf("%s (500 permutations) versus a held-out bootstrap SE of %s: %s SEs", num(-boot9$difference_500),
      num(boot9$bootstrap_se), num(abs(boot9$z), 3L))
  )
)

# ---- Aggregate outputs ----

options(width = 150L)
sink(file.path(output_dir, "package_check.txt"), split = TRUE)
cat(sprintf("SHILD claim assessments with mlr3autoiml %s (analysis/shild/package_check.R)\n\n",
  packageVersion("mlr3autoiml")))
cat("== Fits ==\n")
print(fits)
cat(sprintf("\nHeld-out log loss (mean over folds): XGBoost %s, model without predictors %s, improvement %s.\n",
  num(improvement$loss), num(improvement$loss_baseline), num(improvement$improvement)))
cat("Per fold, with the stored fold scores of the primary run:\n")
print(performance, digits = 6L)
cat(sprintf("Largest absolute difference from the primary run: XGBoost %.2g, model without predictors %.2g.\n\n",
  max(abs(performance$loss - performance$primary_loss)),
  max(abs(performance$loss_baseline - performance$primary_loss_baseline))))
cat("== Held-out PFI ==\n")
print(imp, n = 8L)
cat("\nMean PFI over folds compared with the primary run (20 permutations per item and fold in both; z: fold\n",
  "difference divided by its Monte Carlo standard error):\n", sep = "")
print(pfi_items[1:8], digits = 4L)
cat(sprintf(paste("Largest |z| over all 27 items and 10 folds: %.2f; %d item-fold pairs without permutation",
  "variation in either run (PFI 0 in every permutation) have no z.\n\n"), max_abs(pfi_both$z), sum(is.na(pfi_both$z))))
cat("== Statement (a) ==\n")
print(claim_a)
cat("\n")
print(check_a)
cat("\n")
print(assessment_a)
cat("\nContent (G2): the comparisons of A2 and D2 with each other item (PFI of the named item minus twice that of\n",
  "the other item), the five with the smallest lower limits of the corrected 95% interval:\n", sep = "")
print(content_a$comparisons[order(lower)][1:5, .(predictor, versus, estimate, lower, upper, mc_threshold, status)],
  digits = 4L)
cat(sprintf("All %d comparisons supported: %s.\n", nrow(content_a$comparisons),
  all(content_a$comparisons$status == "supported")))
cat("\nPer fold: PFI of A2 and D2 divided by the largest other item of the fold, and A2 minus D2:\n")
print(fold_ratios, digits = 4L)
cat("\n== Stronger claim ==\n")
print(claim_b)
cat("\n")
print(check_b)
cat("\n")
print(assessment_b)
cat("\nPer fold: A2 minus D2, Monte Carlo threshold, and class of the fold (stability, G5):\n")
print(stability_b$folds[, .(fold = iteration, difference = d, mc_se = se, threshold, class)], digits = 4L)
cat("\n== Stronger claim on other permutations of the same fold models ==\n")
cat(sprintf(paste("Rule of csdg_check() (smallest relevant difference %s, minimum importance %s, at least %d of %d",
  "folds) applied to the\nprimary run's stored permutations and to %d further sets of 20 permutations per fold:\n"),
  num(delta_default), num(tau), need_default, fits$K, nrow(reruns)))
print(permutation_sets, digits = 4L, nrows = 100L)
cat(sprintf("Further sets: met in %d of %d, unresolved in %d.\n", sum(reruns$assessment == "met"), nrow(reruns),
  sum(reruns$assessment == "unresolved")))
cat("\n== Stronger claim with 500 permutations per item and fold ==\n")
cat(sprintf("%s; cross-check: %s.\n", estimates[[1L]]$label, estimates[[2L]]$label))
cat("Per fold (threshold: t(.975, R - 1) x SE; holm_threshold: t(1 - .025/10, R - 1) x SE, as in Supplement D and in\n",
  "the rule of csdg_check() for the largest reversal when every fold is required; class under the default criteria):\n",
  sep = "")
print(precise_folds[, -"estimate"], digits = 4L, nrows = 100L)
cat(sprintf("Fold 9, 500 permutations: difference %s, SE %s, per-fold threshold %s, Holm threshold %s.\n",
  num(fold9$difference), num(fold9$mc_se), num(fold9$threshold), num(fold9$holm_threshold)))
cat("\nAssessment under the default and neighboring criteria (one criterion varied at a time):\n")
print(precise_summary[, .(n_permutations, criterion, smallest_relevant_difference, folds_required, difference, lower,
  upper, sd_fold_differences, folds_A2_larger, folds_failing, content, minimum_importance, stability, assessment)],
  digits = 4L, nrows = 100L)
cat("\n== Held-out sampling uncertainty of the fold differences (fold models held fixed) ==\n")
cat(sprintf(paste("Stratified bootstrap of each fold's held-out respondents (B = %d), PFI difference A2 - D2 with %d",
  "permutations per item\non each resample; z: difference of the 500-permutation run divided by the bootstrap SE;",
  "beyond_1_96 and beyond_holm_2_81:\n|z| above 1.96 and above %s (Holm for 10 folds):\n"), B, R_boot,
  num(stats::qnorm(1 - 0.025 / fits$K), 3L)))
print(fold_bootstrap, digits = 4L)
cat(sprintf(paste("Fold 9: difference %s (500 permutations), bootstrap SE %s (%s without the Monte Carlo error of",
  "%d permutations),\n95%% percentile interval [%s, %s]; the reversal is %s bootstrap SEs, %s 1.96 and %s 2.81.\n"),
  num(boot9$difference_500), num(boot9$bootstrap_se), num(boot9$bootstrap_se_without_mc), R_boot, num(boot9$lower),
  num(boot9$upper), num(abs(boot9$z), 3L), if (boot9$beyond_1_96) "above" else "below",
  if (boot9$beyond_holm_2_81) "above" else "below"))
cat(sprintf("SD of the fold differences (500 permutations): %s.\n", num(precise$sd_fold_differences)))
cat("\n== Comparison with the article ==\n")
cat(sprintf("%-*s  %-*s  %s\n", max(nchar(comparison$quantity)), c("quantity", comparison$quantity),
  max(nchar(comparison$article)), c("article", comparison$article), c("mlr3autoiml 0.3.0", comparison$package)),
  sep = "")
sink()

fwrite(as.data.table(check_a), file.path(output_dir, "check_statement_a.csv"))
fwrite(as.data.table(check_b), file.path(output_dir, "check_stronger_claim.csv"))
fwrite(content_a$comparisons, file.path(output_dir, "statement_a_comparisons.csv"))
fwrite(as.data.table(imp, interval = TRUE)[learner == "xgboost"], file.path(output_dir, "importance_xgboost.csv"))
fwrite(comparison, file.path(output_dir, "comparison_with_article.csv"))
fwrite(permutation_sets, file.path(output_dir, "stronger_claim_permutations.csv"))
fwrite(precise_folds, file.path(output_dir, "stronger_claim_precise_folds.csv"))
fwrite(precise_summary, file.path(output_dir, "stronger_claim_precise_summary.csv"))
fwrite(fold_bootstrap, file.path(output_dir, "stronger_claim_fold_bootstrap.csv"))
for (path in exports) {
  unlink(file.path(output_dir, basename(path)), recursive = TRUE)
  stopifnot(file.copy(path, output_dir, recursive = TRUE))
}
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
message(sprintf("Runtime: %.1f minutes", as.numeric(difftime(Sys.time(), started, units = "mins"))))
