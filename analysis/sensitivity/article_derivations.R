# Derived numbers reported in the article, recomputed from stored fold-level aggregates (no data, no refitting).
# Run from the analysis repository root:
#   Rscript analysis/sensitivity/article_derivations.R
# Inputs: analysis/shild/results/claim_extensions/, analysis/pisa/results/ses_reanalysis/{exploratory,confirmatory}/,
# and, for the SHILD model without predictors, baseline_evidence/STUDY_RESULTS/SHILD/tables/.
# Outputs: analysis/sensitivity/results/article_derivations.csv (one row per number: quantity, stage, learner,
# detail, value) and article_derivations.txt (the same, rounded as in the article). PISA learner codes: ridge_fixed is the
# linear regression and ridge_cv the ridge regression of the article. Intervals are corrected resampled t intervals
# over the K folds (Nadeau & Bengio, 2003): standard error sqrt((1/K + 1/(K - 1)) * var), K - 1 degrees of freedom.

suppressPackageStartupMessages(library(data.table))
root = getwd()
stopifnot(file.exists(file.path(root, "analysis", "sensitivity", "article_derivations.R")))
out = file.path(root, "analysis", "sensitivity", "results")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
shild_dir = file.path(root, "analysis", "shild", "results", "claim_extensions")
pisa_dir = file.path(root, "analysis", "pisa", "results", "ses_reanalysis")

corrected = function(values, level = 0.95) {
  k = length(values)
  se = sqrt((1 / k + 1 / (k - 1)) * stats::var(values))
  q = stats::qt(1 - (1 - level) / 2, df = k - 1L)
  c(mean = mean(values), se = se, lower = mean(values) - q * se, upper = mean(values) + q * se)
}
rows = list()
add = function(quantity, stage, learner = NA_character_, detail = NA_character_, value) {
  rows[[length(rows) + 1L]] <<- data.table(quantity = quantity, stage = stage, learner = learner, detail = detail,
    value = as.numeric(value))
}
report = character()
say = function(...) report <<- c(report, sprintf(...))
pv_order = function(ids) ids[order(as.integer(gsub("\\D", "", ids)))]
pisa_learners = c("xgboost", "random_forest", "ridge_fixed", "ridge_cv")

# ---- Admission of the comparison learners (supplement, section I) ----

losses = dcast(fread(file.path(shild_dir, "loco_fold_losses.csv"))[design == "all_items"], fold ~ learner,
  value.var = "log_loss")
say("SHILD admission: held-out log loss per fold (refits with all 27 items), comparison minus XGBoost")
for (model in c("xgboost", "ridge_logistic", "random_forest")) {
  add("mean_log_loss", "shild", model, value = mean(losses[[model]]))
}
for (model in c("ridge_logistic", "random_forest")) {
  ci = corrected(losses[[model]] - losses$xgboost)
  for (name in names(ci)) add(paste0("loss_difference_", name), "shild", model, value = ci[[name]])
  say("  %-14s loss %.4f; difference %.4f (SE %.4f), interval %.4f to %.4f", model, mean(losses[[model]]),
    ci[["mean"]], ci[["se"]], ci[["lower"]], ci[["upper"]])
}
for (stage in c("exploratory", "confirmatory")) {
  rmse = fread(file.path(pisa_dir, stage, "fold_rmse.csv"))[design == "full"]
  means = rmse[, .(rmse = mean(rmse)), by = learner]
  by_fold = dcast(rmse[, .(rmse = mean(rmse)), by = .(learner, fold)], fold ~ learner, value.var = "rmse")
  reference = means[learner == "xgboost", rmse]
  baseline = means[learner == "featureless", rmse]
  say("PISA admission, %s: RMSE per fold averaged over plausible values, comparison minus XGBoost", stage)
  means[, retained := 100 * (baseline - rmse) / (baseline - reference)]
  for (model in means$learner) {
    add("mean_rmse", stage, model, value = means[learner == model, rmse])
    add("retained_share_percent", stage, model, value = means[learner == model, retained])
  }
  for (model in setdiff(pisa_learners, "xgboost")) {
    ci = corrected(by_fold[[model]] - by_fold$xgboost)
    for (name in names(ci)) add(paste0("rmse_difference_", name), stage, model, value = ci[[name]])
    say("  %-14s RMSE %.2f; retained %.1f%%; difference %.2f (SE %.2f), interval %.2f to %.2f", model,
      means[learner == model, rmse], means[learner == model, retained], ci[["mean"]], ci[["se"]], ci[["lower"]],
      ci[["upper"]])
  }
}

# ---- PISA exploratory: top three, five, and ten across plausible values (supplement, section I) ----

summary_pfi = fread(file.path(pisa_dir, "exploratory", "pfi_summary.csv"))[design == "full"]
single = summary_pfi[!grepl("|", features, fixed = TRUE)]
stopifnot(uniqueN(single$feature_group) == 41L, uniqueN(single$outcome_id) == 10L)
tops = list()
say("PISA exploratory: top K of the PFI averaged over plausible values")
for (k in c(3L, 5L, 10L)) {
  for (model in pisa_learners) {
    x = single[learner == model]
    average = x[, .(pfi = mean(mean_importance)), by = feature_group][order(-pfi)]
    top = average$feature_group[seq_len(k)]
    recur = x[, .(same = setequal(feature_group[order(-mean_importance)][seq_len(k)], top)), by = outcome_id]
    tops[[paste(model, k)]] = top
    add("top_k_recurrence_n_pv", "exploratory", model, paste0("K=", k), sum(recur$same))
    add("top_k_margin_over_next", "exploratory", model, paste0("K=", k), average$pfi[k] - average$pfi[k + 1L])
    say("  K = %2d %-14s recurs in %d of 10; margin over place %d: %.3f; %s", k, model, sum(recur$same), k + 1L,
      average$pfi[k] - average$pfi[k + 1L], paste(top, collapse = ", "))
  }
  for (model in setdiff(pisa_learners, "xgboost")) {
    shared = length(intersect(tops[[paste("xgboost", k)]], tops[[paste(model, k)]]))
    add("top_k_shared_with_xgboost", "exploratory", model, paste0("K=", k), shared)
    say("  K = %2d %-14s shares %d of %d with XGBoost", k, model, shared, k)
  }
}

# ---- PISA exploratory: primary block of linear regression relative to XGBoost and random forest ----

block = dcast(summary_pfi[feature_group == "socioeconomic_joint_block"], outcome_id ~ learner, value.var = "mean_importance")
block_ratio = block$ridge_fixed / pmax(block$xgboost, block$random_forest)
add("block_ratio_min", "exploratory", "ridge_fixed", "over max(xgboost, random_forest)", min(block_ratio))
add("block_ratio_max", "exploratory", "ridge_fixed", "over max(xgboost, random_forest)", max(block_ratio))
say("PISA exploratory: block PFI of linear regression over the larger of XGBoost and random forest, per PV: %.2f to %.2f",
  min(block_ratio), max(block_ratio))

# ---- PISA exploratory: Rubin's total variance across plausible values (supplement, section F) ----

fold_pfi = fread(file.path(pisa_dir, "exploratory", "pfi_fold.csv"))[design == "full"]
half_widths = numeric()
say("PISA exploratory: Rubin's total variance T = W + (1 + 1/M) B, 95%% interval mean +- t(df) sqrt(T)")
for (group in c("METASPAM", "socioeconomic_joint_block", "ses_family")) {
  for (model in pisa_learners) {
    per_pv = fold_pfi[feature_group == group & learner == model,
      .(q = mean(importance), u = (1 / .N + 1 / (.N - 1)) * stats::var(importance), n_folds = .N), by = outcome_id]
    stopifnot(nrow(per_pv) == 10L, all(per_pv$n_folds == 5L))
    m = nrow(per_pv)
    within = mean(per_pv$u)
    between = stats::var(per_pv$q)
    total = within + (1 + 1 / m) * between
    df = (m - 1) * (1 + within / ((1 + 1 / m) * between))^2
    estimate = mean(per_pv$q)
    half = stats::qt(0.975, df) * sqrt(total)
    half_widths = c(half_widths, half)
    values = c(mean = estimate, W = within, B = between, T = total, df = df, lower = estimate - half,
      upper = estimate + half)
    for (name in names(values)) add(paste0("rubin_", name), "exploratory", model, group, values[[name]])
    say("  %-26s %-14s %.2f [%.2f, %.2f]; W %.4f, B %.4f, T %.4f, df %.0f", group, model, estimate,
      estimate - half, estimate + half, within, between, total, df)
  }
}
add("rubin_half_width_min", "exploratory", NA_character_, "all groups and learners", min(half_widths))
add("rubin_half_width_max", "exploratory", NA_character_, "all groups and learners", max(half_widths))
say("  half-widths %.2f to %.2f RMSE points", min(half_widths), max(half_widths))

# ---- PISA confirmatory: claim C2 in linear regression, per plausible value ----

confirmatory = fread(file.path(pisa_dir, "confirmatory", "pfi_fold.csv"))[design == "full"]
linear = confirmatory[learner == "ridge_fixed" & !grepl("|", features, fixed = TRUE)]
leader = linear[, .(pfi = mean(importance)), by = .(outcome_id, feature_group)][, .(leader = feature_group[which.max(pfi)]),
  by = outcome_id]
pair = dcast(linear[feature_group %in% c("METASPAM", "HOMEPOS")], outcome_id + fold ~ feature_group,
  value.var = c("importance", "monte_carlo_se", "n_permutations"))
margins = pair[, .(difference = mean(importance_METASPAM) - mean(importance_HOMEPOS),
  se = sqrt(sum(monte_carlo_se_METASPAM^2) + sum(monte_carlo_se_HOMEPOS^2)) / .N,
  df = min(n_permutations_METASPAM, n_permutations_HOMEPOS) - 1L), by = outcome_id]
margins[, threshold := stats::qt(0.975, df) * se]
margins = merge(margins, leader, by = "outcome_id")[match(pv_order(outcome_id), outcome_id)]
say("PISA confirmatory, C2 in linear regression: assessing credibility minus home possessions per PV (mean over folds)")
for (i in seq_len(nrow(margins))) {
  row = margins[i]
  add("c2_metaspam_minus_homepos", "confirmatory", "ridge_fixed", row$outcome_id, row$difference)
  add("c2_monte_carlo_se", "confirmatory", "ridge_fixed", row$outcome_id, row$se)
  add("c2_threshold_t975", "confirmatory", "ridge_fixed", row$outcome_id, row$threshold)
  add("c2_metaspam_largest", "confirmatory", "ridge_fixed", row$outcome_id, row$leader == "METASPAM")
  say("  %-8s largest %-9s difference %7.4f; SE %.4f; threshold %.4f", row$outcome_id, row$leader, row$difference,
    row$se, row$threshold)
}
n_largest = sum(margins$leader == "METASPAM")
n_within = sum(margins$leader != "METASPAM" & abs(margins$difference) <= margins$threshold)
add("c2_n_pv_metaspam_largest", "confirmatory", "ridge_fixed", value = n_largest)
add("c2_n_pv_reversal_within_mc_error", "confirmatory", "ridge_fixed", value = n_within)
add("c2_n_pv_if_within_mc_error_counted", "confirmatory", "ridge_fixed", value = n_largest + n_within)
say("  largest in %d of 10; reversals within Monte Carlo error: %d; counting them: %d of 10", n_largest, n_within,
  n_largest + n_within)

# ---- PISA confirmatory: claim C3, largest over smallest complete-family PFI across the four learners ----

family = confirmatory[feature_group == "ses_family"]
fold_ratio = family[, .(pfi = mean(importance)), by = .(learner, fold)][, .(ratio = max(pfi) / min(pfi)), by = fold]
pv_ratio = family[, .(pfi = mean(importance)), by = .(learner, outcome_id)][,
  .(ratio = max(pfi) / min(pfi)), by = outcome_id]
cell_ratio = family[, .(ratio = max(importance) / min(importance)), by = .(fold, outcome_id)]
average_ratio = family[, .(pfi = mean(importance)), by = learner][, max(pfi) / min(pfi)]
ci = corrected(fold_ratio$ratio)
add("c3_ratio_average", "confirmatory", "all four", value = average_ratio)
add("c3_ratio_pv_min", "confirmatory", "all four", value = min(pv_ratio$ratio))
add("c3_ratio_pv_max", "confirmatory", "all four", value = max(pv_ratio$ratio))
add("c3_n_pv_ratio_at_most_2", "confirmatory", "all four", value = sum(pv_ratio$ratio <= 2))
for (i in seq_len(nrow(fold_ratio))) {
  add("c3_ratio_fold", "confirmatory", "all four", paste0("fold ", fold_ratio$fold[i]), fold_ratio$ratio[i])
}
for (name in names(ci)) add(paste0("c3_ratio_fold_", name), "confirmatory", "all four", value = ci[[name]])
add("c3_ratio_fold_pv_min", "confirmatory", "all four", value = min(cell_ratio$ratio))
add("c3_ratio_fold_pv_max", "confirmatory", "all four", value = max(cell_ratio$ratio))
say("PISA confirmatory, C3: ratio %.2f on average; per PV %.2f to %.2f (%d of 10 at most 2)", average_ratio,
  min(pv_ratio$ratio), max(pv_ratio$ratio), sum(pv_ratio$ratio <= 2))
say("  per fold %.2f to %.2f, corrected interval %.2f to %.2f; per fold and PV %.3f to %.3f", min(fold_ratio$ratio),
  max(fold_ratio$ratio), ci[["lower"]], ci[["upper"]], min(cell_ratio$ratio), max(cell_ratio$ratio))

# ---- SHILD statement (b): folds in which both items exceed every other item under LOCO ----

loco = fread(file.path(shild_dir, "loco_fold_importance.csv"))
comparisons = fread(file.path(shild_dir, "loco_comparisons.csv"))
say("SHILD LOCO: folds in which A2 and D2 both exceed every other item; largest Holm-adjusted p over 50 comparisons")
for (loco_measure in c("loco", "loco_weighted", "loco_brier", "loco_brier_weighted")) {
  lead = loco[, .(both = min(get(loco_measure)[item %in% c("A2", "D2")]) >
    max(get(loco_measure)[!item %in% c("A2", "D2")])), by = .(learner, fold)]
  for (model in c("xgboost", "ridge_logistic", "random_forest")) {
    n_folds = lead[learner == model, sum(both)]
    max_p = comparisons[learner == model & measure == loco_measure, max(p_holm)]
    add("loco_n_folds_both_items_lead", "shild", model, loco_measure, n_folds)
    add("loco_max_holm_p", "shild", model, loco_measure, max_p)
    say("  %-14s %-13s %d of %d folds; max Holm p %.4f", model, loco_measure, n_folds, lead[learner == model, .N], max_p)
  }
}

# ---- SHILD statement (a): each item's excess over twice every other item (XGBoost, 20 permutations) ----
# The content of statement (a) requires the excess of A2 and of D2 over twice the PFI of every other item (50
# comparisons); the article reports the comparison with the smallest lower limit of each item.

fold_pfi = dcast(fread(file.path(shild_dir, "importance_fold_pfi.csv")), fold ~ item, value.var = "pfi")
items = setdiff(names(fold_pfi), "fold")
stopifnot(length(items) == 27L, nrow(fold_pfi) == 10L)
excess = rbindlist(lapply(c("A2", "D2"), function(j) rbindlist(lapply(setdiff(items, c("A2", "D2")), function(l) {
  ci = corrected(fold_pfi[[j]] - 2 * fold_pfi[[l]])
  data.table(item = j, versus = l, mean = ci[["mean"]], lower = ci[["lower"]], upper = ci[["upper"]])
}))))
for (j in c("A2", "D2")) {
  weakest = excess[item == j][which.min(lower)]
  key = sprintf("%s minus 2 x %s (smallest lower limit of %d comparisons)", j, weakest$versus, excess[item == j, .N])
  add("statement_a_excess_mean", "shild", "xgboost", key, weakest$mean)
  add("statement_a_excess_lower", "shild", "xgboost", key, weakest$lower)
  add("statement_a_excess_upper", "shild", "xgboost", key, weakest$upper)
  say("SHILD statement (a): %s: %.3f [%.3f, %.3f]", key, weakest$mean, weakest$lower, weakest$upper)
}
add("statement_a_comparisons_lower_above_zero", "shild", "xgboost", sprintf("of %d", nrow(excess)), sum(excess$lower > 0))
say("  lower limit above zero in %d of %d comparisons", sum(excess$lower > 0), nrow(excess))

# ---- SHILD stronger claim: self-rated health minus depressed mood per fold (XGBoost, 20 permutations) ----

differences = fread(file.path(shild_dir, "importance_fold_A2_vs_D2.csv"))
n_permutations = min(fread(file.path(shild_dir, "importance_fold_pfi.csv"))[item %in% c("A2", "D2"), n_permutations])
stopifnot(nrow(differences) == 10L, n_permutations == 20L)
reversal = differences[which.min(difference)]
fold_sd = stats::sd(differences$difference)
# Per-fold threshold t(.975, 19) x SE (two-sided .05; Step 4 of the article); Holm-adjusted threshold for the largest
# of 10 reversals at the same two-sided level, t(1 - .025/10, 19) = t(.9975, 19) x SE.
per_fold = stats::qt(0.975, n_permutations - 1L) * reversal$mc_se_difference
holm = stats::qt(1 - 0.025 / 10, n_permutations - 1L) * reversal$mc_se_difference
key = paste0("fold ", reversal$fold)
add("a2_minus_d2_n_folds_positive", "shild", "xgboost", value = sum(differences$difference > 0))
add("a2_minus_d2_reversal", "shild", "xgboost", key, reversal$difference)
add("a2_minus_d2_reversal_mc_se", "shild", "xgboost", key, reversal$mc_se_difference)
add("a2_minus_d2_threshold_t975", "shild", "xgboost", key, per_fold)
add("a2_minus_d2_threshold_holm_t9975", "shild", "xgboost", key, holm)
add("a2_minus_d2_fold_sd", "shild", "xgboost", "10 folds", fold_sd)
add("a2_minus_d2_fold_sd_over_reversal_mc_se", "shild", "xgboost", key, fold_sd / reversal$mc_se_difference)
add("a2_minus_d2_fold_sd_over_mean_mc_se", "shild", "xgboost", "10 folds", fold_sd / mean(differences$mc_se_difference))
say("SHILD A2 - D2 (XGBoost): positive in %d of 10 folds; %s: %.4f, SE %.4f", sum(differences$difference > 0), key,
  reversal$difference, reversal$mc_se_difference)
say("  per-fold threshold t(.975, 19) x SE %.4f; Holm-adjusted threshold t(.9975, 19) x SE %.4f", per_fold, holm)
say("  fold-to-fold SD %.4f = %.1f times the SE in %s (%.1f times the mean SE over folds)", fold_sd,
  fold_sd / reversal$mc_se_difference, key, fold_sd / mean(differences$mc_se_difference))

# ---- Relevance: PFI as a share of XGBoost's improvement over the model without predictors ----
# Share = PFI / (L0 - Lr), with L0 and Lr the mean held-out losses of the model without predictors and of XGBoost.
# SHILD: log loss of the G1 benchmark (source of the admission table in Supplement I) and the XGBoost PFI averaged over
# folds (20 permutations per fold). PISA exploratory: RMSE pooled within each plausible value and averaged over the 10
# values, and the XGBoost PFI averaged over plausible values (single predictors, as for the top K above).

shild_tables = file.path(root, "baseline_evidence", "STUDY_RESULTS", "SHILD", "tables")
benchmark = fread(file.path(shild_tables, "shild_g1_benchmark_summary.csv"))[measure_id == "classif.logloss"]
admission = fread(file.path(shild_tables, "shild_near_equivalence_tolerance_sensitivity.csv"))
stopifnot(all(abs(admission$mean_score - benchmark$mean[match(admission$learner_name, benchmark$learner_id)]) < 1e-12))
shild_l0 = benchmark[learner_id == "featureless", mean]
shild_lr = benchmark[learner_id == "xgboost", mean]
stopifnot(abs(mean(losses$xgboost) - shild_lr) < 1e-4)
item_means = fread(file.path(shild_dir, "importance_item_means.csv"))[order(-mean_pfi)]
stopifnot(nrow(item_means) == 27L, identical(item_means$item[1:3], c("A2", "D2", "M8")))
add("relevance_loss_featureless", "shild", "featureless", "log loss", shild_l0)
add("relevance_loss_reference", "shild", "xgboost", "log loss", shild_lr)
add("relevance_reference_improvement", "shild", "xgboost", "log loss", shild_l0 - shild_lr)
say("Relevance: PFI over XGBoost's improvement over the model without predictors")
say("  SHILD log loss %.4f -> %.4f, improvement %.4f", shild_l0, shild_lr, shild_l0 - shild_lr)
for (i in 1:3) {
  label = sprintf("rank %d: %s", i, item_means$item[i])
  share = 100 * item_means$mean_pfi[i] / (shild_l0 - shild_lr)
  add("relevance_pfi", "shild", "xgboost", label, item_means$mean_pfi[i])
  add("relevance_share_percent", "shild", "xgboost", label, share)
  say("  SHILD %-12s PFI %.4f = %.1f%% of the improvement", label, item_means$mean_pfi[i], share)
}
pooled = fread(file.path(pisa_dir, "exploratory", "pooled_rmse.csv"))[design == "full"]
stopifnot(pooled[, .N, by = learner][, all(N == 10L)])
pooled_means = pooled[, .(rmse = mean(pooled_rmse)), by = learner]
pisa_l0 = pooled_means[learner == "featureless", rmse]
pisa_lr = pooled_means[learner == "xgboost", rmse]
fold_means = fread(file.path(pisa_dir, "exploratory", "fold_rmse.csv"))[design == "full", .(rmse = mean(rmse)), by = learner]
fold_improvement = fold_means[learner == "featureless", rmse] - fold_means[learner == "xgboost", rmse]
stopifnot(abs(fold_improvement - (pisa_l0 - pisa_lr)) < 0.01)
xgb_average = single[learner == "xgboost", .(pfi = mean(mean_importance)), by = feature_group][order(-pfi)]
stopifnot(identical(xgb_average$feature_group[1:5], tops[["xgboost 5"]]), xgb_average$feature_group[1] == "METASPAM")
add("relevance_loss_featureless", "exploratory", "featureless", "pooled RMSE", pisa_l0)
add("relevance_loss_reference", "exploratory", "xgboost", "pooled RMSE", pisa_lr)
add("relevance_reference_improvement", "exploratory", "xgboost", "pooled RMSE", pisa_l0 - pisa_lr)
add("relevance_reference_improvement", "exploratory", "xgboost", "mean of fold RMSE", fold_improvement)
say("  PISA exploratory pooled RMSE %.2f -> %.2f, improvement %.2f (mean of fold RMSE: %.2f)", pisa_l0, pisa_lr,
  pisa_l0 - pisa_lr, fold_improvement)
for (i in 1:5) {
  label = sprintf("rank %d: %s", i, xgb_average$feature_group[i])
  share = 100 * xgb_average$pfi[i] / (pisa_l0 - pisa_lr)
  add("relevance_pfi", "exploratory", "xgboost", label, xgb_average$pfi[i])
  add("relevance_share_percent", "exploratory", "xgboost", label, share)
  say("  PISA  %-18s PFI %.2f = %.1f%% of the improvement", label, xgb_average$pfi[i], share)
}

# ---- PISA exploratory: selection-aware stability of the XGBoost top five across single folds ----
fold_pfi = fread(file.path(pisa_dir, "exploratory", "pfi_fold.csv"))[design == "full" & learner == "xgboost"]
fold_pfi = fold_pfi[!grepl("|", features, fixed = TRUE)]
stopifnot(uniqueN(fold_pfi$feature_group) == 41L, fold_pfi[, .N, by = .(outcome_id, fold)][, all(N == 41L)])
fold_top = fold_pfi[, {
  o = order(-importance)
  .(same = setequal(feature_group[o][1:5], tops[["xgboost 5"]]), gap = importance[o][5] - importance[o][6])
}, by = .(outcome_id, fold)]
add("top_5_recurrence_n_fold_pv", "exploratory", "xgboost", sprintf("of %d fold-by-PV analyses", nrow(fold_top)),
  sum(fold_top$same))
add("top_5_min_margin_fold_pv", "exploratory", "xgboost", "fifth minus sixth", min(fold_top$gap))
say("PISA exploratory, XGBoost top five selected anew in each fold and plausible value: %d of %d; min margin %.2f",
  sum(fold_top$same), nrow(fold_top), min(fold_top$gap))

# ---- SHILD LOCO: robustness of the comparisons to larger standard errors (Supplement M: corrected variance 23% to
# 36% of the actual variance, i.e., standard errors too small by a factor of up to about 2.1) ----
say("SHILD LOCO comparisons with inflated standard errors: largest Holm-adjusted p over the 50 comparisons")
for (factor in c(2, 2.1)) {
  inflated = comparisons[measure %in% c("loco", "loco_weighted")][, .(learner, measure, p = stats::pt(estimate / (se * factor),
    df = 9L, lower.tail = FALSE))][, .(max_p_holm = max(stats::p.adjust(p, method = "holm"))), by = .(learner, measure)]
  for (i in seq_len(nrow(inflated))) {
    add("loco_max_holm_p_inflated_se", "shild", inflated$learner[i], sprintf("%s; SE x %.1f", inflated$measure[i], factor),
      inflated$max_p_holm[i])
    say("  SE x %.1f %-14s %-13s max Holm p %.4f", factor, inflated$learner[i], inflated$measure[i], inflated$max_p_holm[i])
  }
}

# ---- SHILD: observed outcomes predicted with probability 0 (log loss set by the clipping bound of 1e-15) ----
loco_losses = fread(file.path(shild_dir, "loco_fold_losses.csv"))
stopifnot("n_outcome_at_bound" %in% names(loco_losses))
at_bound = loco_losses[, .(total = sum(n_outcome_at_bound), cells = sum(n_outcome_at_bound > 0), refits = .N), by = learner]
for (i in seq_len(nrow(at_bound))) {
  add("outcomes_at_probability_zero_total", "shild", at_bound$learner[i], sprintf("over %d refits", at_bound$refits[i]),
    at_bound$total[i])
  add("refits_with_outcome_at_probability_zero", "shild", at_bound$learner[i], sprintf("of %d refits", at_bound$refits[i]),
    at_bound$cells[i])
}
rf_full = loco_losses[learner == "random_forest" & design == "all_items"][order(fold)]
clip_loss = -log(1e-15)
floor_loss = -log(1e-3)
rf_mean = mean(rf_full$log_loss)
rf_floor = mean(rf_full$log_loss - rf_full$n_outcome_at_bound * (clip_loss - floor_loss) / rf_full$n_test)
add("rf_full_design_outcomes_at_probability_zero", "shild", "random_forest",
  paste("folds", paste(rf_full[n_outcome_at_bound > 0, fold], collapse = ", ")), sum(rf_full$n_outcome_at_bound))
add("rf_increase_per_clipped_respondent", "shild", "random_forest", "log loss of one fold",
  clip_loss / mean(rf_full$n_test))
add("rf_mean_log_loss", "shild", "random_forest", "clipping at 1e-15", rf_mean)
add("rf_mean_log_loss", "shild", "random_forest", "probabilities floored at 0.001", rf_floor)
add("retained_share_percent", "shild", "random_forest", "clipping at 1e-15", 100 * (shild_l0 - rf_mean) / (shild_l0 - shild_lr))
add("retained_share_percent", "shild", "random_forest", "probabilities floored at 0.001",
  100 * (shild_l0 - rf_floor) / (shild_l0 - shild_lr))
say("SHILD random forest: %d observed outcomes at probability 0 over %d LOCO refits (%d refits affected); full design: %d (folds %s)",
  at_bound[learner == "random_forest", total], at_bound[learner == "random_forest", refits],
  at_bound[learner == "random_forest", cells], sum(rf_full$n_outcome_at_bound),
  paste(rf_full[n_outcome_at_bound > 0, fold], collapse = ", "))
say("  one such respondent adds %.4f to its fold's log loss; mean loss %.4f (retained %.1f%%), with a floor of 0.001 %.4f (retained %.1f%%)",
  clip_loss / mean(rf_full$n_test), rf_mean, 100 * (shild_l0 - rf_mean) / (shild_l0 - shild_lr), rf_floor,
  100 * (shild_l0 - rf_floor) / (shild_l0 - shild_lr))

# ---- SHILD statement (a): conditional (subgroup) PFI of the same XGBoost fits ----
conditional = fread(file.path(shild_dir, "conditional_summary.csv"))
for (name in names(conditional)) if (is.numeric(conditional[[name]])) add(paste0("conditional_pfi_", name), "shild", "xgboost",
  value = conditional[[name]])
say("SHILD conditional subgroup PFI (XGBoost): A2 %.4f, D2 %.4f, largest other %s %.4f; per-fold ratios >= %.1f and %.1f; margin in %d of 10 folds",
  conditional$mean_A2, conditional$mean_D2, conditional$largest_other_item, conditional$largest_other_mean,
  conditional$min_ratio_A2, conditional$min_ratio_D2, conditional$folds_margin_holds)

# ---- SHILD statement (d): operating characteristics of the stability criterion ----
p_case = function(p) stats::pbinom(18L, 20L, p, lower.tail = FALSE)  # P(top pair in >= 19 of 20 repetitions)
for (p in c(0.95, 0.98, 0.99)) {
  add("stability_case_pass_probability", "shild", "local surrogate", sprintf("recurrence %.2f", p), p_case(p))
  add("stability_all_30_pass_probability", "shild", "local surrogate", sprintf("recurrence %.2f", p), p_case(p)^30)
}
add("stability_p_at_most_10_of_20_given_95", "shild", "local surrogate", "binomial", stats::pbinom(10L, 20L, 0.95))
say("SHILD statement (d): P(case passes | recurrence .95/.98/.99) = %.3f/%.3f/%.3f; all 30 pass %.4f/%.3f/%.3f; P(<= 10 of 20 | .95) = %.1e",
  p_case(0.95), p_case(0.98), p_case(0.99), p_case(0.95)^30, p_case(0.98)^30, p_case(0.99)^30, stats::pbinom(10L, 20L, 0.95))

# ---- PISA claim C2: assessing credibility versus the runner-up in linear regression, per fold ----
for (stage in c("exploratory", "confirmatory")) {
  pf = fread(file.path(pisa_dir, stage, "pfi_fold.csv"))[design == "full" & learner == "ridge_fixed" &
    !grepl("|", features, fixed = TRUE)]
  averages = pf[, .(pfi = mean(importance)), by = feature_group][order(-pfi)]
  stopifnot(averages$feature_group[1] == "METASPAM")
  runner = averages$feature_group[2]
  per_fold = dcast(pf[feature_group %in% c("METASPAM", runner), .(pfi = mean(importance)), by = .(fold, feature_group)],
    fold ~ feature_group, value.var = "pfi")
  gap = per_fold[["METASPAM"]] - per_fold[[runner]]
  ci = corrected(gap)
  for (name in names(ci)) add(paste0("c2_gap_", name), stage, "ridge_fixed", paste("METASPAM minus", runner), ci[[name]])
  add("c2_gap_n_folds_positive", stage, "ridge_fixed", paste("METASPAM minus", runner), sum(gap > 0))
  say("PISA %s, C2 in linear regression: METASPAM minus %s per fold (averaged over PVs) %s; mean %.2f, interval %.2f to %.2f; %d of %d folds positive",
    stage, runner, paste(sprintf("%.2f", gap), collapse = ", "), ci[["mean"]], ci[["lower"]], ci[["upper"]], sum(gap > 0), length(gap))
}

# ---- PISA claims C3 and C4 with MSE instead of RMSE (permuted MSE approximated by the squared mean permuted RMSE) ----
for (stage in c("exploratory", "confirmatory")) {
  pf = fread(file.path(pisa_dir, stage, "pfi_fold.csv"))[design == "full"]
  pf[, mse_importance := permuted_loss^2 - baseline_loss^2]
  family = pf[feature_group == "ses_family", .(rmse = mean(importance), mse = mean(mse_importance)), by = .(learner, outcome_id)]
  for (loss_name in c("rmse", "mse")) {
    by_pv = family[, .(ratio = max(get(loss_name)) / min(get(loss_name))), by = outcome_id]
    averaged = family[, .(value = mean(get(loss_name))), by = learner]
    add("c3_ratio_average", stage, NA_character_, loss_name, max(averaged$value) / min(averaged$value))
    add("c3_ratio_pv_max", stage, NA_character_, loss_name, max(by_pv$ratio))
    add("c3_n_pv_at_most_2", stage, NA_character_, loss_name, sum(by_pv$ratio <= 2))
    block = pf[learner == "ridge_fixed" & feature_group %in% c("socioeconomic_joint_block", "ses_family"),
      .(value = mean(if (loss_name == "rmse") importance else mse_importance)), by = .(outcome_id, feature_group)]
    c4 = dcast(block, outcome_id ~ feature_group, value.var = "value")[, socioeconomic_joint_block / ses_family]
    add("c4_ratio_pv_min", stage, "ridge_fixed", loss_name, min(c4))
    add("c4_ratio_pv_max", stage, "ridge_fixed", loss_name, max(c4))
    say("PISA %s, %s: C3 ratio largest/smallest %.2f on average, up to %.2f per PV (<= 2 in %d of 10); C4 block/family %.2f to %.2f",
      stage, toupper(loss_name), max(averaged$value) / min(averaged$value), max(by_pv$ratio), sum(by_pv$ratio <= 2), min(c4), max(c4))
  }
}

derivations = rbindlist(rows)
fwrite(derivations, file.path(out, "article_derivations.csv"))
writeLines(c("Derived numbers of the article (analysis/sensitivity/article_derivations.R); values rounded as in the text.",
  "", report), file.path(out, "article_derivations.txt"))
cat(report, sep = "\n")
