# Derived numbers reported in the article, recomputed from stored fold-level aggregates (no data, no refitting).
# Run from the analysis repository root:
#   Rscript analysis/sensitivity/article_derivations.R
# Inputs: analysis/shild/results/claim_extensions/ and analysis/pisa/results/ses_reanalysis/{exploratory,confirmatory}/.
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
for (loco_measure in c("loco", "loco_weighted")) {
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

# ---- SHILD stronger claim: self-rated health minus depressed mood per fold (XGBoost, 20 permutations) ----

differences = fread(file.path(shild_dir, "importance_fold_A2_vs_D2.csv"))
n_permutations = min(fread(file.path(shild_dir, "importance_fold_pfi.csv"))[item %in% c("A2", "D2"), n_permutations])
stopifnot(nrow(differences) == 10L, n_permutations == 20L)
reversal = differences[which.min(difference)]
fold_sd = stats::sd(differences$difference)
# Per-fold threshold t(.975, 19) x SE (Step 4 of the article); Holm-adjusted threshold for the largest of 10
# reversals t(.995, 19) x SE, as defined in the article.
per_fold = stats::qt(0.975, n_permutations - 1L) * reversal$mc_se_difference
holm = stats::qt(0.995, n_permutations - 1L) * reversal$mc_se_difference
key = paste0("fold ", reversal$fold)
add("a2_minus_d2_n_folds_positive", "shild", "xgboost", value = sum(differences$difference > 0))
add("a2_minus_d2_reversal", "shild", "xgboost", key, reversal$difference)
add("a2_minus_d2_reversal_mc_se", "shild", "xgboost", key, reversal$mc_se_difference)
add("a2_minus_d2_threshold_t975", "shild", "xgboost", key, per_fold)
add("a2_minus_d2_threshold_holm_t995", "shild", "xgboost", key, holm)
add("a2_minus_d2_fold_sd", "shild", "xgboost", "10 folds", fold_sd)
add("a2_minus_d2_fold_sd_over_reversal_mc_se", "shild", "xgboost", key, fold_sd / reversal$mc_se_difference)
add("a2_minus_d2_fold_sd_over_mean_mc_se", "shild", "xgboost", "10 folds", fold_sd / mean(differences$mc_se_difference))
say("SHILD A2 - D2 (XGBoost): positive in %d of 10 folds; %s: %.4f, SE %.4f", sum(differences$difference > 0), key,
  reversal$difference, reversal$mc_se_difference)
say("  per-fold threshold t(.975, 19) x SE %.4f; Holm-adjusted threshold t(.995, 19) x SE %.4f", per_fold, holm)
say("  fold-to-fold SD %.4f = %.1f times the SE in %s (%.1f times the mean SE over folds)", fold_sd,
  fold_sd / reversal$mc_se_difference, key, fold_sd / mean(differences$mc_se_difference))

derivations = rbindlist(rows)
fwrite(derivations, file.path(out, "article_derivations.csv"))
writeLines(c("Derived numbers of the article (analysis/sensitivity/article_derivations.R); values rounded as in the text.",
  "", report), file.path(out, "article_derivations.txt"))
cat(report, sep = "\n")
