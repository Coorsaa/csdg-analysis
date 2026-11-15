# Numerical checks of statements in the article against the stored aggregate results.
# Run from the analysis repository root:
#   Rscript analysis/sensitivity/article_checks.R
# Public inputs only, except the optional SHILD case-level check, which reads private case files from
# CSDG_PRIVATE_DIR (outside the repository) and writes only a fold-level aggregate.

suppressPackageStartupMessages(library(data.table))
root = getwd()
stopifnot(file.exists(file.path(root, "analysis", "sensitivity", "article_checks.R")))
out = file.path(root, "analysis", "sensitivity", "results")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# PISA: ranks of assessing credibility, grouped socioeconomic PFI, and fifth-place margins per plausible value.
pisa = fread(file.path(root, "analysis", "targeted", "resources", "pisa_model_pfi_all_pv.csv"))
block = pisa[feature_group == "socioeconomic_joint_block",
  .(learner_name, outcome_id, block_pfi = mean_importance)]
single = pisa[feature_group != "socioeconomic_joint_block" & !grepl("\\|", features)]
single[, rank := frank(-mean_importance, ties.method = "first"), by = .(learner_name, outcome_id)]
credibility = single[feature_group == "METASPAM", .(learner_name, outcome_id, credibility_rank = rank)]
ses = c("ESCS", "HOMEPOS", "WEALTH")
membership = single[rank <= 5L, .(ses_in_top_five = sum(feature_group %in% ses)), by = .(learner_name, outcome_id)]
per_pv = Reduce(function(x, y) merge(x, y, by = c("learner_name", "outcome_id")), list(block, credibility, membership))
wide = dcast(per_pv, outcome_id ~ learner_name, value.var = "block_pfi")
per_pv[, block_ratio_ridge_to_model := {
  r = wide[match(outcome_id, wide$outcome_id), ridge]
  r / block_pfi
}]
fwrite(per_pv[order(learner_name, outcome_id)], file.path(out, "pisa_revised_claim_by_pv.csv"))
summary_pisa = per_pv[, .(block_min = min(block_pfi), block_max = max(block_pfi),
  credibility_best_rank = min(credibility_rank), credibility_worst_rank = max(credibility_rank),
  pv_with_all_three_ses_in_top_five = sum(ses_in_top_five == 3L),
  pv_with_any_ses_in_top_five = sum(ses_in_top_five > 0L)), by = learner_name]
fwrite(summary_pisa, file.path(out, "pisa_revised_claim_summary.csv"))
print(summary_pisa)
stopifnot(
  all(per_pv[learner_name %in% c("xgboost", "random_forest"), credibility_rank] == 1L),
  all(per_pv[learner_name == "ridge", credibility_rank] <= 2L),
  all(per_pv[learner_name == "ridge", ses_in_top_five] == 3L),
  all(per_pv[learner_name != "ridge", ses_in_top_five] == 0L),
  min(per_pv[learner_name != "ridge", block_ratio_ridge_to_model]) >= 2
)

# PISA: fifth-place changes between plausible values relative to Monte Carlo error (Table S7, G5 row).
pairs = list(random_forest = c("SCREADDIFF", "UNDREM"), ridge = c("SCREADCOMP", "ST001D01T"))
fifth = rbindlist(lapply(names(pairs), function(m) {
  x = single[learner_name == m & feature_group %in% pairs[[m]]]
  w = dcast(x, outcome_id ~ feature_group, value.var = c("mean_importance", "mean_monte_carlo_se"))
  a = pairs[[m]][1L]; b = pairs[[m]][2L]
  w[, .(learner_name = m, outcome_id, first = a, second = b,
    difference = get(paste0("mean_importance_", a)) - get(paste0("mean_importance_", b)),
    se = sqrt(get(paste0("mean_monte_carlo_se_", a))^2 + get(paste0("mean_monte_carlo_se_", b))^2) / sqrt(5))]
}))
fifth[, z := difference / se]
fwrite(fifth, file.path(out, "pisa_fifth_place_margins.csv"))

# SHILD: share of respondents with a predicted probability below .05 (calibration bins by prediction decile).
bins = fread(file.path(root, "analysis", "sensitivity", "inputs", "shild_g3_calibration_curve_descriptive_bins.csv"))
upper = as.numeric(sub(".*,\\s*([0-9.]+|Inf)\\]$", "\\1", bins$bin))
share_below = sum(bins$n[upper <= .05]) / sum(bins$n)
fwrite(data.table(threshold = .05, share_of_respondents_at_or_below = share_below),
  file.path(out, "shild_prediction_share_below_05.csv"))
cat(sprintf("SHILD share of respondents with predicted probability <= %.4f (bin edge below .05): %.3f\n",
  max(upper[upper <= .05]), share_below))

# SHILD: paired scale comparison per case and fold (optional; private case files outside the repository).
private_dir = Sys.getenv("CSDG_PRIVATE_DIR")
case_dir = file.path(private_dir, "targeted_shild")
if (nzchar(private_dir) && dir.exists(case_dir)) {
  files = list.files(case_dir, "^paired_case_.*\\.rds$", full.names = TRUE)
  metrics = rbindlist(lapply(files, function(f) readRDS(f)$result$metrics))
  w = dcast(metrics, case_label + fold + prediction_score_decile + replicate ~ surrogate_scale,
    value.var = "weighted_rmse")
  w[, difference := link - response]
  cases = w[, .(difference = median(difference)), by = .(case_label, fold, prediction_score_decile)]
  by_fold = cases[, .(n_cases = .N, n_log_odds_lower = sum(difference < 0), median_difference = median(difference)),
    by = fold][order(fold)]
  fwrite(by_fold, file.path(out, "shild_scale_paired_by_fold.csv"))
  print(by_fold)
  stopifnot(nrow(cases) == 100L, all(cases$difference < 0))
} else {
  message("CSDG_PRIVATE_DIR not set; skipping the SHILD case-level check (see results/shild_scale_paired_by_fold.csv).")
}
