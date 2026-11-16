# Claim K1 of Khan et al. (2024) in Supplement K (Table S41): self-rated health has the largest PFI, assessed for our
# random forest learner. Compares self-rated health (A2) with depressiveness (D2) fold by fold, using the stored
# per-fold PFI of that learner (10 permutations per fold) and the rules of Supplements A.2 and D.4. Run from the
# repository root:
#   Rscript analysis/sensitivity/khan_k1.R
suppressPackageStartupMessages(library(data.table))
pfi = fread("analysis/sensitivity/inputs/shild_gate_g6a__model_generalization__pfi__random_forest__per_iteration.csv")
admission = fread("analysis/sensitivity/results/shild_model_admission.csv")[scenario_order == 1]
precise = fread("analysis/shild/results/package_check/stronger_claim_precise_summary.csv")[
  n_permutations == 500 & grepl("^defaults", criterion)]
loss_xgboost = admission[learner_name == "xgboost", mean_score]
loss_forest = admission[learner_name == "random_forest", mean_score]
# The stored smallest relevant difference of XGBoost is 1% of its improvement over the model without predictors.
loss_featureless = loss_xgboost + 100 * precise$smallest_relevant_difference
smallest_relevant_difference = 0.01 * (loss_featureless - loss_forest)
folds = pfi[, .(
  largest = feature_group[which.max(importance)],
  difference_A2_minus_D2 = importance[feature_group == "A2"] - importance[feature_group == "D2"],
  mc_threshold = qt(0.975, n_permutations[1] - 1) * sqrt(sum(monte_carlo_se[feature_group %in% c("A2", "D2")]^2))
), by = fold]
folds[, D2_ahead_beyond_thresholds := difference_A2_minus_D2 < 0 & -difference_A2_minus_D2 > mc_threshold &
  -difference_A2_minus_D2 >= smallest_relevant_difference]
K = nrow(folds)
m = mean(folds$difference_A2_minus_D2)
half = qt(0.975, K - 1) * sqrt((1 / K + 1 / (K - 1)) * var(folds$difference_A2_minus_D2))
summary = data.table(loss_random_forest = loss_forest, loss_featureless = loss_featureless,
  smallest_relevant_difference = smallest_relevant_difference, folds_A2_largest = sum(folds$largest == "A2"),
  folds_D2_ahead_beyond_thresholds = paste(folds[D2_ahead_beyond_thresholds == TRUE, fold], collapse = " "),
  mean_difference = m, lower = m - half, upper = m + half)
fwrite(folds, "analysis/sensitivity/results/khan_k1_random_forest_folds.csv")
fwrite(summary, "analysis/sensitivity/results/khan_k1_random_forest_summary.csv")
print(summary)
