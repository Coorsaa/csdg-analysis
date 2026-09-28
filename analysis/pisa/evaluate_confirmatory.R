# PISA confirmatory stage: applies the criteria fixed in analysis/pisa/confirmatory_claims.md, exactly as written,
# to the aggregates of analysis/pisa/ses_reanalysis.R. The same code is applied to the exploratory stage for
# comparison. Usage (from the repository root): Rscript analysis/pisa/evaluate_confirmatory.R

suppressPackageStartupMessages(library(data.table))

evaluate = function(stage) {
  dir = file.path("analysis/pisa/results/ses_reanalysis", stage)
  pfi = fread(file.path(dir, "pfi_summary.csv"))
  pooled = fread(file.path(dir, "pooled_rmse.csv"))[design == "full"]
  ale = unique(fread(file.path(dir, "ale.csv"))[, .(learner, outcome_id, fold, feature, q10_to_q90)])
  pvs = paste0("PV", 1:10, "READ")
  blocks = c("socioeconomic_joint_block", "ses_family", "homepos_family")

  # Admission: mean held-out RMSE over plausible values at most 5 points above XGBoost.
  rmse = pooled[, .(mean_rmse = mean(pooled_rmse)), by = learner]
  reference = rmse[learner == "xgboost", mean_rmse]
  baseline = rmse[learner == "featureless", mean_rmse]
  rmse[, `:=`(admitted = mean_rmse <= reference + 5, retained_share = (baseline - mean_rmse) / (baseline - reference))]
  admitted = rmse[learner != "featureless" & admitted == TRUE, learner]

  individual = pfi[design == "full" & !feature_group %in% blocks]
  individual[, rank := frank(-mean_importance, ties.method = "min"), by = .(learner, outcome_id)]
  results = list()

  # C1: the five named predictors have the five largest PFI values of XGBoost in each plausible value.
  named = c("METASPAM", "METASUM", "SCREADCOMP", "JOYREAD", "SCREADDIFF")
  top5 = individual[learner == "xgboost" & rank <= 5, .(set = list(sort(feature_group))), by = outcome_id]
  c1_pv = top5[, identical(unlist(set), sort(named)), by = outcome_id]$V1
  results$C1 = data.table(claim = "C1", met = all(c1_pv) && length(c1_pv) == 10L,
    observation = sprintf("named top five in %d of 10 plausible values", sum(c1_pv)))

  # C2: METASPAM largest averaged over plausible values and largest in >= 9 of 10 plausible values, each admitted learner.
  c2 = rbindlist(lapply(admitted, function(model) {
    averaged = individual[learner == model, .(importance = mean(mean_importance)), by = feature_group][order(-importance)]
    per_pv = individual[learner == model & rank == 1L, .(top = feature_group), by = outcome_id]
    data.table(learner = model, largest_on_average = averaged$feature_group[[1L]] == "METASPAM",
      gap_on_average = averaged$importance[[1L]] - averaged$importance[[2L]],
      n_pv_largest = sum(per_pv$top == "METASPAM"))
  }))
  results$C2 = data.table(claim = "C2", met = all(c2$largest_on_average & c2$n_pv_largest >= 9L),
    observation = paste(sprintf("%s: largest on average %s, largest in %d of 10", c2$learner,
      c2$largest_on_average, c2$n_pv_largest), collapse = "; "))

  # C3: complete socioeconomic family, largest at most twice the smallest across admitted learners, on average and in
  # at least 9 of 10 plausible values.
  family = pfi[design == "full" & feature_group == "ses_family" & learner %in% admitted]
  averaged = family[, .(importance = mean(mean_importance)), by = learner]
  ratio_average = max(averaged$importance) / min(averaged$importance)
  ratio_pv = family[, .(ratio = max(mean_importance) / min(mean_importance)), by = outcome_id]
  results$C3 = data.table(claim = "C3", met = ratio_average <= 2 && sum(ratio_pv$ratio <= 2) >= 9L,
    observation = sprintf("ratio largest/smallest %.2f on average; <= 2 in %d of 10 plausible values (range %.2f-%.2f)",
      ratio_average, sum(ratio_pv$ratio <= 2), min(ratio_pv$ratio), max(ratio_pv$ratio)))

  # C4: fixed-penalty ridge, partial block at least twice the complete family in each plausible value.
  ridge = dcast(pfi[design == "full" & learner == "ridge_fixed" & feature_group %in% c("socioeconomic_joint_block",
    "ses_family")], outcome_id ~ feature_group, value.var = "mean_importance")
  ridge[, ratio := socioeconomic_joint_block / ses_family]
  results$C4 = data.table(claim = "C4", met = all(ridge$ratio >= 2) && nrow(ridge) == 10L,
    observation = sprintf("ratio %.2f-%.2f across plausible values", min(ridge$ratio), max(ridge$ratio)))

  # C5: ALE of METASPAM, 90th minus 10th percentile, averaged over folds, >= 10 points in every admitted learner and PV.
  ale_pv = ale[feature == "METASPAM" & learner %in% admitted, .(difference = mean(q10_to_q90)), by = .(learner, outcome_id)]
  results$C5 = data.table(claim = "C5", met = all(ale_pv$difference >= 10) && nrow(ale_pv) == 10L * length(admitted),
    observation = sprintf("minimum %.1f points (range across learners and plausible values %.1f-%.1f)",
      min(ale_pv$difference), min(ale_pv$difference), max(ale_pv$difference)))

  list(assessment = cbind(stage = stage, rbindlist(results)), admission = cbind(stage = stage, rmse),
    details = list(c2 = cbind(stage = stage, c2), c3 = cbind(stage = stage, averaged, ratio_average = ratio_average),
      c4 = cbind(stage = stage, ridge), c5 = cbind(stage = stage, ale_pv)))
}

out = lapply(c("exploratory", "confirmatory"), evaluate)
assessment = rbindlist(lapply(out, `[[`, "assessment"))
admission = rbindlist(lapply(out, `[[`, "admission"))
dir = "analysis/pisa/results/ses_reanalysis"
fwrite(assessment, file.path(dir, "confirmatory_assessment.csv"))
fwrite(admission, file.path(dir, "confirmatory_admission.csv"))
for (name in c("c2", "c3", "c4", "c5")) {
  fwrite(rbindlist(lapply(out, function(x) x$details[[name]])), file.path(dir, paste0("confirmatory_detail_", name, ".csv")))
}
print(admission)
print(assessment)
