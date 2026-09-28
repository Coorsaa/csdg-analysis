# Run from the analysis repository root, or use --portable INPUT_DIRECTORY OUTPUT_DIRECTORY.
# Both modes use aggregate source tables only, without fitting any model.
args = commandArgs(trailingOnly = TRUE)
portable = length(args) > 0L && args[1L] == "--portable"
library(mlr3autoiml)
stopifnot(packageVersion("mlr3autoiml") >= "0.1.5")
if (portable) {
  stopifnot(length(args) == 3L)
  root = normalizePath(args[2L], mustWork = TRUE)
  out = args[3L]
  source_path = function(name) file.path(root, name)
} else {
  root = normalizePath(if (length(args)) args[1L] else ".")
  stopifnot(file.exists(file.path(root, "analysis", "sensitivity", "run_sensitivities.R")))
  out = file.path(root, "analysis/sensitivity/results")
  source_path = function(name) {
    if (name == "pisa_model_pfi_all_pv.csv") {
      file.path(root, "analysis/targeted/resources", name)
    } else if (grepl("PERFORMANCE_EXPLANATION", name)) {
      file.path(root, "final_docs", name)
    } else if (grepl("near_equivalence", name)) {
      study = if (startsWith(name, "shild")) "SHILD" else "PISA"
      file.path(root, "baseline_evidence/STUDY_RESULTS", study, "tables", name)
    } else {
      file.path(root, "analysis/sensitivity/inputs", name)
    }
  }
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
library(data.table)
inputs = character()
read_input = function(path) {
  assign("inputs", unique(c(inputs, path)), envir = .GlobalEnv)
  fread(path)
}
pisa = read_input(source_path("pisa_model_pfi_all_pv.csv"))
stopifnot(nrow(pisa) == 1260L, uniqueN(pisa$outcome_id) == 10L)
pisa = pisa[feature_group != "socioeconomic_joint_block"]
pisa[, pv_rank := frank(-mean_importance, ties.method = "average"), by = .(learner_name, outcome_id)]
aggregates = pisa[, .(mean_pfi = mean(mean_importance), median_pfi = median(mean_importance),
  mean_rank = mean(pv_rank), min_rank = min(pv_rank), max_rank = max(pv_rank),
  top_five_count = sum(pv_rank <= 5L), n_pv = .N), by = .(learner_name, feature_group)]
for (method in c("mean_pfi", "median_pfi", "mean_rank")) {
  aggregates[, (paste0(method, "_rank")) := frank(if (method == "mean_rank") get(method) else -get(method),
    ties.method = "average"), by = learner_name]
}
fwrite(aggregates, file.path(out, "pisa_aggregation_sensitivity.csv"))
tops = rbindlist(lapply(c("mean_pfi", "median_pfi", "mean_rank"), function(method) {
  aggregates[get(paste0(method, "_rank")) <= 5L, .(learner_name, method,
    feature_group, rank = get(paste0(method, "_rank")), top_five_count)]
}))
fwrite(tops, file.path(out, "pisa_top_five_by_aggregation.csv"))
shild = rbindlist(lapply(c("xgboost", "ridge_logistic", "random_forest"), function(m) {
  x = read_input(source_path(paste0("shild_gate_g6a__model_generalization__pfi__", m, "__summary.csv")))
  x[, learner_name := m]
  x
}))
shild = shild[!grepl("\\|", features)]
stopifnot(nrow(shild) == 81L)
fwrite(shild, file.path(out, "shild_model_pfi_source_aggregates.csv"))
all_results = list()
for (study in c("SHILD", "PISA")) {
  vals = if (study == "SHILD") shild[, .(learner_name, feature_group, importance = mean_importance)] else
    aggregates[, .(learner_name, feature_group, importance = mean_pfi)]
  grid = if (study == "SHILD") c(0, .00005, .0001, .0002, .0005, .001) else c(0, .025, .05, .10, .20, .50)
  history = read_input(source_path(paste0(study, "_PERFORMANCE_EXPLANATION_SENSITIVITY.csv")))
  for (m in setdiff(unique(vals$learner_name), "xgboost")) {
    a = csdg_importance_agreement(vals[learner_name == "xgboost", .(feature_group, importance)],
      vals[learner_name == m, .(feature_group, importance)], top_k = 5L, practical_tolerances = grid)
    res = copy(a$tolerance_sensitivity)
    res[, `:=`(study = study, learner_name = m, top_k_intersection = a$top_k$intersection,
      top_k_union = a$top_k$union, top_k_jaccard = a$top_k$jaccard)]
    old = history[learner_name == m]
    check = res[abs(practical_tolerance - old$practical_tolerance) < 1e-12]
    stopifnot(nrow(check) == 1L,
      abs(check$practical_reversal_fraction - old$practical_reversal_fraction) < 1e-10,
      check$n_comparable_pairs == old$n_comparable_pairs,
      check$n_practical_reversals == old$n_practical_reversals)
    all_results[[paste(study, m)]] = res
  }
  membership = read_input(source_path(paste0(tolower(study), "_near_equivalence_tolerance_sensitivity.csv")))
  expected = membership$distance_from_reference <= membership$tolerance_allowance + 1e-12
  stopifnot(all(expected == membership$accepted))
  fwrite(membership, file.path(out, paste0(tolower(study), "_model_admission.csv")))
}
result = rbindlist(all_results, fill = TRUE)
fwrite(result, file.path(out, "practical_tie_sensitivity.csv"))
fwrite(data.table(path = sub(paste0(root, "/"), "", inputs, fixed = TRUE),
  sha256 = vapply(inputs, digest::digest, character(1L), file = TRUE, algo = "sha256")),
  file.path(out, "input_manifest.csv"))
writeLines(capture.output(sessionInfo()), file.path(out, "sessionInfo.txt"))
print(tops)
print(result[, .(study, learner_name, practical_tolerance, n_comparable_pairs, n_practical_reversals,
  practical_reversal_fraction)])
cat("Stored model-comparison values reproduced. All sensitivity grids completed.\n")
