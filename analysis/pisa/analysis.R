#!/usr/bin/env Rscript
# PISA 2018 reading case study for the Claim-Scoped Diagnostic Gates manuscript.

# ---- Setup and dependencies ----

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/pisa/analysis.R"
}
analysis_dir = dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
source(file.path(analysis_dir, "shared", "study_helpers.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "evidence_helpers.R"), local = FALSE)
source(file.path(analysis_dir, "pisa", "prepare_data.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "output.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "output_audit.R"), local = FALSE)

.assert_canonical_environment()
.assert_packages(c(
  "data.table", "devtools", "digest", "ggplot2", "glmnet", "jsonlite", "mlr3",
  "mlr3learners", "mlr3measures", "mlr3misc", "mlr3pipelines", "png", "ranger", "rpart", "xgboost", "yaml"
))

project_root = .analysis_project_root()
package_source = .package_source_manifest(project_root)
library(mlr3autoiml)
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(mlr3)
  library(mlr3learners)
  library(mlr3misc)
  library(mlr3pipelines)
})

# ---- Study inputs and run contract ----

settings = .analysis_settings("PISA")
data_path = file.path(project_root, "data", "PISA", "pisa2018_reading_compactv2.csv")
source_manifest_path = file.path(project_root, "data", "PISA", "pisa2018_reading_compactv2_manifest.csv")
preprocess_path = file.path(project_root, "data", "PISA", "preprocess_PISA_v2.R")
codebook_path = file.path(project_root, "data", "PISA", "PISA2018_CODEBOOK.xlsx")
questionnaire_path = file.path(project_root, "data", "PISA", "STUDENTS questionnaire PISA 2018 main.pdf")
scale_manual_path = file.path(project_root, "data", "PISA", "Skalenhandbuch_PISA2018_openaccess.pdf")
review_path = file.path(project_root, "analysis", "preprocessing_review.md")
protocol_path = file.path(project_root, "analysis", "protocol.yml")
.require_files(c(
  data_path, source_manifest_path, preprocess_path, codebook_path, questionnaire_path,
  scale_manual_path, review_path, protocol_path
))

upload_expected = c(
  "tables/pisa_run_mode.csv",
  "tables/pisa_sample_flow.csv",
  "tables/pisa_country_sample.csv",
  "tables/pisa_missingness.csv",
  "tables/pisa_missingness_by_country.csv",
  "tables/pisa_langn_recode_audit.csv",
  "tables/pisa_fold_assignment.csv",
  "tables/pisa_fold_balance.csv",
  "tables/pisa_school_split_integrity.csv",
  "tables/pisa_resampling_identity.csv",
  "tables/pisa_all_pv_fold_performance.csv",
  "tables/pisa_all_pv_pooled_performance.csv",
  "tables/pisa_all_pv_descriptive_summary.csv",
  "tables/pisa_all_pv_evidence_scope.csv",
  "tables/pisa_all_pv_pfi_summary.csv",
  "tables/pisa_all_pv_pfi_descriptive_summary.csv",
  "tables/pisa_all_pv_pfi_per_iteration.csv",
  "tables/pisa_all_pv_calibration_summary.csv",
  "tables/pisa_all_pv_calibration_curve.csv",
  "tables/pisa_all_pv_calibration_descriptive_summary.csv",
  "tables/pisa_all_pv_leading_importance_recurrence.csv",
  "tables/pisa_all_pv_missingness_indicator_fold_performance.csv",
  "tables/pisa_all_pv_missingness_indicator_pooled_performance.csv",
  "tables/pisa_all_pv_missingness_indicator_comparison.csv",
  "tables/pisa_pv1_complete_case_sensitivity.csv",
  "tables/pisa_pv1_complete_case_country_counts.csv",
  "tables/pisa_pv1_complete_case_country_metrics.csv",
  "tables/pisa_pv1_technical_subgroup_metrics.csv",
  "tables/pisa_pv1_technical_subgroup_uncertainty.csv",
  "tables/pisa_pv1_technical_subgroup_contrasts.csv",
  "tables/pisa_pv1_technical_subgroup_missingness.csv",
  "tables/pisa_model_ranks.csv",
  "tables/pisa_model_importance_ranks.csv",
  "tables/pisa_model_importance_agreement.csv",
  "tables/pisa_model_importance_direct_differences.csv",
  "tables/pisa_model_importance_pairwise.csv",
  "tables/pisa_model_importance_definitions.csv",
  "tables/pisa_model_pfi_all_pv.csv",
  "tables/pisa_near_equivalence_tolerance_sensitivity.csv",
  "tables/pisa_performance_explanation_sensitivity.csv",
  "tables/pisa_claim_versions.csv",
  "tables/pisa_claim_report_card.csv",
  "tables/pisa_claim_evidence.csv",
  "tables/pisa_multiplicity_status.csv",
  "tables/pisa_multiplicity_distribution.csv",
  "tables/pisa_multiplicity_bins.csv",
  "tables/pisa_loco_all_pv_scores.csv",
  "tables/pisa_loco_descriptive_summary.csv",
  "tables/pisa_loco_country_descriptive_summary.csv",
  "tables/pisa_loco_country_pv_comparison.csv",
  "tables/pisa_loco_country_penalty_summary.csv",
  "tables/pisa_loco_country_calibration.csv",
  "tables/pisa_loco_country_calibration_summary.csv",
  "tables/pisa_pv1_matched_country_exclusion_repetitions.csv",
  "tables/pisa_pv1_matched_country_exclusion_summary.csv",
  "tables/pisa_matched_control_context_by_country_pv.csv",
  "tables/pisa_matched_control_contrasts_by_country.csv",
  "tables/pisa_loco_checkpoint_inventory.csv",
  "tables/pisa_rich_ale_1d.csv",
  "tables/pisa_rich_pdp.csv",
  "tables/pisa_rich_ice_quantiles.csv",
  "tables/pisa_rich_ice_spread.csv",
  "tables/pisa_rich_interaction_screen.csv",
  "tables/pisa_rich_ale_2d.csv",
  "tables/pisa_report_card.csv",
  "tables/pisa_claim_support.csv",
  "tables/pisa_validation.csv",
  "tables/pisa_gate_g1__performance__summary.csv",
  "tables/pisa_gate_g2__pairwise.csv",
  "tables/pisa_gate_g2__support.csv",
  "tables/pisa_g3_calibration_curve_binned.csv",
  "tables/pisa_g3_calibration_plot_source.csv",
  "tables/pisa_gate_g3a__calibration__summary.csv",
  "tables/pisa_gate_g5__pfi__summary.csv",
  "tables/pisa_gate_g5__stability__ranks.csv",
  "figures",
  "audit_bundle/pisa_csdg_audit/report_card.csv",
  "audit_bundle/pisa_csdg_audit/claim_report.csv",
  "audit_bundle/pisa_csdg_audit/gates",
  "logs/console.log",
  "logs/conditions.csv",
  "checkpoints/checkpoints.csv",
  "provenance/run_metadata.csv",
  "provenance/source_files.csv",
  "provenance/input_files.csv",
  "provenance/input_checksums.csv",
  "provenance/package_source_manifest.csv",
  "provenance/package_source_tree_hash.csv",
  "provenance/figure_catalog.csv",
  "provenance/figure_latex_metadata.csv",
  "provenance/preprocessing_review.md",
  "provenance/sessionInfo.txt"
)
run = create_run_contract(
  study = "pisa2018_reading",
  output_root = file.path(project_root, "paper_artifacts"),
  run_id = if (nzchar(settings$run_id)) settings$run_id else NULL,
  mode = if (settings$quick) "smoke_test" else "publication",
  seed = settings$seed,
  expected_artifacts = upload_expected
)
analysis_sources = c(
  pisa_analysis = normalizePath(script_path, winslash = "/", mustWork = TRUE),
  study_helpers = file.path(analysis_dir, "shared", "study_helpers.R"),
  evidence_helpers = file.path(analysis_dir, "shared", "evidence_helpers.R"),
  pisa_preparation = file.path(analysis_dir, "pisa", "prepare_data.R"),
  output = file.path(analysis_dir, "shared", "output.R"),
  output_audit = file.path(analysis_dir, "shared", "output_audit.R")
)
analysis_inputs = c(
  pisa_compact_data = data_path,
  pisa_compact_manifest = source_manifest_path,
  pisa_preprocess_source = preprocess_path,
  pisa_codebook = codebook_path,
  pisa_student_questionnaire = questionnaire_path,
  pisa_scale_manual = scale_manual_path,
  preprocessing_review = review_path,
  analysis_protocol = protocol_path
)
run_signature = .analysis_run_signature(
  package_source,
  analysis_sources,
  analysis_inputs,
  settings,
  "PISA"
)
checkpoint_directory = .checkpoint_directory(project_root, run, settings)

# ---- Data preparation and equal-country sample ----

run_logging = start_run_logging(run, failure_checkpoint = "pisa_analysis")
write_run_checkpoint(run, "pisa_analysis", "started", list(mode = settings$mode))
message("Reading PISA compact data from ", data_path)
source_data = fread(data_path, na.strings = c("", "NA", "NaN"))
pv_columns = paste0("PV", seq_len(10L), "READ")
id_columns = c("CNT", "CNTSCHID", "CNTSTUID")
.assert_expected_columns(source_data, c(id_columns, pv_columns, "LANGN"), "PISA input")
if (any(!complete.cases(source_data[, ..pv_columns]))) {
  stop("The frozen compact input contains missing plausible values.", call. = FALSE)
}
if (uniqueN(source_data$CNT) != 32L) {
  stop("Expected exactly 32 countries in the frozen compact PISA input.", call. = FALSE)
}

manifest = fread(source_manifest_path)
manifest_langn = manifest[variable_out == "LANGN"]
if (nrow(manifest_langn) != 1L) {
  stop("The compact-data manifest does not contain exactly one LANGN record.", call. = FALSE)
}
langn_999_indicator = source_data$LANGN == 999
langn_999_source = sum(langn_999_indicator, na.rm = TRUE)
langn_997_source = sum(source_data$LANGN == 997, na.rm = TRUE)
langn_998_source = sum(source_data$LANGN == 998, na.rm = TRUE)
source_data[LANGN == 999, LANGN := NA_integer_]
langn_audit = data.table(
  variable = "LANGN",
  code = 999L,
  n_recode_source = langn_999_source,
  n_code_997_source = langn_997_source,
  n_code_998_source = langn_998_source,
  source_fraction = langn_999_source / nrow(source_data),
  manifest_missing_rate = as.numeric(manifest_langn$missing_rate),
  manifest_discrepancy = as.numeric(manifest_langn$missing_rate) == 0 && langn_999_source > 0L,
  action = "Runtime recode to NA before sampling, missingness calculation, task creation, and resampling",
  verification = "codebook-verified",
  basis = "The supplied PISA codebook labels LANGN 999 as missing; the compact manifest encoded it as observed"
)
if (!langn_audit$manifest_discrepancy) {
  stop("Expected the prespecified LANGN=999 manifest discrepancy, but it was not observed.", call. = FALSE)
}
if (langn_997_source != 0L || langn_998_source != 0L) {
  stop("Unexpected LANGN 997/998 codes were found; revise the prespecified recode audit.", call. = FALSE)
}

source_n = nrow(source_data)
source_country_n = uniqueN(source_data$CNT)
set.seed(settings$seed)
sample_index = source_data[, {
  selected = sample.int(.N, settings$n_per_country, replace = FALSE)
  .I[selected]
}, by = CNT]$V1
dt = copy(source_data[sample_index])
setorder(dt, CNT, CNTSTUID)
langn_audit[, `:=`(
  n_recode_analytic = sum(langn_999_indicator[sample_index], na.rm = TRUE),
  analytic_fraction = sum(langn_999_indicator[sample_index], na.rm = TRUE) / length(sample_index)
)]
country_sample = dt[, .(
  n = .N,
  n_schools = uniqueN(paste(CNT, CNTSCHID, sep = "::")),
  langn_missing = sum(is.na(LANGN))
), by = CNT][order(CNT)]
if (nrow(country_sample) != 32L || any(country_sample$n != settings$n_per_country)) {
  stop("The PISA equal-country sample is not exactly 32 countries by the requested row count.", call. = FALSE)
}
if (!settings$quick && any(country_sample$n != 1500L)) {
  stop("Publication mode requires exactly 1,500 students from each of 32 countries.", call. = FALSE)
}

predictor_names = setdiff(names(dt), c(id_columns, pv_columns))
if (length(predictor_names) != 41L) {
  stop("Expected exactly 41 PISA predictors; found ", length(predictor_names), ".", call. = FALSE)
}
dt[, student_id := paste(CNT, CNTSTUID, sep = "::")]
dt[, audit_country := factor(CNT)]
dt[, audit_school := paste(CNT, CNTSCHID, sep = "::")]
dt[, audit_gender := factor(as.character(ST004D01T))]
dt[, audit_immigration := factor(as.character(IMMIG))]
audit_columns = c("audit_country", "audit_school", "audit_gender", "audit_immigration")
categorical_predictors = intersect(
  c("ST004D01T", "ST001D01T", "IMMIG", "LANGN", "ISCEDD", "ISCEDL", "ISCEDO"),
  predictor_names
)
for (column in categorical_predictors) {
  set(dt, j = column, value = factor(dt[[column]]))
}
missingness = .missingness_table(dt, predictor_names, "equal-country sample after LANGN recode")
if (missingness[variable == "LANGN", n_missing] <= 0L) {
  stop("LANGN runtime recoding did not propagate into the sampled missingness table.", call. = FALSE)
}
missingness_by_country = rbindlist(lapply(predictor_names, function(variable) {
  table = dt[, .(
    n_country = .N,
    n_missing = sum(is.na(get(variable))),
    missing_fraction = mean(is.na(get(variable)))
  ), by = CNT]
  table[, variable := variable]
  setcolorder(table, c("CNT", "variable", "n_country", "n_missing", "missing_fraction"))
  table
}))
missing_indicator_predictors = missingness[n_missing > 0L, variable]
missing_indicator_names = paste0("missing__", missing_indicator_predictors)

# ---- Plausible-value tasks and school-clustered splits ----

make_task = function(pv_column) {
  backend = dt[, c(predictor_names, audit_columns), with = FALSE]
  backend[, reading := dt[[pv_column]]]
  task = TaskRegr$new(paste0("pisa2018_", pv_column), backend = backend, target = "reading")
  for (column in audit_columns) {
    task$set_col_roles(column, remove_from = "feature")
  }
  if (!setequal(task$feature_names, predictor_names)) {
    stop("Feature-role validation failed for ", pv_column, ".", call. = FALSE)
  }
  task
}
tasks = lapply(pv_columns, make_task)
names(tasks) = pv_columns
primary_task = tasks[[1L]]
for (task in tasks[-1L]) {
  if (!identical(task$row_ids, primary_task$row_ids) ||
      !setequal(task$feature_names, primary_task$feature_names)) {
    stop("Plausible-value tasks are not row- and feature-aligned.", call. = FALSE)
  }
}
if (any(c(id_columns, audit_columns, "student_id") %in% primary_task$feature_names)) {
  stop("A PISA identifier or audit-only variable leaked into model features.", call. = FALSE)
}
make_missing_indicator_task = function(pv_column) {
  backend = dt[, c(predictor_names, audit_columns), with = FALSE]
  for (index in seq_along(missing_indicator_predictors)) {
    set(
      backend,
      j = missing_indicator_names[[index]],
      value = as.integer(is.na(dt[[missing_indicator_predictors[[index]]]]))
    )
  }
  backend[, reading := dt[[pv_column]]]
  task = TaskRegr$new(paste0("pisa2018_missing_indicator_", pv_column), backend = backend, target = "reading")
  for (column in audit_columns) {
    task$set_col_roles(column, remove_from = "feature")
  }
  expected = c(predictor_names, missing_indicator_names)
  if (!setequal(task$feature_names, expected)) {
    stop("Missingness-indicator feature-role validation failed for ", pv_column, ".", call. = FALSE)
  }
  task
}
missing_indicator_tasks = lapply(pv_columns, make_missing_indicator_task)
names(missing_indicator_tasks) = pv_columns
if (!length(missing_indicator_names) || any(vapply(missing_indicator_tasks, function(task) {
  !identical(task$row_ids, primary_task$row_ids)
}, logical(1L)))) {
  stop("PISA missingness-indicator tasks are incomplete or row-misaligned.", call. = FALSE)
}

grouped = csdg_grouped_resampling(
  primary_task,
  group = dt$audit_school,
  folds = settings$folds,
  repeats = 1L,
  strata = dt$audit_country,
  seed = settings$seed
)
fold_assignment = .grouped_fold_assignment(
  grouped,
  data.table(row_id = primary_task$row_ids, student_id = dt$student_id)
)
.assert_school_split_integrity(fold_assignment)
if (nrow(fold_assignment) != primary_task$nrow || anyDuplicated(fold_assignment$row_id)) {
  stop("The PISA fold assignment is not one-to-one with sampled students.", call. = FALSE)
}
country_fold = fold_assignment[, .N, by = .(country = stratum, fold)]
if (nrow(country_fold) != 32L * settings$folds || any(country_fold$N == 0L)) {
  stop("Every PISA country must appear in every assessment fold.", call. = FALSE)
}
fold_balance = fold_assignment[, .(
  n_rows = .N,
  n_schools = uniqueN(school_id),
  n_countries = uniqueN(stratum)
), by = .(repetition, fold)]
load_ratio = max(fold_balance$n_rows) / min(fold_balance$n_rows)
if (!is.finite(load_ratio) || load_ratio > 1.35) {
  stop("PISA fold row loads are not reasonably balanced; max/min ratio = ", load_ratio, ".", call. = FALSE)
}
school_integrity = fold_assignment[, .(
  n_folds = uniqueN(fold),
  n_rows = .N,
  country = unique(stratum)
), by = .(repetition, school_id)]
school_integrity[, crosses_fold := n_folds != 1L]
if (any(school_integrity$crosses_fold)) {
  stop("At least one school crosses a train/assessment partition.", call. = FALSE)
}
fold_by_row = fold_assignment[match(primary_task$row_ids, row_id), fold]

# ---- Learners, measures, and feature groups ----

focal = .make_pipeline_learner(
  lrn(
    "regr.xgboost",
    nrounds = settings$xgb_rounds,
    max_depth = 3L,
    eta = 0.05,
    base_score = 500,
    nthread = settings$threads,
    subsample = 1,
    colsample_bytree = 1
  ),
  "pisa_xgboost"
)
ridge = .make_pipeline_learner(
  lrn("regr.glmnet", alpha = 0, lambda = 0.10),
  "pisa_ridge",
  scale_numeric = TRUE
)
random_forest = .make_pipeline_learner(
  lrn(
    "regr.ranger",
    num.trees = settings$forest_trees,
    min.node.size = 20L,
    num.threads = settings$threads,
    seed = settings$seed
  ),
  "pisa_ranger"
)
decision_tree = .make_pipeline_learner(
  lrn("regr.rpart", maxdepth = 6L, minsplit = if (settings$quick) 20L else 100L),
  "pisa_rpart"
)
featureless = lrn("regr.featureless")
featureless$id = "pisa_featureless"
candidates = list(
  xgboost = focal,
  ridge = ridge,
  random_forest = random_forest,
  decision_tree = decision_tree,
  featureless = featureless
)
measures = msrs(c("regr.rmse", "regr.mae", "regr.rsq"))
feature_groups = setNames(lapply(predictor_names, function(feature) feature), predictor_names)
school_level_groups = intersect(
  names(feature_groups),
  manifest[level == "School", variable_out]
)
if (!identical(sort(school_level_groups), sort(grep("^SCH_", predictor_names, value = TRUE)))) {
  stop("School-level PFI groups do not match the source manifest and predictor names.", call. = FALSE)
}
school_constancy = dt[, lapply(.SD, uniqueN), by = audit_school, .SDcols = school_level_groups]
if (any(as.matrix(school_constancy[, ..school_level_groups]) != 1L)) {
  stop("A declared school-level PFI group varies within at least one school.", call. = FALSE)
}
socioeconomic_joint_block = c("ESCS", "HISEI", "HOMEPOS")
if (!all(socioeconomic_joint_block %in% predictor_names)) {
  stop("The prespecified ESCS-HISEI-HOMEPOS joint PFI block is incomplete.", call. = FALSE)
}
feature_groups$socioeconomic_joint_block = socioeconomic_joint_block

# ---- Focal claim, measurement, and diagnostic configuration ----

pisa_claim_versions = .analysis_pisa_claim_registry()
pisa_c0 = pisa_claim_versions[claim_id == "PISA-C0"]
if (nrow(pisa_c0) != 1L) {
  stop("The canonical PISA-C0 claim is missing or duplicated.", call. = FALSE)
}
claim = csdg_claim(
  id = pisa_c0$claim_id,
  statement = pisa_c0$statement,
  claim_type = c("predictive_performance", "calibration", "global_explanation"),
  target = pisa_c0$target,
  semantics = pisa_c0$semantics,
  unit = "sampled student",
  population = pisa_c0$analytic_distribution,
  analytic_distribution = pisa_c0$analytic_distribution,
  model_scope = pisa_c0$model_scope,
  setting_scope = "analytic_sample",
  scientific_use = pisa_c0$scientific_use,
  explanation_design = pisa_c0$explanation_design,
  claim_level = "substantive",
  claim_version = "C0",
  revision_relation = pisa_c0$revision_relation,
  confirmatory = FALSE,
  notes = paste(
    "Alternative models, technical subgroups, and observed-country exclusion refits are boundary and sensitivity",
    "analyses outside the focal claim. No causal, country-ranking,",
    "design-weighted population, individual-decision, new-country, or new-cycle claim is made."
  )
)
measurement = csdg_measurement(
  outcome = pv_columns,
  predictors = predictor_names,
  data_source = "Supplied compact PISA 2018 analysis CSV and lineage manifest",
  sample_definition = paste("Seeded equal-country sample of", settings$n_per_country, "students per country"),
  unit = "student",
  outcome_scale = paste(
    "Ten operational reading plausible-value draws; these are multiple imputations of a latent proficiency",
    "distribution, not ten observed scores and not one error-free latent reading variable"
  ),
  missingness = list(
    primary = "Fold-contained median/mode imputation",
    sensitivity = "Complete cases under the retained school-clustered fold allocation",
    LANGN = "Code 999 recoded to NA at runtime before task construction"
  ),
  preprocessing = list(
    description = paste(
      "Training-fold median/mode imputation, one-hot encoding, constant-column removal,",
      "training-fold scaling for ridge only, and a prespecified XGBoost base score of 500 on the",
      "PISA scale"
    ),
    split_policy = "Country-stratified school-clustered load-balanced folds shared by all PV tasks"
  ),
  verification = list(
    status = "verified_by_responsible_coauthors",
    artifact = "provenance/preprocessing_review.md",
    notes = paste(
      "Group-level co-author verification dated 2026-09-09; this is not an independent reconstruction of the",
      "compact source."
    )
  ),
  audit_variables = audit_columns,
  weights = "No final or replicate survey weights are present in the compact file",
  clusters = "Country-qualified school identifier",
  notes = paste(
    "Country, school, student identifiers, and audit duplicates are excluded from model features.",
    "All ten plausible values are analyzed separately on aligned splits; no student-level plausible-value average",
    "or generic Rubin rule is applied to cross-validated predictive statistics."
  )
)
explanation = csdg_explanation(
  method_ids = c("ale", "ice", "pdp", "pfi", "other"),
  scope = "global",
  target = "Predicted operational reading plausible-value draw",
  feature_groups = feature_groups,
  perturbation = list(
    type = "held-out within-country PFI with coherent school-level perturbation plus empirical-interval ALE",
    distribution = "constructed equal-country analytic distribution"
  ),
  aggregation = paste(
    "Held-out PFI is computed for every plausible value and aggregated on its value scale before ranking;",
    "full-fit ALE and ICE remain PV1-only diagnostic illustrations"
  ),
  notes = "ALE, PDP, ICE, H, and two-dimensional ALE retain distinct estimand labels in source tables."
)
config = csdg_config(
  seed = settings$seed,
  resampling = list(folds = settings$folds, repeats = 1L, store_models = TRUE),
  performance = list(
    measures = c("regr.rmse", "regr.mae", "regr.rsq"),
    primary = "regr.rmse",
    baseline = TRUE
  ),
  dependence = list(max_levels = 50L),
  calibration = list(bins = 10L),
  stability = list(
    pfi_repetitions = settings$pfi_repetitions,
    loss = "rmse",
    top_k = 5L,
    min_top_k_overlap = 0.40
  ),
  generalization = list(
    rashomon_tolerance_absolute = 5,
    rashomon_tolerance_relative = 0
  ),
  subgroup = list(min_n = if (settings$quick) 20L else 200L, thresholds = 0.5),
  export = list(include_models = FALSE, include_predictions = FALSE)
)
subgroup = data.table(
  row_id = primary_task$row_ids,
  audit_gender = dt$audit_gender,
  audit_immigration = dt$audit_immigration
)

# ---- Primary claim-scoped diagnostic audit ----

message("Running the primary PV1 CSDG audit")
result = csdg_audit(
  task = primary_task,
  learner = focal,
  claim = claim,
  measurement = measurement,
  explanation = explanation,
  config = config,
  resampling = grouped,
  pfi_strata = dt$audit_country,
  pfi_cluster = dt$audit_school,
  pfi_cluster_level_groups = school_level_groups
)
dependence = .analysis_pisa_dependence_evidence(result$artifacts$dependence)
result$artifacts$dependence = dependence
result$gates$G2$evidence = dependence
result$gates$G2$limitations = dependence$limitations
report_card = csdg_report_card(result)
claim_support = csdg_claim_report(result)
gate_errors = report_card[required == TRUE & status == "error"]
if (nrow(gate_errors)) {
  stop("Required PISA gate error(s): ", paste(gate_errors$gate_id, collapse = ", "), call. = FALSE)
}

# ---- Model-class and subgroup boundary analyses ----

checkpoint_inventory_state = new.env(parent = emptyenv())
checkpoint_inventory_state$items = list()
add_checkpoint = function(name, signature, kind, outcome_id, held_out_group = NA_character_) {
  checkpoint_inventory_state$items[[length(checkpoint_inventory_state$items) + 1L]] = data.table(
    checkpoint = name,
    signature = signature,
    kind = kind,
    outcome_id = outcome_id,
    held_out_group = held_out_group,
    checkpoint_root = checkpoint_directory
  )
}

message("Running the separate PV1 model-class and subgroup boundary diagnostics")
rashomon_checkpoint = .run_checkpoint(
  checkpoint_directory,
  "pisa_pv1_model_sensitivity",
  run_signature,
  list(stage = "technical_model_sensitivity", outcome = "PV1READ"),
  function() {
    current_rashomon = csdg_rashomon(
      primary_task,
      candidates,
      grouped,
      primary_measure = "regr.rmse",
      tolerance_absolute = config$generalization$rashomon_tolerance_absolute,
      tolerance_relative = config$generalization$rashomon_tolerance_relative,
      seed = settings$seed + 2000000L,
      store_models = TRUE
    )
    accepted = current_rashomon$candidates[accepted == TRUE, learner_name]
    model_pfi = lapply(seq_along(accepted), function(index) {
      learner_name = accepted[[index]]
      csdg_fold_pfi(
        current_rashomon$resamples[[learner_name]],
        task = primary_task,
        feature_groups = feature_groups,
        loss = "rmse",
        repetitions = settings$pfi_repetitions,
        strata = dt$audit_country,
        cluster = dt$audit_school,
        cluster_level_groups = school_level_groups,
        batch_size = 8L,
        seed = settings$seed + 2100000L + index * 10000L
      )
    })
    names(model_pfi) = accepted
    list(
      rashomon = current_rashomon,
      pfi = model_pfi,
      agreement = csdg_rashomon_agreement(
        current_rashomon,
        model_pfi,
        top_k = config$stability$top_k
      )
    )
  }
)
rashomon_boundary = rashomon_checkpoint$value
rashomon = rashomon_boundary$rashomon
focal_boundary_rmse = rashomon$candidates[learner_name == "xgboost", mean_score]
baseline_boundary_rmse = rashomon$candidates[learner_name == "featureless", mean_score]
if (length(focal_boundary_rmse) != 1L || length(baseline_boundary_rmse) != 1L ||
    !is.finite(focal_boundary_rmse) || !is.finite(baseline_boundary_rmse) ||
    focal_boundary_rmse >= baseline_boundary_rmse) {
  stop("The focal PISA learner did not outperform the featureless baseline in held-out PV1 RMSE.", call. = FALSE)
}
model_importance_ranks = copy(rashomon_boundary$agreement$ranks)
model_importance_ranks[, interpretation := paste(
  "Held-out PV1 importance rank within each near-equivalent model; technical model-sensitivity",
  "evidence outside the focal-model claim"
)]
focal_model_accepted = rashomon$candidates[learner_name == "xgboost", accepted]
if (length(focal_model_accepted) != 1L || is.na(focal_model_accepted)) {
  stop("The PISA model-sensitivity table has no unique focal-model status.", call. = FALSE)
}
performance_explanation_sensitivity = if (focal_model_accepted) {
  csdg_explanation_sensitivity(
    rashomon,
    model_importance_ranks,
    focal_learner = "xgboost",
    top_k = config$stability$top_k
  )
} else {
  data.table(
    status = "not_estimable",
    reason = paste(
      "The focal XGBoost model is outside the prespecified held-out PV1 RMSE tolerance;",
      "a focal-relative explanation comparison is therefore undefined."
    ),
    focal_learner = "xgboost",
    focal_model_accepted = FALSE,
    primary_measure = rashomon$primary_measure,
    best_score = rashomon$best_score,
    acceptance_limit = rashomon$acceptance_limit,
    uncertainty_scope = "Status record only; no confidence interval or hypothesis test."
  )
}
model_names = sort(unique(model_importance_ranks$learner_name))
model_pairs = if (length(model_names) >= 2L) combn(model_names, 2L, simplify = FALSE) else list()
kendall_agreement = if (length(model_pairs)) {
  rbindlist(lapply(model_pairs, function(pair) {
    left = model_importance_ranks[learner_name == pair[[1L]], .(feature_group, rank_1 = rank)]
    right = model_importance_ranks[learner_name == pair[[2L]], .(feature_group, rank_2 = rank)]
    joined = merge(left, right, by = "feature_group")
    non_focal_learner = setdiff(pair, "xgboost")
    kendall_tau = if (focal_model_accepted && length(non_focal_learner) == 1L) {
      performance_explanation_sensitivity[
        learner_name == non_focal_learner,
        kendall_tau_vs_focal
      ]
    } else {
      cor(joined$rank_1, joined$rank_2, method = "kendall")
    }
    data.table(
      learner_1 = pair[[1L]],
      learner_2 = pair[[2L]],
      kendall_tau_all_groups = kendall_tau
    )
  }))
} else {
  data.table(learner_1 = character(), learner_2 = character(), kendall_tau_all_groups = numeric())
}
model_importance_agreement = merge(
  copy(rashomon_boundary$agreement$pairwise_top_k),
  kendall_agreement,
  by = c("learner_1", "learner_2"),
  all = TRUE
)
model_importance_agreement[, interpretation := paste(
  "Pairwise top-k Jaccard agreement for near-equivalent PV1 models; descriptive,",
  "not a hypothesis test"
)]
add_checkpoint(
  "pisa_pv1_model_sensitivity",
  rashomon_checkpoint$signature,
  "technical_model_sensitivity",
  "PV1READ"
)
technical_subgroup_audit = csdg_subgroup_metrics(
  result$artifacts$oof,
  subgroup = subgroup,
  min_n = if (settings$quick) 20L else 200L,
  cluster = data.table(
    row_id = primary_task$row_ids,
    school = dt$audit_school
  ),
  bootstrap_strata = data.table(
    row_id = primary_task$row_ids,
    country = dt$audit_country
  ),
  bootstrap_repetitions = if (settings$quick) 100L else 1000L,
  seed = settings$seed + 2200000L
)
technical_subgroups = technical_subgroup_audit$metrics
technical_subgroups[, interpretation := paste(
  "PV1 focal-model OOF boundary diagnostic; not a subgroup comparison claim,",
  "and no group-specific models were fitted"
)]
technical_subgroup_missingness = .analysis_pisa_subgroup_missingness(subgroup)
public_subgroup_tables = .analysis_pisa_public_subgroup_tables(
  technical_subgroup_audit,
  technical_subgroup_missingness
)

# ---- Plausible-value prediction and importance sensitivity ----

message("Running aligned focal OOF models and held-out PFI for all ten plausible values")
focal_oof = vector("list", length(tasks))
all_pfi = vector("list", length(tasks))
for (pv_index in seq_along(tasks)) {
  pv = pv_columns[[pv_index]]
  oof_checkpoint = .run_checkpoint(
    checkpoint_directory,
    paste0("pisa_focal_oof_", pv),
    run_signature,
    list(stage = "focal_oof", outcome = pv),
    function() {
      if (pv_index == 1L) {
        result$artifacts$oof
      } else {
        csdg_resample(
          tasks[[pv_index]],
          focal$clone(deep = TRUE),
          .copy_resampling(grouped, tasks[[pv_index]]),
          measures = measures,
          store_models = TRUE,
          seed = settings$seed
        )
      }
    }
  )
  focal_oof[[pv_index]] = oof_checkpoint$value
  add_checkpoint(paste0("pisa_focal_oof_", pv), oof_checkpoint$signature, "focal_oof", pv)

  pfi_checkpoint = .run_checkpoint(
    checkpoint_directory,
    paste0("pisa_heldout_pfi_", pv),
    run_signature,
    list(stage = "heldout_pfi", outcome = pv, repetitions = settings$pfi_repetitions),
    function() {
      if (pv_index == 1L) {
        result$artifacts$pfi
      } else {
        csdg_fold_pfi(
          focal_oof[[pv_index]],
          task = tasks[[pv_index]],
          feature_groups = feature_groups,
          loss = "rmse",
          repetitions = settings$pfi_repetitions,
          strata = dt$audit_country,
          cluster = dt$audit_school,
          cluster_level_groups = school_level_groups,
          batch_size = 8L,
          seed = settings$seed + 1000000L + pv_index * 10000L
        )
      }
    }
  )
  all_pfi[[pv_index]] = pfi_checkpoint$value
  add_checkpoint(paste0("pisa_heldout_pfi_", pv), pfi_checkpoint$signature, "heldout_pfi", pv)
  write_run_checkpoint(run, paste0("pisa_focal_", pv), "completed")
}
names(focal_oof) = pv_columns
names(all_pfi) = pv_columns

message("Running the all-PV missingness-indicator sensitivity on the same school-clustered folds")
missing_indicator_oof = vector("list", length(pv_columns))
for (pv_index in seq_along(pv_columns)) {
  pv = pv_columns[[pv_index]]
  checkpoint_name = paste0("pisa_missing_indicator_", pv)
  checkpoint = .run_checkpoint(
    checkpoint_directory,
    checkpoint_name,
    run_signature,
    list(
      stage = "missingness_indicator_sensitivity",
      outcome = pv,
      indicators = missing_indicator_names
    ),
    function() {
      csdg_resample(
        missing_indicator_tasks[[pv]],
        focal$clone(deep = TRUE),
        .copy_resampling(grouped, missing_indicator_tasks[[pv]]),
        measures = measures,
        store_models = FALSE,
        seed = settings$seed + 1100000L + pv_index * 10000L
      )
    }
  )
  missing_indicator_oof[[pv_index]] = checkpoint$value
  add_checkpoint(checkpoint_name, checkpoint$signature, "missingness_indicator_sensitivity", pv)
}
names(missing_indicator_oof) = pv_columns

# ---- Calibration across plausible values ----

message("Estimating school-within-country bootstrap calibration intervals for all plausible values")
school_by_row = stats::setNames(as.character(dt$audit_school), primary_task$row_ids)
country_by_row = stats::setNames(as.character(dt$audit_country), primary_task$row_ids)
all_calibration = vector("list", length(pv_columns))
for (pv_index in seq_along(pv_columns)) {
  pv = pv_columns[[pv_index]]
  calibration_checkpoint = .run_checkpoint(
    checkpoint_directory,
    paste0("pisa_calibration_", pv),
    run_signature,
    list(
      stage = "calibration_bootstrap",
      outcome = pv,
      repetitions = settings$calibration_bootstrap_replicates,
      workers = settings$threads,
      curve_method = "regression_spline",
      spline_df = 4L,
      resampling_unit = "school_within_country"
    ),
    function() {
      current = csdg_calibration_bootstrap(
        focal_oof[[pv]],
        task_type = "regr",
        bins = 10L,
        cluster = school_by_row,
        strata = country_by_row,
        repetitions = settings$calibration_bootstrap_replicates,
        confidence = 0.95,
        curve_method = "regression_spline",
        spline_df = 4L,
        seed = settings$seed + 1200000L + pv_index * 10000L,
        workers = settings$threads
      )
      current$bootstrap_summary = NULL
      current$bins = NULL
      .analysis_standardize_calibration_metric(current)
    }
  )
  all_calibration[[pv_index]] = calibration_checkpoint$value
  add_checkpoint(
    paste0("pisa_calibration_", pv),
    calibration_checkpoint$signature,
    "calibration_bootstrap",
    pv
  )
}
names(all_calibration) = pv_columns
calibration_summary = rbindlist(lapply(pv_columns, function(pv) {
  table = copy(all_calibration[[pv]]$summary)
  table[, outcome_id := pv]
  table
}))
calibration_curve = rbindlist(lapply(pv_columns, function(pv) {
  table = copy(all_calibration[[pv]]$curve)
  table[, outcome_id := pv]
  table
}))
calibration_descriptive = calibration_summary[, .(
  n_plausible_values = .N,
  mean_estimate = mean(estimate, na.rm = TRUE),
  minimum_estimate = min(estimate, na.rm = TRUE),
  maximum_estimate = max(estimate, na.rm = TRUE),
  minimum_interval_endpoint = min(lower, na.rm = TRUE),
  maximum_interval_endpoint = max(upper, na.rm = TRUE),
  interpretation = paste(
    "Estimates are reported separately for ten aligned plausible values.",
    "Across-PV ranges are descriptive and are not Rubin-pooled confidence intervals."
  )
), by = estimand]
if (any(calibration_descriptive$n_plausible_values != 10L) ||
    any(calibration_curve[, uniqueN(outcome_id)] != 10L)) {
  stop("PISA calibration evidence is incomplete across the ten plausible values.", call. = FALSE)
}

# ---- Common-split model benchmark across plausible values ----

message("Running all learner/PV combinations on the same school-clustered splits")
benchmark_oof = list(xgboost = focal_oof)
for (learner_id in setdiff(names(candidates), "xgboost")) {
  learner_results = vector("list", length(tasks))
  for (pv_index in seq_along(tasks)) {
    pv = pv_columns[[pv_index]]
    checkpoint_name = paste0("pisa_benchmark_", learner_id, "_", pv)
    benchmark_checkpoint = .run_checkpoint(
      checkpoint_directory,
      checkpoint_name,
      run_signature,
      list(stage = "benchmark", learner = learner_id, outcome = pv),
      function() {
        csdg_resample(
          tasks[[pv_index]],
          candidates[[learner_id]]$clone(deep = TRUE),
          .copy_resampling(grouped, tasks[[pv_index]]),
          measures = measures,
          store_models = TRUE,
          seed = settings$seed + match(learner_id, names(candidates)) * 100000L
        )
      }
    )
    learner_results[[pv_index]] = benchmark_checkpoint$value
    add_checkpoint(checkpoint_name, benchmark_checkpoint$signature, "benchmark", pv)
  }
  names(learner_results) = pv_columns
  benchmark_oof[[learner_id]] = learner_results
  write_run_checkpoint(run, paste0("pisa_benchmark_", learner_id), "completed")
}

reference_train_sets = lapply(result$artifacts$oof$train_sets, sort)
reference_test_sets = lapply(result$artifacts$oof$test_sets, sort)
split_identity = function(oof) {
  train_sets = lapply(oof$train_sets, sort)
  test_sets = lapply(oof$test_sets, sort)
  c(
    train_identical = identical(train_sets, reference_train_sets),
    test_identical = identical(test_sets, reference_test_sets)
  )
}
resampling_identity = rbindlist(lapply(names(benchmark_oof), function(learner_id) {
  rbindlist(lapply(pv_columns, function(pv) {
    identity = split_identity(benchmark_oof[[learner_id]][[pv]])
    data.table(
      analysis_role = "all-PV benchmark",
      learner_id = learner_id,
      outcome_id = pv,
      train_identical = identity[["train_identical"]],
      test_identical = identity[["test_identical"]]
    )
  }))
}))
missing_indicator_identity = rbindlist(lapply(pv_columns, function(pv) {
  identity = split_identity(missing_indicator_oof[[pv]])
  data.table(
    analysis_role = "all-PV missingness-indicator sensitivity",
    learner_id = "xgboost_missing_indicators",
    outcome_id = pv,
    train_identical = identity[["train_identical"]],
    test_identical = identity[["test_identical"]]
  )
}))
rashomon_identity = rbindlist(lapply(names(rashomon$resamples), function(learner_id) {
  identity = split_identity(rashomon$resamples[[learner_id]])
  data.table(
    analysis_role = "PV1 technical model sensitivity",
    learner_id = learner_id,
    outcome_id = "PV1READ",
    train_identical = identity[["train_identical"]],
    test_identical = identity[["test_identical"]]
  )
}))
resampling_identity = rbindlist(list(resampling_identity, missing_indicator_identity, rashomon_identity))
if (any(!resampling_identity$train_identical) || any(!resampling_identity$test_identical)) {
  stop("At least one PISA PV or learner did not use the exact prespecified train/test sets.", call. = FALSE)
}

fold_performance = rbindlist(lapply(names(benchmark_oof), function(learner_id) {
  rbindlist(lapply(pv_columns, function(pv) {
    .fold_performance_long(benchmark_oof[[learner_id]][[pv]], pv, learner_id)
  }))
}))
pooled_performance = rbindlist(lapply(names(benchmark_oof), function(learner_id) {
  rbindlist(lapply(pv_columns, function(pv) {
    .pooled_regression_performance(benchmark_oof[[learner_id]][[pv]], pv, learner_id)
  }))
}))
missing_indicator_fold_performance = rbindlist(lapply(pv_columns, function(pv) {
  .fold_performance_long(missing_indicator_oof[[pv]], pv, "xgboost_missing_indicators")
}))
missing_indicator_fold_performance[, interpretation := paste(
  "Same-fold XGBoost sensitivity with one pre-imputation missingness indicator for every incomplete predictor;",
  "fold values are descriptive and not independent inferential observations."
)]
missing_indicator_pooled_performance = rbindlist(lapply(pv_columns, function(pv) {
  .pooled_regression_performance(missing_indicator_oof[[pv]], pv, "xgboost_missing_indicators")
}))
missing_indicator_comparison = merge(
  pooled_performance[learner_id == "xgboost", .(
    outcome_id,
    measure_id,
    primary_estimate = estimate
  )],
  missing_indicator_pooled_performance[, .(
    outcome_id,
    measure_id,
    missing_indicator_estimate = estimate
  )],
  by = c("outcome_id", "measure_id"),
  all = TRUE
)
if (nrow(missing_indicator_comparison) != length(pv_columns) * length(measures) ||
    anyNA(missing_indicator_comparison)) {
  stop("The all-PV missingness-indicator comparison is incomplete.", call. = FALSE)
}
missing_indicator_comparison[, `:=`(
  difference_missing_indicator_minus_primary = missing_indicator_estimate - primary_estimate,
  n_missingness_indicators = length(missing_indicator_names),
  interpretation = paste(
    "Same rows, outcomes, folds, learner specification, and fold-contained imputation; only explicit",
    "pre-imputation missingness indicators are added. Differences are descriptive sensitivity results."
  )
)]
pv_descriptive = .descriptive_pv_summary(pooled_performance)
pfi_summary = rbindlist(lapply(pv_columns, function(pv) {
  table = copy(all_pfi[[pv]]$summary)
  table[, outcome_id := pv]
  table
}), fill = TRUE)
pfi_per_iteration = rbindlist(lapply(pv_columns, function(pv) {
  table = copy(all_pfi[[pv]]$per_iteration)
  table[, outcome_id := pv]
  table
}), fill = TRUE)
if (nrow(pfi_summary) != length(feature_groups) * length(pv_columns) ||
    any(pfi_summary[, .N, by = outcome_id]$N != length(feature_groups))) {
  stop("The held-out PFI table is incomplete across predictors, the joint block, or PVs.", call. = FALSE)
}
pfi_summary[feature_group != "socioeconomic_joint_block", rank := frank(
  -mean_importance,
  ties.method = "average"
), by = outcome_id]
pfi_summary[feature_group == "socioeconomic_joint_block", rank := NA_real_]
pfi_summary[, rank_scope := ifelse(
  feature_group == "socioeconomic_joint_block",
  "Overlapping joint query; reported separately and not ranked with its component variables",
  "Atomic feature-group rank within plausible value"
)]
leading_importance_recurrence = .evidence_pisa_pv_importance_recurrence(pfi_summary, top_k = 5L)
pfi_pv_summary = csdg_plausible_value_summary(
  pfi_summary[, .(
    feature_group,
    outcome_id,
    estimate = mean_importance
  )],
  plausible_value = "outcome_id",
  estimate = "estimate",
  by = "feature_group",
  direction = "decreasing"
)
pfi_descriptive = merge(
  pfi_pv_summary$summary,
  unique(pfi_summary[, .(feature_group, features, loss)]),
  by = "feature_group",
  all.x = TRUE,
  sort = FALSE
)
setnames(
  pfi_descriptive,
  c(
    "n_plausible_values", "mean_estimate", "standard_deviation_across_plausible_values",
    "minimum_estimate", "maximum_estimate", "rank_after_value_scale_aggregation"
  ),
  c("n_pv", "mean_across_pv", "sd_across_pv", "minimum_across_pv", "maximum_across_pv", "rank_across_pv")
)
pfi_descriptive[, interpretation := paste(
  "Importance was aggregated on the RMSE-increase scale before ranking.",
  "The overlapping socioeconomic joint query is reported separately from its components.",
  "Across-PV variation is descriptive, not Rubin-pooled uncertainty or inference."
)]
pfi_descriptive[feature_group != "socioeconomic_joint_block", rank_across_pv := frank(
  -mean_across_pv,
  ties.method = "average"
)]
pfi_descriptive[feature_group == "socioeconomic_joint_block", rank_across_pv := NA_real_]
setorder(pfi_descriptive, rank_across_pv, feature_group)
if (any(pfi_descriptive$n_pv != 10L)) {
  stop("At least one PFI feature group is missing a plausible-value result.", call. = FALSE)
}

# ---- All-PV model-class and explanation sensitivity ----

model_ranks = pooled_performance[measure_id == "regr.rmse", .(
  mean_score = mean(estimate),
  sd_across_pv = sd(estimate),
  minimum_score = min(estimate),
  maximum_score = max(estimate),
  n_plausible_values = .N
), by = .(learner_name = learner_id)]
model_ranks[, `:=`(
  learner_id = vapply(learner_name, function(id) candidates[[id]]$id, character(1L)),
  primary_measure = "regr.rmse",
  score_aggregation = "mean pooled held-out RMSE across ten aligned plausible values",
  direction = "minimize"
)]
all_pv_best_score = min(model_ranks$mean_score)
all_pv_reference_score = model_ranks[learner_name == "xgboost", mean_score]
if (length(all_pv_reference_score) != 1L || !is.finite(all_pv_reference_score)) {
  stop("The all-PV focal XGBoost reference score is unavailable.", call. = FALSE)
}
all_pv_acceptance_limit = all_pv_reference_score + 5
model_ranks[, `:=`(
  accepted = mean_score <= all_pv_acceptance_limit,
  distance_from_best = mean_score - all_pv_best_score,
  distance_from_reference = mean_score - all_pv_reference_score,
  best_score = all_pv_best_score,
  acceptance_reference_learner = "xgboost",
  acceptance_reference_score = all_pv_reference_score,
  acceptance_basis = "declared focal XGBoost",
  acceptance_limit = all_pv_acceptance_limit,
  tolerance_absolute = 5,
  tolerance_relative = 0,
  interpretation = paste(
    "Common-split sensitivity screen based on the mean held-out RMSE across ten aligned plausible values.",
    "The focal-relative five-point tolerance equals 0.05 of the conventional 100-point PISA scale standard deviation;",
    "it is a prespecified analytic boundary, not a universal equivalence threshold or a hypothesis test."
  )
)]
setorder(model_ranks, mean_score, learner_name)
near_equivalence_tolerance_sensitivity = csdg_near_equivalence_sensitivity(
  model_ranks,
  tolerance_absolute = c(2.5, 5, 10),
  reference_learner = "xgboost",
  direction = "minimize",
  scenario_id = c("focal_plus_2_5_points", "focal_plus_5_points", "focal_plus_10_points")
)
near_equivalence_tolerance_sensitivity[, interpretation := paste(
  "Sensitivity of accepted-model membership to three focal-XGBoost-relative all-PV RMSE allowances;",
  "the values equal 0.025, 0.05, and 0.10 of the conventional 100-point scale standard deviation",
  "and are descriptive analytic boundaries rather than universal equivalence thresholds."
)]
accepted_check = if (settings$quick) {
  "xgboost" %in% model_ranks[accepted == TRUE, learner_name] && sum(model_ranks$accepted) >= 2L
} else {
  all(c("xgboost", "ridge", "random_forest") %in% model_ranks[accepted == TRUE, learner_name])
}
if (any(model_ranks$n_plausible_values != 10L) || !accepted_check) {
  stop("The all-PV model-sensitivity screen is incomplete or lost a prespecified accepted model.", call. = FALSE)
}

accepted_models_all_pv = model_ranks[accepted == TRUE, learner_name]
all_model_pfi = list(xgboost = all_pfi)
for (learner_index in seq_along(setdiff(accepted_models_all_pv, "xgboost"))) {
  learner_name = setdiff(accepted_models_all_pv, "xgboost")[[learner_index]]
  learner_pfi = vector("list", length(pv_columns))
  for (pv_index in seq_along(pv_columns)) {
    pv = pv_columns[[pv_index]]
    checkpoint_name = paste0("pisa_model_pfi_", learner_name, "_", pv)
    pfi_checkpoint = .run_checkpoint(
      checkpoint_directory,
      checkpoint_name,
      run_signature,
      list(
        stage = "all_pv_model_pfi",
        learner = learner_name,
        outcome = pv,
        repetitions = settings$pfi_repetitions,
        permutation = "within_country_school_coherent"
      ),
      function() {
        csdg_fold_pfi(
          benchmark_oof[[learner_name]][[pv]],
          task = tasks[[pv]],
          feature_groups = feature_groups,
          loss = "rmse",
          repetitions = settings$pfi_repetitions,
          strata = dt$audit_country,
          cluster = dt$audit_school,
          cluster_level_groups = school_level_groups,
          batch_size = 8L,
          seed = settings$seed + 2300000L + learner_index * 100000L + pv_index * 10000L
        )
      }
    )
    learner_pfi[[pv_index]] = pfi_checkpoint$value
    add_checkpoint(checkpoint_name, pfi_checkpoint$signature, "all_pv_model_pfi", pv)
  }
  names(learner_pfi) = pv_columns
  all_model_pfi[[learner_name]] = learner_pfi
}

model_pfi_all_pv = rbindlist(lapply(names(all_model_pfi), function(learner_name) {
  rbindlist(lapply(pv_columns, function(pv) {
    table = copy(all_model_pfi[[learner_name]][[pv]]$summary)
    table[, `:=`(learner_name = learner_name, outcome_id = pv)]
    table
  }))
}))
model_importance_values = model_pfi_all_pv[, .(
  n_plausible_values = uniqueN(outcome_id),
  mean_importance = mean(mean_importance),
  sd_across_pv = sd(mean_importance),
  minimum_importance = min(mean_importance),
  maximum_importance = max(mean_importance),
  mean_monte_carlo_se = mean(mean_monte_carlo_se),
  maximum_monte_carlo_se = max(maximum_monte_carlo_se)
), by = .(learner_name, feature_group, features, loss)]
if (any(model_importance_values$n_plausible_values != 10L)) {
  stop("At least one accepted model lacks PFI evidence for a plausible value.", call. = FALSE)
}

atomic_groups = setdiff(names(feature_groups), "socioeconomic_joint_block")
model_importance_ranks = model_importance_values[feature_group %in% atomic_groups]
model_importance_ranks[, rank := frank(-mean_importance, ties.method = "average"), by = learner_name]
model_importance_ranks[, interpretation := paste(
  "Importance was aggregated on the held-out RMSE-increase scale across ten plausible values before ranking.",
  "The overlapping socioeconomic joint block is reported separately and excluded from the atomic-feature rank."
)]
setorder(model_importance_ranks, learner_name, rank, feature_group)

agreement_objects = lapply(setdiff(accepted_models_all_pv, "xgboost"), function(current_learner) {
  reference = model_importance_values[
    learner_name == "xgboost" & feature_group %in% atomic_groups,
    .(feature_group, importance = mean_importance)
  ]
  comparison = model_importance_values[
    learner_name == current_learner & feature_group %in% atomic_groups,
    .(feature_group, importance = mean_importance)
  ]
  csdg_importance_agreement(
    reference,
    comparison,
    top_k = 5L,
    practical_tolerances = c(0, 0.1, 0.5)
  )
})
names(agreement_objects) = setdiff(accepted_models_all_pv, "xgboost")
model_importance_agreement = rbindlist(lapply(names(agreement_objects), function(learner_name) {
  table = copy(agreement_objects[[learner_name]]$tolerance_sensitivity)
  top = agreement_objects[[learner_name]]$top_k
  table[, `:=`(
    learner_1 = "xgboost",
    learner_2 = learner_name,
    top_k = top$effective_top_k,
    top_k_intersection = top$intersection,
    top_k_union = top$union,
    top_k_jaccard = top$jaccard,
    interpretation = paste(
      "Descriptive agreement after value-scale aggregation across ten plausible values;",
      "the practical-tie thresholds are sensitivity values in held-out RMSE points, not universal cutoffs."
    )
  )]
  table
}))
model_importance_agreement = .analysis_add_practical_tie_denominators(model_importance_agreement)
model_importance_direct_differences = rbindlist(lapply(names(agreement_objects), function(learner_name) {
  table = copy(agreement_objects[[learner_name]]$direct_differences)
  table[, `:=`(learner_1 = "xgboost", learner_2 = learner_name)]
  table
}))
model_importance_pairwise = rbindlist(lapply(names(agreement_objects), function(learner_name) {
  table = copy(agreement_objects[[learner_name]]$pairwise)
  table[, `:=`(learner_1 = "xgboost", learner_2 = learner_name)]
  table
}))
model_importance_definitions = unique(rbindlist(lapply(
  agreement_objects,
  function(object) object$definitions
)))

performance_explanation_sensitivity = csdg_explanation_sensitivity(
  model_ranks,
  model_importance_ranks,
  focal_learner = "xgboost",
  top_k = 5L
)
practical_agreement = model_importance_agreement[practical_tolerance == 0.1, .(
  learner_name = learner_2,
  practical_tolerance,
  n_total_pairs,
  n_practical_ties_excluded,
  n_comparable_pairs,
  n_practical_reversals,
  practical_reversal_percent_among_comparable,
  practical_reversal_fraction,
  importance_weighted_reversal_fraction,
  top_k_jaccard_practical = top_k_jaccard
)]
performance_explanation_sensitivity = merge(
  performance_explanation_sensitivity,
  practical_agreement,
  by = "learner_name",
  all.x = TRUE,
  sort = FALSE
)
performance_explanation_sensitivity[learner_name == "xgboost", `:=`(
  practical_tolerance = 0.1,
  n_total_pairs = n_feature_group_pairs,
  n_practical_ties_excluded = 0L,
  n_comparable_pairs = n_feature_group_pairs,
  n_practical_reversals = 0L,
  practical_reversal_percent_among_comparable = 0,
  practical_reversal_fraction = 0,
  importance_weighted_reversal_fraction = 0,
  top_k_jaccard_practical = 1
)]
performance_explanation_sensitivity[, interpretation := paste(
  "Performance and explanation sensitivity are based on the same accepted set, aligned splits,",
  "and ten plausible values. Quantities are descriptive and do not constitute equivalence tests or CIs."
)]
performance_explanation_sensitivity[, claim_id := "PISA-C1"]

pisa_claim_report_card = .analysis_pisa_claim_report_card()
pisa_claim_evidence = .analysis_pisa_claim_evidence(
  performance = pooled_performance,
  calibration = calibration_descriptive,
  recurrence = leading_importance_recurrence,
  sensitivity = performance_explanation_sensitivity
)
linked_evidence_ids = unique(unlist(strsplit(
  pisa_claim_versions$linked_evidence_ids,
  "|",
  fixed = TRUE
)))
if (!identical(pisa_claim_versions$claim_id, c("PISA-C0", "PISA-C1")) ||
    !identical(pisa_claim_versions$decision, c("unresolved", "met")) ||
    !identical(pisa_claim_report_card$claim_id, pisa_claim_versions$claim_id) ||
    !identical(pisa_claim_report_card$decision, pisa_claim_versions$decision) ||
    !setequal(pisa_claim_evidence$evidence_id, linked_evidence_ids) ||
    anyDuplicated(pisa_claim_evidence$evidence_id) ||
    pisa_claim_evidence[evidence_id == "PISA-E01", availability] != "unavailable" ||
    pisa_claim_evidence[evidence_id == "PISA-E01", claim_consequence] != "unresolved" ||
    !all(pisa_claim_evidence[evidence_id %in% c("PISA-E03", "PISA-E04"), result_direction] == "supports")) {
  stop("The canonical PISA claim-version artifacts are internally inconsistent.", call. = FALSE)
}

# ---- Complete-case sensitivity ----

message("Running PV1 complete-case sensitivity with the original school-fold allocation")
primary_backend = dt[, c(predictor_names, audit_columns), with = FALSE]
primary_backend[, reading := dt$PV1READ]
complete_case = .complete_case_resample(
  primary_backend,
  "reading",
  audit_columns,
  fold_by_row,
  focal,
  measures,
  settings$seed + 2000000L
)
complete_case_summary = csdg_performance(complete_case$oof)$summary
complete_case_summary[, `:=`(
  n_primary = nrow(dt),
  n_complete_case = sum(complete_case$retained),
  retained_fraction = mean(complete_case$retained),
  estimand_note = paste(
    "PV1 complete-case sensitivity under retained school folds; country contributions are no longer exactly",
    "equal, and fold variation is descriptive rather than inferential."
  )
)]
complete_case_country_counts = dt[, .(
  n_primary = .N,
  n_complete_case = sum(complete_case$retained[.I]),
  retained_fraction = mean(complete_case$retained[.I])
), by = CNT]
complete_case_lost_countries = complete_case_country_counts[n_complete_case == 0L, CNT]
if (!settings$quick && !identical(
  sort(complete_case_lost_countries),
  sort(c("AUT", "BEL", "CAN", "JPN", "NOR"))
)) {
  stop(
    "Publication PV1 complete cases did not lose the five prespecified structurally incomplete countries.",
    call. = FALSE
  )
}
complete_case_summary[, `:=`(
  n_countries_retained = 32L - length(complete_case_lost_countries),
  countries_lost = paste(complete_case_lost_countries, collapse = " | ")
)]
complete_case_retained_rows = .analysis_pisa_complete_case_retained_rows(
  primary_task,
  complete_case$oof
)
expected_complete_case_row_ids = primary_task$row_ids[complete_case$retained]
if (!identical(complete_case_retained_rows$row_id, expected_complete_case_row_ids)) {
  stop("The recomputed complete-case rows do not match the retained primary row IDs.", call. = FALSE)
}
complete_case_membership = .analysis_pisa_complete_case_membership(
  fold_assignment,
  complete_case_retained_rows,
  complete_case_country_counts
)
cc_subgroup = data.table(
  row_id = complete_case$oof$task$row_ids,
  audit_country = dt$audit_country[complete_case$retained]
)
complete_case_country_metrics = csdg_subgroup_metrics(
  complete_case$oof,
  subgroup = cc_subgroup,
  min_n = 1L
)$metrics
complete_case_country_metrics[, estimand_note := paste(
  "Country-specific PV1 complete-case OOF behavior; the unweighted country mean is descriptive and",
  "does not restore survey weighting or the original equal-country row count."
)]

# ---- Full-fit effects, interactions, and prediction multiplicity ----

message("Computing full-fit PV1 rich effect and interaction diagnostics")
fitted_focal = focal$clone(deep = TRUE)
fitted_focal$train(primary_task)
ale_2d_min_cell_n = if (settings$quick) 5L else 20L
ale_1d_min_interval_n = if (settings$quick) 5L else 20L
rich_effects = csdg_effect_diagnostics(
  primary_task,
  fitted_focal,
  features = c("HISEI", "PARED", "JOYREAD", "EFFORT2"),
  sample_n = min(settings$effect_rows, primary_task$nrow),
  ale_bins = max(3L, settings$effect_grid),
  grid_n = max(3L, settings$effect_grid),
  ice_keep_n = min(30L, settings$effect_rows),
  seed = settings$seed + 3000000L
)
message("Computing country-stratified school-bootstrap model-refit intervals for one-dimensional ALE")
pisa_ale_bootstrap_units = primary_task$data(cols = c("audit_school", "audit_country"))
rich_ale_bootstrap = csdg_ale_bootstrap(
  primary_task,
  fitted_focal,
  features = c("HISEI", "PARED", "JOYREAD", "EFFORT2"),
  sample_n = min(settings$effect_rows, primary_task$nrow),
  ale_bins = max(3L, settings$effect_grid),
  replicates = settings$ale_bootstrap_replicates,
  confidence_level = 0.95,
  resampling_unit = "cluster",
  cluster = pisa_ale_bootstrap_units$audit_school,
  strata = pisa_ale_bootstrap_units$audit_country,
  min_interval_n = ale_1d_min_interval_n,
  min_success_fraction = 0.80,
  seed = settings$seed + 3000000L,
  keep_replicates = TRUE,
  parallel_workers = settings$ale_bootstrap_workers,
  verbose = TRUE
)
rich_interactions = csdg_interaction_diagnostics(
  primary_task,
  fitted_focal,
  features = c("HISEI", "PARED", "JOYREAD", "EFFORT2"),
  sample_n = min(settings$effect_rows, primary_task$nrow),
  h_grid_n = min(6L, max(3L, settings$effect_grid)),
  ale_bins = min(8L, max(3L, settings$effect_grid)),
  top_n_pairs = 1L,
  max_features = 4L,
  seed = settings$seed + 3100000L
)
rich_ale_1d_public = rich_ale_bootstrap$ale_1d
rich_ale_1d_public[, limitation := paste(
  limitation,
  "For PISA, the intervals condition on PV1 and do not pool plausible-value uncertainty."
)]
rich_ale_2d_public = csdg_mask_effect_support(rich_interactions$ale_2d, ale_2d_min_cell_n)
ale_2d_available = any(is.finite(rich_ale_2d_public$ale2d))
ale_2d_status = if (ale_2d_available) {
  "computed"
} else {
  recorded = unique(rich_ale_2d_public$surface_status)
  recorded = recorded[!is.na(recorded) & nzchar(recorded)]
  if (length(recorded)) recorded[[1L]] else "not_computed"
}

pv1_multiplicity_model_ranks = copy(rashomon$candidates)
pv1_multiplicity_model_ranks[, interpretation := paste(
  "PV1-only sensitivity screen retained solely for the supplementary row-level prediction-multiplicity query;",
  "the manuscript model-class comparison uses the all-PV accepted set above"
)]
accepted_models = pv1_multiplicity_model_ranks[accepted == TRUE, learner_name]
multiplicity_available = length(accepted_models) >= 2L
multiplicity_status = data.table(
  status = if (multiplicity_available) "estimated" else "not_estimable",
  n_accepted_models = length(accepted_models),
  required_minimum = 2L,
  interpretation = if (multiplicity_available) {
    "PV1 OOF prediction dispersion across held-out near-equivalent model predictions"
  } else {
    "Only one model met the held-out RMSE tolerance; prediction multiplicity was not fabricated"
  }
)
if (multiplicity_available) {
  multiplicity = .rashomon_prediction_multiplicity(rashomon)
  multiplicity_distribution = multiplicity$quantiles
  multiplicity_bins = multiplicity$bins
  multiplicity_private = multiplicity$row_level
} else {
  multiplicity_distribution = data.table(
    status = "not_estimable",
    reason = "Fewer than two models met the prespecified held-out tolerance"
  )
  multiplicity_bins = copy(multiplicity_distribution)
  multiplicity_private = data.table()
}

# ---- Leave-one-country-out boundary stress test ----

message("Running genuine cached leave-one-country-out refits for every PV-country combination")
countries = levels(dt$audit_country)
loco_scores = list()
loco_predictions = list()
for (pv_index in seq_along(tasks)) {
  pv = pv_columns[[pv_index]]
  task = tasks[[pv_index]]
  country_vector = as.character(dt$audit_country)
  for (country_index in seq_along(countries)) {
    country = countries[[country_index]]
    checkpoint_name = paste0("pisa_loco_", pv, "_", country)
    loco_checkpoint = .run_checkpoint(
      checkpoint_directory,
      checkpoint_name,
      run_signature,
      list(stage = "loco", outcome = pv, held_out_country = country),
      function() {
        test_rows = task$row_ids[country_vector == country]
        train_rows = setdiff(task$row_ids, test_rows)
        custom = rsmp("custom")
        custom$instantiate(task, train_sets = list(train_rows), test_sets = list(test_rows))
        oof = csdg_resample(
          task,
          focal$clone(deep = TRUE),
          custom,
          measures = measures,
          store_models = FALSE,
          seed = settings$seed + pv_index * 100000L + country_index
        )
        score = copy(oof$fold_scores)
        score[, `:=`(
          held_out_group = country,
          n_train = length(train_rows),
          n_assessment = length(test_rows)
        )]
        predictions = copy(oof$predictions)
        predictions[, `:=`(held_out_group = country)]
        list(score = score, predictions = predictions)
      }
    )
    current = loco_checkpoint$value
    current$score[, outcome_id := pv]
    current$predictions[, outcome_id := pv]
    loco_scores[[length(loco_scores) + 1L]] = current$score
    loco_predictions[[length(loco_predictions) + 1L]] = current$predictions
    add_checkpoint(checkpoint_name, loco_checkpoint$signature, "loco", pv, country)
  }
  write_run_checkpoint(run, paste0("pisa_loco_", pv), "completed", list(countries = length(countries)))
}
loco_scores = rbindlist(loco_scores, fill = TRUE)
loco_predictions = rbindlist(loco_predictions, fill = TRUE)
if (nrow(loco_scores) != length(pv_columns) * length(countries) ||
    uniqueN(loco_scores[, .(outcome_id, held_out_group)]) != 320L) {
  stop("The LOCO score table does not contain exactly 10 x 32 genuine refits.", call. = FALSE)
}
loco_long = melt(
  loco_scores,
  id.vars = intersect(
    c("outcome_id", "held_out_group", "n_train", "n_assessment", "iteration", "repetition", "fold"),
    names(loco_scores)
  ),
  measure.vars = c("regr.rmse", "regr.mae", "regr.rsq"),
  variable.name = "measure_id",
  value.name = "estimate"
)
loco_descriptive = loco_long[, .(
  n_pv_country = .N,
  mean = mean(estimate, na.rm = TRUE),
  sd = sd(estimate, na.rm = TRUE),
  minimum = min(estimate, na.rm = TRUE),
  median = median(estimate, na.rm = TRUE),
  maximum = max(estimate, na.rm = TRUE),
  interpretation = "Descriptive variation across aligned PV-country refits; not sampling inference"
), by = .(measure_id)]
loco_country_descriptive = loco_long[, .(
  n_pv = uniqueN(outcome_id),
  mean = mean(estimate, na.rm = TRUE),
  sd = sd(estimate, na.rm = TRUE),
  minimum = min(estimate, na.rm = TRUE),
  median = median(estimate, na.rm = TRUE),
  maximum = max(estimate, na.rm = TRUE),
  interpretation = "Descriptive variation across ten aligned PV refits; not sampling inference"
), by = .(held_out_group, measure_id)]
if (nrow(loco_country_descriptive) != 32L * 3L || any(loco_country_descriptive$n_pv != 10L)) {
  stop("The country-specific LOCO summary is not complete across all ten plausible values.", call. = FALSE)
}
loco_penalty_tables = .analysis_pisa_loco_penalty_tables(
  focal_oof,
  loco_scores,
  loco_predictions
)
loco_country_pv_comparison = loco_penalty_tables$comparison
loco_country_penalty_summary = loco_penalty_tables$country_summary
if (nrow(loco_country_pv_comparison) != 320L ||
    uniqueN(loco_country_pv_comparison[, .(outcome_id, held_out_group)]) != 320L ||
    nrow(loco_country_penalty_summary) != 32L ||
    any(loco_country_penalty_summary$n_pv != 10L)) {
  stop("The matched PISA LOCO penalty tables are not complete across 10 PVs and 32 countries.", call. = FALSE)
}

loco_country_calibration = loco_predictions[, {
  summary = csdg_calibration(
    data.frame(truth = truth, response = response),
    task_type = "regr",
    collapse_repeats = FALSE
  )$summary
  summary
}, by = .(outcome_id, held_out_group)]
loco_country_calibration_summary = loco_country_calibration[, .(
  n_plausible_values = .N,
  mean_calibration_in_the_large = mean(calibration_in_the_large),
  minimum_calibration_in_the_large = min(calibration_in_the_large),
  maximum_calibration_in_the_large = max(calibration_in_the_large),
  mean_calibration_slope = mean(calibration_slope),
  minimum_calibration_slope = min(calibration_slope),
  maximum_calibration_slope = max(calibration_slope),
  interpretation = paste(
    "Descriptive country-specific calibration across ten aligned plausible values.",
    "Ranges are not confidence intervals and concern only the observed 2018 country settings."
  )
), by = held_out_group]
if (nrow(loco_country_calibration) != 320L ||
    any(loco_country_calibration_summary$n_plausible_values != 10L)) {
  stop("Country-level LOCO calibration is incomplete across countries or plausible values.", call. = FALSE)
}

message("Running PV1 training-size-matched country-exclusion controls")
matched_exclusion_checkpoint = .run_checkpoint(
  checkpoint_directory,
  "pisa_pv1_matched_country_exclusion",
  run_signature,
  list(
    stage = "matched_country_exclusion",
    outcome = "PV1READ",
    repetitions = settings$matched_setting_repetitions
  ),
  function() {
    csdg_matched_setting_exclusion(
      focal_oof[["PV1READ"]],
      learner = focal,
      group = country_by_row,
      loss = "rmse",
      repetitions = settings$matched_setting_repetitions,
      seed = settings$seed + 4000000L,
      store_models = FALSE
    )
  }
)
matched_country_exclusion = matched_exclusion_checkpoint$value
add_checkpoint(
  "pisa_pv1_matched_country_exclusion",
  matched_exclusion_checkpoint$signature,
  "matched_country_exclusion",
  "PV1READ"
)
if (nrow(matched_country_exclusion$summary) != 32L ||
    any(matched_country_exclusion$summary$n_matched_repetitions != settings$matched_setting_repetitions)) {
  stop("The PV1 matched country-exclusion control is incomplete.", call. = FALSE)
}
matched_control_contrasts = .analysis_pisa_matched_control_contrasts(
  loco_country_pv_comparison,
  matched_country_exclusion$summary
)
checkpoint_inventory = rbindlist(checkpoint_inventory_state$items, fill = TRUE)
evidence_scope = data.table(
  evidence_component = c(
    "PV1 package audit component",
    "all-PV focal prediction and calibration",
    "common-split learner benchmark",
    "held-out permutation importance",
    "complete-case sensitivity",
    "rich effect and interaction diagnostics",
    "model multiplicity and importance agreement",
    "technical subgroup metrics",
    "leave-one-country-out refits with common-split comparator",
    "training-size-matched country-exclusion control"
  ),
  outcome_scope = c(
    "PV1READ",
    "PV1READ-PV10READ",
    "PV1READ-PV10READ",
    "PV1READ-PV10READ",
    "PV1READ",
    "PV1READ",
    "PV1READ",
    "PV1READ",
    "PV1READ-PV10READ by 32 countries",
    "PV1READ by 32 countries"
  ),
  evidence_role = c(
    "single-outcome package component of the formal target",
    "formal all-PV focal-target evidence",
    "aligned outcome and model-class sensitivity",
    "formal all-PV focal explanation",
    "missing-data sensitivity",
    "complementary focal-model global description",
    "technical model-class boundary diagnostic",
    "technical audience boundary diagnostic",
    "technical observed-country exclusion-penalty stress test",
    "technical training-size control"
  ),
  expands_focal_claim_scope = FALSE,
  interpretation = c(
    paste(
      "The package gate runner currently accepts one outcome task at a time; this PV1 run verifies the",
      "single-outcome computation but cannot by itself decide the all-PV claim."
    ),
    paste(
      "Prediction and calibration are estimated separately for all ten plausible values on aligned splits;",
      "across-PV ranges are descriptive and no student-level averaging or generic Rubin rule is used."
    ),
    "Across-PV and across-model variation is descriptive and does not select a new focal model.",
    "PFI is computed for each plausible value and aggregated on the RMSE-increase scale before ranking.",
    "Complete cases change country contributions and the analytic estimand.",
    "Full-fit diagnostics omit model-fitting uncertainty and are not causal effects.",
    "Near-equivalent model diagnostics do not expand the claim beyond the focal model.",
    "Subgroup summaries are descriptive and do not constitute comparative subgroup claims.",
    paste(
      "Each country/PV LOCO RMSE is compared with same-country common-split OOF XGBoost RMSE;",
      "positive penalties indicate worse prediction after exclusion. Results concern only the 32 observed",
      "countries and do not claim new-country transport."
    ),
    paste(
      paste0(settings$matched_setting_repetitions, " PV1 refits per observed country match the median"),
      "outer-analysis-set size by sampling rows from",
      "other countries. This controls training size descriptively but does not identify a causal country effect."
    )
  )
)

# ---- Validation, calibration, and run summaries ----

validation = data.table(
  diagnostic = c(
    "publication sample",
    "LANGN=999 runtime recode",
    "aligned plausible-value tasks",
    "school split integrity",
    "country coverage and fold balance",
    "all-PV benchmark",
    "held-out all-PV PFI",
    "all-PV calibration with school-within-country bootstrap",
    "PV1 complete-case sensitivity",
    "rich ALE with model-refit intervals/ICE/H/two-dimensional ALE",
    "PV1 OOF prediction multiplicity",
    "10x32 LOCO refits and common-split penalties",
    "PV1 training-size-matched country-exclusion control",
    "preprocessing verification",
    "manuscript eligibility"
  ),
  status = c(
    if (!settings$quick) "exact_32x1500" else "quick_32x100",
    "applied_manifest_discrepancy",
    "verified",
    "verified_zero_crossing",
    "verified",
    "computed_descriptive",
    "computed_held_out",
    "computed_all_ten_pv_with_pointwise_intervals",
    "computed_school_preserving_membership_verified",
    "computed_distinct_estimands_with_ale_refit_intervals",
    multiplicity_status$status,
    "verified_320_refits_and_row_matched_comparisons",
    paste0("verified_32x", settings$matched_setting_repetitions, "_refits"),
    "verified_by_responsible_coauthors",
    if (settings$quick) "not_eligible_quick" else "eligible_after_structural_output_audit"
  ),
  limitation = c(
    "Unweighted equal-country analytic sample; not a design-based PISA population sample.",
    "The frozen input was not regenerated; the runtime audit documents the manifest discrepancy.",
    "Across-PV variation is descriptive and is not Rubin-pooled uncertainty.",
    "School clustering prevents within-fold leakage but does not remove unmeasured school dependence.",
    paste0("Observed max/min fold row-load ratio: ", signif(load_ratio, 4L), "."),
    "Alternative learners are sensitivity probes; folds are not independent inferential units.",
    "PFI is perturbation- and model-specific and can redistribute importance among correlated variables.",
    paste(
      "Pointwise percentile intervals condition on supplied OOF predictions and omit model-training variation;",
      "across-PV ranges are descriptive rather than Rubin-pooled uncertainty."
    ),
    paste(
      "Complete cases alter country contributions and the analytic estimand; countries with zero retained rows:",
      paste(complete_case_lost_countries, collapse = ", "),
      ". Membership is matched by original primary row ID and validated by country and assessment fold."
    ),
    paste(
      "Full-fit descriptive model diagnostics are not causal. One-dimensional ALE has pointwise 95%",
      "country-stratified school-bootstrap intervals from complete learner refits; they are not simultaneous",
      "and do not include learner-selection, hyperparameter-search, or plausible-value uncertainty.",
      paste0("Two-dimensional ALE cells with n < ", ale_2d_min_cell_n, " are suppressed without refit intervals.")
    ),
    multiplicity_status$interpretation,
    paste(
      "LOCO penalties use the same country/PV rows and compare against common-split OOF XGBoost RMSE.",
      "This technical diagnostic is outside the focal claim, limited to the 32 observed countries, and",
      "does not establish new-country or causal transport."
    ),
    paste(
      "The matched control uses only PV1 and repeated row samples from the other observed countries.",
      "It controls analysis-set size, not composition, exchangeability, or unobserved-country transport."
    ),
    paste(
      "The responsible co-authors verified the analytic cut and preprocessing on 2026-09-09; this status is not",
      "an independent reconstruction of the compact source."
    ),
    if (settings$quick) {
      "Quick outputs must never enter the manuscript."
    } else {
      "Publication settings and co-author preprocessing verification are recorded; structural output audit remains."
    }
  )
)

calibration_raw = as.data.table(copy(result$artifacts$calibration$curve))
if (!setequal(names(calibration_raw), c("prediction", "truth")) ||
    nrow(calibration_raw) != nrow(dt) || any(!is.finite(as.matrix(calibration_raw)))) {
  stop("PV1 regression calibration evidence is not a complete finite OOF table.", call. = FALSE)
}
setorder(calibration_raw, prediction, truth)
calibration_bin_count = min(10L, nrow(calibration_raw))
calibration_raw[, bin := pmin(
  calibration_bin_count,
  ceiling(seq_len(.N) * calibration_bin_count / .N)
)]
calibration_binned = calibration_raw[, .(
  n = .N,
  predicted = mean(prediction),
  observed = mean(truth)
), by = bin]
if (nrow(calibration_binned) != calibration_bin_count ||
    sum(calibration_binned$n) != nrow(calibration_raw)) {
  stop("PV1 regression calibration binning did not preserve every OOF observation.", call. = FALSE)
}
calibration_plot_source = calibration_curve[
  outcome_id == "PV1READ",
  .(prediction, estimate, lower, upper)
]
calibration_plot_source[, interval_scope := paste(
  "Pointwise 95% school-within-country bootstrap percentile interval conditional on supplied OOF predictions;",
  "not a simultaneous band and not model-training or plausible-value uncertainty"
)]

sample_flow = data.table(
  stage = c("source compact file", "equal-country analytic sample", "PV1 complete cases"),
  n = c(source_n, nrow(dt), sum(complete_case$retained)),
  countries = c(source_country_n, uniqueN(dt$CNT), uniqueN(dt$CNT[complete_case$retained]))
)
run_mode = data.table(
  mode = settings$mode,
  publication_settings = !settings$quick,
  manuscript_ready = !settings$quick,
  eligibility_blocker = if (settings$quick) "nonpublication quick mode" else "none",
  seed = settings$seed,
  folds = settings$folds,
  n_per_country = settings$n_per_country,
  warning = if (settings$quick) {
    "NONPUBLICATION quick run: never use these results in the manuscript."
  } else {
    "Publication-settings run with responsible-co-author preprocessing verification dated 2026-09-09."
  }
)

# ---- Output tables and private analysis objects ----

.write_table(run_mode, file.path(run$dirs$tables, "pisa_run_mode.csv"))
.write_table(sample_flow, file.path(run$dirs$tables, "pisa_sample_flow.csv"))
.write_table(country_sample, file.path(run$dirs$tables, "pisa_country_sample.csv"))
.write_table(missingness, file.path(run$dirs$tables, "pisa_missingness.csv"))
.write_table(missingness_by_country, file.path(run$dirs$tables, "pisa_missingness_by_country.csv"))
.write_table(langn_audit, file.path(run$dirs$tables, "pisa_langn_recode_audit.csv"))
.write_table(fold_assignment, file.path(run$dirs$tables, "pisa_fold_assignment.csv"))
.write_table(fold_balance, file.path(run$dirs$tables, "pisa_fold_balance.csv"))
.write_table(school_integrity, file.path(run$dirs$tables, "pisa_school_split_integrity.csv"))
.write_table(resampling_identity, file.path(run$dirs$tables, "pisa_resampling_identity.csv"))
.write_table(fold_performance, file.path(run$dirs$tables, "pisa_all_pv_fold_performance.csv"))
.write_table(pooled_performance, file.path(run$dirs$tables, "pisa_all_pv_pooled_performance.csv"))
.write_table(pv_descriptive, file.path(run$dirs$tables, "pisa_all_pv_descriptive_summary.csv"))
.write_table(evidence_scope, file.path(run$dirs$tables, "pisa_all_pv_evidence_scope.csv"))
.write_table(pfi_summary, file.path(run$dirs$tables, "pisa_all_pv_pfi_summary.csv"))
.write_table(
  pfi_descriptive,
  file.path(run$dirs$tables, "pisa_all_pv_pfi_descriptive_summary.csv")
)
.write_table(pfi_per_iteration, file.path(run$dirs$tables, "pisa_all_pv_pfi_per_iteration.csv"))
.write_table(
  leading_importance_recurrence,
  file.path(run$dirs$tables, "pisa_all_pv_leading_importance_recurrence.csv")
)
.write_table(
  calibration_summary,
  file.path(run$dirs$tables, "pisa_all_pv_calibration_summary.csv")
)
.write_table(
  calibration_curve,
  file.path(run$dirs$tables, "pisa_all_pv_calibration_curve.csv")
)
.write_table(
  calibration_descriptive,
  file.path(run$dirs$tables, "pisa_all_pv_calibration_descriptive_summary.csv")
)
.write_table(
  missing_indicator_fold_performance,
  file.path(run$dirs$tables, "pisa_all_pv_missingness_indicator_fold_performance.csv")
)
.write_table(
  missing_indicator_pooled_performance,
  file.path(run$dirs$tables, "pisa_all_pv_missingness_indicator_pooled_performance.csv")
)
.write_table(
  missing_indicator_comparison,
  file.path(run$dirs$tables, "pisa_all_pv_missingness_indicator_comparison.csv")
)
.write_table(
  complete_case_summary,
  file.path(run$dirs$tables, "pisa_pv1_complete_case_sensitivity.csv")
)
.write_table(
  complete_case_country_counts,
  file.path(run$dirs$tables, "pisa_pv1_complete_case_country_counts.csv")
)
.write_table(
  complete_case_country_metrics,
  file.path(run$dirs$tables, "pisa_pv1_complete_case_country_metrics.csv")
)
.write_table(
  public_subgroup_tables$metrics,
  file.path(run$dirs$tables, "pisa_pv1_technical_subgroup_metrics.csv")
)
.write_table(
  public_subgroup_tables$uncertainty,
  file.path(run$dirs$tables, "pisa_pv1_technical_subgroup_uncertainty.csv")
)
.write_table(
  public_subgroup_tables$contrasts,
  file.path(run$dirs$tables, "pisa_pv1_technical_subgroup_contrasts.csv")
)
.write_table(
  public_subgroup_tables$missingness,
  file.path(run$dirs$tables, "pisa_pv1_technical_subgroup_missingness.csv")
)
.write_table(model_ranks, file.path(run$dirs$tables, "pisa_model_ranks.csv"))
.write_table(model_importance_ranks, file.path(run$dirs$tables, "pisa_model_importance_ranks.csv"))
.write_table(
  model_importance_agreement,
  file.path(run$dirs$tables, "pisa_model_importance_agreement.csv")
)
.write_table(
  model_importance_direct_differences,
  file.path(run$dirs$tables, "pisa_model_importance_direct_differences.csv")
)
.write_table(
  model_importance_pairwise,
  file.path(run$dirs$tables, "pisa_model_importance_pairwise.csv")
)
.write_table(
  model_importance_definitions,
  file.path(run$dirs$tables, "pisa_model_importance_definitions.csv")
)
.write_table(model_pfi_all_pv, file.path(run$dirs$tables, "pisa_model_pfi_all_pv.csv"))
.write_table(
  near_equivalence_tolerance_sensitivity,
  file.path(run$dirs$tables, "pisa_near_equivalence_tolerance_sensitivity.csv")
)
.write_table(
  performance_explanation_sensitivity,
  file.path(run$dirs$tables, "pisa_performance_explanation_sensitivity.csv")
)
.write_table(pisa_claim_versions, file.path(run$dirs$tables, "pisa_claim_versions.csv"))
.write_table(pisa_claim_report_card, file.path(run$dirs$tables, "pisa_claim_report_card.csv"))
.write_table(pisa_claim_evidence, file.path(run$dirs$tables, "pisa_claim_evidence.csv"))
.write_table(multiplicity_status, file.path(run$dirs$tables, "pisa_multiplicity_status.csv"))
.write_table(
  multiplicity_distribution,
  file.path(run$dirs$tables, "pisa_multiplicity_distribution.csv")
)
.write_table(multiplicity_bins, file.path(run$dirs$tables, "pisa_multiplicity_bins.csv"))
.write_table(loco_scores, file.path(run$dirs$tables, "pisa_loco_all_pv_scores.csv"))
.write_table(loco_descriptive, file.path(run$dirs$tables, "pisa_loco_descriptive_summary.csv"))
.write_table(
  loco_country_descriptive,
  file.path(run$dirs$tables, "pisa_loco_country_descriptive_summary.csv")
)
.write_table(
  loco_country_pv_comparison,
  file.path(run$dirs$tables, "pisa_loco_country_pv_comparison.csv")
)
.write_table(
  loco_country_penalty_summary,
  file.path(run$dirs$tables, "pisa_loco_country_penalty_summary.csv")
)
.write_table(
  loco_country_calibration,
  file.path(run$dirs$tables, "pisa_loco_country_calibration.csv")
)
.write_table(
  loco_country_calibration_summary,
  file.path(run$dirs$tables, "pisa_loco_country_calibration_summary.csv")
)
.write_table(
  matched_country_exclusion$per_repetition,
  file.path(run$dirs$tables, "pisa_pv1_matched_country_exclusion_repetitions.csv")
)
.write_table(
  matched_country_exclusion$summary,
  file.path(run$dirs$tables, "pisa_pv1_matched_country_exclusion_summary.csv")
)
.write_table(
  matched_control_contrasts$all_pv_context,
  file.path(run$dirs$tables, "pisa_matched_control_context_by_country_pv.csv")
)
.write_table(
  matched_control_contrasts$contrasts,
  file.path(run$dirs$tables, "pisa_matched_control_contrasts_by_country.csv")
)
.write_table(
  checkpoint_inventory,
  file.path(run$dirs$tables, "pisa_loco_checkpoint_inventory.csv")
)
.write_table(rich_ale_1d_public, file.path(run$dirs$tables, "pisa_rich_ale_1d.csv"))
.write_table(rich_effects$pdp, file.path(run$dirs$tables, "pisa_rich_pdp.csv"))
.write_table(rich_effects$ice_quantiles, file.path(run$dirs$tables, "pisa_rich_ice_quantiles.csv"))
.write_table(rich_effects$ice_spread, file.path(run$dirs$tables, "pisa_rich_ice_spread.csv"))
.write_table(
  rich_interactions$interaction_screen,
  file.path(run$dirs$tables, "pisa_rich_interaction_screen.csv")
)
.write_table(rich_ale_2d_public, file.path(run$dirs$tables, "pisa_rich_ale_2d.csv"))
.write_table(report_card, file.path(run$dirs$tables, "pisa_report_card.csv"))
.write_table(claim_support, file.path(run$dirs$tables, "pisa_claim_support.csv"))
.write_table(validation, file.path(run$dirs$tables, "pisa_validation.csv"))
.write_table(
  calibration_binned,
  file.path(run$dirs$tables, "pisa_g3_calibration_curve_binned.csv")
)
.write_table(
  calibration_plot_source,
  file.path(run$dirs$tables, "pisa_g3_calibration_plot_source.csv")
)
.write_gate_tables(result, run$dirs$tables, "pisa_gate")

.write_table(rich_effects$ice_private, file.path(run$dirs$objects, "pisa_rich_ice_private.csv"))
.write_table(
  rich_effects$ale_1d,
  file.path(run$dirs$objects, "pisa_rich_ale_1d_unsuppressed_private.csv")
)
.write_table(
  rich_ale_bootstrap$replicates_private,
  file.path(run$dirs$objects, "pisa_rich_ale_bootstrap_replicates_private.csv")
)
.write_table(
  rich_ale_bootstrap$failures,
  file.path(run$dirs$objects, "pisa_rich_ale_bootstrap_failures_private.csv")
)
.write_table(
  rich_interactions$ale_2d,
  file.path(run$dirs$objects, "pisa_rich_ale_2d_unsuppressed_private.csv")
)
.write_table(
  complete_case_membership,
  file.path(run$dirs$objects, "pisa_pv1_complete_case_membership_private.csv")
)
if (nrow(multiplicity_private)) {
  .write_table(
    multiplicity_private,
    file.path(run$dirs$objects, "pisa_multiplicity_row_level_private.csv")
  )
}
saveRDS(result, file.path(run$dirs$objects, "pisa_csdg_result_private.rds"), version = 3)
saveRDS(focal_oof, file.path(run$dirs$objects, "pisa_all_pv_focal_oof_private.rds"), version = 3)
saveRDS(
  missing_indicator_oof,
  file.path(run$dirs$objects, "pisa_all_pv_missingness_indicator_oof_private.rds"),
  version = 3
)
saveRDS(benchmark_oof, file.path(run$dirs$objects, "pisa_all_pv_benchmark_oof_private.rds"), version = 3)
saveRDS(loco_predictions, file.path(run$dirs$objects, "pisa_loco_predictions_private.rds"), version = 3)
saveRDS(complete_case$oof, file.path(run$dirs$objects, "pisa_pv1_complete_case_oof_private.rds"), version = 3)
csdg_export(result, run$dirs$audit_bundle, prefix = "pisa")

# ---- Publication figures ----

.analysis_reset_figure_registry()
.analysis_clear_figure_files(run$dirs$figures)

pisa_feature_labels = .analysis_pisa_feature_labels(manifest)
pisa_plot_feature_labels = .analysis_pisa_plot_feature_labels(manifest)
pisa_audit_labels = .analysis_pisa_audit_labels()
pisa_subgroup_labels = .analysis_pisa_subgroup_labels()
.save_plot(
  csdg_plot_data(
    report_card,
    type = "gate_status",
    title = "PISA PV1 claim-scoped diagnostic report card",
    gate_order = report_card$gate_id
  ),
  file.path(run$dirs$figures, "pisa_gate_overview")
)
.save_plot(
  csdg_plot(result, gate = "G1", base_size = 12) +
    labs(title = "PISA PV1 focal XGBoost held-out predictive performance") +
    theme(axis.title.x = element_text(size = 11.5)),
  file.path(run$dirs$figures, "pisa_g1_performance"),
  width = 15,
  height = 3.4
)
.save_plot(
  csdg_plot_data(
    dependence,
    type = "dependence",
    title = "PISA PV1 mixed-type predictor dependence",
    subtitle = paste(
      "Lower triangle for the 15 strongest variables. Cells combine absolute Spearman correlation,",
      "Cramer's V, and eta-squared; these descriptive magnitudes are not directly equivalent."
    ),
    feature_labels = pisa_plot_feature_labels,
    top_n = 15L,
    base_size = 16,
    axis_text_size = 12.5,
    legend_text_size = 12.5,
    legend_title_size = 12.5
  ),
  file.path(run$dirs$figures, "pisa_g2_dependence"),
  width = 11,
  height = 9
)
.save_plot(
  csdg_plot_data(
    calibration_plot_source,
    type = "calibration",
    title = "PISA PV1 flexible out-of-fold calibration illustration",
    subtitle = paste(
      "The curve is a natural cubic regression spline with four degrees of freedom;",
      "the band is a pointwise 95% school-within-country bootstrap percentile interval conditional on the",
      "supplied out-of-fold predictions. The band is not simultaneous and omits model-training and",
      "plausible-value uncertainty;",
      "all ten plausible-value summaries are reported in the accompanying tables."
    ),
    x_label = "Held-out predicted reading score",
    y_label = "Estimated observed reading score"
  ),
  file.path(run$dirs$figures, "pisa_g3_calibration")
)
.save_plot(
  csdg_plot_data(
    fread(file.path(run$dirs$tables, "pisa_gate_g5__pfi__summary.csv")),
    type = "pfi",
    title = "PISA PV1 held-out permutation importance",
    subtitle = paste(
      "Top 15 groups; blue whiskers are descriptive fold 10th-90th percentiles, not confidence intervals;",
      "red points are means"
    ),
    feature_labels = pisa_feature_labels,
    x_label = "Increase in held-out RMSE",
    base_size = 15,
    direct_label_size = 3.4
  ),
  file.path(run$dirs$figures, "pisa_g5_pfi"),
  width = 10,
  height = 7
)

model_plot = csdg_plot_data(
  model_ranks,
  type = "model_comparison",
  title = "PISA all-PV technical model-class sensitivity screen",
  subtitle = paste(
    "Points compare mean held-out RMSE across ten aligned plausible values with focal XGBoost. Whiskers are",
    "descriptive matched PV-fold 10th-90th percentiles, not confidence intervals. The dashed line is the",
    "prespecified +5-point RMSE boundary; lower values are better."
  ),
  x_label = "Held-out RMSE difference from focal XGBoost",
  fold_scores = fold_performance[measure_id == "regr.rmse"],
  reference_learner = "xgboost",
  base_size = 13,
  direct_label_size = 3.6,
  limit_label_size = 3.5
)
.save_plot(
  model_plot,
  file.path(run$dirs$figures, "pisa_g6_model_sensitivity"),
  width = 9,
  height = 6
)

importance_plot = csdg_plot_data(
  model_importance_ranks,
  type = "importance_comparison",
  title = "PISA all-PV held-out PFI values across near-equivalent models",
  subtitle = paste(
    "Points are means and whiskers are descriptive minima and maxima across ten plausible values.",
    "The union of each model's top ten atomic groups is shown; the overlapping socioeconomic block is separate."
  ),
  top_n = 10L,
  feature_labels = pisa_plot_feature_labels,
  learner_order = c("xgboost", "random_forest", "ridge"),
  base_size = 16,
  x_label = "Held-out RMSE increase",
  axis_text_size = 12.5,
  axis_title_size = 13.5,
  strip_text_size = 13
)
.save_plot(
  importance_plot,
  file.path(run$dirs$figures, "pisa_model_importance_ranks"),
  width = 11,
  height = 8,
  min_component_width = 7.5
)

immigration_missing = technical_subgroup_missingness[
  audit_variable == "audit_immigration",
  .(n_missing, n_total)
]
immigration_missing_note = sprintf(
  "Immigration-audit values are missing for %s/%s students and are excluded from that panel.",
  format(immigration_missing$n_missing, big.mark = ",", scientific = FALSE),
  format(immigration_missing$n_total, big.mark = ",", scientific = FALSE)
)
subgroup_plot = csdg_plot_data(
  technical_subgroup_audit,
  type = "subgroup",
  metric = "rmse",
  title = "PISA PV1 technical subgroup boundary diagnostic",
  subtitle = paste(
    "School-clustered 95% percentile-bootstrap intervals stratified within country and conditional on OOF",
    "predictions; the RMSE axis starts at zero, and this is not a population subgroup claim.",
    immigration_missing_note
  ),
  audit_labels = pisa_audit_labels,
  subgroup_labels = pisa_subgroup_labels,
  subgroup_order = c("Female", "Male", "Native", "Second-generation", "First-generation"),
  x_limits = c(
    0,
    max(technical_subgroup_audit$uncertainty[metric == "rmse", ci_high], na.rm = TRUE) * 1.08
  ),
  base_size = 13,
  direct_label_size = 3,
  axis_text_size = 11,
  axis_title_size = 12
)
.save_plot(subgroup_plot, file.path(run$dirs$figures, "pisa_g7_subgroups"), width = 10, height = 4.6)
contrast_plot = csdg_plot_data(
  technical_subgroup_audit$contrasts,
  type = "subgroup_contrast",
  metric = "rmse",
  title = "PISA PV1 technical subgroup RMSE contrasts",
  subtitle = paste(
    "Red points are first-minus-second RMSE differences; blue whiskers are school-clustered 95%",
    "percentile-bootstrap intervals stratified within country and conditional on OOF predictions.",
    "This is a technical boundary diagnostic, not a population subgroup claim."
  ),
  caption = immigration_missing_note,
  audit_labels = pisa_audit_labels,
  subgroup_labels = pisa_subgroup_labels,
  x_label = "RMSE difference (first - second)",
  base_size = 13,
  direct_label_size = 3.4,
  axis_text_size = 11
)
.save_plot(
  contrast_plot,
  file.path(run$dirs$figures, "pisa_g7_subgroup_contrasts"),
  width = 10,
  height = 5
)

pv_plot_data = pooled_performance[measure_id %in% c("regr.rmse", "regr.rsq")]
pv_plot = csdg_plot_data(
  pv_plot_data,
  type = "measurement_trajectory",
  title = "PISA all-PV common-split learner benchmark",
  subtitle = paste(
    "Each line connects one model from PV1 through PV10 in numeric order; across-PV variation is descriptive",
    "and is not Rubin-pooled uncertainty"
  ),
  x_label = "Plausible value",
  y_label = "Pooled out-of-fold estimate",
  base_size = 13,
  axis_text_size = 11,
  axis_title_size = 11.5,
  legend_text_size = 11,
  legend_title_size = 11.5,
  strip_text_size = 11
) +
  labs(color = "Model", shape = "Model", linetype = "Model")
pv_legend_plot = csdg_plot_legend(pv_plot)
.save_plot(
  pv_plot + theme(legend.position = "none"),
  file.path(run$dirs$figures, "pisa_all_pv_benchmark"),
  width = 10,
  height = 4.5
)
.save_plot(
  pv_legend_plot,
  file.path(run$dirs$figures, "pisa_all_pv_benchmark__legend"),
  width = 10,
  height = 1.25,
  metadata = list(
    figure_group_id = "pisa_all_pv_benchmark",
    is_grouped_figure = TRUE,
    latex_group_id = "pisa_all_pv_benchmark",
    panel_id = "shared_legend",
    panel_order = 3L,
    panel_heading = "Shared model legend",
    group_title = pv_plot$labels$title,
    group_subtitle = pv_plot$labels$subtitle,
    latex_assembly_note = paste(
      "Shared legend for the two all-PV benchmark panels; place it once beneath the panels with LaTeX."
    )
  )
)

pfi_plot = .analysis_pisa_pfi_plot(
  pfi_descriptive,
  feature_labels = pisa_feature_labels,
  top_n = 15L,
  base_size = 15,
  direct_label_size = 3.8
)
pfi_plot = pfi_plot + labs(
  title = "PISA held-out PFI across all plausible values",
  subtitle = paste(
    "Fourteen leading atomic groups plus the separately marked overlapping socioeconomic joint query;",
    "whiskers are descriptive plausible-value ranges, not confidence intervals."
  )
)
.save_plot(pfi_plot, file.path(run$dirs$figures, "pisa_all_pv_pfi"), width = 10, height = 7)

relative_loco_penalty = 100 * mean(loco_country_penalty_summary$mean_penalty) /
  mean(loco_country_penalty_summary$mean_common_split_oof_rmse)
loco_plot = csdg_plot_data(
  loco_country_penalty_summary,
  type = "setting_penalty",
  title = "PISA observed-country exclusion stress test",
  subtitle = paste(
    "Penalty = LOCO RMSE minus matched same-country common-split OOF XGBoost RMSE.",
    paste(
      "Red points are means across 10 plausible values; blue whiskers are descriptive PV min-max ranges,",
      "not 95% CIs."
    ),
    sprintf(
      "The observed-country average is %.1f RMSE points (%.1f%% of matched OOF RMSE); positive values are worse.",
      mean(loco_country_penalty_summary$mean_penalty),
      relative_loco_penalty
    ),
    "Training sizes and compositions differ, so this boundary diagnostic is not a causal effect."
  ),
  x_label = "Country-exclusion penalty in RMSE (LOCO - common-split OOF)",
  y_label = "Held-out country",
  base_size = 13,
  axis_text_size = 11,
  axis_title_size = 11.5
)
.save_plot(loco_plot, file.path(run$dirs$figures, "pisa_g6_transport"), width = 10, height = 9)

ale_plot = csdg_plot_data(
  rich_ale_1d_public,
  type = "ale",
  title = "PISA PV1 dependence-aware one-dimensional ALE",
  subtitle = paste(
    "Ribbon: pointwise 95% country-stratified school-bootstrap percentile interval from complete learner",
    "refits; not simultaneous and conditional on PV1 and the fixed learner specification. A ribbon is omitted",
    "where fewer than 80% of refits meet the interval-support threshold."
  ),
  feature_labels = pisa_feature_labels,
  base_size = 13,
  axis_title_size = 10.5
)
.save_plot(ale_plot, file.path(run$dirs$figures, "pisa_rich_ale_1d"), width = 10, height = 6)

ice_prediction_range = range(c(
  rich_effects$ice_quantiles$q05_prediction,
  rich_effects$ice_quantiles$q95_prediction
), finite = TRUE)
if (length(ice_prediction_range) != 2L || any(!is.finite(ice_prediction_range)) || diff(ice_prediction_range) <= 0) {
  stop("PISA ICE quantiles do not define a finite nonzero prediction range.", call. = FALSE)
}
pisa_ice_y_limits = range(pretty(ice_prediction_range, n = 6L))
ice_plot = csdg_plot_data(
  rich_effects$ice_quantiles,
  type = "ice",
  title = "PISA PV1 aggregate ICE model queries",
  subtitle = paste(
    "The outer band is pointwise q05-q95, the inner band is q25-q75, and the line is",
    "the pointwise median across row-level ICE curves; these are descriptive curve quantiles, not CIs."
  ),
  feature_labels = pisa_feature_labels,
  y_label = "Predicted reading score",
  y_limits = pisa_ice_y_limits,
  base_size = 13,
  axis_title_size = 11
)
ice_legend_plot = csdg_plot_legend(ice_plot)
.save_plot(
  ice_plot + theme(legend.position = "none"),
  file.path(run$dirs$figures, "pisa_rich_ice_quantiles"),
  width = 10,
  height = 6,
  metadata = list(
    latex_assembly_note = paste(
      "Assemble the four ICE panels in panel_order as a 2-row by 2-column figure on the common y scale, then",
      "place the shared quantile legend once beneath them with LaTeX."
    )
  )
)
.save_plot(
  ice_legend_plot,
  file.path(run$dirs$figures, "pisa_rich_ice_quantiles__legend"),
  width = 10,
  height = 1.25,
  metadata = list(
    figure_group_id = "pisa_rich_ice_quantiles",
    is_grouped_figure = TRUE,
    latex_group_id = "pisa_rich_ice_quantiles",
    panel_id = "shared_legend",
    panel_order = 5L,
    panel_heading = "Shared ICE quantile legend",
    group_title = ice_plot$labels$title,
    group_subtitle = ice_plot$labels$subtitle,
    latex_assembly_note = paste(
      "Shared legend for the four common-scale ICE panels; place it once beneath the 2-row by 2-column panel",
      "assembly with LaTeX."
    )
  )
)

interaction_plot = csdg_plot_data(
  rich_interactions$interaction_screen,
  type = "interaction",
  title = "PISA PV1 interaction screen",
  feature_labels = pisa_feature_labels,
  base_size = 13,
  direct_label_size = 3.6
)
.save_plot(
  interaction_plot,
  file.path(run$dirs$figures, "pisa_rich_interaction_screen"),
  width = 9,
  height = 5.5
)

pisa_ale_2d_feature_labels = .analysis_pisa_plot_feature_labels(manifest)
ale_2d_plot = if (ale_2d_available) {
  csdg_plot_data(
    rich_ale_2d_public,
    type = "ale_2d",
    title = "PISA PV1 accumulated local interaction surface",
    subtitle = paste0(
      "Irregular rectangles follow empirical cell boundaries; gray cells have fewer than ",
      ale_2d_min_cell_n,
      " sampled observations. Model-fitting uncertainty is not represented."
    ),
    feature_labels = pisa_ale_2d_feature_labels,
    base_size = 13
  )
} else {
  ggplot() +
    annotate(
      "rect",
      xmin = 0.08,
      xmax = 0.92,
      ymin = 0.28,
      ymax = 0.72,
      fill = "grey96",
      color = .analysis_palette()$metric[["primary"]],
      linewidth = 0.7
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = paste(
        "2D ALE not computed",
        "No screened H statistic exceeded numerical tolerance.",
        sep = "\n"
      ),
      lineheight = 1.35,
      size = 4.2,
      color = "grey20"
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(
      title = "PISA PV1 accumulated local interaction surface",
      subtitle = .analysis_wrap_text(paste(
        "Technical status record: the prespecified interaction screen did not support selecting a pair,",
        "so no second-order surface was estimated."
      ))
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "plain", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, color = "grey30", margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 14, 12, 14)
    )
}
.save_plot(
  ale_2d_plot,
  file.path(run$dirs$figures, "pisa_rich_ale_2d"),
  width = 9,
  height = if (ale_2d_available) 6 else 5
)

if (multiplicity_available) {
  multiplicity_plot = csdg_plot_data(
    multiplicity_bins,
    type = "multiplicity",
    quantiles = multiplicity_distribution,
    title = "PISA PV1 prediction multiplicity across near-equivalent models",
    subtitle = paste(
      "Binned empirical CDF from shareable aggregate counts; red markers identify exact aggregate p50, p90,",
      "and p95 values and therefore need not lie on bin steps. Row-level ranges remain private."
    ),
    x_label = "Within-student OOF prediction range (reading-score points)",
    base_size = 13,
    direct_label_size = 3.6
  )
} else {
  multiplicity_plot = ggplot(data.table(x = 1, y = 1), aes(x = x, y = y)) +
    annotate("text", x = 1, y = 1, label = multiplicity_status$interpretation, size = 4) +
    xlim(0, 2) +
    ylim(0, 2) +
    labs(title = "PISA prediction multiplicity: not estimable") +
    theme_void()
}
.save_plot(
  multiplicity_plot,
  file.path(run$dirs$figures, "pisa_prediction_multiplicity"),
  width = 9,
  height = 5.5
)

# ---- Figure catalog and LaTeX metadata ----

figure_catalog = .analysis_pisa_figure_catalog(
  support_threshold = max(ale_1d_min_interval_n, ale_2d_min_cell_n),
  reference_n = min(settings$effect_rows, primary_task$nrow),
  bootstrap_replicates = settings$ale_bootstrap_replicates
)
if (!ale_2d_available) {
  unavailable_ale_2d_semantics = .analysis_ale_semantics(
    "PISA",
    "2d",
    support_threshold = max(ale_1d_min_interval_n, ale_2d_min_cell_n),
    reference_n = min(settings$effect_rows, primary_task$nrow),
    bootstrap_replicates = settings$ale_bootstrap_replicates,
    available = FALSE
  )
  figure_catalog[figure_id == "pisa_rich_ale_2d", `:=`(
    estimand_scope = unavailable_ale_2d_semantics$estimand_or_scope,
    uncertainty_semantics = unavailable_ale_2d_semantics$uncertainty_semantics,
    role = "technical status record"
  )]
}
figure_catalog = .analysis_expand_figure_catalog(
  figure_catalog,
  study = "PISA",
  generator_script = "analysis/pisa/analysis.R",
  generator_function = "PISA publication figure construction block",
  run_dir = run$private_dir
)
pdf_stems = sub(
  "\\.pdf$",
  "",
  basename(list.files(run$dirs$figures, pattern = "\\.pdf$", full.names = TRUE))
)
png_stems = sub(
  "\\.png$",
  "",
  basename(list.files(run$dirs$figures, pattern = "\\.png$", full.names = TRUE))
)
if (anyDuplicated(figure_catalog$figure_id) ||
    !setequal(figure_catalog$figure_id, pdf_stems) ||
    !setequal(figure_catalog$figure_id, png_stems)) {
  stop("The PISA figure catalog does not cover every PDF/PNG figure pair exactly.", call. = FALSE)
}
catalog_sources = .analysis_catalog_source_paths(figure_catalog)
validate_expected_artifacts(run$private_dir, catalog_sources)
.write_table(figure_catalog, file.path(run$dirs$provenance, "figure_catalog.csv"))
.analysis_write_figure_latex_metadata(
  figure_catalog,
  file.path(run$dirs$provenance, "figure_latex_metadata.csv"),
  run_dir = run$private_dir
)

# ---- Provenance and reproducibility records ----

input_records = rbindlist(lapply(seq_along(analysis_inputs), function(index) {
  .input_record(analysis_inputs[[index]], names(analysis_inputs)[[index]])
}))
.write_table(input_records, file.path(run$dirs$provenance, "input_checksums.csv"))
.write_table(package_source$manifest, file.path(run$dirs$provenance, "package_source_manifest.csv"))
.write_table(
  data.table(tree_hash = package_source$tree_hash, algorithm = "SHA-256"),
  file.path(run$dirs$provenance, "package_source_tree_hash.csv")
)
review_copied = file.copy(
  review_path,
  file.path(run$dirs$provenance, "preprocessing_review.md"),
  overwrite = TRUE
)
if (!isTRUE(review_copied)) {
  stop("Could not copy preprocessing_review.md into the run provenance.", call. = FALSE)
}
review_run_path = file.path(run$dirs$provenance, "preprocessing_review.md")
writeLines(
  c(
    readLines(review_run_path, warn = FALSE),
    "",
    "## Run-specific evidence reference",
    "",
    paste0("- Run ID: `", run$run_id, "`"),
    "- Verification status: `verified_by_responsible_coauthors`",
    "- Verification date: `2026-09-09`",
    "- Evidence: `tables/`, `audit_bundle/`, `logs/`, `checkpoints/`, and `provenance/` in this run.",
    paste(
      "- This records group-level co-author verification and run linkage; it is not an independent compact-source",
      "reconstruction."
    )
  ),
  review_run_path
)
writeLines(capture.output(sessionInfo()), file.path(run$dirs$provenance, "sessionInfo.txt"))
write_run_provenance(
  run,
  source_files = analysis_sources,
  input_files = analysis_inputs,
  extra = list(
    quick = settings$quick,
    threads = settings$threads,
    folds = settings$folds,
    n_per_country = settings$n_per_country,
    n_analytic = nrow(dt),
    n_countries = uniqueN(dt$CNT),
    n_schools = uniqueN(dt$audit_school),
    n_predictors = length(predictor_names),
    n_plausible_values = length(tasks),
    n_loco_refits = uniqueN(loco_scores[, .(outcome_id, held_out_group)]),
    n_loco_matched_comparisons = nrow(loco_country_pv_comparison),
    mean_loco_country_exclusion_penalty = mean(loco_country_penalty_summary$mean_penalty),
    langn_999_source = langn_999_source,
    manifest_langn_missing_rate = manifest_langn$missing_rate,
    survey_weights_available = FALSE,
    inferential_target = "unweighted equal-country analytic sample",
    across_pv_summary = "descriptive, not Rubin pooled",
    package_source_tree_hash = package_source$tree_hash,
    analysis_run_signature = run_signature,
    checkpoint_directory = checkpoint_directory,
    loaded_package_path = find.package("mlr3autoiml"),
    preprocessing_verification = "verified_by_responsible_coauthors",
    preprocessing_verification_date = "2026-09-09",
    preprocessing_verification_scope = "group-level co-author confirmation; no independent source reconstruction",
    ale_bootstrap_resampling = "schools independently within country",
    ale_bootstrap_replicates_requested = rich_ale_bootstrap$settings$replicates,
    ale_bootstrap_refits_successful = rich_ale_bootstrap$settings$successful_refits,
    ale_bootstrap_interval = "pointwise 95% percentile; not simultaneous; PV1 conditional",
    ale_bootstrap_grid = "common boundaries from the reproducibly sampled point-reference rows",
    ale_bootstrap_minimum_interval_support_fraction = rich_ale_bootstrap$settings$min_success_fraction
  )
)

# ---- Completion checks ----

write_run_checkpoint(
  run,
  "pisa_core_analysis",
  "completed",
  list(
    overall_status = claim_support$overall_status,
    n_loco_refits = 320L,
    n_loco_matched_comparisons = 320L
  )
)
stop_run_logging(run_logging)

# ---- Sanitized release finalization ----

upload_dir = .finalize_study_output(
  run = run,
  upload_expected = upload_expected,
  package_source = package_source,
  project_root = project_root,
  analysis_checkpoint = "pisa_analysis",
  finalization_checkpoint = "pisa_finalization",
  label = "PISA"
)

cat("PISA private run: ", run$private_dir, "\n", sep = "")
cat("PISA sanitized upload: ", upload_dir, "\n", sep = "")
