#!/usr/bin/env Rscript
# SHILD case study for the Claim-Scoped Diagnostic Gates manuscript.

# ---- Setup and dependencies ----

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/shild/analysis.R"
}
analysis_dir = dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
source(file.path(analysis_dir, "shared", "study_helpers.R"), local = FALSE)
source(file.path(analysis_dir, "shild", "prepare_data.R"), local = FALSE)
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

settings = .analysis_settings("SHILD")
data_path = file.path(
  project_root, "data", "SHILD", "archive", "Danish_SHILD_draft_data.csv"
)
dictionary_path = file.path(project_root, "data", "SHILD", "archive", "Danish_SHILD_dictionnary.csv")
review_path = file.path(project_root, "analysis", "preprocessing_review.md")
protocol_path = file.path(project_root, "analysis", "protocol.yml")
.require_files(c(data_path, dictionary_path, review_path, protocol_path))

upload_expected = c(
  "tables/shild_run_mode.csv",
  "tables/shild_sample_flow.csv",
  "tables/shild_class_balance.csv",
  "tables/shild_missingness.csv",
  "tables/shild_sentinel_recode_audit.csv",
  "tables/shild_variable_recode_spec.csv",
  "tables/shild_value_recode_audit.csv",
  "tables/shild_domain_value_audit.csv",
  "tables/shild_fold_assignment.csv",
  "tables/shild_g1_benchmark_fold_scores.csv",
  "tables/shild_g1_benchmark_summary.csv",
  "tables/shild_benchmark_model_specification.csv",
  "tables/shild_complete_case_sensitivity.csv",
  "tables/shild_complete_case_fold_scores.csv",
  "tables/shild_g3_calibration_plot_source.csv",
  "tables/shild_g3_calibration_bootstrap_summary.csv",
  "tables/shild_g3_calibration_bootstrap_curve.csv",
  "tables/shild_g3_calibration_curve_descriptive_bins.csv",
  "tables/shild_g4_local_fidelity_source.csv",
  "tables/shild_claim_versions.csv",
  "tables/shild_report_card.csv",
  "tables/shild_claim_support.csv",
  "tables/shild_optional_model_query_profiles.csv",
  "tables/shild_optional_model_query_interactions.csv",
  "tables/shild_rich_ale_1d.csv",
  "tables/shild_rich_pdp.csv",
  "tables/shild_rich_ice_quantiles.csv",
  "tables/shild_rich_ice_spread.csv",
  "tables/shild_rich_interaction_screen.csv",
  "tables/shild_rich_ale_2d.csv",
  "tables/shild_conditional_shap_quality_summary.csv",
  "tables/shild_model_ranks.csv",
  "tables/shild_model_importance_agreement.csv",
  "tables/shild_model_importance_direct_differences.csv",
  "tables/shild_model_importance_pairwise.csv",
  "tables/shild_model_importance_definitions.csv",
  "tables/shild_grouped_pfi_summary.csv",
  "tables/shild_grouped_pfi_per_iteration.csv",
  "tables/shild_near_equivalence_tolerance_sensitivity.csv",
  "tables/shild_performance_explanation_sensitivity.csv",
  "tables/shild_multiplicity_status.csv",
  "tables/shild_multiplicity_distribution.csv",
  "tables/shild_multiplicity_bins.csv",
  "tables/shild_validation.csv",
  "tables/shild_gate_g1__performance__summary.csv",
  "tables/shild_gate_g2__pairwise.csv",
  "tables/shild_gate_g2__support.csv",
  "tables/shild_gate_g3a__calibration__curve.csv",
  "tables/shild_gate_g3a__calibration__summary.csv",
  "tables/shild_gate_g3b__decision_curve.csv",
  "tables/shild_gate_g5__pfi__summary.csv",
  "tables/shild_gate_g6a__model_generalization__candidates.csv",
  "tables/shild_gate_g6a__model_generalization__explanation_agreement__pairwise_top_k.csv",
  "tables/shild_gate_g6a__model_generalization__explanation_agreement__ranks.csv",
  "tables/shild_gate_g7a__subgroup_audit__counts.csv",
  "tables/shild_gate_g7a__subgroup_audit__metrics.csv",
  "tables/shild_gate_g7a__subgroup_audit__uncertainty.csv",
  "tables/shild_gate_g7a__subgroup_audit__contrasts.csv",
  "figures",
  "audit_bundle/shild_csdg_audit/report_card.csv",
  "audit_bundle/shild_csdg_audit/claim_report.csv",
  "audit_bundle/shild_csdg_audit/gates",
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
  study = "shild",
  output_root = file.path(project_root, "paper_artifacts"),
  run_id = if (nzchar(settings$run_id)) settings$run_id else NULL,
  mode = if (settings$quick) "smoke_test" else "publication",
  seed = settings$seed,
  expected_artifacts = upload_expected
)
analysis_sources = c(
  shild_analysis = normalizePath(script_path, winslash = "/", mustWork = TRUE),
  study_helpers = file.path(analysis_dir, "shared", "study_helpers.R"),
  shild_preparation = file.path(analysis_dir, "shild", "prepare_data.R"),
  output = file.path(analysis_dir, "shared", "output.R"),
  output_audit = file.path(analysis_dir, "shared", "output_audit.R")
)
analysis_inputs = c(
  shild_data = data_path,
  shild_dictionary = dictionary_path,
  preprocessing_review = review_path,
  analysis_protocol = protocol_path
)
run_signature = .analysis_run_signature(
  package_source,
  analysis_sources,
  analysis_inputs,
  settings,
  "SHILD"
)
checkpoint_directory = .checkpoint_directory(project_root, run, settings)

# ---- Data preparation and analytic sample ----

run_logging = start_run_logging(run, failure_checkpoint = "shild_analysis")
write_run_checkpoint(run, "shild_analysis", "started", list(mode = settings$mode))
message("Reading SHILD data from ", data_path)
source_data = fread(data_path, na.strings = c("", "NA", "NaN"))
preparation = .prepare_shild_raw(source_data)
target = "life_satisfaction"
source_predictors = preparation$predictors
if (length(source_predictors) != 27L) {
  stop("Expected exactly 27 SHILD predictors; found ", length(source_predictors), ".", call. = FALSE)
}
dt = preparation$data
if (!nrow(dt) || nlevels(dt[[target]]) != 2L) {
  stop("The raw-derived SHILD target is not a non-empty binary factor.", call. = FALSE)
}
if (!identical(range(dt$age), c(16L, 64L))) {
  stop("The SHILD analytic age range is not exactly 16 through 64.", call. = FALSE)
}
missingness = rbindlist(
  list(preparation$missingness_before, preparation$missingness_after),
  use.names = TRUE
)
sentinel_audit = preparation$sentinel_audit

dt[, audit_gender := factor(as.character(gender))]
dt[, audit_age_group := cut(
  age,
  breaks = c(15, 24, 34, 44, 54, 64),
  labels = c("16-24", "25-34", "35-44", "45-54", "55-64"),
  include.lowest = TRUE,
  right = TRUE
)]
for (column in source_predictors[vapply(dt[, ..source_predictors], is.character, logical(1L))]) {
  set(dt, j = column, value = factor(dt[[column]]))
}
audit_columns = c("respondent_id", "audit_gender", "audit_age_group")

# ---- Task definition and held-out splits ----

task = TaskClassif$new(
  id = "shild_life_satisfaction",
  backend = dt,
  target = target,
  positive = "discontent"
)
for (column in audit_columns) {
  task$set_col_roles(column, remove_from = "feature")
}
if (!setequal(task$feature_names, source_predictors) || any(audit_columns %in% task$feature_names)) {
  stop("SHILD feature-role validation failed.", call. = FALSE)
}

split = .stratified_resampling(task, dt[[target]], settings$folds, settings$seed)
resampling = split$resampling
fold_assignment = merge(
  split$assignment,
  data.table(row_id = task$row_ids, respondent_id = dt$respondent_id),
  by = "row_id",
  all.x = TRUE,
  sort = FALSE
)
if (any(fold_assignment[, uniqueN(stratum), by = fold]$V1 < 2L)) {
  stop("At least one SHILD fold lacks one outcome class.", call. = FALSE)
}

# ---- Learners and common-split benchmark ----

focal = .make_pipeline_learner(
  lrn(
    "classif.xgboost",
    predict_type = "prob",
    nrounds = settings$xgb_rounds,
    max_depth = 3L,
    eta = 0.05,
    nthread = settings$threads,
    subsample = 1,
    colsample_bytree = 1
  ),
  "shild_xgboost"
)
ridge_logistic = .make_pipeline_learner(
  lrn(
    "classif.cv_glmnet",
    predict_type = "prob",
    alpha = 0,
    nfolds = 5L,
    s = "lambda.1se"
  ),
  "shild_ridge_logistic"
)
random_forest = .make_pipeline_learner(
  lrn(
    "classif.ranger",
    predict_type = "prob",
    num.trees = settings$forest_trees,
    min.node.size = 5L,
    num.threads = settings$threads,
    seed = settings$seed
  ),
  "shild_ranger"
)
decision_tree = .make_pipeline_learner(
  lrn("classif.rpart", predict_type = "prob", maxdepth = 5L, minsplit = 40L),
  "shild_rpart"
)
featureless = lrn("classif.featureless", predict_type = "prob")
featureless$id = "shild_featureless"
candidates = list(
  xgboost = focal,
  ridge_logistic = ridge_logistic,
  random_forest = random_forest,
  decision_tree = decision_tree,
  featureless = featureless
)
benchmark_model_specification = data.table(
  learner_id = names(candidates),
  model = c(
    "XGBoost classifier",
    "Ridge logistic regression with nested training-fold cross-validation",
    "Random forest",
    "Decision tree",
    "Featureless prevalence baseline"
  ),
  prespecified_settings = c(
    paste0(
      "nrounds=", settings$xgb_rounds,
      "; max_depth=3; eta=0.05; subsample=1; colsample_bytree=1"
    ),
    "alpha=0; lambda selected by five-fold internal cross-validation; prediction at lambda.1se",
    paste0("num.trees=", settings$forest_trees, "; min.node.size=5"),
    "maxdepth=5; minsplit=40",
    "Training-fold outcome prevalence"
  ),
  rationale = c(
    "Prespecified focal nonlinear learner",
    paste(
      "Penalization prevents the separation and nonconvergence produced by unpenalized logistic regression",
      "with sparse questionnaire factor levels"
    ),
    "Prespecified nonlinear model-class sensitivity",
    "Prespecified low-complexity model-class sensitivity",
    "Prespecified no-information baseline"
  )
)
measures = msrs(c("classif.auc", "classif.prauc", "classif.bbrier", "classif.logloss"))

message("Running the prespecified common-split SHILD learner benchmark")
benchmark_checkpoint = .run_checkpoint(
  checkpoint_directory,
  "shild_g1_benchmark",
  run_signature,
  list(
    folds = settings$folds,
    learners = names(candidates),
    measures = vapply(measures, function(measure) measure$id, character(1L))
  ),
  function() {
    results = lapply(seq_along(candidates), function(index) {
      csdg_resample(
        task,
        candidates[[index]]$clone(deep = TRUE),
        resampling,
        measures = measures,
        store_models = FALSE,
        seed = settings$seed + 2000000L + index * 10000L
      )
    })
    names(results) = names(candidates)
    fold_scores = rbindlist(lapply(names(results), function(learner_id) {
      .fold_performance_long(results[[learner_id]], target, learner_id)
    }))
    summary = fold_scores[, .(
      n_folds = sum(is.finite(estimate)),
      mean = mean(estimate, na.rm = TRUE),
      sd = sd(estimate, na.rm = TRUE),
      q10 = quantile(estimate, 0.10, na.rm = TRUE, names = FALSE),
      median = median(estimate, na.rm = TRUE),
      q90 = quantile(estimate, 0.90, na.rm = TRUE, names = FALSE),
      uncertainty = "Descriptive fold variation; folds are not independent inferential units"
    ), by = .(learner_id, measure_id)]
    list(results = results, fold_scores = fold_scores, summary = summary)
  }
)
benchmark = benchmark_checkpoint$value

message("Selecting two post-hoc communication cases from benchmark OOF probabilities")
case_selection = .select_probability_cases(benchmark$results$xgboost, c(0.10, 0.90))
case_selection = merge(
  case_selection,
  data.table(row_id = task$row_ids, respondent_id = dt$respondent_id),
  by = "row_id",
  all.x = TRUE,
  sort = FALSE
)

# ---- Focal claim, measurement, and diagnostic configuration ----

claim_registry = .analysis_shild_claim_registry()
c0 = claim_registry[claim_id == "SHILD-C0"]
c1 = claim_registry[claim_id == "SHILD-C1"]
c2 = claim_registry[claim_id == "SHILD-C2"]

claim_original = csdg_claim(
  id = "SHILD-C0",
  statement = c0$statement,
  claim_type = c("local_explanation", "decision"),
  target = c0$target,
  semantics = "fitted_model_description",
  unit = "SHILD respondent",
  population = "Supplied Danish SHILD respondents aged 16-64 with an observed extreme-response label",
  analytic_distribution = c0$analytic_distribution,
  model_scope = "cross_fitted_pipeline",
  setting_scope = "analytic_sample",
  scientific_use = c0$scientific_use,
  explanation_design = c0$explanation_design,
  claim_level = "substantive",
  use_claim = TRUE,
  claim_version = "C0",
  revision_relation = "original",
  intended_users = "Trained research staff in a methods demonstration",
  action = "Offer a non-diagnostic follow-up assessment",
  thresholds = seq(0.03, 0.20, by = 0.01),
  consequences = paste(
    "False positives add assessment burden, false negatives can miss an opportunity for follow-up,",
    "and no action is autonomous."
  ),
  subgroup_variables = c("audit_gender", "audit_age_group"),
  confirmatory = FALSE,
  notes = paste(
    "Cross-sectional predictions and explanations do not identify causal or treatment effects. Marginal PFI",
    "and independently perturbed local surrogates do not establish within-support explanation validity."
  )
)
claim = csdg_claim_revision(
  claim_original,
  id = "SHILD-C1",
  claim_version = "C1",
  revision_relation = "alternative_or_incomparable",
  statement = c1$statement,
  claim_type = c("predictive_performance", "local_explanation"),
  target = c1$target,
  analytic_distribution = c1$analytic_distribution,
  model_scope = "cross_fitted_pipeline",
  intended_users = "Methods researchers evaluating the local-surrogate procedure",
  action = NULL,
  thresholds = NULL,
  consequences = NULL,
  subgroup_variables = NULL,
  scientific_use = c1$scientific_use,
  explanation_design = c1$explanation_design,
  claim_level = "substantive",
  use_claim = FALSE
)
claim_stronger_excluded = csdg_claim_revision(
  claim_original,
  id = "SHILD-C2",
  claim_version = "C2",
  revision_relation = "narrower",
  statement = c2$statement,
  claim_type = "local_explanation",
  target = c2$target,
  analytic_distribution = c2$analytic_distribution,
  model_scope = "cross_fitted_pipeline",
  intended_users = "Users of a specified individual-level interpretation workflow",
  action = NULL,
  thresholds = NULL,
  consequences = NULL,
  subgroup_variables = NULL,
  scientific_use = c2$scientific_use,
  explanation_design = c2$explanation_design,
  claim_level = "substantive",
  use_claim = TRUE
)
claim_versions = claim_registry[, .(
  claim_id,
  claim_version,
  relation = revision_relation,
  parent_claim_id,
  statement,
  evidential_status = decision,
  status_basis = decision_rationale,
  linked_evidence_ids,
  linked_evidence_sources
)]
measurement = csdg_measurement(
  outcome = target,
  predictors = source_predictors,
  data_source = "Public SHILD 2012 release (Dryad, CC0) as CSV, with an explicit reproducible recode ledger",
  sample_definition = "A1 codes 1/2 versus 4/5 and age 16-64 inclusive; A1 codes 3/8/9 excluded",
  unit = "respondent",
  outcome_scale = "binary factor: content, discontent",
  missingness = list(
    primary = "Fold-contained median/mode imputation",
    sensitivity = "Complete cases using the retained prespecified fold allocation",
    table = "tables/shild_missingness.csv"
  ),
  preprocessing = list(
    description = paste(
      "Raw-code mapping before resampling, followed by training-fold median/mode imputation,",
      "one-hot encoding, and constant-column removal"
    ),
    sentinel_rule = paste(
      "Documented categorical nonresponses, audited high codes, F15 values outside 0-10, and impossible",
      "or implausible adult height below 100 cm and weight below 20 kg are recoded. Alternative units are not",
      "inferred or converted. The responsible co-authors verified the analytic cut and preprocessing."
    )
  ),
  verification = list(
    status = "verified_by_responsible_coauthors",
    artifact = "provenance/preprocessing_review.md",
    notes = paste(
      "Group-level co-author verification dated 2026-09-09; this is not an independent rerun from the source data",
      "or external reconstruction."
    )
  ),
  audit_variables = c("audit_gender", "audit_age_group"),
  weights = "No analysis weights were supplied",
  notes = "Raw age and gender remain predictors; derived audit duplicates do not."
)
explanation = csdg_explanation(
  method_ids = c("ale", "pdp", "ice", "shapley", "pfi", "local_surrogate"),
  scope = c("global", "local"),
  target = "Probability of discontent",
  perturbation = list(
    ale = "empirical numeric intervals; associational full-fit model description",
    pfi = "marginal permutation in held-out analytic folds; model-query sensitivity only",
    local_surrogate = "independent empirical perturbations; communication fidelity only",
    shapley = "approximate conditional background sampling; private communication output only"
  ),
  local_cases = as.integer(case_selection$row_id),
  case_selection = "post_hoc_communication",
  aggregation = "Foldwise held-out PFI; local fidelity for two selected OOF communication cases",
  notes = paste(
    "Optional fitted-model query plots and marginal perturbation diagnostics are descriptive, non-causal",
    "sensitivity outputs. They are not Gate 2 support evidence and do not support explanation generalization."
  )
)
config = csdg_config(
  seed = settings$seed,
  resampling = list(folds = settings$folds, repeats = 1L, store_models = TRUE),
  performance = list(
    measures = c("classif.auc", "classif.prauc", "classif.bbrier", "classif.logloss"),
    primary = "classif.logloss",
    baseline = TRUE
  ),
  dependence = list(max_levels = 50L),
  calibration = list(bins = 10L),
  faithfulness = list(
    n_perturb = settings$local_perturbations,
    kernel_width = 0.75,
    target_scale = "response"
  ),
  stability = list(
    pfi_repetitions = settings$pfi_repetitions,
    loss = "logloss",
    top_k = 5L,
    min_top_k_overlap = NULL
  ),
  generalization = list(
    rashomon_tolerance_absolute = 0,
    rashomon_tolerance_relative = 0.10,
    rashomon_reference_learner = "xgboost"
  ),
  subgroup = list(
    min_n = if (settings$quick) 20L else 200L,
    thresholds = 0.10,
    conditional_bootstrap = if (settings$quick) 100L else 1000L
  ),
  decision = list(thresholds = seq(0.03, 0.20, by = 0.01)),
  export = list(include_models = FALSE, include_predictions = FALSE)
)
subgroup = data.table(
  row_id = task$row_ids,
  audit_gender = dt$audit_gender,
  audit_age_group = dt$audit_age_group
)

# ---- Claim-scoped diagnostic audit ----

message("Running the SHILD CSDG audit")
result = csdg_audit(
  task = task,
  learner = focal,
  claim = claim,
  measurement = measurement,
  explanation = explanation,
  config = config,
  resampling = resampling,
  candidate_learners = candidates,
  subgroup = subgroup,
  local_cases = as.integer(case_selection$row_id),
  run_gates = c("G0a", "G0b", "G1", "G2", "G3a", "G3b", "G4", "G5", "G6a", "G7a", "G7b")
)
message("Estimating respondent-bootstrap calibration intervals from the focal out-of-fold predictions")
calibration_bootstrap = csdg_calibration_bootstrap(
  result$artifacts$oof,
  task_type = "classif",
  positive = "discontent",
  bins = 10L,
  repetitions = settings$calibration_bootstrap_replicates,
  confidence = 0.95,
  seed = settings$seed + 500000L
)
calibration_bootstrap = .analysis_standardize_calibration_metric(calibration_bootstrap)
result$artifacts$calibration_bootstrap = calibration_bootstrap
result$gates$G3a$evidence$calibration_bootstrap = calibration_bootstrap
message("Running the held-out grouped-PFI sensitivity for the two leading dependent variables")
grouped_pfi = csdg_fold_pfi(
  result$artifacts$oof,
  task = task,
  feature_groups = list(A2_D2_joint = c("A2", "D2")),
  loss = "logloss",
  repetitions = settings$pfi_repetitions,
  seed = settings$seed + 510000L
)
grouped_pfi_summary = copy(grouped_pfi$summary)
grouped_pfi_summary[, interpretation := paste(
  "Held-out joint marginal PFI for A2 and D2 under the same focal fold models and log-loss estimand;",
  "this overlapping sensitivity query is not additive with the two individual PFI values and is not conditional PFI."
)]
grouped_pfi_per_iteration = copy(grouped_pfi$per_iteration)
grouped_pfi_per_iteration[, interpretation := paste(
  "Fold-level joint marginal PFI with Monte Carlo variation from repeated held-out permutations;",
  "folds are dependent partitions and do not define confidence intervals."
)]
report_card = csdg_report_card(result)
claim_support = csdg_claim_report(result)
benchmark_predictions = csdg_oof_predictions(benchmark$results$xgboost, collapse_repeats = TRUE)
audit_predictions = csdg_oof_predictions(result$artifacts$oof, collapse_repeats = TRUE)
benchmark_probability = .prediction_column(benchmark_predictions, "discontent")
audit_probability = .prediction_column(audit_predictions, "discontent")
oof_alignment = merge(
  benchmark_predictions[, .(row_id, benchmark_probability = get(benchmark_probability))],
  audit_predictions[, .(row_id, audit_probability = get(audit_probability))],
  by = "row_id",
  all = TRUE
)
if (nrow(oof_alignment) != task$nrow || anyNA(oof_alignment) ||
    max(abs(oof_alignment$benchmark_probability - oof_alignment$audit_probability)) > 1e-12) {
  stop("Benchmark and final-audit focal OOF predictions are not identical.", call. = FALSE)
}

# ---- Sensitivity analyses and explanatory diagnostics ----

message("Running the SHILD complete-case sensitivity on the retained split allocation")
complete_case = .complete_case_resample(
  dt,
  target,
  audit_columns,
  split$assignment$fold,
  focal,
  measures,
  settings$seed + 2000L
)
complete_case_performance = csdg_performance(complete_case$oof)
complete_case_fold_scores = as.data.table(copy(complete_case_performance$per_iteration))
required_complete_case_columns = c("iteration", "repetition", "fold", "measure", "value")
expected_complete_case_rows = nrow(complete_case$oof$fold_scores) *
  (ncol(complete_case$oof$fold_scores) - 3L)
if (!all(required_complete_case_columns %in% names(complete_case_fold_scores)) ||
    nrow(complete_case_fold_scores) != expected_complete_case_rows ||
    anyNA(complete_case_fold_scores[, ..required_complete_case_columns]) ||
    any(!is.finite(complete_case_fold_scores$value))) {
  stop("SHILD complete-case fold-score export failed its schema or completeness check.", call. = FALSE)
}
complete_case_fold_scores[, interpretation := paste(
  "Complete-case held-out fold score; descriptive sensitivity only, because the retained population differs",
  "from the primary imputed analysis and folds are not independent inferential units."
)]
complete_case_summary = complete_case_performance$summary
complete_case_summary[, `:=`(
  n_source_analytic = nrow(dt),
  n_complete_case = sum(complete_case$retained),
  retained_fraction = mean(complete_case$retained),
  estimand_note = paste(
    "Complete-case sensitivity in the retained subset; the target population can differ from the",
    "primary imputed analysis. Fold scores are descriptive, not independent-sample inference."
  )
)]

message("Computing optional full-fit model-query sensitivity profiles")
optional_effect = .fitted_effect_profiles(
  task,
  focal,
  c("A2", "D2", "M8", "D8"),
  settings$effect_rows,
  settings$effect_grid,
  settings$seed + 3000L,
  positive = "discontent"
)
optional_interactions = .fitted_interaction_profiles(
  task,
  optional_effect$model,
  list(c("A2", "D2"), c("M8", "D8")),
  settings$effect_rows,
  settings$effect_grid,
  settings$seed + 3001L,
  positive = "discontent"
)
ale_2d_min_cell_n = if (settings$quick) 5L else 20L
ale_1d_min_interval_n = if (settings$quick) 5L else 20L
message("Computing dependence-aware numeric ALE, ICE summaries, H screening, and two-dimensional ALE")
rich_effects = csdg_effect_diagnostics(
  task,
  optional_effect$model,
  features = c("age", "E1", "E2", "M6"),
  sample_n = min(settings$effect_rows, task$nrow),
  ale_bins = max(3L, settings$effect_grid),
  grid_n = max(3L, settings$effect_grid),
  ice_keep_n = min(30L, settings$effect_rows),
  seed = settings$seed + 3100L,
  class_label = "discontent"
)
message("Computing respondent-bootstrap model-refit intervals for one-dimensional ALE")
rich_ale_bootstrap = csdg_ale_bootstrap(
  task,
  optional_effect$model,
  features = c("age", "E1", "E2", "M6"),
  sample_n = min(settings$effect_rows, task$nrow),
  ale_bins = max(3L, settings$effect_grid),
  replicates = settings$ale_bootstrap_replicates,
  confidence_level = 0.95,
  resampling_unit = "row",
  min_interval_n = ale_1d_min_interval_n,
  min_success_fraction = 0.80,
  seed = settings$seed + 3100L,
  class_label = "discontent",
  keep_replicates = TRUE,
  parallel_workers = settings$ale_bootstrap_workers,
  verbose = TRUE
)
rich_interactions = csdg_interaction_diagnostics(
  task,
  optional_effect$model,
  features = c("age", "E1", "E2", "M6"),
  sample_n = min(settings$effect_rows, task$nrow),
  h_grid_n = min(6L, max(3L, settings$effect_grid)),
  ale_bins = min(8L, max(3L, settings$effect_grid)),
  top_n_pairs = 1L,
  max_features = 4L,
  seed = settings$seed + 3200L,
  class_label = "discontent"
)
rich_ale_1d_public = rich_ale_bootstrap$ale_1d
rich_ale_2d_public = csdg_mask_effect_support(rich_interactions$ale_2d, ale_2d_min_cell_n)
ale_2d_available = any(is.finite(rich_ale_2d_public$ale2d))
ale_2d_status = if (ale_2d_available) {
  "computed"
} else {
  recorded = unique(rich_ale_2d_public$surface_status)
  recorded = recorded[!is.na(recorded) & nzchar(recorded)]
  if (length(recorded)) recorded[[1L]] else "not_computed"
}
rich_shap = csdg_shapley_diagnostics(
  task,
  optional_effect$model,
  case_rows = as.integer(case_selection$row_id),
  background_n = min(if (settings$quick) 50L else 200L, task$nrow),
  sample_size = if (settings$quick) 32L else 256L,
  seed = settings$seed + 3300L,
  class_label = "discontent",
  mode = "conditional",
  conditional_k = 5L,
  case_selection = "post_hoc_communication"
)
shap_case_ids = sprintf("case_%03d", seq_len(nrow(case_selection)))
observed_shap_case_ids = unique(as.character(rich_shap$local_private$case_id))
observed_additivity_case_ids = as.character(rich_shap$additivity_private$case_id)
if (!identical(observed_shap_case_ids, shap_case_ids) ||
    !identical(observed_additivity_case_ids, shap_case_ids)) {
  stop("SHILD conditional Shapley cases do not follow the declared communication-case order.", call. = FALSE)
}
rich_shap$additivity_private = .analysis_shap_additivity_quality(
  rich_shap$additivity_private,
  absolute_tolerance = 0.01,
  relative_tolerance = 0.25
)
shap_quality_pass = all(rich_shap$additivity_private$meets_additivity_quality)
shap_quality_summary = rich_shap$additivity_private[, .(
  n_communication_cases = .N,
  n_meeting_display_rule = sum(meets_additivity_quality),
  all_cases_meet_display_rule = all(meets_additivity_quality),
  minimum_absolute_additivity_residual = min(absolute_additivity_residual),
  maximum_absolute_additivity_residual = max(absolute_additivity_residual),
  minimum_case_specific_tolerance = min(additivity_tolerance),
  maximum_case_specific_tolerance = max(additivity_tolerance)
)]
shap_quality_summary[, `:=`(
  display_rule = paste(
    "absolute additivity residual <= 0.01 + 0.25 * absolute(case prediction - background baseline)"
  ),
  interpretation = paste(
    "Aggregate quality result for two post-hoc communication cases. Conditional Shapley bars are displayed only",
    "when every case satisfies the declared rule; this is not held-out local-fidelity evidence."
  )
)]
local_case_crosswalk = data.table(
  communication_case = sprintf("Communication case %d", seq_len(nrow(case_selection))),
  g4_case_id = as.character(case_selection$row_id),
  shap_case_id = shap_case_ids,
  selection_quantile = as.numeric(case_selection$selection_quantile),
  focal_oof_probability = as.numeric(case_selection$probability),
  row_id = as.integer(case_selection$row_id),
  respondent_id = case_selection$respondent_id
)
if (!identical(local_case_crosswalk$selection_quantile, c(0.10, 0.90)) ||
    anyNA(local_case_crosswalk) || anyDuplicated(local_case_crosswalk$communication_case) ||
    anyDuplicated(local_case_crosswalk$g4_case_id) || anyDuplicated(local_case_crosswalk$shap_case_id)) {
  stop("SHILD local communication-case crosswalk is incomplete or out of order.", call. = FALSE)
}

local_fidelity_private = as.data.table(copy(result$gates$G4$evidence$summary))
if (!identical(as.character(local_fidelity_private$case_id), local_case_crosswalk$g4_case_id) ||
    nrow(local_fidelity_private) != nrow(local_case_crosswalk)) {
  stop("SHILD Gate 4 local cases do not match the declared communication-case order.", call. = FALSE)
}
local_fidelity_source = local_fidelity_private[, .(
  communication_case = local_case_crosswalk$communication_case,
  selection_quantile = local_case_crosswalk$selection_quantile,
  selection_scope = "Post-hoc case nearest the declared focal out-of-fold prediction-score quantile",
  analysis_role = "Communication-only two-case diagnostic; not the systematic local-fidelity extension",
  generalization_scope = "No generalization beyond the two selected communication cases",
  n_perturb,
  n_crossfit_folds,
  evaluation_method,
  weighted_r2 = as.numeric(weighted_r2),
  weighted_rmse = as.numeric(weighted_rmse),
  apparent_weighted_r2 = as.numeric(apparent_weighted_r2),
  apparent_weighted_rmse = as.numeric(apparent_weighted_rmse),
  effective_weight,
  ridge_penalty_fraction,
  historical_weighted_r2_reference = 0.80,
  meets_historical_reference = weighted_r2 >= 0.80
)]
local_fidelity_metrics = as.matrix(local_fidelity_source[, .(
  weighted_r2, weighted_rmse, apparent_weighted_r2, apparent_weighted_rmse
)])
if (nrow(local_fidelity_source) != 2L || any(!is.finite(local_fidelity_metrics)) ||
    any(local_fidelity_source$meets_historical_reference !=
      (local_fidelity_source$weighted_r2 >= local_fidelity_source$historical_weighted_r2_reference))) {
  stop("SHILD shareable local-fidelity source is incomplete.", call. = FALSE)
}
calibration_plot_source = copy(calibration_bootstrap$curve)
calibration_plot_source[, interval_scope := paste(
  "Pointwise 95% respondent-bootstrap percentile interval conditional on the supplied OOF predictions;",
  "not a simultaneous band and not model-training uncertainty"
)]
complete_case_membership = .analysis_shild_complete_case_membership(complete_case, fold_assignment)

model_ranks = copy(result$artifacts$rashomon$candidates)
near_equivalence_tolerance_sensitivity = csdg_near_equivalence_sensitivity(
  result$artifacts$rashomon,
  tolerance_relative = c(0.05, 0.10, 0.20),
  reference_learner = "xgboost",
  scenario_id = c("focal_plus_5_percent", "focal_plus_10_percent", "focal_plus_20_percent")
)
near_equivalence_tolerance_sensitivity[, interpretation := paste(
  "Sensitivity of accepted-model membership to three focal-XGBoost-relative held-out log-loss allowances;",
  "the grid is descriptive and none of its values is proposed as a universal equivalence threshold."
)]
focal_model_status = result$gates$G6a$diagnostics
focal_model_accepted = isTRUE(focal_model_status$focal_accepted)
performance_explanation_sensitivity = if (focal_model_accepted) {
  csdg_explanation_sensitivity(
    result$artifacts$rashomon,
    result$artifacts$rashomon$explanation_agreement$ranks,
    focal_learner = "xgboost",
    top_k = config$stability$top_k
  )
} else {
  data.table(
    status = "not_estimable",
    reason = paste(
      "The focal XGBoost model is outside the exploratory held-out log-loss tolerance;",
      "a focal-relative explanation comparison is therefore undefined."
    ),
    focal_learner = "xgboost",
    focal_model_accepted = FALSE,
    primary_measure = result$artifacts$rashomon$primary_measure,
    best_score = result$artifacts$rashomon$best_score,
    acceptance_limit = result$artifacts$rashomon$acceptance_limit,
    uncertainty_scope = "Status record only; no confidence interval or hypothesis test."
  )
}
importance_values = copy(result$artifacts$rashomon$explanation_agreement$ranks)
accepted_alternatives = setdiff(model_ranks[accepted == TRUE, learner_name], "xgboost")
importance_agreement_objects = lapply(accepted_alternatives, function(current_learner) {
  reference = importance_values[
    learner_name == "xgboost",
    .(feature_group, importance = mean_importance)
  ]
  comparison = importance_values[
    learner_name == current_learner,
    .(feature_group, importance = mean_importance)
  ]
  csdg_importance_agreement(
    reference,
    comparison,
    top_k = 5L,
    practical_tolerances = c(0, 0.0001, 0.001)
  )
})
names(importance_agreement_objects) = accepted_alternatives
model_importance_agreement = rbindlist(lapply(names(importance_agreement_objects), function(learner_name) {
  table = copy(importance_agreement_objects[[learner_name]]$tolerance_sensitivity)
  top = importance_agreement_objects[[learner_name]]$top_k
  table[, `:=`(
    learner_1 = "xgboost",
    learner_2 = learner_name,
    top_k = top$effective_top_k,
    top_k_intersection = top$intersection,
    top_k_union = top$union,
    top_k_jaccard = top$jaccard,
    interpretation = paste(
      "Descriptive agreement on held-out log-loss PFI. The 0.0001 and 0.001 values are a prespecified",
      "sensitivity grid for near-zero importance separations, not universal practical-equivalence thresholds."
    )
  )]
  table
}), fill = TRUE)
model_importance_agreement = .analysis_add_practical_tie_denominators(model_importance_agreement)
model_importance_direct_differences = rbindlist(lapply(names(importance_agreement_objects), function(learner_name) {
  table = copy(importance_agreement_objects[[learner_name]]$direct_differences)
  table[, `:=`(learner_1 = "xgboost", learner_2 = learner_name)]
  table
}), fill = TRUE)
model_importance_pairwise = rbindlist(lapply(names(importance_agreement_objects), function(learner_name) {
  table = copy(importance_agreement_objects[[learner_name]]$pairwise)
  table[, `:=`(learner_1 = "xgboost", learner_2 = learner_name)]
  table
}), fill = TRUE)
model_importance_definitions = unique(rbindlist(lapply(
  importance_agreement_objects,
  function(object) object$definitions
), fill = TRUE))
if (focal_model_accepted) {
  practical_agreement = model_importance_agreement[practical_tolerance == 0.0001, .(
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
    practical_tolerance = 0.0001,
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
    "Performance and explanation sensitivity use the same accepted set and outer folds.",
    "Practical-tie results use the 0.0001 log-loss-PFI sensitivity value; all quantities are descriptive."
  )]
}
model_ranks[, interpretation := paste(
  "Common-split held-out sensitivity screen under an exploratory 10% focal-XGBoost-relative log-loss tolerance;",
  "not fold-level significance testing or post-hoc model selection;",
  if (focal_model_accepted) {
    "the focal model is inside the accepted set"
  } else {
    "the focal model is outside the accepted set"
  }
)]
accepted_models = model_ranks[accepted == TRUE, learner_name]
g6_resamples = result$artifacts$rashomon$resamples
g6_fold_scores = rbindlist(lapply(names(g6_resamples), function(learner_name) {
  .fold_performance_long(g6_resamples[[learner_name]], target, learner_name)
}))
g6_primary = result$artifacts$rashomon$primary_measure
g6_means = g6_fold_scores[measure_id == g6_primary, .(fold_mean = mean(estimate)), by = learner_id]
g6_check = merge(
  model_ranks[, .(learner_id = learner_name, mean_score)],
  g6_means,
  by = "learner_id",
  all = TRUE
)
if (anyNA(g6_check) || any(abs(g6_check$mean_score - g6_check$fold_mean) > 1e-12)) {
  stop("SHILD G6 candidate means and their plotted fold-score source are inconsistent.", call. = FALSE)
}
common_fold_key = c("learner_id", "iteration", "repetition", "fold", "measure_id")
benchmark_g6_check = merge(
  benchmark$fold_scores[measure_id == g6_primary, c(
    setNames(mget(common_fold_key), common_fold_key),
    list(benchmark_estimate = estimate)
  )],
  g6_fold_scores[measure_id == g6_primary, c(
    setNames(mget(common_fold_key), common_fold_key),
    list(g6_estimate = estimate)
  )],
  by = common_fold_key,
  all = TRUE
)
expected_g6_rows = length(candidates) * settings$folds
if (nrow(benchmark_g6_check) != expected_g6_rows || anyNA(benchmark_g6_check) ||
    any(abs(benchmark_g6_check$benchmark_estimate - benchmark_g6_check$g6_estimate) > 1e-12)) {
  stop("The duplicate SHILD G1 and G6a common-fold log-loss analyses are not seed-identical.", call. = FALSE)
}
multiplicity_available = length(accepted_models) >= 2L
multiplicity_status = data.table(
  status = if (multiplicity_available) "estimated" else "not_estimable",
  n_accepted_models = length(accepted_models),
  required_minimum = 2L,
  focal_model_accepted = focal_model_accepted,
  interpretation = if (multiplicity_available) {
    if (focal_model_accepted) {
      paste(
        "Raw-probability dispersion across common-split OOF predictions from models accepted on log loss;",
        "this combines model-class and calibration differences and is not uncertainty"
      )
    } else {
      paste(
        "The focal model was outside the log-loss tolerance. Dispersion compares accepted alternatives only,",
        "combines model-class and calibration differences, and is not uncertainty around the focal model"
      )
    }
  } else {
    "Only one model met the prespecified held-out tolerance; multiplicity was not estimated"
  }
)
if (multiplicity_available) {
  multiplicity = .rashomon_prediction_multiplicity(result$artifacts$rashomon, "discontent")
  multiplicity_quantiles = multiplicity$quantiles
  multiplicity_bins = multiplicity$bins
  multiplicity_private = multiplicity$row_level
} else {
  multiplicity_quantiles = data.table(
    status = "not_estimable",
    reason = "Fewer than two models met the prespecified held-out tolerance"
  )
  multiplicity_bins = copy(multiplicity_quantiles)
  multiplicity_private = data.table()
}

# ---- Validation and run summaries ----

validation = data.table(
  diagnostic = c(
    "preprocessing verification",
    "Gate 2 dependence/support",
    "numeric one-dimensional ALE",
    "marginal PFI and local-surrogate semantics",
    "categorical A2/D2/M8/D8 model queries",
    "interaction H screen",
    "two-dimensional ALE",
    "Gate 6 focal-model membership",
    "duplicate benchmark and Gate 6 alignment",
    "Gate 4 two-case communication local fidelity",
    "conditional Shapley communication output",
    "complete-case membership bridge",
    "OOF raw-probability dispersion",
    "publication eligibility"
  ),
  status = c(
    "verified_by_responsible_coauthors",
    result$gates$G2$status,
    "computed_with_model_refit_intervals",
    "model_query_communication_only",
    "optional_not_ALE",
    "computed",
    ale_2d_status,
    result$gates$G6a$status,
    "verified_seed_identical",
    result$gates$G4$status,
    if (shap_quality_pass) {
      "private_full_fit_communication_quality_pass"
    } else {
      "private_full_fit_communication_suppressed"
    },
    "verified_private_row_bridge",
    multiplicity_status$status,
    if (settings$quick) "not_eligible" else "eligible_after_structural_output_audit"
  ),
  limitation = c(
    paste(
      "The responsible co-authors verified the analytic cut and preprocessing on 2026-09-09; this status is not",
      "an independent rerun from the source data or external reconstruction."
    ),
    "Mixed-type dependence describes the supplied analytic distribution and is not causal.",
    paste(
      "Full-fit ALE is associational model description. Pointwise 95% respondent-bootstrap intervals include",
      "resampling, complete learner refitting, and ALE recomputation, but are not simultaneous curve bands and",
      "do not include learner-selection or hyperparameter-search uncertainty."
    ),
    paste(
      "Marginal PFI and independently perturbed local surrogates do not establish within-support",
      "explanation validity or explanation generalization."
    ),
    "These categorical PDP queries are not ALE and can leave empirical joint support.",
    "The grid-based H approximation is dependence-sensitive and is not a causal interaction estimate.",
    if (ale_2d_available) {
      paste0("Second-order ALE cells with n < ", ale_2d_min_cell_n, " are masked; fit uncertainty is omitted.")
    } else {
      "No screened H statistic exceeded numerical tolerance, so no arbitrary pair or 2D ALE surface was used."
    },
    if (focal_model_accepted) {
      "The focal learner is represented in the prespecified near-equivalent model set."
    } else {
      "The focal learner is outside the exploratory log-loss tolerance; accepted-model comparisons exclude it."
    },
    paste(
      "G1 and G6a use identical learner-specific seeds and common outer folds; every learner-fold log loss",
      "matched to numerical tolerance 1e-12."
    ),
    paste0(
      "Communication-only two-case diagnostic. ",
      "Cross-fitted weighted R-squared ranged from ",
      sprintf("%.3f", min(result$gates$G4$evidence$summary$weighted_r2)),
      " to ", sprintf("%.3f", max(result$gates$G4$evidence$summary$weighted_r2)),
      ". The historical 0.800 relative-fidelity reference is descriptive, not a universal adequacy threshold.",
      "Cases were selected post hoc, so fidelity does not generalize."
    ),
    if (shap_quality_pass) {
      paste(
        "Approximate conditional SHAP uses the full fit and is not held-out Gate 4 evidence; both cases met",
        "the prespecified additivity display rule."
      )
    } else {
      paste(
        "The private SHAP bar figure was suppressed because at least one case failed the prespecified",
        "additivity rule; exact private diagnostics were retained."
      )
    },
    paste(
      "Every complete-case OOF row ID and fold was reconciled to the original analytic row, stratum, and",
      "private respondent identifier."
    ),
    multiplicity_status$interpretation,
    if (settings$quick) {
      "Quick outputs must never enter the manuscript."
    } else {
      "The analytic cut and preprocessing were verified by the responsible co-authors on 2026-09-09."
    }
  )
)

sample_flow = rbindlist(list(
  preparation$sample_flow,
  data.table(
    stage = "primary complete cases after documented recoding",
    n = sum(complete_case$retained),
    exclusion_from_previous = nrow(dt) - sum(complete_case$retained)
  )
), use.names = TRUE)
class_balance = dt[, .(n = .N), by = target]
class_balance[, proportion := n / sum(n)]
setnames(class_balance, target, "outcome")
run_mode = data.table(
  mode = settings$mode,
  publication_settings = !settings$quick,
  manuscript_ready = !settings$quick,
  eligibility_blocker = if (settings$quick) "nonpublication quick mode" else "none",
  seed = settings$seed,
  folds = settings$folds,
  warning = if (settings$quick) {
    "NONPUBLICATION quick run: never use these results in the manuscript."
  } else {
    "Publication-settings run with responsible-co-author preprocessing verification dated 2026-09-09."
  }
)

# ---- Output tables and private analysis objects ----

.write_table(run_mode, file.path(run$dirs$tables, "shild_run_mode.csv"))
.write_table(sample_flow, file.path(run$dirs$tables, "shild_sample_flow.csv"))
.write_table(class_balance, file.path(run$dirs$tables, "shild_class_balance.csv"))
.write_table(missingness, file.path(run$dirs$tables, "shild_missingness.csv"))
.write_table(sentinel_audit, file.path(run$dirs$tables, "shild_sentinel_recode_audit.csv"))
.write_table(
  preparation$variable_recode_spec,
  file.path(run$dirs$tables, "shild_variable_recode_spec.csv")
)
.write_table(
  preparation$value_recode_audit,
  file.path(run$dirs$tables, "shild_value_recode_audit.csv")
)
.write_table(
  preparation$domain_value_audit,
  file.path(run$dirs$tables, "shild_domain_value_audit.csv")
)
.write_table(
  fold_assignment[, .(row_id, fold, stratum)],
  file.path(run$dirs$tables, "shild_fold_assignment.csv")
)
.write_table(
  benchmark$fold_scores,
  file.path(run$dirs$tables, "shild_g1_benchmark_fold_scores.csv")
)
.write_table(benchmark$summary, file.path(run$dirs$tables, "shild_g1_benchmark_summary.csv"))
.write_table(
  benchmark_model_specification,
  file.path(run$dirs$tables, "shild_benchmark_model_specification.csv")
)
.write_table(case_selection, file.path(run$dirs$tables, "shild_local_case_selection_private.csv"))
.write_table(complete_case_summary, file.path(run$dirs$tables, "shild_complete_case_sensitivity.csv"))
.write_table(
  complete_case_fold_scores,
  file.path(run$dirs$tables, "shild_complete_case_fold_scores.csv")
)
.write_table(
  calibration_plot_source,
  file.path(run$dirs$tables, "shild_g3_calibration_plot_source.csv")
)
.write_table(
  calibration_bootstrap$summary,
  file.path(run$dirs$tables, "shild_g3_calibration_bootstrap_summary.csv")
)
.write_table(
  calibration_bootstrap$curve,
  file.path(run$dirs$tables, "shild_g3_calibration_bootstrap_curve.csv")
)
.write_table(
  calibration_bootstrap$bins,
  file.path(run$dirs$tables, "shild_g3_calibration_curve_descriptive_bins.csv")
)
.write_table(
  local_fidelity_source,
  file.path(run$dirs$tables, "shild_g4_local_fidelity_source.csv")
)
.write_table(report_card, file.path(run$dirs$tables, "shild_report_card.csv"))
.write_table(claim_support, file.path(run$dirs$tables, "shild_claim_support.csv"))
.write_table(claim_versions, file.path(run$dirs$tables, "shild_claim_versions.csv"))
.write_table(
  optional_effect$profiles,
  file.path(run$dirs$tables, "shild_optional_model_query_profiles.csv")
)
.write_table(
  optional_interactions,
  file.path(run$dirs$tables, "shild_optional_model_query_interactions.csv")
)
.write_table(rich_ale_1d_public, file.path(run$dirs$tables, "shild_rich_ale_1d.csv"))
.write_table(rich_effects$pdp, file.path(run$dirs$tables, "shild_rich_pdp.csv"))
.write_table(rich_effects$ice_quantiles, file.path(run$dirs$tables, "shild_rich_ice_quantiles.csv"))
.write_table(rich_effects$ice_spread, file.path(run$dirs$tables, "shild_rich_ice_spread.csv"))
.write_table(
  rich_interactions$interaction_screen,
  file.path(run$dirs$tables, "shild_rich_interaction_screen.csv")
)
.write_table(rich_ale_2d_public, file.path(run$dirs$tables, "shild_rich_ale_2d.csv"))
.write_table(
  shap_quality_summary,
  file.path(run$dirs$tables, "shild_conditional_shap_quality_summary.csv")
)
.write_table(model_ranks, file.path(run$dirs$tables, "shild_model_ranks.csv"))
.write_table(
  model_importance_agreement,
  file.path(run$dirs$tables, "shild_model_importance_agreement.csv")
)
.write_table(
  model_importance_direct_differences,
  file.path(run$dirs$tables, "shild_model_importance_direct_differences.csv")
)
.write_table(
  model_importance_pairwise,
  file.path(run$dirs$tables, "shild_model_importance_pairwise.csv")
)
.write_table(
  model_importance_definitions,
  file.path(run$dirs$tables, "shild_model_importance_definitions.csv")
)
.write_table(grouped_pfi_summary, file.path(run$dirs$tables, "shild_grouped_pfi_summary.csv"))
.write_table(
  grouped_pfi_per_iteration,
  file.path(run$dirs$tables, "shild_grouped_pfi_per_iteration.csv")
)
.write_table(
  near_equivalence_tolerance_sensitivity,
  file.path(run$dirs$tables, "shild_near_equivalence_tolerance_sensitivity.csv")
)
.write_table(
  performance_explanation_sensitivity,
  file.path(run$dirs$tables, "shild_performance_explanation_sensitivity.csv")
)
.write_table(multiplicity_status, file.path(run$dirs$tables, "shild_multiplicity_status.csv"))
.write_table(
  multiplicity_quantiles,
  file.path(run$dirs$tables, "shild_multiplicity_distribution.csv")
)
.write_table(multiplicity_bins, file.path(run$dirs$tables, "shild_multiplicity_bins.csv"))
.write_table(validation, file.path(run$dirs$tables, "shild_validation.csv"))
.write_gate_tables(result, run$dirs$tables, "shild_gate")
.write_table(
  complete_case_membership,
  file.path(run$dirs$objects, "shild_complete_case_membership_private.csv")
)
.write_table(
  local_case_crosswalk,
  file.path(run$dirs$objects, "shild_local_case_crosswalk_private.csv")
)

saveRDS(result, file.path(run$dirs$objects, "shild_csdg_result_private.rds"), version = 3)
saveRDS(benchmark$results, file.path(run$dirs$objects, "shild_benchmark_oof_private.rds"), version = 3)
saveRDS(complete_case$oof, file.path(run$dirs$objects, "shild_complete_case_oof_private.rds"), version = 3)
.write_table(rich_effects$ice_private, file.path(run$dirs$objects, "shild_rich_ice_private.csv"))
.write_table(
  rich_effects$ale_1d,
  file.path(run$dirs$objects, "shild_rich_ale_1d_unsuppressed_private.csv")
)
.write_table(
  rich_ale_bootstrap$replicates_private,
  file.path(run$dirs$objects, "shild_rich_ale_bootstrap_replicates_private.csv")
)
.write_table(
  rich_ale_bootstrap$failures,
  file.path(run$dirs$objects, "shild_rich_ale_bootstrap_failures_private.csv")
)
.write_table(
  rich_interactions$ale_2d,
  file.path(run$dirs$objects, "shild_rich_ale_2d_unsuppressed_private.csv")
)
.write_table(rich_shap$local_private, file.path(run$dirs$objects, "shild_shap_private.csv"))
.write_table(rich_shap$global, file.path(run$dirs$objects, "shild_shap_two_case_summary_private.csv"))
.write_table(
  rich_shap$additivity_private,
  file.path(run$dirs$objects, "shild_shap_additivity_private.csv")
)
if (nrow(multiplicity_private)) {
  .write_table(
    multiplicity_private,
    file.path(run$dirs$objects, "shild_multiplicity_row_level_private.csv")
  )
}
csdg_export(result, run$dirs$audit_bundle, prefix = "shild")

# ---- Publication figures ----

shild_feature_labels = .analysis_shild_feature_labels()
shild_audit_labels = c(audit_gender = "Gender", audit_age_group = "Age group")
shild_subgroup_labels = c(
  "audit_gender::Man" = "Men",
  "audit_gender::Woman" = "Women",
  "audit_age_group::16-24" = "16-24",
  "audit_age_group::25-34" = "25-34",
  "audit_age_group::35-44" = "35-44",
  "audit_age_group::45-54" = "45-54",
  "audit_age_group::55-64" = "55-64"
)

.analysis_reset_figure_registry()
.analysis_clear_figure_files(run$dirs$figures)

overview_plot = csdg_plot_data(
  report_card,
  type = "gate_status",
  title = "SHILD claim-scoped diagnostic report card",
  gate_order = report_card$gate_id
)
.save_plot(overview_plot, file.path(run$dirs$figures, "shild_gate_overview"), height = 6)
performance_plot = csdg_plot(result, gate = "G1", base_size = 12) +
  labs(title = "SHILD focal XGBoost held-out predictive performance")
.save_plot(performance_plot, file.path(run$dirs$figures, "shild_g1_performance"), width = 9, height = 6)
dependence_evidence = copy(result$gates$G2$evidence)
dependence_evidence$pairwise[, `:=`(
  feature_1 = .analysis_feature_labels(feature_1, shild_feature_labels),
  feature_2 = .analysis_feature_labels(feature_2, shild_feature_labels)
)]
dependence_plot = csdg_plot(dependence_evidence, gate = "G2", base_size = 15) +
  labs(
    title = "SHILD mixed-type pairwise dependence strength",
    subtitle = .analysis_wrap_text(paste(
      "Lower triangle for the 15 strongest variables. Cells combine absolute Spearman correlation,",
      "Cramer's V, and eta-squared; these descriptive magnitudes are not directly equivalent."
    ))
  )
.save_plot(dependence_plot, file.path(run$dirs$figures, "shild_g2_dependence"), width = 11, height = 9)
calibration_plot = csdg_plot_data(
  calibration_plot_source,
  type = "calibration",
  title = "SHILD flexible out-of-fold calibration",
  subtitle = paste(
    "The curve is a loess estimate; the band is a pointwise 95% respondent-bootstrap percentile interval",
    "conditional on the supplied out-of-fold predictions. It is neither simultaneous nor inclusive of",
    "model-training variation; the diagonal denotes perfect calibration."
  ),
  x_label = "Mean held-out probability of discontent",
  y_label = "Observed discontent fraction"
)
.save_plot(calibration_plot, file.path(run$dirs$figures, "shild_g3_calibration"), height = 6)
fidelity_plot = csdg_plot_data(
  local_fidelity_source,
  type = "local_fidelity",
  title = "SHILD cross-fitted local surrogate fidelity",
  subtitle = paste(
    "Cases 1 and 2 follow the declared 10th- and 90th-percentile focal OOF-probability order.",
    "The dashed 0.80 line is a historical relative-fidelity reference, not a universal adequacy threshold."
  )
)
.save_plot(fidelity_plot, file.path(run$dirs$figures, "shild_g4_local_fidelity"), height = 5)
pfi_plot = csdg_plot_data(
  result$gates$G5$evidence$pfi$summary,
  type = "pfi",
  title = "SHILD foldwise held-out permutation importance",
  subtitle = paste(
    "Top 10 features by mean importance; blue whiskers show descriptive fold 10th-90th percentiles,",
    "not confidence intervals"
  ),
  feature_labels = shild_feature_labels,
  top_n = 10L,
  x_label = "Increase in held-out log loss",
  base_size = 15,
  direct_label_size = 2.9
)
.save_plot(pfi_plot, file.path(run$dirs$figures, "shild_g5_pfi"), width = 10, height = 7)
model_plot = csdg_plot_data(
  result$gates$G6a$evidence$model_generalization$candidates,
  type = "model_comparison",
  title = "SHILD near-equivalent model sensitivity screen",
  x_label = "Mean held-out log loss (lower is better)",
  x_limits = c(
    0,
    1.05 * max(
      result$gates$G6a$evidence$model_generalization$candidates$acceptance_limit,
      result$gates$G6a$evidence$model_generalization$candidates$mean_score,
      g6_fold_scores$estimate,
      na.rm = TRUE
    )
  ),
  fold_scores = g6_fold_scores,
  base_size = 13,
  direct_label_size = 3.4,
  limit_label_size = 3.4,
  axis_text_size = 10.5,
  legend_text_size = 10.5
)
.save_plot(model_plot, file.path(run$dirs$figures, "shild_g6_model_sensitivity"), width = 9, height = 6)
subgroup_plot = csdg_plot_data(
  result$gates$G7a$evidence$subgroup_audit,
  type = "subgroup",
  metric = "auc",
  title = "SHILD focal-model subgroup ROC AUC audit",
  subtitle = paste(
    "Row-bootstrap 95% intervals conditional on out-of-fold predictions; direct labels show AUC and n.\n",
    "The full 0.5–1.0 AUC scale avoids magnifying small absolute gaps."
  ),
  audit_labels = shild_audit_labels,
  subgroup_labels = shild_subgroup_labels,
  subgroup_order = c("16-24", "25-34", "35-44", "45-54", "55-64", "Women", "Men"),
  x_limits = c(0.5, 1),
  base_size = 13,
  direct_label_size = 3,
  axis_text_size = 11,
  axis_title_size = 12
)
.save_plot(subgroup_plot, file.path(run$dirs$figures, "shild_g7_subgroups"), width = 10, height = 4.6)
contrast_plot = csdg_plot_data(
  result$gates$G7a$evidence$subgroup_audit$contrasts,
  type = "subgroup_contrast",
  metric = "auc",
  title = "SHILD pairwise subgroup AUC contrasts",
  subtitle = paste(
    "Red points are first-minus-second differences; blue whiskers are unadjusted conditional 95% bootstrap",
    "intervals. Model-training variation is excluded, and no significance claim is made."
  ),
  audit_labels = shild_audit_labels,
  subgroup_labels = shild_subgroup_labels,
  x_label = "ROC AUC difference (first - second)",
  base_size = 15,
  direct_label_size = 3.8,
  axis_text_size = 12
)
.save_plot(
  contrast_plot,
  file.path(run$dirs$figures, "shild_g7_subgroup_contrasts"),
  width = 10,
  height = 7
)

importance_plot = csdg_plot_data(
  result$artifacts$rashomon$explanation_agreement$ranks,
  type = "rank_heatmap",
  title = "SHILD held-out PFI ranks across near-equivalent models",
  subtitle = if (focal_model_accepted) {
    "Union of each model's top ten; rank values are printed in every cell"
  } else {
    paste(
      "Accepted alternatives only: the focal XGBoost model was outside tolerance.",
      "Rank values are printed in every cell."
    )
  },
  top_n = 10L,
  feature_labels = shild_feature_labels,
  learner_order = c("xgboost", "ridge_logistic", "random_forest"),
  base_size = 15,
  cell_label_size = 3.8,
  axis_text_size = 12,
  legend_text_size = 12,
  legend_title_size = 12
)
.save_plot(
  importance_plot,
  file.path(run$dirs$figures, "shild_model_importance_ranks"),
  width = 10,
  height = 7
)

benchmark_plot = csdg_plot_data(
  benchmark$summary,
  type = "performance",
  title = "SHILD common-split learner sensitivity benchmark",
  base_size = 13,
  direct_label_size = 2.9,
  axis_text_size = 10.5,
  axis_title_size = 9.2
)
.save_plot(benchmark_plot, file.path(run$dirs$figures, "shild_g1_benchmark"), width = 10, height = 7)

ale_plot = csdg_plot_data(
  rich_ale_1d_public,
  type = "ale",
  title = "SHILD dependence-aware numeric ALE",
  subtitle = paste(
    "Ribbon: pointwise 95% respondent-bootstrap percentile interval from complete learner refits;",
    "not a simultaneous band and conditional on the fixed learner specification."
  ),
  feature_labels = shild_feature_labels,
  y_label = "Accumulated local probability difference",
  base_size = 11,
  axis_text_size = 11,
  axis_title_size = 10
)
.save_plot(ale_plot, file.path(run$dirs$figures, "shild_rich_ale_1d"), width = 9, height = 6)

ice_plot = csdg_plot_data(
  rich_effects$ice_quantiles,
  type = "ice",
  title = "SHILD aggregate ICE model queries",
  subtitle = paste(
    "The outer band is pointwise q05-q95, the inner band is q25-q75, and the line is the pointwise median",
    "across row-level ICE curves; these are descriptive curve quantiles, not CIs."
  ),
  feature_labels = shild_feature_labels,
  y_label = "Fitted probability of discontent (logit scale)",
  y_limits = c(0.003, 0.95),
  y_scale = "logit",
  y_breaks = c(0.005, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50, 0.90),
  base_size = 11,
  axis_text_size = 11,
  axis_title_size = 9.5
)
ice_legend_plot = csdg_plot_legend(ice_plot)
.save_plot(
  ice_plot + theme(legend.position = "none"),
  file.path(run$dirs$figures, "shild_rich_ice_quantiles"),
  width = 9,
  height = 6,
  metadata = list(
    latex_assembly_note = paste(
      "Assemble the four ICE panels in panel_order as a 2-row by 2-column figure, then place the shared",
      "quantile legend once beneath them with LaTeX."
    )
  )
)
.save_plot(
  ice_legend_plot,
  file.path(run$dirs$figures, "shild_rich_ice_quantiles__legend"),
  width = 9,
  height = 1.25,
  metadata = list(
    figure_group_id = "shild_rich_ice_quantiles",
    is_grouped_figure = TRUE,
    latex_group_id = "shild_rich_ice_quantiles",
    panel_id = "shared_legend",
    panel_order = 5L,
    panel_heading = "Shared ICE quantile legend",
    group_title = ice_plot$labels$title,
    group_subtitle = ice_plot$labels$subtitle,
    latex_assembly_note = paste(
      "Shared legend for the four ICE panels; place it once beneath the 2-row by 2-column panel assembly",
      "with LaTeX."
    )
  )
)

interaction_plot = csdg_plot_data(
  rich_interactions$interaction_screen,
  type = "interaction",
  title = "SHILD grid-based interaction screen",
  subtitle = "Grid-based H approximation; dependence-sensitive model diagnostic, not a causal interaction",
  feature_labels = shild_feature_labels
)
.save_plot(
  interaction_plot,
  file.path(run$dirs$figures, "shild_rich_interaction_screen"),
  width = 8,
  height = 5
)

ale_2d_plot = if (ale_2d_available) {
  csdg_plot_data(
    rich_ale_2d_public,
    type = "ale_2d",
    title = "SHILD second-order ALE interaction surface",
    subtitle = paste0(
      "First-order effects are removed; irregular rectangles follow empirical cells, and gray cells have n < ",
      ale_2d_min_cell_n,
      ". Model-fitting uncertainty is not represented."
    ),
    feature_labels = shild_feature_labels
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
      title = "SHILD second-order ALE interaction surface",
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
  file.path(run$dirs$figures, "shild_rich_ale_2d"),
  width = 8,
  height = if (ale_2d_available) 6 else 5
)

if (multiplicity_available) {
  multiplicity_plot = csdg_plot_data(
    multiplicity_bins,
    type = "multiplicity",
    quantiles = multiplicity_quantiles,
    title = "SHILD raw-probability dispersion across log-loss-near-equivalent models",
    subtitle = if (focal_model_accepted) {
      paste(
        "Binned empirical OOF CDF from shareable aggregate counts. Because equivalence is defined on log loss,",
        "red markers are exact aggregate quantiles and need not lie on bin steps; ranges are not CIs."
      )
    } else {
      paste(
        "Binned empirical OOF CDF for accepted alternatives only; focal XGBoost was outside tolerance. Ranges",
        "combine model-class and calibration differences. Exact red quantiles need not lie on bin steps."
      )
    },
    x_label = "Within-respondent probability range",
    base_size = 11,
    direct_label_size = 3.1
  )
} else {
  multiplicity_plot = ggplot(data.table(x = 1, y = 1), aes(x = x, y = y)) +
    annotate("text", x = 1, y = 1, label = multiplicity_status$interpretation, size = 4) +
    xlim(0, 2) +
    ylim(0, 2) +
    labs(title = "SHILD raw-probability dispersion: not estimable") +
    theme_void()
}
.save_plot(
  multiplicity_plot,
  file.path(run$dirs$figures, "shild_prediction_multiplicity"),
  width = 8,
  height = 6
)

shap_case_labels = setNames(
  local_case_crosswalk$communication_case,
  local_case_crosswalk$shap_case_id
)
shap_data = copy(rich_shap$local_private[absolute_rank <= 10L])
shap_data[, `:=`(
  feature_label = .analysis_feature_labels(feature, shild_feature_labels),
  direction = factor(ifelse(phi >= 0, "Higher fitted risk", "Lower fitted risk")),
  case_label = unname(shap_case_labels[as.character(case_id)]),
  phi_low = phi - stats::qnorm(0.975) * phi_monte_carlo_se,
  phi_high = phi + stats::qnorm(0.975) * phi_monte_carlo_se
)]
if (anyNA(shap_data$case_label)) {
  stop("SHILD conditional Shapley case labels are incomplete.", call. = FALSE)
}
shap_plot = if (shap_quality_pass) {
  ggplot(shap_data, aes(x = phi, y = reorder(feature_label, absolute_phi), fill = direction)) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.6) +
    geom_col() +
    geom_errorbar(
      aes(xmin = phi_low, xmax = phi_high),
      orientation = "y",
      width = 0.16,
      color = "grey20",
      linewidth = 0.6
    ) +
    facet_wrap(~ case_label, scales = "free_y") +
    scale_fill_manual(values = c("Lower fitted risk" = "#4C72B0", "Higher fitted risk" = "#C44E52")) +
    labs(
      x = "Approximate conditional Shapley contribution",
      y = NULL,
      fill = NULL,
      title = "Private SHILD post-hoc communication cases",
      subtitle = .analysis_wrap_text(paste(
        "Bars are conditional Shapley point estimates; gray lines are normal-approximation 95% Monte Carlo",
        "intervals conditional on the fixed background and fit. Both cases passed the additivity display rule."
      ))
    ) +
    .analysis_theme()
} else {
  shap_quality_data = merge(
    local_case_crosswalk[, .(communication_case, shap_case_id)],
    rich_shap$additivity_private,
    by.x = "shap_case_id",
    by.y = "case_id",
    all.x = TRUE,
    sort = FALSE
  )
  if (anyNA(shap_quality_data[, .(absolute_additivity_residual, additivity_tolerance)])) {
    stop("SHILD conditional Shapley suppression evidence is incomplete.", call. = FALSE)
  }
  shap_quality_data[, `:=`(
    communication_case = factor(communication_case, levels = rev(local_case_crosswalk$communication_case)),
    value_label = sprintf("residual %.3f; limit %.3f", absolute_additivity_residual, additivity_tolerance)
  )]
  ggplot(shap_quality_data, aes(y = communication_case)) +
    geom_segment(
      aes(x = 0, xend = additivity_tolerance, yend = communication_case),
      color = "#4C72B0",
      linewidth = 3.2,
      alpha = 0.55
    ) +
    geom_point(aes(x = absolute_additivity_residual), color = "#C44E52", size = 3) +
    geom_text(
      aes(x = pmax(absolute_additivity_residual, additivity_tolerance), label = value_label),
      hjust = -0.08,
      size = 3.1,
      color = "grey20"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.42))) +
    labs(
      x = "Absolute additivity residual",
      y = NULL,
      title = "Private SHILD conditional SHAP figure suppressed",
      subtitle = .analysis_wrap_text(paste(
        "At least one red point exceeds its blue case-specific display limit. Numeric contributions and",
        "Monte Carlo diagnostics remain in the private tables, but the communication bars are not shown",
        "as reliable."
      ))
    ) +
    .analysis_theme() +
    theme(panel.grid.major.y = element_blank())
}
.save_plot(shap_plot, file.path(run$dirs$objects, "shild_shap_communication_private"), width = 9, height = 6)

decision_plot = csdg_plot_data(
  result$gates$G3b$evidence$decision_curve,
  type = "decision_curve",
  title = "SHILD out-of-fold decision-curve quantities",
  subtitle = paste(
    "Illustrative threshold-level utility quantities without confidence intervals; not evidence of",
    "real-world benefit. The shareable figure contains no row-level probability marks."
  )
)
.save_plot(decision_plot, file.path(run$dirs$figures, "shild_g3_decision_curve"), height = 6)

optional_plots = .plot_effect_profiles(
  optional_effect$profiles,
  "Optional SHILD full-fit model queries (descriptive and non-causal)",
  axis_text_size = 11,
  axis_title_size = 9.5
)
if (is.null(optional_plots$categorical)) {
  stop("Expected categorical SHILD model-query profiles were not produced.", call. = FALSE)
}
.save_plot(
  optional_plots$categorical,
  file.path(run$dirs$figures, "shild_optional_model_query_categorical"),
  width = 9,
  height = 6
)
.save_plot(
  .plot_interaction_profiles(
    optional_interactions,
    "Optional SHILD two-feature model queries (descriptive and non-causal)",
    axis_text_size = 11,
    legend_text_size = 11,
    legend_title_size = 11
  ),
  file.path(run$dirs$figures, "shild_optional_model_query_interactions"),
  width = 9,
  height = 6
)

# ---- Figure catalog and LaTeX metadata ----

g6_fold_source_tables = paste(
  file.path(
    "tables",
    paste0(
      "shild_gate_g6a__model_generalization__resamples__",
      names(g6_resamples),
      "__fold_scores.csv"
    )
  ),
  collapse = " | "
)
shild_ale_1d_semantics = .analysis_ale_semantics(
  "SHILD",
  "1d",
  support_threshold = max(ale_1d_min_interval_n, ale_2d_min_cell_n),
  reference_n = min(settings$effect_rows, task$nrow),
  bootstrap_replicates = settings$ale_bootstrap_replicates
)
shild_ale_2d_semantics = .analysis_ale_semantics(
  "SHILD",
  "2d",
  support_threshold = max(ale_1d_min_interval_n, ale_2d_min_cell_n),
  reference_n = min(settings$effect_rows, task$nrow),
  bootstrap_replicates = settings$ale_bootstrap_replicates,
  available = ale_2d_available
)
figure_catalog = data.table(
  figure_id = c(
    "shild_gate_overview", "shild_g1_performance", "shild_g2_dependence", "shild_g3_calibration",
    "shild_g3_decision_curve", "shild_g4_local_fidelity", "shild_g5_pfi", "shild_g6_model_sensitivity",
    "shild_model_importance_ranks", "shild_g7_subgroups", "shild_g7_subgroup_contrasts",
    "shild_g1_benchmark", "shild_rich_ale_1d", "shild_rich_ice_quantiles",
    "shild_rich_interaction_screen", "shild_rich_ale_2d", "shild_prediction_multiplicity",
    "shild_optional_model_query_categorical", "shild_optional_model_query_interactions",
    "shild_shap_communication_private"
  ),
  source_tables = c(
    "tables/shild_report_card.csv", "tables/shild_gate_g1__performance__summary.csv",
    "tables/shild_gate_g2__pairwise.csv",
    "tables/shild_g3_calibration_plot_source.csv | tables/shild_g3_calibration_bootstrap_summary.csv",
    "tables/shild_gate_g3b__decision_curve.csv", "tables/shild_g4_local_fidelity_source.csv",
    "tables/shild_gate_g5__pfi__summary.csv",
    paste(
      "tables/shild_gate_g6a__model_generalization__candidates.csv",
      g6_fold_source_tables,
      sep = " | "
    ),
    "tables/shild_gate_g6a__model_generalization__explanation_agreement__ranks.csv",
    "tables/shild_gate_g7a__subgroup_audit__metrics.csv | tables/shild_gate_g7a__subgroup_audit__uncertainty.csv",
    "tables/shild_gate_g7a__subgroup_audit__contrasts.csv", "tables/shild_g1_benchmark_summary.csv",
    "tables/shild_rich_ale_1d.csv", "tables/shild_rich_ice_quantiles.csv",
    "tables/shild_rich_interaction_screen.csv", "tables/shild_rich_ale_2d.csv",
    "tables/shild_multiplicity_distribution.csv | tables/shild_multiplicity_bins.csv",
    "tables/shild_optional_model_query_profiles.csv", "tables/shild_optional_model_query_interactions.csv",
    paste(
      "objects/shild_shap_private.csv | objects/shild_shap_two_case_summary_private.csv |",
      "objects/shild_shap_additivity_private.csv | objects/shild_local_case_crosswalk_private.csv"
    )
  ),
  uncertainty_semantics = c(
    "administrative status; no uncertainty", "descriptive fold 10th-90th percentiles; not a CI",
    "method-specific descriptive association",
    "pointwise respondent-bootstrap 95% percentile intervals conditional on supplied OOF predictions",
    "illustrative utility quantities; no uncertainty interval", "cross-fitted diagnostic point estimates only",
    "descriptive fold 10th-90th percentiles; not a CI",
    "descriptive fold 10th-90th percentiles plus a tolerance screen; not a CI or fold test",
    "rank sensitivity across accepted models", "conditional percentile bootstrap 95% intervals",
    "unadjusted conditional percentile bootstrap 95% intervals",
    "descriptive fold 10th-90th percentiles; not a CI",
    shild_ale_1d_semantics$uncertainty_semantics,
    "empirical curve quantiles; not a CI",
    "grid-based H approximation; no uncertainty",
    shild_ale_2d_semantics$uncertainty_semantics,
    "binned empirical OOF prediction-range CDF; exact aggregate quantile markers; not a CI",
    "descriptive 10th-90th reference-row range; not a CI",
    "descriptive two-feature fitted-model query",
    if (shap_quality_pass) {
      paste(
        "normal-approximation intervals from per-feature Monte Carlo standard errors, conditional on the",
        "fixed background and fit; both cases passed the prespecified additivity display rule"
      )
    } else {
      "communication bars suppressed because at least one case failed the prespecified additivity display rule"
    }
  ),
  role = c(
    "supplement", "main diagnostic", "supplement", "main diagnostic", "supplement", "supplement",
    "main diagnostic", "supplement", "supplement", "supplement", "supplement", "supplement",
    "main model-description diagnostic", "supplement", "supplement",
    if (ale_2d_available) "supplement" else "technical status record", "supplement",
    "supplement", "supplement",
    if (shap_quality_pass) "private review" else "private quality-suppression record"
  ),
  privacy_scope = c(rep("shareable aggregate", 19L), "private only")
)
figure_catalog[, estimand_or_scope := c(
  "claim-scoped gate status", "focal-model held-out prediction", "analytic-distribution dependence",
  "focal-model out-of-fold calibration", "focal-model out-of-fold decision utility",
  "two post-hoc communication cases", "focal-model held-out loss sensitivity",
  "near-equivalent model-class sensitivity",
  if (focal_model_accepted) {
    "importance-rank agreement across accepted models including the focal model"
  } else {
    "importance-rank agreement across accepted alternatives; the focal model is excluded"
  },
  "focal-model subgroup boundary audit", "pairwise focal-model subgroup contrasts",
  "common-split model-class benchmark", shild_ale_1d_semantics$estimand_or_scope,
  "full-fit interventional model queries", "full-fit grid-based interaction screen",
  shild_ale_2d_semantics$estimand_or_scope,
  if (focal_model_accepted) {
    "accepted-model raw-probability model-class and calibration dispersion"
  } else {
    "accepted-alternative dispersion only; the focal model is excluded"
  },
  "full-fit categorical model queries", "full-fit two-feature model queries",
  if (shap_quality_pass) {
    "private full-fit post-hoc communication cases passing the additivity display rule"
  } else {
    "private additivity-quality record; conditional SHAP communication bars suppressed"
  }
)]
figure_catalog = .analysis_expand_figure_catalog(
  figure_catalog,
  study = "SHILD",
  generator_script = "analysis/shild/analysis.R",
  generator_function = "SHILD publication figure construction block",
  run_dir = run$private_dir
)
public_figure_catalog = figure_catalog[privacy_scope != "private only"]
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
if (anyDuplicated(public_figure_catalog$figure_id) ||
    !setequal(public_figure_catalog$figure_id, pdf_stems) ||
    !setequal(public_figure_catalog$figure_id, png_stems)) {
  stop("The shareable SHILD figure catalog does not cover every PDF/PNG pair exactly.", call. = FALSE)
}
public_catalog_sources = .analysis_catalog_source_paths(public_figure_catalog)
missing_public_sources = public_catalog_sources[
  !file.exists(file.path(run$dirs$root, public_catalog_sources))
]
if (length(missing_public_sources)) {
  stop(
    "The shareable SHILD figure catalog references missing aggregate sources: ",
    paste(missing_public_sources, collapse = ", "),
    call. = FALSE
  )
}
.write_table(public_figure_catalog, file.path(run$dirs$provenance, "figure_catalog.csv"))
.analysis_write_figure_latex_metadata(
  public_figure_catalog,
  file.path(run$dirs$provenance, "figure_latex_metadata.csv"),
  run_dir = run$private_dir
)
.write_table(figure_catalog, file.path(run$dirs$objects, "shild_figure_catalog_private.csv"))

# ---- Provenance and reproducibility records ----

.assert_package_source_snapshot(package_source, project_root, "the SHILD core analysis")
input_records = rbindlist(list(
  .input_record(data_path, "SHILD 2012 release (Dryad) as CSV, used to construct the analytic data"),
  .input_record(dictionary_path, "SHILD variable dictionary without value labels"),
  .input_record(review_path, "preprocessing review record"),
  .input_record(protocol_path, "analysis protocol")
))
.write_table(input_records, file.path(run$dirs$provenance, "input_checksums.csv"))
.write_table(
  package_source$manifest,
  file.path(run$dirs$provenance, "package_source_manifest.csv")
)
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
      "- This records group-level co-author verification and run linkage; it is not an independent source-data",
      "rerun or external reconstruction."
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
    n_analytic = nrow(dt),
    n_predictors = length(source_predictors),
    age_min = min(dt$age),
    age_max = max(dt$age),
    prevalence_discontent = mean(dt[[target]] == "discontent"),
    package_source_tree_hash = package_source$tree_hash,
    analysis_run_signature = run_signature,
    checkpoint_directory = checkpoint_directory,
    benchmark_checkpoint_signature = benchmark_checkpoint$signature,
    loaded_package_path = find.package("mlr3autoiml"),
    preprocessing_verification = "verified_by_responsible_coauthors",
    preprocessing_verification_date = "2026-09-09",
    preprocessing_verification_scope = "group-level co-author confirmation; no independent source-data rerun",
    local_case_selection = "post-hoc from the exact benchmark/final focal OOF probabilities",
    optional_effect_scope = "full-fit model-query sensitivity; non-causal; not Gate 2 support evidence",
    ale_bootstrap_resampling = "respondent rows",
    ale_bootstrap_replicates_requested = rich_ale_bootstrap$settings$replicates,
    ale_bootstrap_refits_successful = rich_ale_bootstrap$settings$successful_refits,
    ale_bootstrap_interval = "pointwise 95% percentile; not simultaneous",
    ale_bootstrap_grid = "common boundaries from the reproducibly sampled point-reference rows",
    ale_bootstrap_minimum_interval_support_fraction = rich_ale_bootstrap$settings$min_success_fraction
  )
)

# ---- Completion checks ----

gate_errors = report_card[required == TRUE & status == "error"]
if (nrow(gate_errors)) {
  stop("Required SHILD gate error(s): ", paste(gate_errors$gate_id, collapse = ", "), call. = FALSE)
}
write_run_checkpoint(
  run,
  "shild_core_analysis",
  "completed",
  list(overall_status = claim_support$overall_status, n_analytic = nrow(dt))
)
stop_run_logging(run_logging)

# ---- Sanitized release finalization ----

upload_dir = .finalize_study_output(
  run = run,
  upload_expected = upload_expected,
  package_source = package_source,
  project_root = project_root,
  analysis_checkpoint = "shild_analysis",
  finalization_checkpoint = "shild_finalization",
  label = "SHILD"
)

cat("SHILD private run: ", run$private_dir, "\n", sep = "")
cat("SHILD sanitized upload: ", upload_dir, "\n", sep = "")
