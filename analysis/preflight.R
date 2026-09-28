#!/usr/bin/env Rscript

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/preflight.R"
}
analysis_dir = dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
source(file.path(analysis_dir, "shared", "study_helpers.R"), local = FALSE)
source(file.path(analysis_dir, "shild", "prepare_data.R"), local = FALSE)
source(file.path(analysis_dir, "pisa", "prepare_data.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "output.R"), local = FALSE)

.assert_canonical_environment()
.assert_packages(c(
  "checkmate", "data.table", "devtools", "digest", "ggplot2", "glmnet", "jsonlite",
  "mlr3", "mlr3learners", "mlr3measures", "mlr3misc", "mlr3pipelines", "png", "ranger", "rpart",
  "testthat", "withr", "xgboost", "yaml"
))

project_root = .analysis_project_root()
library(mlr3autoiml)
suppressPackageStartupMessages({
  library(data.table)
  library(mlr3)
  library(mlr3learners)
})

if (!identical(as.character(packageVersion("mlr3autoiml")), "0.1.1")) {
  stop("The study scripts require mlr3autoiml 0.1.1: ",
    "remotes::install_github(\"coorsaa/mlr3autoiml@v0.1.1\")", call. = FALSE)
}

required_api = c(
  "csdg_audit", "csdg_calibration", "csdg_claim", "csdg_claim_report", "csdg_config",
  "csdg_adjudicate_claim", "csdg_claim_matrix", "csdg_claim_relation", "csdg_evidence_record",
  "csdg_explanation", "csdg_explanation_sensitivity", "csdg_export",
  "csdg_effect_diagnostics", "csdg_fold_pfi", "csdg_grouped_resampling", "csdg_interaction_diagnostics",
  "csdg_leave_one_group_out",
  "csdg_local_fidelity_audit", "csdg_local_support", "csdg_measurement", "csdg_oof_predictions",
  "csdg_mask_effect_support", "csdg_plot", "csdg_plot_claim_matrix", "csdg_plot_data", "csdg_plot_legend",
  "csdg_prediction_multiplicity", "csdg_rashomon", "csdg_rashomon_agreement", "csdg_report_card",
  "csdg_resample", "csdg_shapley_diagnostics", "csdg_subgroup_metrics", "csdg_summarize_local_fidelity"
)
missing_api = required_api[!vapply(required_api, exists, logical(1L), mode = "function", inherits = TRUE)]
if (length(missing_api)) {
  stop("Missing required CSDG API functions: ", paste(missing_api, collapse = ", "), call. = FALSE)
}

required_contract = c(
  "create_run_contract", "create_sanitized_upload_bundle", "start_run_logging", "stop_run_logging",
  "read_run_checkpoints", "validate_expected_artifacts", "verify_recursive_manifest",
  "verify_sanitized_upload_bundle", "write_recursive_manifest", "write_run_checkpoint",
  "write_run_provenance"
)
missing_contract = required_contract[
  !vapply(required_contract, exists, logical(1L), mode = "function", inherits = TRUE)
]
if (length(missing_contract)) {
  stop("Missing run-contract functions: ", paste(missing_contract, collapse = ", "), call. = FALSE)
}

analysis_scripts = list.files(analysis_dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
parse_failures = unlist(lapply(analysis_scripts, function(path) {
  tryCatch(
    {
      parse(file = path, keep.source = TRUE)
      character()
    },
    error = function(error) paste(basename(path), conditionMessage(error))
  )
}), use.names = FALSE)
if (length(parse_failures)) {
  stop("Analysis parse failures:\n", paste(parse_failures, collapse = "\n"), call. = FALSE)
}

protocol_path = file.path(analysis_dir, "protocol.yml")
protocol = yaml::read_yaml(protocol_path)
if (length(protocol$protocol_version) != 1L || as.numeric(protocol$protocol_version) < 2) {
  stop("protocol.yml must declare protocol version 2.0 or later.", call. = FALSE)
}

learner_ids = c(
  "classif.xgboost", "classif.cv_glmnet", "classif.ranger", "classif.rpart", "classif.featureless",
  "regr.xgboost", "regr.glmnet", "regr.ranger", "regr.rpart", "regr.featureless"
)
learner_errors = vapply(learner_ids, function(id) {
  tryCatch({
    lrn(id)
    ""
  }, error = conditionMessage)
}, character(1L))
if (any(nzchar(learner_errors))) {
  stop(
    "Learner backend preflight failed: ",
    paste(paste0(names(learner_errors)[nzchar(learner_errors)], "=", learner_errors[nzchar(learner_errors)]),
      collapse = " | "),
    call. = FALSE
  )
}

metadata_probe_data = data.frame(
  x = seq_len(18L),
  y = seq_len(18L) + 0.5
)
metadata_probe_task = TaskRegr$new("metadata_probe", backend = metadata_probe_data, target = "y")
metadata_probe_grouped = csdg_grouped_resampling(
  metadata_probe_task,
  group = rep(seq_len(9L), each = 2L),
  folds = 3L,
  repeats = 2L,
  seed = 41L
)
metadata_probe_copy = .copy_resampling(metadata_probe_grouped, metadata_probe_task)
metadata_probe_oof = csdg_resample(
  metadata_probe_task,
  lrn("regr.featureless"),
  metadata_probe_copy,
  store_models = FALSE,
  seed = 42L
)
if (!inherits(metadata_probe_copy, "CSDGGroupedResampling") ||
    anyNA(metadata_probe_oof$fold_scores$repetition) ||
    !identical(metadata_probe_oof$fold_scores$repetition, rep(1:2, each = 3L)) ||
    !identical(metadata_probe_oof$fold_scores$fold, rep(1:3, times = 2L))) {
  stop("Copied grouped-resampling repetition/fold metadata were not preserved.", call. = FALSE)
}

shild_path = file.path(
  project_root, "data", "SHILD", "archive", "Danish_SHILD_draft_data.csv"
)
pisa_path = file.path(project_root, "data", "PISA", "pisa2018_reading_compactv2.csv")
pisa_manifest_path = file.path(project_root, "data", "PISA", "pisa2018_reading_compactv2_manifest.csv")
.require_files(c(shild_path, pisa_path, pisa_manifest_path))

shild_raw = fread(shild_path)
shild_prepared = .prepare_shild_raw(shild_raw)
shild_analytic = shild_prepared$data
if (nrow(shild_raw) != 18957L || nrow(shild_analytic) != 16971L ||
    !identical(range(shild_analytic$age), c(16L, 64L)) ||
    sum(shild_analytic$life_satisfaction == "content") != 15943L ||
    sum(shild_analytic$life_satisfaction == "discontent") != 1028L ||
    any(!is.na(shild_analytic$F15) & !between(shild_analytic$F15, 0, 10))) {
  stop(
    paste(
      "SHILD raw-input invariant failed: expected 18,957 source rows, 16,971 substantive extreme-outcome rows,",
      "15,943 content outcomes, 1,028 discontent outcomes, age 16-64, and F15 in 0-10 after recoding."
    ),
    call. = FALSE
  )
}

pv_columns = paste0("PV", seq_len(10L), "READ")
pisa = fread(pisa_path, select = c("CNT", "CNTSCHID", "CNTSTUID", "LANGN", pv_columns))
if (nrow(pisa) != 260641L || uniqueN(pisa$CNT) != 32L || any(!complete.cases(pisa[, ..pv_columns]))) {
  stop("PISA input invariant failed: expected 260,641 rows, 32 countries, and complete PVs.", call. = FALSE)
}
if (sum(pisa$LANGN == 999, na.rm = TRUE) != 7345L ||
    any(pisa$LANGN %in% c(997L, 998L), na.rm = TRUE)) {
  stop("PISA LANGN invariant failed: expected 7,345 code-999 values and no 997/998 values.", call. = FALSE)
}
pisa_manifest = fread(pisa_manifest_path)
if (pisa_manifest[variable_out == "LANGN", missing_rate] != 0) {
  stop("Expected the frozen PISA manifest to report LANGN missing_rate=0.", call. = FALSE)
}
country_sizes = pisa[, .N, by = CNT]
if (any(country_sizes$N < 1500L)) {
  stop("At least one PISA country cannot support the locked 1,500-row publication sample.", call. = FALSE)
}

settings_shild = .analysis_settings("SHILD")
settings_pisa = .analysis_settings("PISA")
cat("Preflight passed.\n")
cat("  mlr3autoiml version: ", as.character(packageVersion("mlr3autoiml")), "\n", sep = "")
cat("  loaded source: ", loaded_path, "\n", sep = "")
cat("  mode: ", settings_pisa$mode, "\n", sep = "")
cat("  seed: ", settings_pisa$seed, "\n", sep = "")
cat("  SHILD folds: ", settings_shild$folds, "\n", sep = "")
cat("  PISA folds / rows per country: ", settings_pisa$folds, " / ", settings_pisa$n_per_country, "\n", sep = "")
cat("  parsed analysis scripts: ", length(analysis_scripts), "\n", sep = "")
if (settings_pisa$quick) {
  cat("  IMPORTANT: NONPUBLICATION quick mode; outputs must not enter the manuscript.\n")
}
