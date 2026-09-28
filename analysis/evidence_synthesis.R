#!/usr/bin/env Rscript
# Systematic SHILD local-fidelity analysis and cross-study evidence synthesis.

# ---- Setup and dependencies ----

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/evidence_synthesis.R"
}
analysis_dir = dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
source(file.path(analysis_dir, "shared", "study_helpers.R"), local = FALSE)
source(file.path(analysis_dir, "shild", "prepare_data.R"), local = FALSE)
source(file.path(analysis_dir, "pisa", "prepare_data.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "output.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "output_audit.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "evidence_helpers.R"), local = FALSE)

.assert_canonical_environment()
.assert_packages(c(
  "data.table", "devtools", "digest", "ggplot2", "jsonlite", "mlr3", "mlr3pipelines",
  "png", "yaml"
))

project_root = .analysis_project_root()
package_source = .package_source_manifest(project_root)
library(mlr3autoiml)
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(mlr3)
  library(mlr3pipelines)
})

# ---- Protocol and frozen parent artifacts ----

protocol_path = file.path(analysis_dir, "protocol.yml")
.require_files(protocol_path)
protocol_document = yaml::read_yaml(protocol_path)
protocol = protocol_document$SHILD$systematic_local_fidelity
if (!is.list(protocol) || !identical(protocol$protocol_id, "shild_g4_systematic_local_fidelity_v2")) {
  stop("The systematic local-fidelity protocol is missing or has an unexpected identifier.", call. = FALSE)
}

.resolve_study_pair = function(path, label) {
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  candidates = list(
    list(private = file.path(path, "private"), upload = file.path(path, "upload")),
    list(private = path, upload = paste0(path, "_upload")),
    list(private = path, upload = file.path(dirname(path), "upload"))
  )
  valid = vapply(candidates, function(pair) {
    file.exists(file.path(pair$private, "MANIFEST.csv")) &&
      file.exists(file.path(pair$upload, "UPLOAD_MANIFEST.csv"))
  }, logical(1L))
  if (sum(valid) != 1L) {
    stop(label, " must resolve unambiguously to one private/upload study pair.", call. = FALSE)
  }
  candidates[[which(valid)]]
}

shild_pair = .resolve_study_pair(
  Sys.getenv(
    "CSDG_SHILD_PARENT",
    unset = file.path(project_root, "paper_artifacts", protocol$default_shild_pair)
  ),
  "CSDG_SHILD_PARENT"
)
pisa_pair = .resolve_study_pair(
  Sys.getenv(
    "CSDG_PISA_PARENT",
    unset = file.path(project_root, "paper_artifacts", protocol$default_pisa_pair)
  ),
  "CSDG_PISA_PARENT"
)
parent_shild = shild_pair$private
parent_shild_upload = shild_pair$upload
parent_pisa_root = pisa_pair$private
parent_pisa_upload = pisa_pair$upload
parent_shild_audit_summary_path = file.path(
  parent_shild_upload,
  "provenance",
  "output_audit_summary.csv"
)
parent_pisa_audit_summary_path = file.path(
  parent_pisa_upload,
  "provenance",
  "output_audit_summary.csv"
)
helper_path = file.path(analysis_dir, "shared", "evidence_helpers.R")
shild_source_filenames = c(
  frozen_shild_claim_support = "shild_claim_support.csv",
  frozen_shild_sensitivity = "shild_performance_explanation_sensitivity.csv",
  frozen_shild_importance_agreement = "shild_model_importance_agreement.csv"
)
shild_source_tables = setNames(
  file.path(parent_shild_upload, "tables", unname(shild_source_filenames)),
  names(shild_source_filenames)
)
pisa_source_filenames = c(
  frozen_pisa_claim_support = "pisa_claim_support.csv",
  frozen_pisa_sensitivity = "pisa_performance_explanation_sensitivity.csv",
  frozen_pisa_importance_agreement = "pisa_model_importance_agreement.csv",
  frozen_pisa_all_pv_performance = "pisa_all_pv_descriptive_summary.csv",
  frozen_pisa_all_pv_pfi = "pisa_all_pv_pfi_summary.csv",
  frozen_pisa_loco_penalties = "pisa_loco_country_penalty_summary.csv"
)
pisa_source_tables = setNames(
  file.path(parent_pisa_upload, "tables", unname(pisa_source_filenames)),
  names(pisa_source_filenames)
)
if (!identical(names(shild_source_tables), names(shild_source_filenames)) ||
    !identical(names(pisa_source_tables), names(pisa_source_filenames))) {
  stop("Frozen aggregate source paths lost their provenance names.", call. = FALSE)
}

parent_result_path = file.path(parent_shild, "objects", "shild_csdg_result_private.rds")
parent_multiplicity_path = file.path(parent_shild, "objects", "shild_multiplicity_row_level_private.csv")

# ---- Input integrity and source compatibility ----

required_inputs = c(
  parent_result_path,
  parent_multiplicity_path,
  file.path(parent_shild, "MANIFEST.csv"),
  file.path(parent_shild_upload, "UPLOAD_MANIFEST.csv"),
  file.path(parent_pisa_upload, "UPLOAD_MANIFEST.csv"),
  parent_shild_audit_summary_path,
  parent_pisa_audit_summary_path,
  protocol_path,
  helper_path,
  normalizePath(script_path, winslash = "/", mustWork = TRUE),
  shild_source_tables,
  pisa_source_tables
)
.require_files(required_inputs)
verify_recursive_manifest(parent_shild, file.path(parent_shild, "MANIFEST.csv"))
verify_sanitized_upload_bundle(
  parent_shild_upload,
  "shild",
  file.path(parent_shild_upload, "UPLOAD_MANIFEST.csv")
)
verify_sanitized_upload_bundle(
  parent_pisa_upload,
  "pisa2018_reading",
  file.path(parent_pisa_upload, "UPLOAD_MANIFEST.csv")
)
parent_readiness = .evidence_parent_readiness(
  parent_shild_audit_summary_path,
  parent_pisa_audit_summary_path
)
parent_source_manifest = fread(file.path(parent_shild, "provenance", "package_source_manifest.csv"))
frozen_helper_paths = c(
  "R/csdg_local_faithfulness.R",
  "R/csdg_local_fidelity_audit.R",
  "R/csdg_local_tree_comparator.R",
  "R/csdg_utils.R"
)
frozen_helper_hash_check = merge(
  parent_source_manifest[path %in% frozen_helper_paths, .(path, parent_sha256 = sha256)],
  package_source$manifest[path %in% frozen_helper_paths, .(path, current_sha256 = sha256)],
  by = "path",
  all = TRUE
)
if (nrow(frozen_helper_hash_check) != length(frozen_helper_paths) || anyNA(frozen_helper_hash_check) ||
    any(frozen_helper_hash_check$parent_sha256 != frozen_helper_hash_check$current_sha256)) {
  stop(
    "The current local-fidelity or prediction helper differs from the frozen SHILD parent source.",
    call. = FALSE
  )
}

# ---- Prespecified sampling and local-fidelity parameters ----

selection_seed = as.integer(protocol$case_sampling$selection_seed)
n_folds = as.integer(protocol$case_sampling$folds)
n_deciles = as.integer(protocol$case_sampling$prediction_score_deciles)
cases_per_cell = as.integer(protocol$case_sampling$cases_per_stratum)
n_cases = as.integer(protocol$case_sampling$total_cases)
perturbation_seed_base = as.integer(protocol$local_evaluation$perturbation_seed_base)
perturbation_seed_stride = as.integer(protocol$local_evaluation$perturbation_seed_stride)
crossfit_seed_base = as.integer(protocol$local_evaluation$crossfit_seed_base)
primary_kernel_width = as.numeric(protocol$local_evaluation$primary_kernel_width)
sensitivity_kernel_widths = as.numeric(protocol$local_evaluation$sensitivity_kernel_widths)
kernel_widths = sort(c(primary_kernel_width, sensitivity_kernel_widths))
n_perturb = as.integer(protocol$local_evaluation$perturbations_per_case_seed)
n_replicates = as.integer(protocol$local_evaluation$perturbation_replicates)
randomness_seeds_per_source = as.integer(protocol$secondary_sensitivity$randomness_seeds_per_source)
historical_weighted_r2_reference = as.numeric(
  protocol$local_evaluation$historical_weighted_r2_reference
)
protocol_values = c(
  selection_seed = selection_seed,
  n_folds = n_folds,
  n_deciles = n_deciles,
  cases_per_cell = cases_per_cell,
  n_cases = n_cases,
  perturbation_seed_base = perturbation_seed_base,
  perturbation_seed_stride = perturbation_seed_stride,
  crossfit_seed_base = crossfit_seed_base,
  primary_kernel_width = primary_kernel_width,
  sensitivity_kernel_width_1 = sensitivity_kernel_widths[[1L]],
  sensitivity_kernel_width_2 = sensitivity_kernel_widths[[2L]],
  sensitivity_kernel_width_3 = sensitivity_kernel_widths[[3L]],
  n_perturb = n_perturb,
  n_replicates = n_replicates,
  randomness_seeds_per_source = randomness_seeds_per_source,
  historical_weighted_r2_reference = historical_weighted_r2_reference,
  surrogate_folds = as.integer(protocol$local_evaluation$surrogate_folds)
)
expected_protocol_values = c(
  selection_seed = 20260812,
  n_folds = 10,
  n_deciles = 10,
  cases_per_cell = 3,
  n_cases = 300,
  perturbation_seed_base = 202608120,
  perturbation_seed_stride = 1000000,
  crossfit_seed_base = 302608120,
  primary_kernel_width = 0.75,
  sensitivity_kernel_width_1 = 0.25,
  sensitivity_kernel_width_2 = 0.50,
  sensitivity_kernel_width_3 = 1.00,
  n_perturb = 500,
  n_replicates = 20,
  randomness_seeds_per_source = 5,
  historical_weighted_r2_reference = 0.80,
  surrogate_folds = 5
)
protocol_text = c(
  cell_seed_rule = protocol$case_sampling$cell_seed_rule,
  quartile_definition = protocol$reporting$quartile_definition,
  support_primary_measure = protocol$support_diagnostic$primary_measure
)
expected_protocol_text = c(
  cell_seed_rule = "selection seed + 100 * fold number + within-fold prediction-score decile",
  quartile_definition = "R quantile type 8",
  support_primary_measure =
    "10th-nearest fold-training distance; smaller values indicate closer observed training rows"
)
if (!isTRUE(all.equal(protocol_values, expected_protocol_values, tolerance = 0)) ||
    !identical(protocol_text, expected_protocol_text) ||
    n_folds * n_deciles * cases_per_cell != n_cases) {
  stop("The systematic local-fidelity protocol differs from the prespecification.", call. = FALSE)
}
workers = suppressWarnings(as.integer(Sys.getenv("CSDG_G4_WORKERS", unset = "3")))
if (!is.finite(workers) || workers < 1L) workers = 1L
workers = min(workers, 6L)
run_id = Sys.getenv("CSDG_RUN_ID", unset = "")
if (!nzchar(run_id)) {
  stop("Set `CSDG_RUN_ID` explicitly for the systematic evidence synthesis.", call. = FALSE)
}

# ---- Run contract and reproducibility safeguards ----

upload_expected = c(
  "tables/evidence_synthesis_run_mode.csv",
  "tables/shild_g4_case_sampling_plan.csv",
  "tables/shild_g4_local_fidelity_replicates.csv",
  "tables/shild_g4_local_fidelity_summary.csv",
  "tables/shild_g4_local_fidelity_cases.csv",
  "tables/shild_g4_local_fidelity_bandwidth_cases.csv",
  "tables/shild_g4_coefficient_stability.csv",
  "tables/shild_g4_coefficient_stability_summary.csv",
  "tables/shild_g4_fidelity_by_prediction_score_decile.csv",
  "tables/shild_g4_fidelity_by_model_disagreement.csv",
  "tables/shild_g4_fidelity_by_local_support.csv",
  "tables/shild_g4_method_sensitivity_cases.csv",
  "tables/shild_g4_method_sensitivity_summary.csv",
  "tables/shild_g4_randomness_grid.csv",
  "tables/shild_g4_randomness_cases.csv",
  "tables/shild_g4_randomness_summary.csv",
  "tables/pisa_all_pv_leading_importance_recurrence.csv",
  "tables/shild_g4_systematic_conclusion.csv",
  "tables/shild_performance_explanation_sensitivity.csv",
  "tables/pisa_performance_explanation_sensitivity.csv",
  "tables/pisa_evidence_boundaries.csv",
  "figures",
  "EVIDENCE_SYNTHESIS.md",
  "logs/console.log",
  "logs/conditions.csv",
  "checkpoints/checkpoints.csv",
  "provenance/run_metadata.csv",
  "provenance/source_files.csv",
  "provenance/input_files.csv",
  "provenance/input_checksums.csv",
  "provenance/package_source_manifest.csv",
  "provenance/package_source_tree_hash.csv",
  "provenance/parent_artifact_integrity.csv",
  "provenance/frozen_helper_source_hashes.csv",
  "provenance/frozen_shild_output_audit_summary.csv",
  "provenance/frozen_pisa_output_audit_summary.csv",
  "provenance/figure_catalog.csv",
  "provenance/figure_latex_metadata.csv",
  "provenance/protocol.yml",
  "provenance/sessionInfo.txt"
)
run = create_run_contract(
  study = "evidence_synthesis",
  output_root = file.path(project_root, "paper_artifacts"),
  run_id = run_id,
  mode = "publication",
  seed = selection_seed,
  expected_artifacts = upload_expected
)

analysis_sources = c(
  evidence_synthesis = normalizePath(script_path, winslash = "/", mustWork = TRUE),
  evidence_synthesis_helpers = helper_path,
  study_helpers = file.path(analysis_dir, "shared", "study_helpers.R"),
  shild_preparation = file.path(analysis_dir, "shild", "prepare_data.R"),
  pisa_preparation = file.path(analysis_dir, "pisa", "prepare_data.R"),
  output = file.path(analysis_dir, "shared", "output.R"),
  output_audit = file.path(analysis_dir, "shared", "output_audit.R")
)
analysis_inputs = c(
  frozen_shild_result = parent_result_path,
  frozen_shild_multiplicity = parent_multiplicity_path,
  frozen_shild_manifest = file.path(parent_shild, "MANIFEST.csv"),
  frozen_shild_upload_manifest = file.path(parent_shild_upload, "UPLOAD_MANIFEST.csv"),
  frozen_pisa_upload_manifest = file.path(parent_pisa_upload, "UPLOAD_MANIFEST.csv"),
  frozen_shild_output_audit_summary = parent_shild_audit_summary_path,
  frozen_pisa_output_audit_summary = parent_pisa_audit_summary_path,
  frozen_protocol = protocol_path,
  shild_source_tables,
  pisa_source_tables
)
analysis_source_hashes = vapply(analysis_sources, digest::digest, character(1L), file = TRUE, algo = "sha256")
analysis_input_hashes = vapply(analysis_inputs, digest::digest, character(1L), file = TRUE, algo = "sha256")
.assert_synthesis_hash_snapshot = function(context) {
  current_source_hashes = vapply(analysis_sources, digest::digest, character(1L), file = TRUE, algo = "sha256")
  current_input_hashes = vapply(analysis_inputs, digest::digest, character(1L), file = TRUE, algo = "sha256")
  if (!identical(current_source_hashes, analysis_source_hashes) ||
      !identical(current_input_hashes, analysis_input_hashes)) {
    stop("Analysis sources or frozen inputs changed during ", context, ".", call. = FALSE)
  }
  invisible(TRUE)
}
.assert_parent_artifact_snapshots = function(context) {
  .assert_synthesis_hash_snapshot(context)
  verify_recursive_manifest(parent_shild, file.path(parent_shild, "MANIFEST.csv"))
  verify_sanitized_upload_bundle(
    parent_shild_upload,
    "shild",
    file.path(parent_shild_upload, "UPLOAD_MANIFEST.csv")
  )
  verify_sanitized_upload_bundle(
    parent_pisa_upload,
    "pisa2018_reading",
    file.path(parent_pisa_upload, "UPLOAD_MANIFEST.csv")
  )
  current_parent_readiness = .evidence_parent_readiness(
    parent_shild_audit_summary_path,
    parent_pisa_audit_summary_path
  )
  if (!identical(current_parent_readiness, parent_readiness)) {
    stop("Parent output-audit readiness changed during ", context, ".", call. = FALSE)
  }
  invisible(TRUE)
}
synthesis_signature = .evidence_canonical_signature(
  package_tree_hash = package_source$tree_hash,
  source_hashes = analysis_source_hashes,
  input_hashes = analysis_input_hashes,
  parameters = c(
    selection_seed = selection_seed,
    perturbation_seed_base = perturbation_seed_base,
    perturbation_seed_stride = perturbation_seed_stride,
    crossfit_seed_base = crossfit_seed_base,
    kernel_widths = paste(kernel_widths, collapse = ","),
    n_perturb = n_perturb,
    n_replicates = n_replicates,
    randomness_seeds_per_source = randomness_seeds_per_source,
    historical_weighted_r2_reference = historical_weighted_r2_reference
  )
)
cache_dir = file.path(
  project_root,
  "paper_artifacts",
  ".checkpoint_cache",
  "shild_g4_systematic_v2",
  synthesis_signature
)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Local validation and reporting helpers ----

.evidence_replay_focal_oof = function(oof) {
  replayed = rbindlist(lapply(seq_along(oof$models), function(index) {
    rows = oof$test_sets[[index]]
    newdata = oof$task$data(rows = rows, cols = oof$task$feature_names)
    prediction = oof$models[[index]]$predict_newdata(newdata)
    data.table(
      row_id = as.integer(rows),
      replay_probability = .prediction_value(prediction, oof$task, oof$positive)
    )
  }))
  archived = as.data.table(oof$predictions)[, .(
    row_id = as.integer(row_id),
    archived_probability = as.numeric(prob.discontent)
  )]
  check = merge(archived, replayed, by = "row_id", all = TRUE)
  if (nrow(check) != oof$task$nrow || anyNA(check) || anyDuplicated(check$row_id)) {
    stop("Frozen focal OOF replay did not preserve every analytic row exactly once.", call. = FALSE)
  }
  maximum_delta = max(abs(check$archived_probability - check$replay_probability))
  if (!is.finite(maximum_delta) || maximum_delta > 1e-12) {
    stop("Frozen focal OOF replay differs from the archived predictions by ", maximum_delta, ".", call. = FALSE)
  }
  maximum_delta
}

.evidence_frozen_model_hash = function(model) {
  state = model$state
  state$predict_time = NULL
  digest::digest(state, algo = "sha256")
}

.evidence_write_claim_markdown = function(shild_claims, pisa_claims, path) {
  table_lines = function(data) {
    c(
      "| Claim | Evidence needed | Permissible conclusion |",
      "|---|---|---|",
      vapply(seq_len(nrow(data)), function(index) {
        paste0(
          "| ", gsub("\\|", "\\\\|", data$claim[[index]]),
          " | ", data$evidence_needed[[index]],
          " | ", gsub("\\|", "\\\\|", data$conclusion[[index]]), " |"
        )
      }, character(1L))
    )
  }
  writeLines(c(
    "# Claim-specific evidence boundaries",
    "",
    paste(
      "Each row is a distinct interpretive claim boundary derived from the archived evidence.",
      "These rows were not separately rerun as formal gate plans, and no total score or passed-gate count exists."
    ),
    "",
    "## SHILD",
    "",
    table_lines(shild_claims),
    "",
    "## PISA",
    "",
    table_lines(pisa_claims),
    ""
  ), path, useBytes = TRUE)
}

run_logging = start_run_logging(run, failure_checkpoint = "evidence_synthesis")
write_run_checkpoint(run, "evidence_synthesis", "started", list(signature = synthesis_signature))

# ---- Frozen model replay and systematic case sampling ----

message("Loading and verifying the frozen SHILD result without predictive-model retraining.")
parent_file_hash_before = digest::digest(file = parent_result_path, algo = "sha256")
result = readRDS(parent_result_path)
if (!inherits(result, "CSDGResult") || is.null(result$artifacts$oof$models) ||
    length(result$artifacts$oof$models) != 10L) {
  stop("The frozen SHILD result does not contain ten held-out fold models.", call. = FALSE)
}
oof = result$artifacts$oof
model_hash_before = vapply(
  oof$models,
  .evidence_frozen_model_hash,
  character(1L)
)
replay_delta = .evidence_replay_focal_oof(oof)
message("Frozen focal-model OOF replay maximum absolute difference: ", format(replay_delta, scientific = TRUE))

sampling = .evidence_select_local_cases(
  oof$predictions,
  folds = n_folds,
  deciles = n_deciles,
  cases_per_cell = cases_per_cell,
  seed = selection_seed
)
multiplicity = fread(parent_multiplicity_path)

# ---- Fold-specific local-fidelity evaluation ----

all_fold_results = vector("list", n_folds)
for (fold_index in seq_len(n_folds)) {
  checkpoint_path = file.path(cache_dir, sprintf("fold_%02d.rds", fold_index))
  fold_cases = sampling$private[fold == fold_index]
  if (file.exists(checkpoint_path)) {
    checkpoint = readRDS(checkpoint_path)
    if (!is.list(checkpoint) || !identical(checkpoint$signature, synthesis_signature) ||
        !identical(checkpoint$fold, fold_index)) {
      stop("A systematic G4 checkpoint has incompatible provenance: ", checkpoint_path, call. = FALSE)
    }
    .evidence_validate_fold_result(
      checkpoint$result,
      fold_cases,
      fold_index,
      n_replicates,
      kernel_widths,
      primary_kernel_width = primary_kernel_width,
      fidelity_threshold = historical_weighted_r2_reference
    )
    all_fold_results[[fold_index]] = checkpoint$result
    message("Loaded validated systematic G4 checkpoint for fold ", fold_index, ".")
    next
  }
  jobs = fold_cases[, .(
    row_id = as.integer(row_id),
    case_label,
    fold = as.integer(fold),
    prediction_score_decile = as.integer(prediction_score_decile)
  )]
  evaluator = function(index) {
    tryCatch({
      job = jobs[index]
      case_seeds = .evidence_perturbation_seed(
        perturbation_seed_base,
        rep(job$case_label, n_replicates),
        seq_len(n_replicates),
        perturbation_seed_stride
      )
      crossfit_seeds = .evidence_perturbation_seed(
        crossfit_seed_base,
        rep(job$case_label, n_replicates),
        seq_len(n_replicates),
        perturbation_seed_stride
      )
      csdg_local_fidelity_audit(
        x = oof,
        cases = job$row_id,
        seeds = matrix(case_seeds, nrow = 1L),
        crossfit_seeds = matrix(crossfit_seeds, nrow = 1L),
        kernel_widths = kernel_widths,
        primary_kernel_width = primary_kernel_width,
        n_perturb = n_perturb,
        crossfit_folds = as.integer(protocol$local_evaluation$surrogate_folds),
        fidelity_threshold = historical_weighted_r2_reference,
        case_labels = job$case_label,
        case_metadata = data.frame(prediction_score_decile = job$prediction_score_decile)
      )
    }, error = function(error) {
      structure(list(message = conditionMessage(error), job = jobs[index]), class = "synthesis_job_error")
    })
  }
  evaluated = if (.Platform$OS.type != "windows" && workers > 1L) {
    parallel::mclapply(seq_len(nrow(jobs)), evaluator, mc.cores = workers, mc.preschedule = TRUE)
  } else {
    lapply(seq_len(nrow(jobs)), evaluator)
  }
  failed = vapply(evaluated, inherits, logical(1L), what = "synthesis_job_error")
  if (any(failed)) {
    stop(
      "At least one parallel systematic G4 job failed: ",
      paste(unique(vapply(evaluated[failed], `[[`, character(1L), "message")), collapse = " | "),
      call. = FALSE
    )
  }
  fold_result = list(
    replicates = rbindlist(lapply(evaluated, `[[`, "replicates")),
    support = unique(rbindlist(lapply(evaluated, `[[`, "support")), by = "case_label"),
    coefficients = rbindlist(lapply(evaluated, `[[`, "coefficients"), fill = TRUE)
  )
  .evidence_validate_fold_result(
    fold_result,
    fold_cases,
    fold_index,
    n_replicates,
    kernel_widths,
    primary_kernel_width = primary_kernel_width,
    fidelity_threshold = historical_weighted_r2_reference
  )
  checkpoint = list(signature = synthesis_signature, fold = fold_index, result = fold_result)
  saveRDS(checkpoint, checkpoint_path, version = 3)
  all_fold_results[[fold_index]] = fold_result
  write_run_checkpoint(
    run,
    paste0("shild_g4_fold_", sprintf("%02d", fold_index)),
    "completed",
    list(n_cases = nrow(fold_cases), n_replicate_rows = nrow(fold_result$replicates))
  )
  message("Completed systematic G4 fold ", fold_index, " of ", n_folds, ".")
}

# ---- Local-fidelity summaries and conclusion ----

replicates = rbindlist(lapply(all_fold_results, `[[`, "replicates"))
support = rbindlist(lapply(all_fold_results, `[[`, "support"))
coefficients = rbindlist(lapply(all_fold_results, `[[`, "coefficients"), fill = TRUE)
local_summary = .evidence_summarize_local_fidelity(
  replicates,
  support,
  sampling$private,
  multiplicity,
  coefficients = coefficients,
  primary_kernel_width = primary_kernel_width,
  historical_reference = historical_weighted_r2_reference,
  expected_replicates = n_replicates,
  expected_widths = kernel_widths
)
primary_summary = local_summary$summary[scope == "primary case medians"]
if (nrow(primary_summary) != 1L) stop("Primary systematic G4 summary is ambiguous.", call. = FALSE)

# The secondary audit changes one design choice at a time for one case in every
# fold-by-prediction-score cell. The first five primary replicates form the
# response-scale, synthetic-neighborhood additive reference.
secondary_replicates = as.integer(protocol$secondary_sensitivity$perturbation_replicates)
secondary_cases = sampling$private[selection_draw == 1L]
if (secondary_replicates != 5L || randomness_seeds_per_source != 5L ||
    nrow(secondary_cases) != n_folds * n_deciles) {
  stop("The secondary local-fidelity sensitivity grid differs from the protocol.", call. = FALSE)
}
secondary_fold_results = vector("list", n_folds)
for (fold_index in seq_len(n_folds)) {
  checkpoint_path = file.path(cache_dir, sprintf("secondary_fold_%02d.rds", fold_index))
  fold_cases = secondary_cases[fold == fold_index]
  if (file.exists(checkpoint_path)) {
    checkpoint = readRDS(checkpoint_path)
    if (!is.list(checkpoint) || !identical(checkpoint$signature, synthesis_signature) ||
        !identical(checkpoint$fold, fold_index)) {
      stop("A secondary local-fidelity checkpoint has incompatible provenance.", call. = FALSE)
    }
    secondary_fold_results[[fold_index]] = checkpoint$result
    next
  }
  perturbation_seeds = vapply(seq_len(secondary_replicates), function(replicate) {
    .evidence_perturbation_seed(
      perturbation_seed_base,
      fold_cases$case_label,
      replicate,
      perturbation_seed_stride
    )
  }, integer(nrow(fold_cases)))
  crossfit_seeds = vapply(seq_len(secondary_replicates), function(replicate) {
    .evidence_perturbation_seed(
      crossfit_seed_base,
      fold_cases$case_label,
      replicate,
      perturbation_seed_stride
    )
  }, integer(nrow(fold_cases)))
  randomness_audit = csdg_local_fidelity_randomness_audit(
    x = oof,
    cases = fold_cases$row_id,
    perturbation_seeds = perturbation_seeds[, seq_len(randomness_seeds_per_source), drop = FALSE],
    crossfit_seeds = crossfit_seeds[, seq_len(randomness_seeds_per_source), drop = FALSE],
    kernel_width = primary_kernel_width,
    n_perturb = n_perturb,
    crossfit_folds = as.integer(protocol$local_evaluation$surrogate_folds),
    target_scale = "response",
    neighborhood_method = "synthetic",
    case_labels = fold_cases$case_label,
    case_metadata = data.frame(prediction_score_decile = fold_cases$prediction_score_decile)
  )
  common_arguments = list(
    x = oof,
    cases = fold_cases$row_id,
    seeds = perturbation_seeds,
    crossfit_seeds = crossfit_seeds,
    kernel_widths = primary_kernel_width,
    primary_kernel_width = primary_kernel_width,
    n_perturb = n_perturb,
    crossfit_folds = as.integer(protocol$local_evaluation$surrogate_folds),
    fidelity_threshold = NULL,
    case_labels = fold_cases$case_label,
    case_metadata = data.frame(prediction_score_decile = fold_cases$prediction_score_decile)
  )
  link_audit = do.call(csdg_local_fidelity_audit, c(
    common_arguments,
    list(target_scale = "link", neighborhood_method = "synthetic")
  ))
  empirical_audit = do.call(csdg_local_fidelity_audit, c(
    common_arguments,
    list(
      target_scale = "response",
      neighborhood_method = "empirical_knn",
      empirical_neighbors = as.integer(protocol$secondary_sensitivity$empirical_neighbors)
    )
  ))
  tree_audit = csdg_local_tree_comparator(
    x = oof,
    cases = fold_cases$row_id,
    seeds = perturbation_seeds,
    crossfit_seeds = crossfit_seeds,
    kernel_width = primary_kernel_width,
    n_perturb = n_perturb,
    crossfit_folds = as.integer(protocol$local_evaluation$surrogate_folds),
    target_scale = "response",
    neighborhood_method = "synthetic",
    case_labels = fold_cases$case_label,
    case_metadata = data.frame(prediction_score_decile = fold_cases$prediction_score_decile)
  )
  result_secondary = list(
    link = link_audit$replicates,
    empirical = empirical_audit$replicates,
    tree = tree_audit$replicates,
    randomness_grid = randomness_audit$grid,
    randomness_cases = randomness_audit$cases
  )
  saveRDS(
    list(signature = synthesis_signature, fold = fold_index, result = result_secondary),
    checkpoint_path,
    version = 3
  )
  secondary_fold_results[[fold_index]] = result_secondary
  write_run_checkpoint(
    run,
    paste0("shild_g4_secondary_fold_", sprintf("%02d", fold_index)),
    "completed",
    list(n_cases = nrow(fold_cases), n_replicates = secondary_replicates)
  )
}

.summarize_method_cases = function(source, method_label, surrogate_family) {
  source = copy(as.data.table(source))
  source[, .(
    method = method_label,
    surrogate_family = surrogate_family,
    target_scale = target_scale[[1L]],
    neighborhood_method = neighborhood_method[[1L]],
    n_replicates = .N,
    median_weighted_r2 = stats::median(weighted_r2),
    median_weighted_rmse = stats::median(weighted_rmse),
    median_weighted_mae = stats::median(weighted_mae),
    median_maximum_absolute_error = stats::median(maximum_absolute_error),
    median_target_case_absolute_error = stats::median(target_case_absolute_error)
  ), by = .(case_label, fold, prediction_score_decile)]
}
secondary_labels = secondary_cases$case_label
primary_secondary = replicates[
  case_label %in% secondary_labels &
    abs(kernel_width - primary_kernel_width) < 1e-12 &
    perturbation_replicate <= secondary_replicates
]
link_secondary = rbindlist(lapply(secondary_fold_results, `[[`, "link"))
empirical_secondary = rbindlist(lapply(secondary_fold_results, `[[`, "empirical"))
tree_secondary = rbindlist(lapply(secondary_fold_results, `[[`, "tree"))
randomness_grid = rbindlist(lapply(secondary_fold_results, `[[`, "randomness_grid"))
randomness_cases = rbindlist(lapply(secondary_fold_results, `[[`, "randomness_cases"))
expected_randomness_grid_rows = nrow(secondary_cases) * randomness_seeds_per_source^2
expected_randomness_case_rows = nrow(secondary_cases) * 5L
if (nrow(randomness_grid) != expected_randomness_grid_rows ||
    uniqueN(randomness_grid, by = c(
      "case_label", "perturbation_seed_index", "crossfit_seed_index"
    )) != expected_randomness_grid_rows ||
    nrow(randomness_cases) != expected_randomness_case_rows ||
    uniqueN(randomness_cases, by = c("case_label", "metric")) != expected_randomness_case_rows) {
  stop("The complete crossed local-fidelity randomness audit is incomplete or duplicated.", call. = FALSE)
}
method_sensitivity_cases = rbindlist(list(
  .summarize_method_cases(primary_secondary, "Additive: probability / synthetic", "additive ridge"),
  .summarize_method_cases(link_secondary, "Additive: logit / synthetic", "additive ridge"),
  .summarize_method_cases(empirical_secondary, "Additive: probability / empirical kNN", "additive ridge"),
  .summarize_method_cases(tree_secondary, "Nonlinear: probability / synthetic", "regression tree")
), use.names = TRUE)
method_sensitivity_summary = method_sensitivity_cases[, .(
  n_cases = .N,
  median_weighted_r2 = stats::median(median_weighted_r2),
  q25_weighted_r2 = .evidence_quantile(median_weighted_r2, 0.25),
  q75_weighted_r2 = .evidence_quantile(median_weighted_r2, 0.75),
  median_weighted_rmse = stats::median(median_weighted_rmse),
  q25_weighted_rmse = .evidence_quantile(median_weighted_rmse, 0.25),
  q75_weighted_rmse = .evidence_quantile(median_weighted_rmse, 0.75),
  median_target_case_absolute_error = stats::median(median_target_case_absolute_error),
  uncertainty_semantics = "Descriptive across 100 prespecified cases; no confidence interval or hypothesis test"
), by = .(method, surrogate_family, target_scale, neighborhood_method)]

.finite_median = function(x) {
  finite = x[is.finite(x)]
  if (length(finite)) stats::median(finite) else NA_real_
}
randomness_summary = randomness_cases[, .(
  n_cases = .N,
  n_perturbation_seeds = unique(n_perturbation_seeds),
  n_crossfit_seeds = unique(n_crossfit_seeds),
  median_grid_mean = stats::median(grid_mean),
  median_perturbation_marginal_sd = stats::median(perturbation_marginal_sd),
  q25_perturbation_marginal_sd = .evidence_quantile(perturbation_marginal_sd, 0.25),
  q75_perturbation_marginal_sd = .evidence_quantile(perturbation_marginal_sd, 0.75),
  median_crossfit_marginal_sd = stats::median(crossfit_marginal_sd),
  q25_crossfit_marginal_sd = .evidence_quantile(crossfit_marginal_sd, 0.25),
  q75_crossfit_marginal_sd = .evidence_quantile(crossfit_marginal_sd, 0.75),
  median_interaction_rms = stats::median(interaction_rms),
  median_full_grid_sd = stats::median(full_grid_sd),
  median_perturbation_sum_squares_fraction = .finite_median(perturbation_sum_squares_fraction),
  median_crossfit_sum_squares_fraction = .finite_median(crossfit_sum_squares_fraction),
  median_interaction_sum_squares_fraction = .finite_median(interaction_sum_squares_fraction),
  n_zero_variation_cases = sum(!is.finite(perturbation_sum_squares_fraction)),
  uncertainty_semantics = paste(
    "Balanced deterministic decomposition of a complete crossed seed grid;",
    "computational variation only, not a confidence interval or hypothesis test"
  )
), by = metric]
metric_order = c(
  "weighted_r2", "weighted_rmse", "weighted_mae", "maximum_absolute_error",
  "target_case_absolute_error"
)
randomness_summary[, metric_order := match(metric, metric_order)]
setorder(randomness_summary, metric_order)
randomness_summary[, metric_order := NULL]

coefficient_stability = local_summary$coefficient_stability
coefficient_stability_summary = coefficient_stability[term != "(Intercept)", .(
  n_cases = uniqueN(case_label),
  n_case_terms = .N,
  median_sign_consistency = stats::median(sign_consistency),
  q10_sign_consistency = .evidence_quantile(sign_consistency, 0.10),
  proportion_case_terms_below_80_percent_sign_consistency = mean(sign_consistency < 0.80),
  median_absolute_rank_sd = stats::median(absolute_rank_sd),
  uncertainty_semantics = "Descriptive perturbation and cross-fit stability; not model-fit uncertainty"
), by = .(target_scale, neighborhood_method)]
mixed_seed_cases = local_summary$cases[
  proportion_replicates_below_threshold > 0 & proportion_replicates_below_threshold < 1,
  .N
]
low_risk_rmse = local_summary$by_prediction_score[prediction_score_decile == 1L, median_case_median_weighted_rmse]
high_risk_rmse = local_summary$by_prediction_score[prediction_score_decile == 10L, median_case_median_weighted_rmse]
randomness_r2 = randomness_summary[metric == "weighted_r2"]
if (nrow(randomness_r2) != 1L) stop("The weighted-R-squared randomness summary is ambiguous.", call. = FALSE)
g4_conclusion = data.table(
  audit_status = "descriptive_multimetric_evidence_for_a_narrow_declared_surrogate_design",
  joint_evidence_review_status = "prespecified_joint_pattern_review_complete",
  joint_pattern_role = "potential_defeater",
  joint_pattern_materiality = "materialized_for_SHILD-C0",
  joint_pattern_adjudication_basis = "substantive_adjudication",
  joint_pattern_claim_consequence = "exact_claim_not_retained",
  joint_pattern_criterion_source = "Canonical SHILD-C0 semantics and the prespecified joint evidence review",
  joint_pattern_criterion_rationale = paste(
    "SHILD-C0 asserts generally reliable person-specific explanations for the declared follow-up context;",
    "convergent design dependence and explanation instability are therefore material to that exact claim"
  ),
  joint_pattern_scope = paste(
    "Convergent apparent-versus-cross-fitted optimism, response/link-scale and synthetic/empirical-neighborhood",
    "sensitivity, kernel/locality sensitivity, perturbation-dominated randomness, encoded-term sign/rank",
    "instability, nonlinear-comparator and conditional-Shapley evidence, and absolute/relative error distributions;",
    "no aggregate score and no universal threshold"
  ),
  case_summary_rule = paste(
    "Metric medians across 20 independently generated paired perturbation and cross-fit seeds;",
    "a secondary complete 5-by-5 crossing separates the two sources"
  ),
  n_prespecified_cases = primary_summary$n_cases,
  n_cases_below_historical_reference = primary_summary$n_below_threshold,
  proportion_cases_below_historical_reference = primary_summary$proportion_below_threshold,
  historical_weighted_r2_reference = historical_weighted_r2_reference,
  median_case_weighted_rmse = stats::median(local_summary$cases$median_weighted_rmse),
  median_case_weighted_mae = stats::median(local_summary$cases$median_weighted_mae),
  median_case_maximum_absolute_error = stats::median(local_summary$cases$median_maximum_absolute_error),
  median_target_case_absolute_error = stats::median(local_summary$cases$median_target_case_absolute_error),
  median_case_monte_carlo_sd = stats::median(local_summary$cases$monte_carlo_sd),
  randomness_n_cases = randomness_r2$n_cases,
  randomness_median_perturbation_marginal_sd_weighted_r2 =
    randomness_r2$median_perturbation_marginal_sd,
  randomness_median_crossfit_marginal_sd_weighted_r2 = randomness_r2$median_crossfit_marginal_sd,
  randomness_median_interaction_rms_weighted_r2 = randomness_r2$median_interaction_rms,
  randomness_median_perturbation_sum_squares_fraction_weighted_r2 =
    randomness_r2$median_perturbation_sum_squares_fraction,
  randomness_median_crossfit_sum_squares_fraction_weighted_r2 =
    randomness_r2$median_crossfit_sum_squares_fraction,
  randomness_median_interaction_sum_squares_fraction_weighted_r2 =
    randomness_r2$median_interaction_sum_squares_fraction,
  n_cases_with_mixed_seed_status = mixed_seed_cases,
  prediction_score_decile_1_median_case_weighted_rmse = low_risk_rmse,
  prediction_score_decile_10_median_case_weighted_rmse = high_risk_rmse,
  canonical_gate_status_changed = FALSE,
  interpretation = paste(
    "Systematic descriptive audit of the retained fold models;",
    "the two post-hoc cases remain communication examples."
  ),
  limitation = paste(
    "The synthetic neighborhood is marginal and perturbation-dependent and does not guarantee joint empirical",
    paste(
      "support. Primary summaries pair independently generated perturbation and cross-fit seeds; the secondary",
      "complete crossing separates their marginal and interaction components. This computational variation is not"
    ),
    paste(
      "model-fit or sampling uncertainty. Weighted R-squared is relative to local prediction variance and",
      "interpreted jointly with weighted RMSE, weighted MAE, maximum absolute error, and target-case error."
    )
  )
)

# ---- Cross-study claim and model-sensitivity synthesis ----

message("Building cross-study claim and model-sensitivity outputs from frozen aggregate tables.")
shild_claim = fread(shild_source_tables[["frozen_shild_claim_support"]])
shild_sensitivity = fread(shild_source_tables[["frozen_shild_sensitivity"]])
shild_importance_agreement = fread(shild_source_tables[["frozen_shild_importance_agreement"]])
pisa_frozen_claim = fread(pisa_source_tables[["frozen_pisa_claim_support"]])
pisa_sensitivity = fread(pisa_source_tables[["frozen_pisa_sensitivity"]])
pisa_importance_agreement = fread(pisa_source_tables[["frozen_pisa_importance_agreement"]])
pisa_all_pv = fread(pisa_source_tables[["frozen_pisa_all_pv_performance"]])
pisa_all_pv_pfi = fread(pisa_source_tables[["frozen_pisa_all_pv_pfi"]])
pisa_loco = fread(pisa_source_tables[["frozen_pisa_loco_penalties"]])

.validate_sensitivity = function(source, study) {
  .evidence_require_columns(
    source,
    c(
      "learner_name", "focal_learner", "fraction_of_prespecified_tolerance_from_focal",
      "practical_reversal_fraction", "importance_weighted_reversal_fraction", "top_k_jaccard_practical"
    ),
    paste(study, "performance-explanation sensitivity")
  )
  if (nrow(source) < 2L || !"xgboost" %in% source$learner_name ||
      any(!is.finite(source$fraction_of_prespecified_tolerance_from_focal)) ||
      any(source$practical_reversal_fraction < 0 | source$practical_reversal_fraction > 1) ||
      any(source$top_k_jaccard_practical < 0 | source$top_k_jaccard_practical > 1)) {
    stop(study, " performance-explanation sensitivity failed its structural checks.", call. = FALSE)
  }
  source
}
shild_sensitivity = .validate_sensitivity(shild_sensitivity, "SHILD")
pisa_sensitivity = .validate_sensitivity(pisa_sensitivity, "PISA")
shild_sensitivity = .evidence_attach_practical_tie_denominators(
  shild_sensitivity,
  shild_importance_agreement,
  practical_tolerance = 0.0001
)
pisa_sensitivity = .evidence_attach_practical_tie_denominators(
  pisa_sensitivity,
  pisa_importance_agreement,
  practical_tolerance = 0.1
)
shild_sensitivity[, `:=`(study = "SHILD", claim_id = "SHILD-C3")]
pisa_claim_id = .analysis_pisa_claim_registry()[claim_id == "PISA-C1", claim_id]
if (length(pisa_claim_id) != 1L || nrow(pisa_frozen_claim) != 1L) {
  stop("The reconciled and frozen PISA claim records are not structurally unique.", call. = FALSE)
}
pisa_sensitivity[, `:=`(study = "PISA", claim_id = pisa_claim_id)]

pisa_pv_recurrence = .evidence_pisa_pv_importance_recurrence(pisa_all_pv_pfi)
pisa_boundaries = .evidence_pisa_boundary_source(pisa_all_pv, pisa_sensitivity, pisa_loco)

run_mode = data.table(
  mode = "publication",
  publication_settings = TRUE,
  manuscript_ready = parent_readiness$manuscript_ready,
  eligibility_blocker = parent_readiness$eligibility_blocker,
  parent_shild_structural_status = parent_readiness$parent_shild_structural_status,
  parent_shild_release_eligibility = parent_readiness$parent_shild_release_eligibility,
  parent_shild_eligibility_blockers = parent_readiness$parent_shild_eligibility_blockers,
  parent_pisa_structural_status = parent_readiness$parent_pisa_structural_status,
  parent_pisa_release_eligibility = parent_readiness$parent_pisa_release_eligibility,
  parent_pisa_eligibility_blockers = parent_readiness$parent_pisa_eligibility_blockers,
  seed = selection_seed,
  predictive_models_retrained = FALSE,
  pisa_models_retrained = FALSE,
  canonical_results_overwritten = FALSE,
  warning = if (parent_readiness$manuscript_ready) {
    "Interpret only within the recorded claim boundaries"
  } else {
    paste(
      "Structurally verified review delivery only; not eligible for manuscript use.",
      parent_readiness$eligibility_blocker
    )
  }
)

# ---- Output tables and private analysis objects ----

.write_table(run_mode, file.path(run$dirs$tables, "evidence_synthesis_run_mode.csv"))
.write_table(sampling$public, file.path(run$dirs$tables, "shild_g4_case_sampling_plan.csv"))
.write_table(replicates, file.path(run$dirs$tables, "shild_g4_local_fidelity_replicates.csv"))
.write_table(local_summary$summary, file.path(run$dirs$tables, "shild_g4_local_fidelity_summary.csv"))
.write_table(local_summary$cases, file.path(run$dirs$tables, "shild_g4_local_fidelity_cases.csv"))
.write_table(
  local_summary$bandwidth_cases,
  file.path(run$dirs$tables, "shild_g4_local_fidelity_bandwidth_cases.csv")
)
.write_table(
  coefficient_stability,
  file.path(run$dirs$tables, "shild_g4_coefficient_stability.csv")
)
.write_table(
  coefficient_stability_summary,
  file.path(run$dirs$tables, "shild_g4_coefficient_stability_summary.csv")
)
.write_table(
  local_summary$by_prediction_score,
  file.path(run$dirs$tables, "shild_g4_fidelity_by_prediction_score_decile.csv")
)
.write_table(
  local_summary$by_disagreement,
  file.path(run$dirs$tables, "shild_g4_fidelity_by_model_disagreement.csv")
)
.write_table(
  local_summary$by_support,
  file.path(run$dirs$tables, "shild_g4_fidelity_by_local_support.csv")
)
.write_table(
  method_sensitivity_cases,
  file.path(run$dirs$tables, "shild_g4_method_sensitivity_cases.csv")
)
.write_table(
  method_sensitivity_summary,
  file.path(run$dirs$tables, "shild_g4_method_sensitivity_summary.csv")
)
.write_table(randomness_grid, file.path(run$dirs$tables, "shild_g4_randomness_grid.csv"))
.write_table(randomness_cases, file.path(run$dirs$tables, "shild_g4_randomness_cases.csv"))
.write_table(randomness_summary, file.path(run$dirs$tables, "shild_g4_randomness_summary.csv"))
.write_table(
  pisa_pv_recurrence,
  file.path(run$dirs$tables, "pisa_all_pv_leading_importance_recurrence.csv")
)
.write_table(g4_conclusion, file.path(run$dirs$tables, "shild_g4_systematic_conclusion.csv"))
.write_table(
  shild_sensitivity,
  file.path(run$dirs$tables, "shild_performance_explanation_sensitivity.csv")
)
.write_table(
  pisa_sensitivity,
  file.path(run$dirs$tables, "pisa_performance_explanation_sensitivity.csv")
)
.write_table(pisa_boundaries, file.path(run$dirs$tables, "pisa_evidence_boundaries.csv"))
.write_table(
  sampling$private,
  file.path(run$dirs$objects, "shild_g4_case_sampling_plan_private.csv")
)

# ---- Publication figures ----

.analysis_reset_figure_registry()
.analysis_clear_figure_files(run$dirs$figures)

systematic_panels = .evidence_systematic_g4_panels(local_summary$cases, local_summary$bandwidth_cases)
systematic_group_text = .evidence_systematic_g4_group_text()
systematic_panel_specs = data.table(
  panel_name = names(systematic_panels),
  figure_id = paste0("shild_g4_systematic_local_audit__", names(systematic_panels)),
  panel_id = c(
    "a_apparent_vs_cross_fitted",
    "b_relative_fidelity_by_prediction",
    "c_absolute_fidelity_by_prediction",
    "d_locality_curve"
  ),
  panel_order = 1:4,
  panel_heading = c(
    "A  Apparent versus cross-fitted fidelity",
    "B  Relative fidelity by prediction-score decile",
    "C  Absolute fidelity by prediction-score decile",
    "D  Fidelity-locality curve"
  )
)
for (panel_index in seq_len(nrow(systematic_panel_specs))) {
  panel_spec = systematic_panel_specs[panel_index]
  .save_plot(
    systematic_panels[[panel_spec$panel_name]],
    file.path(run$dirs$figures, panel_spec$figure_id),
    width = 7,
    height = 5.2,
    metadata = list(
      study = "SHILD",
      figure_group_id = "shild_g4_systematic_local_audit",
      is_grouped_figure = TRUE,
      latex_group_id = "shild_g4_systematic_local_audit",
      panel_id = panel_spec$panel_id,
      panel_order = panel_spec$panel_order,
      panel_heading = panel_spec$panel_heading,
      group_title = systematic_group_text$title,
      group_subtitle = systematic_group_text$subtitle,
      group_caption = systematic_group_text$caption,
      generator_script = "analysis/evidence_synthesis.R",
      generator_function = ".evidence_systematic_g4_panels",
      latex_assembly_note = paste(
        "Grouped 2-row by 2-column figure; assemble the four component files in panel_order",
        "with LaTeX. Per-panel title and subtitle fields record the text removed from each component."
      )
    )
  )
}
sensitivity_source = rbindlist(list(
  shild_sensitivity,
  pisa_sensitivity
), fill = TRUE)
for (study_name in c("SHILD", "PISA")) {
  sensitivity_plot = .evidence_performance_explanation_plot(sensitivity_source, study_name)
  .save_plot(
    sensitivity_plot,
    file.path(run$dirs$figures, paste0("performance_explanation_sensitivity__", tolower(study_name))),
    width = 10.2,
    height = 2.65,
    metadata = list(
      study = study_name,
      figure_group_id = "performance_explanation_sensitivity",
      is_grouped_figure = TRUE,
      latex_group_id = "performance_explanation_sensitivity",
      group_title = "Near-equivalent prediction can coexist with explanation disagreement",
      group_subtitle = "Accepted alternatives relative to focal XGBoost",
      group_caption = paste(
        "The four columns report separate descriptive quantities; their alignment does not imply association.",
        paste(
          "Practical-pair labels give reversal count/comparable pairs and excluded practical ties;",
          "importance-weighted reversals use the study-specific prespecified sensitivity value."
        )
      ),
      generator_script = "analysis/evidence_synthesis.R",
      generator_function = ".evidence_performance_explanation_plot",
      latex_assembly_note = paste(
        "Grouped 2-row by 4-column figure; arrange SHILD components above PISA components.",
        "Use equal column widths so corresponding quantities and plotting regions align vertically."
      )
    ),
    min_component_width = 2.55,
    min_component_height = 2.65,
    align_component_widths = TRUE
  )
}
pisa_boundary_plot = .evidence_pisa_boundary_plot(pisa_boundaries)
.save_plot(
  pisa_boundary_plot,
  file.path(run$dirs$figures, "pisa_evidence_boundaries"),
  width = 10,
  height = 5.2,
  metadata = list(
    study = "PISA",
    generator_script = "analysis/evidence_synthesis.R",
    generator_function = ".evidence_pisa_boundary_plot"
  )
)

# ---- Figure catalog and LaTeX metadata ----

figure_catalog = data.table(
  figure_id = c(
    "shild_g4_systematic_local_audit",
    "performance_explanation_sensitivity",
    "pisa_evidence_boundaries"
  ),
  title = c(
    "SHILD held-out additive-surrogate fidelity",
    "Performance and explanation sensitivity in separate aligned columns",
    "PISA outcome, model, and setting evidence boundaries"
  ),
  role = c(
    "systematic local-fidelity diagnostic",
    "cross-study model-sensitivity diagnostic",
    "PISA evidence-boundary diagnostic"
  ),
  source_tables = c(
    paste(
      "tables/shild_g4_local_fidelity_cases.csv",
      "tables/shild_g4_local_fidelity_bandwidth_cases.csv",
      sep = " | "
    ),
    paste(
      "tables/shild_performance_explanation_sensitivity.csv",
      "tables/pisa_performance_explanation_sensitivity.csv",
      sep = " | "
    ),
    "tables/pisa_evidence_boundaries.csv"
  ),
  estimand_or_scope = c(
    paste(
      "Apparent and cross-fitted relative fidelity, absolute error, and locality sensitivity under the declared",
      "additive-surrogate design"
    ),
    paste(
      "Performance-tolerance consumption, practical-tie reversals, importance-weighted reversals, and top-five",
      "overlap among accepted models"
    ),
    "Three distinct descriptive sensitivity dimensions from the frozen PISA outputs"
  ),
  uncertainty_semantics = c(
    paste(
      "Seed variation includes neighborhood draws and surrogate partitions;",
      "no confidence intervals or tests"
    ),
    "Deterministic descriptive summaries; no confidence intervals or tests",
    "Descriptive across-PV and observed-country ranges; no confidence intervals or tests"
  ),
  privacy = "Shareable aggregate or pseudonymous diagnostic; no direct row identifier, outcome, or row prediction"
)
figure_catalog = .analysis_expand_figure_catalog(
  figure_catalog,
  study = "SHILD and PISA",
  generator_script = "analysis/evidence_synthesis.R",
  generator_function = "Evidence-synthesis publication figure construction block",
  run_dir = run$private_dir
)
figure_catalog[
  figure_group_id == "performance_explanation_sensitivity",
  `:=`(
    panel_id = paste(tolower(study), panel_id, sep = "_"),
    panel_order = panel_order + ifelse(study == "PISA", 4L, 0L),
    latex_assembly_note = paste(
      "Grouped 2-row by 4-column figure; arrange SHILD components above PISA components in panel_order.",
      "Use equal column widths so corresponding quantities and plotting regions align vertically."
    )
  )
]
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
  stop("The evidence-synthesis figure catalog does not cover every PDF/PNG figure pair exactly.", call. = FALSE)
}
catalog_sources = .analysis_catalog_source_paths(figure_catalog)
validate_expected_artifacts(run$private_dir, catalog_sources)
.write_table(figure_catalog, file.path(run$dirs$provenance, "figure_catalog.csv"))
.analysis_write_figure_latex_metadata(
  figure_catalog,
  file.path(run$dirs$provenance, "figure_latex_metadata.csv"),
  run_dir = run$private_dir
)

# ---- Narrative evidence summary ----

parent_release_state = if (parent_readiness$manuscript_ready) "ELIGIBLE" else "NOT_ELIGIBLE"
writeLines(c(
  "# Systematic local-fidelity and cross-study evidence synthesis",
  "",
  paste0("- Synthesis ID: `", run$run_id, "`"),
  paste0("- Frozen SHILD parent: `", basename(parent_shild), "`"),
  paste0("- Frozen PISA parent: `", basename(parent_pisa_root), "`"),
  paste0("- Parent-derived release state: `", parent_release_state, "`"),
  paste0(
    "- SHILD parent: structural `",
    parent_readiness$parent_shild_structural_status,
    "`, release `",
    parent_readiness$parent_shild_release_eligibility,
    "`"
  ),
  paste0(
    "- PISA parent: structural `",
    parent_readiness$parent_pisa_structural_status,
    "`, release `",
    parent_readiness$parent_pisa_release_eligibility,
    "`"
  ),
  if (parent_readiness$manuscript_ready) {
    "- Both frozen parent bundles satisfied their release gates at synthesis time."
  } else {
    paste0("- Review-delivery eligibility blocker: ", parent_readiness$eligibility_blocker)
  },
  "- Predictive models retrained: `FALSE`",
  "- Canonical results overwritten: `FALSE`",
  paste0("- Prespecified SHILD cases: `", primary_summary$n_cases, "`"),
  paste0("- Cases below the historical weighted-R-squared reference of 0.800: `",
    primary_summary$n_below_threshold, "` (`",
    sprintf("%.1f%%", 100 * primary_summary$proportion_below_threshold), "`)"),
  paste0("- Median case-median weighted R-squared: `", sprintf("%.4f", primary_summary$median), "`"),
  paste0("- Interquartile range: `", sprintf("%.4f", primary_summary$first_quartile), "\u2013",
    sprintf("%.4f", primary_summary$third_quartile), "`"),
  paste0("- Median case-level Monte Carlo SD across seeds: `",
    sprintf("%.4f", stats::median(local_summary$cases$monte_carlo_sd)), "`"),
  paste0("- Cases crossing the threshold across individual seeds: `", mixed_seed_cases, "` (`",
    sprintf("%.1f%%", 100 * mixed_seed_cases / primary_summary$n_cases), "`)"),
  paste0("- Median case-median weighted RMSE in prediction-score deciles 1 and 10: `",
    sprintf("%.4f", low_risk_rmse), "` and `", sprintf("%.4f", high_risk_rmse), "`"),
  paste0("- Secondary method variants evaluated on the common 100-case grid: `",
    nrow(method_sensitivity_summary), "`"),
  paste0(
    "- Crossed-seed weighted-R-squared marginal SD medians (perturbation; cross-fit): `",
    sprintf("%.4f", randomness_r2$median_perturbation_marginal_sd), "`; `",
    sprintf("%.4f", randomness_r2$median_crossfit_marginal_sd), "`"
  ),
  paste0("- Crossed-seed weighted-R-squared interaction RMS median: `",
    sprintf("%.4f", randomness_r2$median_interaction_rms), "`"),
  "",
  "## Interpretation boundary",
  "",
  paste(
    "The systematic G4 result describes fidelity to frozen fold-specific XGBoost predictions in the package's",
    "declared local neighborhoods. It does not establish causal explanation, actionable individual recourse,",
    "joint empirical support, model-fit uncertainty, decision readiness, or out-of-setting validity."
  ),
  paste(
    "Weighted R-squared is relative to the model-prediction variance inside each synthetic neighborhood and is",
    "not an absolute-error measure. Prediction-score-region patterns must therefore be read together with weighted",
    "RMSE. The crossed-seed decomposition describes only the declared computational grid."
  ),
  "",
  paste(
    "The PISA boundary and cross-study model-sensitivity artifacts are deterministic derivations from the",
    "manifest-verified aggregate tables. No PISA model or explanation was recomputed."
  ),
  ""
), file.path(run$private_dir, "EVIDENCE_SYNTHESIS.md"), useBytes = TRUE)

# ---- Parent integrity and provenance records ----

protocol_copied = file.copy(
  protocol_path,
  file.path(run$dirs$provenance, "protocol.yml"),
  overwrite = TRUE
)
if (!isTRUE(protocol_copied)) stop("Could not copy the frozen G4 protocol.", call. = FALSE)
parent_audit_copied = file.copy(
  c(parent_shild_audit_summary_path, parent_pisa_audit_summary_path),
  c(
    file.path(run$dirs$provenance, "frozen_shild_output_audit_summary.csv"),
    file.path(run$dirs$provenance, "frozen_pisa_output_audit_summary.csv")
  ),
  overwrite = TRUE
)
if (length(parent_audit_copied) != 2L || !all(parent_audit_copied)) {
  stop("Could not copy both frozen parent output-audit summaries.", call. = FALSE)
}

model_hash_after = vapply(
  oof$models,
  .evidence_frozen_model_hash,
  character(1L)
)
parent_file_hash_after = digest::digest(file = parent_result_path, algo = "sha256")
if (!identical(model_hash_before, model_hash_after) ||
    !identical(parent_file_hash_before, parent_file_hash_after)) {
  stop("A retained SHILD model state or parent result file changed during the synthesis.", call. = FALSE)
}
parent_integrity = data.table(
  check = c(
    "private parent manifest verified",
    "SHILD sanitized parent manifest verified",
    "PISA sanitized parent manifest verified",
    "SHILD parent output-audit summary validated",
    "PISA parent output-audit summary validated",
    "focal OOF replay maximum absolute difference",
    "frozen parent result SHA-256 unchanged",
    "frozen fold-model substantive-state hashes unchanged",
    "frozen local-fidelity helper source hashes match",
    "predictive-model training invoked",
    "PISA model or explanation recomputation invoked"
  ),
  value = c(
    "TRUE",
    "TRUE",
    "TRUE",
    paste0(
      "structural=",
      parent_readiness$parent_shild_structural_status,
      "; release=",
      parent_readiness$parent_shild_release_eligibility
    ),
    paste0(
      "structural=",
      parent_readiness$parent_pisa_structural_status,
      "; release=",
      parent_readiness$parent_pisa_release_eligibility
    ),
    format(replay_delta, scientific = TRUE),
    "TRUE",
    "TRUE",
    "TRUE",
    "FALSE",
    "FALSE"
  ),
  status = "pass"
)
parent_integrity[check == "frozen fold-model substantive-state hashes unchanged", value := paste(
  "TRUE; prediction-time counters excluded because inference updates runtime metadata only"
)]
.write_table(parent_integrity, file.path(run$dirs$provenance, "parent_artifact_integrity.csv"))
frozen_helper_hash_check[, status := fifelse(parent_sha256 == current_sha256, "match", "mismatch")]
.write_table(
  frozen_helper_hash_check,
  file.path(run$dirs$provenance, "frozen_helper_source_hashes.csv")
)
.assert_parent_artifact_snapshots("evidence-synthesis provenance finalization")
input_records = rbindlist(lapply(names(analysis_inputs), function(name) {
  .input_record(analysis_inputs[[name]], name)
}))
.write_table(input_records, file.path(run$dirs$provenance, "input_checksums.csv"))
.write_table(
  package_source$manifest,
  file.path(run$dirs$provenance, "package_source_manifest.csv")
)
.write_table(
  data.table(tree_hash = package_source$tree_hash, algorithm = "SHA-256"),
  file.path(run$dirs$provenance, "package_source_tree_hash.csv")
)
writeLines(capture.output(sessionInfo()), file.path(run$dirs$provenance, "sessionInfo.txt"))
write_run_provenance(
  run,
  source_files = analysis_sources,
  input_files = analysis_inputs,
  extra = list(
    synthesis_signature = synthesis_signature,
    package_source_tree_hash = package_source$tree_hash,
    parent_shild_run = basename(parent_shild),
    parent_pisa_run = basename(parent_pisa_root),
    parent_shild_structural_status = parent_readiness$parent_shild_structural_status,
    parent_shild_release_eligibility = parent_readiness$parent_shild_release_eligibility,
    parent_pisa_structural_status = parent_readiness$parent_pisa_structural_status,
    parent_pisa_release_eligibility = parent_readiness$parent_pisa_release_eligibility,
    manuscript_ready = parent_readiness$manuscript_ready,
    eligibility_blocker = parent_readiness$eligibility_blocker,
    parent_shild_result_sha256 = parent_file_hash_before,
    selection_seed = selection_seed,
    perturbation_seed_base = perturbation_seed_base,
    perturbation_seed_stride = perturbation_seed_stride,
    n_cases = n_cases,
    n_perturbation_replicates = n_replicates,
    n_perturb = n_perturb,
    primary_kernel_width = primary_kernel_width,
    sensitivity_kernel_widths = paste(setdiff(kernel_widths, primary_kernel_width), collapse = " | "),
    historical_weighted_r2_reference = historical_weighted_r2_reference,
    secondary_sensitivity_cases = nrow(secondary_cases),
    secondary_sensitivity_replicates = secondary_replicates,
    randomness_seeds_per_source = randomness_seeds_per_source,
    randomness_grid_cells = nrow(randomness_grid),
    predictive_models_retrained = FALSE,
    pisa_model_or_explanation_recomputation_invoked = FALSE,
    canonical_results_overwritten = FALSE,
    worker_processes = workers,
    checkpoint_directory = cache_dir
  )
)

# ---- Completion checks ----

.assert_package_source_snapshot(package_source, project_root, "the evidence synthesis")
write_run_checkpoint(
  run,
  "evidence_synthesis",
  "completed",
  list(
    n_cases = primary_summary$n_cases,
    proportion_below_threshold = primary_summary$proportion_below_threshold,
    predictive_models_retrained = FALSE,
    manuscript_ready = parent_readiness$manuscript_ready,
    parent_shild_release_eligibility = parent_readiness$parent_shild_release_eligibility,
    parent_pisa_release_eligibility = parent_readiness$parent_pisa_release_eligibility
  )
)
stop_run_logging(run_logging)

# ---- Sanitized review-delivery finalization ----

upload_dir = .finalize_study_output(
  run = run,
  upload_expected = upload_expected,
  package_source = package_source,
  project_root = project_root,
  analysis_checkpoint = "evidence_synthesis",
  finalization_checkpoint = "evidence_synthesis_finalization",
  label = "evidence synthesis",
  require_release_eligibility = FALSE,
  before_publish = function() {
    .assert_parent_artifact_snapshots("evidence-synthesis upload release")
  }
)

cat("Evidence-synthesis private run: ", run$private_dir, "\n", sep = "")
cat("Evidence-synthesis sanitized upload: ", upload_dir, "\n", sep = "")
