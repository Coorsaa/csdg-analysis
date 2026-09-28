#!/usr/bin/env Rscript

# Refit the fixed PISA PV1 learner after training-size-matched country exclusions.
#
# This focused runner starts from the frozen all-PV out-of-fold object. It does not
# require rerunning unrelated diagnostics, and it checkpoints one aggregate-only
# file per country so an interrupted run can be resumed without exposing row-level
# predictions or sampled row identifiers.

suppressPackageStartupMessages({
  library(data.table)
  library(mlr3)
})

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
runner_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  file.path("analysis", "pisa", "run_pv1_matched_controls.R")
}
runner_path = normalizePath(runner_path, winslash = "/", mustWork = TRUE)
source_identity_path = file.path(dirname(dirname(runner_path)), "shared", "source_identity.R")
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package `digest` is required to record reproducibility identities.", call. = FALSE)
}
source(source_identity_path, local = FALSE)

input_path = Sys.getenv("CSDG_FOCAL_OOF_RDS", unset = "")
output_directory = Sys.getenv("CSDG_MATCHED_OUTPUT_DIR", unset = "")
package_source = Sys.getenv("CSDG_PACKAGE_SOURCE", unset = normalizePath(".", mustWork = TRUE))
repetitions = as.integer(Sys.getenv("CSDG_MATCHED_REPETITIONS", unset = "50"))
workers = as.integer(Sys.getenv("CSDG_MATCHED_WORKERS", unset = "10"))
base_seed = as.integer(Sys.getenv("CSDG_ANALYSIS_SEED", unset = "20260714")) + 4000000L

if (!nzchar(input_path) || !file.exists(input_path)) {
  stop("Set CSDG_FOCAL_OOF_RDS to the frozen PISA all-PV focal OOF object.", call. = FALSE)
}
if (!nzchar(output_directory)) {
  stop("Set CSDG_MATCHED_OUTPUT_DIR to a writable output directory.", call. = FALSE)
}
if (is.na(repetitions) || repetitions < 2L) {
  stop("CSDG_MATCHED_REPETITIONS must be an integer of at least two.", call. = FALSE)
}
if (is.na(workers) || workers < 1L) {
  stop("CSDG_MATCHED_WORKERS must be a positive integer.", call. = FALSE)
}

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
checkpoint_directory = file.path(output_directory, "country_checkpoints")
dir.create(checkpoint_directory, recursive = TRUE, showWarnings = FALSE)

input_sha256 = .analysis_sha256(input_path)
runner_sha256 = .analysis_declared_runner_sha256(runner_path)
metadata_writer_runner_sha256 = .analysis_sha256(runner_path)
identity_helper_sha256 = .analysis_sha256(source_identity_path)
package_identity = .analysis_package_source_identity(package_source)
computation_source_sha256 = .analysis_computation_source_sha256(
  runner_sha256,
  package_identity$package_source_tree_sha256
)

library(mlr3autoiml)
all_pv_oof = readRDS(input_path)
if (!is.list(all_pv_oof) || !"PV1READ" %in% names(all_pv_oof)) {
  stop("The frozen OOF object must contain a named PV1READ element.", call. = FALSE)
}
focal_oof = all_pv_oof[["PV1READ"]]
if (!inherits(focal_oof, "CSDGResample")) {
  stop("PV1READ must be a CSDGResample object.", call. = FALSE)
}

task = focal_oof$task
row_ids = task$row_ids
country = as.character(task$data(rows = row_ids, cols = "audit_country")$audit_country)
if (length(country) != length(row_ids) || anyNA(country)) {
  stop("The audit-country vector is not aligned with the frozen task rows.", call. = FALSE)
}
countries = sort(unique(country))
if (length(countries) != 32L) {
  stop("The frozen PISA task must contain exactly 32 observed countries.", call. = FALSE)
}

predictions = as.data.table(focal_oof$predictions)
if (nrow(predictions) != length(row_ids) || anyDuplicated(predictions$row_id)) {
  stop("The frozen PV1 object must contain one OOF prediction per task row.", call. = FALSE)
}
reference_by_row = setNames(predictions$response, as.character(predictions$row_id))
reference_train_n = as.integer(round(stats::median(vapply(focal_oof$train_sets, length, integer(1L)))))

fit_country = function(country_index) {
  held_out = countries[[country_index]]
  checkpoint_path = file.path(checkpoint_directory, paste0("country_", held_out, ".csv"))
  if (file.exists(checkpoint_path)) {
    existing = fread(checkpoint_path)
    valid = nrow(existing) == repetitions &&
      identical(sort(existing$matched_repetition), seq_len(repetitions)) &&
      all(existing$held_out_group == held_out)
    if (valid) {
      return(checkpoint_path)
    }
  }

  assessment = row_ids[country == held_out]
  candidates = row_ids[country != held_out]
  if (length(candidates) < reference_train_n) {
    stop("Too few candidate rows after holding out country ", held_out, ".", call. = FALSE)
  }
  truth = task$truth(rows = assessment)
  reference = unname(reference_by_row[as.character(assessment)])
  reference_loss = sqrt(mean((truth - reference)^2))
  rows = vector("list", repetitions)

  for (replicate in seq_len(repetitions)) {
    set.seed(base_seed + country_index * 10000L + replicate)
    analysis_rows = sample(candidates, reference_train_n, replace = FALSE)
    learner = focal_oof$learner$clone(deep = TRUE)
    thread_parameter = grep("nthread$", learner$param_set$ids(), value = TRUE)
    if (length(thread_parameter) == 1L) {
      learner$param_set$set_values(.values = setNames(list(1L), thread_parameter))
    }
    learner$train(task, row_ids = analysis_rows)
    prediction = learner$predict(task, row_ids = assessment)$response
    excluded_loss = sqrt(mean((truth - prediction)^2))
    rows[[replicate]] = data.table(
      held_out_group = held_out,
      matched_repetition = as.integer(replicate),
      n_reference_analysis = reference_train_n,
      n_candidate_other_setting = length(candidates),
      n_matched_exclusion_analysis = length(analysis_rows),
      n_assessment = length(assessment),
      loss = "rmse",
      reference_oof_loss = reference_loss,
      matched_exclusion_loss = excluded_loss,
      matched_exclusion_penalty = excluded_loss - reference_loss
    )
  }

  result = rbindlist(rows)
  temporary_path = paste0(checkpoint_path, ".tmp-", Sys.getpid())
  fwrite(result, temporary_path)
  if (!file.rename(temporary_path, checkpoint_path)) {
    stop("Could not finalize checkpoint for country ", held_out, ".", call. = FALSE)
  }
  message(sprintf("Completed %s (%d matched refits).", held_out, repetitions))
  checkpoint_path
}

checkpoint_paths = if (.Platform$OS.type == "unix" && workers > 1L) {
  unlist(parallel::mclapply(
    seq_along(countries),
    fit_country,
    mc.cores = min(workers, length(countries)),
    mc.preschedule = FALSE
  ))
} else {
  vapply(seq_along(countries), fit_country, character(1L))
}

if (length(checkpoint_paths) != length(countries) || !all(file.exists(checkpoint_paths))) {
  stop("At least one country checkpoint did not complete.", call. = FALSE)
}
per_repetition = rbindlist(lapply(checkpoint_paths, fread))
setorder(per_repetition, held_out_group, matched_repetition)
if (nrow(per_repetition) != length(countries) * repetitions ||
    uniqueN(per_repetition, by = c("held_out_group", "matched_repetition")) != nrow(per_repetition)) {
  stop("The combined matched-control table is incomplete or contains duplicate rows.", call. = FALSE)
}

summary = per_repetition[, .(
  n_matched_repetitions = .N,
  n_assessment = unique(n_assessment),
  reference_oof_loss = unique(reference_oof_loss),
  mean_matched_exclusion_loss = mean(matched_exclusion_loss),
  standard_deviation_matched_exclusion_loss = stats::sd(matched_exclusion_loss),
  minimum_matched_exclusion_loss = min(matched_exclusion_loss),
  maximum_matched_exclusion_loss = max(matched_exclusion_loss),
  mean_matched_exclusion_penalty = mean(matched_exclusion_penalty),
  standard_deviation_matched_exclusion_penalty = stats::sd(matched_exclusion_penalty),
  minimum_matched_exclusion_penalty = min(matched_exclusion_penalty),
  maximum_matched_exclusion_penalty = max(matched_exclusion_penalty)
), by = .(held_out_group, loss)]

fwrite(
  per_repetition,
  file.path(output_directory, "pisa_pv1_matched_country_exclusion_repetitions.csv")
)
fwrite(
  summary,
  file.path(output_directory, "pisa_pv1_matched_country_exclusion_summary.csv")
)

metadata = data.table(
  field = c(
    "outcome", "observed_countries", "repetitions_per_country", "total_refits", "reference_analysis_rows",
    "base_seed", "sampling", "assessment", "stored_models", "stored_row_level_predictions",
    "focal_oof_input_sha256", "computation_runner_sha256", "metadata_writer_runner_sha256",
    "source_identity_helper_sha256",
    "package_version", "package_source_tree_sha256", "computation_source_sha256", "package_source_scope"
  ),
  value = c(
    "PV1READ", length(countries), repetitions, length(countries) * repetitions, reference_train_n,
    base_seed, "simple random sample from all other observed countries without replacement",
    "all rows from the held-out observed country", "FALSE", "FALSE", input_sha256, runner_sha256,
    metadata_writer_runner_sha256, identity_helper_sha256, package_identity$package_version,
    package_identity$package_source_tree_sha256, computation_source_sha256, package_identity$package_source_scope
  )
)
fwrite(metadata, file.path(output_directory, "pisa_pv1_matched_country_exclusion_metadata.csv"))

message(sprintf(
  "Assembled %d training-size-matched PV1 country-exclusion records (%d countries x %d repetitions).",
  nrow(per_repetition),
  length(countries),
  repetitions
))
