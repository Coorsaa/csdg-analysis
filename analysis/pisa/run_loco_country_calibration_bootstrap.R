#!/usr/bin/env Rscript

# Add school-cluster bootstrap intervals to stored observed-country LOCO predictions.
#
# This calculation conditions on the already fitted country-exclusion models and
# their stored predictions. It resamples schools within each held-out country and
# does not represent model-training, plausible-value, survey-design, or new-country
# uncertainty. Only aggregate calibration estimates are written.

suppressPackageStartupMessages(library(data.table))

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
runner_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  file.path("analysis", "pisa", "run_loco_country_calibration_bootstrap.R")
}
runner_path = normalizePath(runner_path, winslash = "/", mustWork = TRUE)
source_identity_path = file.path(dirname(dirname(runner_path)), "shared", "source_identity.R")
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package `digest` is required to record reproducibility identities.", call. = FALSE)
}
source(source_identity_path, local = FALSE)

prediction_path = Sys.getenv("CSDG_LOCO_PREDICTIONS_RDS", unset = "")
focal_oof_path = Sys.getenv("CSDG_FOCAL_OOF_RDS", unset = "")
output_directory = Sys.getenv("CSDG_LOCO_CALIBRATION_OUTPUT_DIR", unset = "")
repetitions = as.integer(Sys.getenv("CSDG_LOCO_CALIBRATION_REPETITIONS", unset = "500"))
workers = as.integer(Sys.getenv("CSDG_LOCO_CALIBRATION_WORKERS", unset = "10"))
confidence = as.numeric(Sys.getenv("CSDG_LOCO_CALIBRATION_CONFIDENCE", unset = "0.95"))
base_seed = as.integer(Sys.getenv("CSDG_ANALYSIS_SEED", unset = "20260714")) + 5000000L

for (path in c(prediction_path, focal_oof_path)) {
  if (!nzchar(path) || !file.exists(path)) {
    stop("Set both private input RDS paths to existing frozen objects.", call. = FALSE)
  }
}
if (!nzchar(output_directory)) {
  stop("Set CSDG_LOCO_CALIBRATION_OUTPUT_DIR to a writable output directory.", call. = FALSE)
}
if (is.na(repetitions) || repetitions < 20L) {
  stop("CSDG_LOCO_CALIBRATION_REPETITIONS must be at least 20.", call. = FALSE)
}
if (is.na(workers) || workers < 1L) {
  stop("CSDG_LOCO_CALIBRATION_WORKERS must be positive.", call. = FALSE)
}
if (!is.finite(confidence) || confidence <= 0.5 || confidence >= 1) {
  stop("CSDG_LOCO_CALIBRATION_CONFIDENCE must lie strictly between .5 and 1.", call. = FALSE)
}

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
checkpoint_directory = file.path(output_directory, "country_pv_checkpoints")
dir.create(checkpoint_directory, recursive = TRUE, showWarnings = FALSE)

prediction_input_sha256 = .analysis_sha256(prediction_path)
focal_oof_input_sha256 = .analysis_sha256(focal_oof_path)
runner_sha256 = .analysis_declared_runner_sha256(runner_path)
metadata_writer_runner_sha256 = .analysis_sha256(runner_path)
identity_helper_sha256 = .analysis_sha256(source_identity_path)
computation_source_sha256 = .analysis_computation_source_sha256(
  runner_sha256
)

predictions = as.data.table(readRDS(prediction_path))
required = c("row_id", "truth", "response", "held_out_group", "outcome_id")
if (length(setdiff(required, names(predictions))) ||
    uniqueN(predictions, by = c("row_id", "outcome_id")) != nrow(predictions)) {
  stop("LOCO predictions must contain one row per row ID and plausible value.", call. = FALSE)
}

all_pv_oof = readRDS(focal_oof_path)
if (!is.list(all_pv_oof) || !"PV1READ" %in% names(all_pv_oof)) {
  stop("The frozen focal OOF object must contain PV1READ.", call. = FALSE)
}
task = all_pv_oof[["PV1READ"]]$task
audit = as.data.table(task$data(rows = task$row_ids, cols = c("audit_school", "audit_country")))
audit[, row_id := task$row_ids]
audit[, `:=`(
  audit_school = as.character(audit_school),
  audit_country = as.character(audit_country)
)]
predictions = audit[predictions, on = "row_id"]
if (anyNA(predictions$audit_school) || any(predictions$audit_country != predictions$held_out_group)) {
  stop("School/country audit fields are not aligned with the stored LOCO predictions.", call. = FALSE)
}

outcomes = paste0("PV", seq_len(10L), "READ")
countries = sort(unique(predictions$held_out_group))
if (!identical(sort(unique(predictions$outcome_id)), sort(outcomes)) || length(countries) != 32L) {
  stop("LOCO predictions must cover ten plausible values and 32 observed countries.", call. = FALSE)
}

calibration_values = function(truth, response) {
  prediction_variance = stats::var(response)
  slope = if (is.finite(prediction_variance) && prediction_variance > 0) {
    stats::cov(response, truth) / prediction_variance
  } else {
    NA_real_
  }
  c(
    calibration_in_the_large = mean(truth - response),
    calibration_intercept = mean(truth) - slope * mean(response),
    calibration_slope = slope
  )
}

design = CJ(outcome_id = outcomes, held_out_group = countries)
design[, `:=`(
  outcome_index = match(outcome_id, outcomes),
  country_index = match(held_out_group, countries),
  job_index = .I
)]

run_job = function(job_index) {
  job = design[job_index]
  checkpoint_path = file.path(
    checkpoint_directory,
    paste0(job$outcome_id, "_", job$held_out_group, ".csv")
  )
  if (file.exists(checkpoint_path)) {
    existing = fread(checkpoint_path)
    if (nrow(existing) == repetitions && identical(existing$bootstrap_replicate, seq_len(repetitions))) {
      return(checkpoint_path)
    }
  }

  data = predictions[
    outcome_id == job$outcome_id & held_out_group == job$held_out_group,
    .(truth, response, audit_school)
  ]
  if (nrow(data) != 1500L || uniqueN(data$audit_school) < 2L) {
    stop("Unexpected country/PV assessment structure for job ", job_index, ".", call. = FALSE)
  }
  rows_by_school = split(seq_len(nrow(data)), data$audit_school)
  school_ids = names(rows_by_school)
  set.seed(base_seed + job$outcome_index * 100000L + job$country_index * 1000L)
  rows = vector("list", repetitions)
  for (replicate in seq_len(repetitions)) {
    sampled_school = sample(school_ids, length(school_ids), replace = TRUE)
    index = unlist(rows_by_school[sampled_school], use.names = FALSE)
    values = calibration_values(data$truth[index], data$response[index])
    rows[[replicate]] = data.table(
      outcome_id = job$outcome_id,
      held_out_group = job$held_out_group,
      bootstrap_replicate = as.integer(replicate),
      n_sampled_rows = length(index),
      calibration_in_the_large = values[["calibration_in_the_large"]],
      calibration_intercept = values[["calibration_intercept"]],
      calibration_slope = values[["calibration_slope"]]
    )
  }
  result = rbindlist(rows)
  temporary_path = paste0(checkpoint_path, ".tmp-", Sys.getpid())
  fwrite(result, temporary_path)
  if (!file.rename(temporary_path, checkpoint_path)) {
    stop("Could not finalize calibration checkpoint for job ", job_index, ".", call. = FALSE)
  }
  checkpoint_path
}

checkpoint_paths = if (.Platform$OS.type == "unix" && workers > 1L) {
  unlist(parallel::mclapply(
    design$job_index,
    run_job,
    mc.cores = min(workers, nrow(design)),
    mc.preschedule = FALSE
  ))
} else {
  vapply(design$job_index, run_job, character(1L))
}
if (length(checkpoint_paths) != nrow(design) || !all(file.exists(checkpoint_paths))) {
  stop("At least one country/PV calibration bootstrap did not complete.", call. = FALSE)
}

bootstrap = rbindlist(lapply(checkpoint_paths, fread))
setorder(bootstrap, outcome_id, held_out_group, bootstrap_replicate)
if (nrow(bootstrap) != nrow(design) * repetitions ||
    uniqueN(bootstrap, by = c("outcome_id", "held_out_group", "bootstrap_replicate")) != nrow(bootstrap)) {
  stop("The combined country calibration bootstrap is incomplete or duplicated.", call. = FALSE)
}

alpha = (1 - confidence) / 2
point = predictions[, as.list(calibration_values(truth, response)), by = .(outcome_id, held_out_group)]
intervals = melt(
  bootstrap,
  id.vars = c("outcome_id", "held_out_group", "bootstrap_replicate", "n_sampled_rows"),
  measure.vars = c("calibration_in_the_large", "calibration_intercept", "calibration_slope"),
  variable.name = "estimand",
  value.name = "bootstrap_estimate"
)[, .(
  bootstrap_repetitions = .N,
  lower = as.numeric(stats::quantile(bootstrap_estimate, alpha, na.rm = TRUE, names = FALSE, type = 8)),
  upper = as.numeric(stats::quantile(bootstrap_estimate, 1 - alpha, na.rm = TRUE, names = FALSE, type = 8)),
  minimum = min(bootstrap_estimate, na.rm = TRUE),
  maximum = max(bootstrap_estimate, na.rm = TRUE)
), by = .(outcome_id, held_out_group, estimand)]
point_long = melt(
  point,
  id.vars = c("outcome_id", "held_out_group"),
  variable.name = "estimand",
  value.name = "estimate"
)
summary = point_long[intervals, on = c("outcome_id", "held_out_group", "estimand")]
assessment_structure = predictions[, .(
  n_assessment = .N,
  n_schools = uniqueN(audit_school)
), by = .(outcome_id, held_out_group)]
summary = assessment_structure[summary, on = c("outcome_id", "held_out_group")]
summary[, `:=`(
  confidence = confidence,
  bootstrap_unit = "school within held-out observed country",
  prediction_conditioning = "stored LOCO predictions held fixed",
  pipeline_refitted = FALSE,
  descriptive_only = TRUE,
  omitted_uncertainty = paste(
    "model training; plausible-value generation and between-PV uncertainty; survey design;",
    "new-country and future-cycle variation"
  )
)]
setorder(summary, outcome_id, held_out_group, estimand)

fwrite(bootstrap, file.path(output_directory, "pisa_loco_country_calibration_bootstrap_replicates.csv"))
fwrite(summary, file.path(output_directory, "pisa_loco_country_calibration_bootstrap_intervals.csv"))

metadata = data.table(
  field = c(
    "outcomes", "observed_countries", "assessment_rows_per_country_pv", "schools_total",
    "bootstrap_repetitions", "confidence", "base_seed", "bootstrap_unit", "prediction_conditioning",
    "pipeline_refitted", "descriptive_only", "loco_predictions_input_sha256", "focal_oof_input_sha256",
    "computation_runner_sha256", "metadata_writer_runner_sha256", "source_identity_helper_sha256",
    "computation_source_sha256", "computation_source_scope"
  ),
  value = c(
    length(outcomes), length(countries), 1500L, uniqueN(predictions$audit_school), repetitions, confidence,
    base_seed, "school within held-out observed country", "stored LOCO predictions held fixed", FALSE, TRUE,
    prediction_input_sha256, focal_oof_input_sha256, runner_sha256, metadata_writer_runner_sha256,
    identity_helper_sha256, computation_source_sha256,
    "focused runner and source-identity helper; private input objects independently hashed"
  )
)
fwrite(metadata, file.path(output_directory, "pisa_loco_country_calibration_bootstrap_metadata.csv"))

message(sprintf(
  "Assembled %d school-bootstrap replicates for %d country/PV calibration records.",
  nrow(bootstrap),
  nrow(design)
))
