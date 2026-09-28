#!/usr/bin/env Rscript

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/tests/test_shild_preprocessing.R"
}
analysis_dir = dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
project_root = normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("The SHILD preprocessing self-test requires data.table.", call. = FALSE)
}
suppressPackageStartupMessages(library(data.table))
source(file.path(analysis_dir, "shared", "study_helpers.R"), local = FALSE)
source(file.path(analysis_dir, "shild", "prepare_data.R"), local = FALSE)

raw_path = file.path(project_root, "data", "SHILD", "archive", "Danish_SHILD_draft_data.csv")
if (!file.exists(raw_path)) {
  cat(paste(
    "SHILD raw-preprocessing self-test skipped: the protected raw file is not included;",
    "the responsible-coauthor verification record does not claim an independent raw-data rerun.\n"
  ))
  quit(save = "no", status = 0L)
}
raw = fread(raw_path)
prepared = .prepare_shild_raw(raw)
analytic = prepared$data
missingness_probe = .missingness_table(
  data.table(first = c(1, NA), second = c(NA, NA)),
  c("first", "second"),
  "probe"
)

stopifnot(
  nrow(raw) == 18957L,
  nrow(analytic) == 16971L,
  identical(prepared$sample_flow$n, c(18957L, 16971L, 16971L)),
  identical(prepared$sample_flow$exclusion_from_previous, c(0L, 1986L, 0L)),
  sum(analytic$life_satisfaction == "content") == 15943L,
  sum(analytic$life_satisfaction == "discontent") == 1028L,
  identical(range(analytic$age), c(16L, 64L)),
  all(is.na(analytic$F15) | data.table::between(analytic$F15, 0, 10)),
  prepared$sentinel_audit[variable == "F15" & code == 98, n_recoded] == 1L,
  prepared$sentinel_audit[variable == "F15" & code == 99, n_recoded] == 27L,
  prepared$domain_value_audit[variable == "E1" & flagged_range == "value = 0 cm", n_flagged] == 1L,
  prepared$domain_value_audit[variable == "E2" & flagged_range == "value = 0 kg", n_flagged] == 5L,
  prepared$domain_value_audit[variable == "E1" & flagged_range == "0 < value < 100 cm", n_recoded] == 34L,
  prepared$domain_value_audit[variable == "E2" & flagged_range == "0 < value < 20 kg", n_recoded] == 1L,
  sum(is.na(analytic$E1)) == 43L,
  sum(is.na(analytic$E2)) == 56L,
  all(is.na(analytic$E1) | analytic$E1 >= 100),
  all(is.na(analytic$E2) | analytic$E2 >= 20),
  identical(missingness_probe$variable, c("second", "first")),
  identical(missingness_probe$n_missing, c(2L, 1L)),
  all(missingness_probe$stage == "probe"),
  identical(
    unique(prepared$missingness_before$stage),
    "before documented categorical, sentinel, and implausible domain-value recoding"
  ),
  identical(
    unique(prepared$missingness_after$stage),
    "after documented categorical, sentinel, and implausible domain-value recoding"
  ),
  nrow(prepared$variable_recode_spec) == 28L,
  all(nzchar(prepared$variable_recode_spec$transformation))
)

target_audit = prepared$value_recode_audit[variable == "A1"]
stopifnot(
  target_audit[source_code == "8", action] == "exclude non-substantive outcome",
  target_audit[source_code == "3", n] == 1975L,
  target_audit[source_code == "8", n] == 1L,
  target_audit[source_code == "9", n] == 10L
)

snapshot_root = tempfile("source-snapshot-")
dir.create(file.path(snapshot_root, "R"), recursive = TRUE)
dir.create(file.path(snapshot_root, "tests", "testthat"), recursive = TRUE)
writeLines("Package: snapshot", file.path(snapshot_root, "DESCRIPTION"))
writeLines("exportPattern(\"^[^.]\")", file.path(snapshot_root, "NAMESPACE"))
writeLines("value = 1", file.path(snapshot_root, "R", "source.R"))
writeLines("generated plot", file.path(snapshot_root, "tests", "testthat", "Rplots.pdf"))
snapshot = .package_source_manifest(snapshot_root)
stopifnot(!"tests/testthat/Rplots.pdf" %in% snapshot$manifest$path)
.assert_package_source_snapshot(snapshot, snapshot_root, "self-test")
writeLines("value = 2", file.path(snapshot_root, "R", "source.R"))
snapshot_error = tryCatch(
  {
    .assert_package_source_snapshot(snapshot, snapshot_root, "self-test")
    NULL
  },
  error = function(error) error
)
stopifnot(
  inherits(snapshot_error, "error"),
  grepl("Package source changed during self-test", conditionMessage(snapshot_error), fixed = TRUE),
  grepl("R/source.R", conditionMessage(snapshot_error), fixed = TRUE)
)
unlink(snapshot_root, recursive = TRUE, force = TRUE)

cat("SHILD raw-preprocessing self-test passed\n")
