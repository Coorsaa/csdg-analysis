#!/usr/bin/env Rscript
# Converts the public SHILD 2012 release on Dryad (Bengtsson & Datta Gupta, 2018;
# https://doi.org/10.5061/dryad.qd2nj) into the input files that the SHILD scripts read.
#
# Usage, from the repository root:
#   Rscript analysis/shild/prepare_dryad_input.R <path to SHILD2012_cens_eng_16-64.dta> [CSV to compare]
#
# Writes data/SHILD/archive/Danish_SHILD_draft_data.csv (the release without value labels, in the variable order of
# the dictionary) and copies analysis/shild/inputs/Danish_SHILD_dictionnary.csv next to it. If a second path is given,
# the values of the written file are compared with the values of that file.
args = commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:2) {
  stop("Usage: Rscript analysis/shild/prepare_dryad_input.R <release .dta> [CSV to compare]")
}
if (!requireNamespace("haven", quietly = TRUE)) stop("Install the R package haven.")
dictionary_path = file.path("analysis", "shild", "inputs", "Danish_SHILD_dictionnary.csv")
if (!file.exists(dictionary_path)) stop("Run the script from the root of the analysis repository.")
dictionary = utils::read.csv(dictionary_path, stringsAsFactors = FALSE, check.names = FALSE)
release = as.data.frame(haven::zap_labels(haven::zap_formats(haven::read_dta(args[[1]]))))
missing = setdiff(dictionary$variable, names(release))
if (length(missing)) {
  stop("Variables of the dictionary missing from the release: ", paste(missing, collapse = ", "))
}
release = release[, dictionary$variable, drop = FALSE]
out_dir = file.path("data", "SHILD", "archive")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv = file.path(out_dir, "Danish_SHILD_draft_data.csv")
utils::write.csv(release, out_csv, row.names = FALSE)
file.copy(dictionary_path, file.path(out_dir, "Danish_SHILD_dictionnary.csv"), overwrite = TRUE)
message(sprintf("Wrote %s (%d respondents, %d variables).", out_csv, nrow(release), ncol(release)))
if (length(args) == 2) {
  reference = utils::read.csv(args[[2]], check.names = FALSE)
  written = utils::read.csv(out_csv, check.names = FALSE)
  same = isTRUE(all.equal(reference, written, check.attributes = FALSE))
  message(if (same) "The values agree with the comparison file." else "The values differ from the comparison file.")
  if (!same) quit(status = 1)
}
