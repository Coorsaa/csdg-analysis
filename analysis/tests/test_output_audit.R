#!/usr/bin/env Rscript
# Standalone self-test for analysis/shared/output_audit.R ---------------

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/tests/test_output_audit.R"
}
analysis_dir = dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
source(file.path(analysis_dir, "shared", "output.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "output_audit.R"), local = FALSE)

.self_test_write_figure = function(stem, pages = 1L, pixels = 300L, dpi = 300, transparent = FALSE) {
  grDevices::pdf(paste0(stem, ".pdf"), width = 4, height = 4, onefile = TRUE)
  for (page in seq_len(pages)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, paste("Page", page))
  }
  grDevices::dev.off()
  image = if (isTRUE(transparent)) {
    array(0, dim = c(pixels, pixels, 4L))
  } else {
    array(
      rep(c(0.3, 0.55, 0.8), each = pixels * pixels),
      dim = c(pixels, pixels, 3L)
    )
  }
  png::writePNG(image, paste0(stem, ".png"), dpi = rep(dpi, 2L))
  invisible(stem)
}

.self_test_figure_metadata = function(
    figure_id,
    figure_group_id = figure_id,
    is_grouped_figure = FALSE,
    latex_group_id = figure_group_id,
    panel_id = NA_character_,
    panel_order = seq_along(figure_id),
    panel_heading = NA_character_) {
  n = length(figure_id)
  multipanel = rep(as.logical(is_grouped_figure), length.out = n)
  data.frame(
    study = rep("synthetic", n),
    figure_id = figure_id,
    figure_group_id = rep(figure_group_id, length.out = n),
    is_grouped_figure = multipanel,
    latex_group_id = rep(latex_group_id, length.out = n),
    panel_id = rep(panel_id, length.out = n),
    panel_order = rep(as.integer(panel_order), length.out = n),
    panel_heading = rep(panel_heading, length.out = n),
    pdf_path = file.path("figures", paste0(figure_id, ".pdf")),
    png_path = file.path("figures", paste0(figure_id, ".png")),
    title = ifelse(multipanel, NA_character_, "Synthetic figure"),
    subtitle = ifelse(multipanel, NA_character_, "Synthetic subtitle"),
    caption = NA_character_,
    group_title = ifelse(multipanel, "Synthetic grouped figure", NA_character_),
    group_subtitle = ifelse(multipanel, "Synthetic group subtitle", NA_character_),
    group_caption = NA_character_,
    source_tables = rep("tables/intervals.csv", n),
    generator_script = rep("analysis/tests/test_output_audit.R", n),
    generator_function = rep(".self_test_write_figure", n),
    width_in = rep(4, n),
    height_in = rep(4, n),
    role = rep("self-test", n),
    estimand_or_scope = rep("synthetic figure validation", n),
    uncertainty_semantics = rep("synthetic; no statistical uncertainty", n),
    privacy_scope = rep("synthetic aggregate", n),
    latex_assembly_note = ifelse(
      multipanel,
      "Assemble the components in panel order with LaTeX.",
      "Supply the outer text with LaTeX."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.self_test_write_figure_metadata = function(value, run_dir) {
  utils::write.csv(
    value,
    file.path(run_dir, .publication_audit_figure_latex_metadata_path),
    row.names = FALSE,
    na = ""
  )
}

root = tempfile("publication-output-audit-self-test-")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

good = file.path(root, "good run")
dir.create(file.path(good, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(good, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(good, "logs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(good, "provenance"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  data.frame(estimate = c(0.2, 0.4), ci_low = c(0.1, 0.3), ci_high = c(0.3, 0.5)),
  file.path(good, "tables", "intervals.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(
    bin_index = 1:2,
    bin_label = c("[0, 0.5)", "[0.5, 1]"),
    n = c(40L, 60L),
    predicted_value = c(0.2, 0.7),
    observed_value = c(0.1, 0.8),
    ci_low = c(0.04, 0.69),
    ci_high = c(0.23, 0.88),
    interval_method = "Wilson score interval for a binomial proportion",
    confidence_level = 0.95,
    point_size_semantics = "point area proportional to bin n"
  ),
  file.path(good, "tables", "calibration_plot_source.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(key = c("study", "mode"), value = c("synthetic", "publication")),
  file.path(good, "provenance", "run_metadata.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(mode = "PUBLICATION", publication_settings = TRUE, manuscript_ready = TRUE),
  file.path(good, "tables", "synthetic_run_mode.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(type = "message", message = "Synthetic progress message."),
  file.path(good, "logs", "conditions.csv"),
  row.names = FALSE
)
writeLines('{"status":"ok"}', file.path(good, "metadata.json"), useBytes = TRUE)
.self_test_write_figure(file.path(good, "figures", "figure"))

missing_metadata_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(missing_metadata_result$summary$structural_status[[1L]], "FAIL"))
stopifnot(identical(missing_metadata_result$summary$figure_latex_metadata_status[[1L]], "FAIL"))
stopifnot(any(
  missing_metadata_result$checks$check_id == "figure_latex_metadata_presence" &
    missing_metadata_result$checks$status == "fail"
))

.self_test_write_figure_metadata(.self_test_figure_metadata("figure"), good)
good_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(good_result$summary$overall_status[[1L]], "PASS"))
stopifnot(identical(good_result$summary$structural_status[[1L]], "PASS"))
stopifnot(identical(good_result$summary$release_eligibility[[1L]], "ELIGIBLE"))
stopifnot(identical(good_result$summary$figure_latex_metadata_status[[1L]], "PASS"))
stopifnot(identical(good_result$summary$run_mode[[1L]], "PUBLICATION"))
stopifnot(identical(good_result$summary$manuscript_ready[[1L]], "TRUE"))
stopifnot(good_result$summary$interval_pairs[[1L]] == 2L)
stopifnot(!any(
  good_result$checks$path == "tables/calibration_plot_source.csv" &
    good_result$checks$check_id == "row_level_predictions" & good_result$checks$status == "fail"
))
stopifnot(good_result$figure_inventory$pdf_pages[[1L]] == 1L)
stopifnot(good_result$figure_inventory$png_width_px[[1L]] == 300L)
stopifnot(all(file.exists(file.path(good, .publication_audit_report_paths))))

good_metadata_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(good_metadata_result$summary$structural_status[[1L]], "PASS"))
stopifnot(identical(good_metadata_result$summary$release_eligibility[[1L]], "ELIGIBLE"))
stopifnot(identical(good_metadata_result$summary$figure_latex_metadata_status[[1L]], "PASS"))
stopifnot(good_metadata_result$summary$figure_latex_metadata_rows[[1L]] == 1L)
stopifnot(all(
  good_metadata_result$checks$status[
    grepl("^figure_latex_metadata_", good_metadata_result$checks$check_id)
  ] == "pass"
))
summary_file = utils::read.csv(
  file.path(good, "provenance", "output_audit_summary.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stopifnot(all(c(
  "structural_status", "release_eligibility", "run_mode", "manuscript_ready", "substantive_warning_count"
) %in% names(summary_file)))

utils::write.csv(
  data.frame(
    type = c("message", "warning"),
    message = c("Synthetic progress message.", "Synthetic substantive warning.")
  ),
  file.path(good, "logs", "conditions.csv"),
  row.names = FALSE
)
warning_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(warning_result$summary$structural_status[[1L]], "PASS"))
stopifnot(identical(warning_result$summary$release_eligibility[[1L]], "NOT_ELIGIBLE"))
stopifnot(warning_result$summary$logged_warnings[[1L]] == 1L)
stopifnot(warning_result$summary$substantive_warning_count[[1L]] == 1L)
stopifnot(grepl("warning", warning_result$summary$eligibility_blockers[[1L]], fixed = TRUE))

utils::write.csv(
  data.frame(key = c("study", "mode"), value = c("synthetic", "smoke_test")),
  file.path(good, "provenance", "run_metadata.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(mode = "NONPUBLICATION_QUICK", publication_settings = FALSE, manuscript_ready = FALSE),
  file.path(good, "tables", "synthetic_run_mode.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(type = "message", message = "Synthetic progress message."),
  file.path(good, "logs", "conditions.csv"),
  row.names = FALSE
)
quick_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(quick_result$summary$structural_status[[1L]], "PASS"))
stopifnot(identical(quick_result$summary$release_eligibility[[1L]], "NOT_ELIGIBLE"))
stopifnot(identical(quick_result$summary$run_mode[[1L]], "SMOKE_TEST"))
quick_markdown = readLines(file.path(good, "OUTPUT_AUDIT.md"), warn = FALSE, encoding = "UTF-8")
stopifnot(any(grepl("DO NOT RELEASE FOR MANUSCRIPT USE", quick_markdown, fixed = TRUE)))

utils::write.csv(
  data.frame(key = c("study", "mode"), value = c("synthetic", "publication")),
  file.path(good, "provenance", "run_metadata.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(mode = "PUBLICATION", publication_settings = TRUE, manuscript_ready = FALSE),
  file.path(good, "tables", "synthetic_run_mode.csv"),
  row.names = FALSE
)
not_ready_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(not_ready_result$summary$structural_status[[1L]], "PASS"))
stopifnot(identical(not_ready_result$summary$release_eligibility[[1L]], "NOT_ELIGIBLE"))
stopifnot(identical(not_ready_result$summary$manuscript_ready[[1L]], "FALSE"))

utils::write.csv(
  data.frame(mode = "PUBLICATION", manuscript_ready = TRUE),
  file.path(good, "tables", "synthetic_run_mode.csv"),
  row.names = FALSE
)
unknown_settings_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(unknown_settings_result$summary$structural_status[[1L]], "PASS"))
stopifnot(identical(unknown_settings_result$summary$publication_settings[[1L]], "UNKNOWN"))
stopifnot(identical(unknown_settings_result$summary$release_eligibility[[1L]], "NOT_ELIGIBLE"))
stopifnot(grepl("publication settings", unknown_settings_result$summary$eligibility_blockers[[1L]], fixed = TRUE))
stopifnot(identical(.publication_audit_state(c(TRUE, NA)), "INVALID"))

utils::write.csv(
  data.frame(mode = "PUBLICATION", publication_settings = TRUE, manuscript_ready = TRUE),
  file.path(good, "tables", "synthetic_run_mode.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(type = "notice", message = "An unrecognized condition type."),
  file.path(good, "logs", "conditions.csv"),
  row.names = FALSE
)
invalid_conditions_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(invalid_conditions_result$summary$conditions_status[[1L]], "INVALID"))
stopifnot(identical(invalid_conditions_result$summary$release_eligibility[[1L]], "NOT_ELIGIBLE"))

utils::write.csv(
  data.frame(mode = "", publication_settings = TRUE, manuscript_ready = TRUE),
  file.path(good, "tables", "synthetic_run_mode.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(type = "message", message = "Synthetic progress message."),
  file.path(good, "logs", "conditions.csv"),
  row.names = FALSE
)
invalid_mode_result = audit_publication_output(good, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(invalid_mode_result$summary$run_mode[[1L]], "INVALID"))
stopifnot(identical(invalid_mode_result$summary$release_eligibility[[1L]], "NOT_ELIGIBLE"))

multipanel_run = file.path(root, "multipanel run")
dir.create(file.path(multipanel_run, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(multipanel_run, "provenance"), recursive = TRUE, showWarnings = FALSE)
.self_test_write_figure(file.path(multipanel_run, "figures", "grouped__panel_a"))
.self_test_write_figure(file.path(multipanel_run, "figures", "grouped__panel_b"))
multipanel_metadata = .self_test_figure_metadata(
  figure_id = c("grouped__panel_a", "grouped__panel_b"),
  figure_group_id = "grouped",
  is_grouped_figure = TRUE,
  latex_group_id = "grouped",
  panel_id = c("panel_a", "panel_b"),
  panel_order = 1:2,
  panel_heading = c("Panel A", "Panel B")
)
multipanel_metadata$title = c("Removed title for panel A", "Removed title for panel B")
multipanel_metadata$subtitle = c("Removed subtitle for panel A", "Removed subtitle for panel B")
multipanel_metadata$caption = c("Removed caption for panel A", "Removed caption for panel B")
multipanel_metadata$group_title = "Synthetic grouped title"
multipanel_metadata$group_subtitle = "Synthetic grouped subtitle"
multipanel_metadata$group_caption = "Synthetic grouped caption"
.self_test_write_figure_metadata(multipanel_metadata, multipanel_run)
multipanel_result = audit_publication_output(multipanel_run, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(multipanel_result$summary$structural_status[[1L]], "PASS"))
stopifnot(identical(multipanel_result$figure_latex_metadata$title, multipanel_metadata$title))
stopifnot(identical(multipanel_result$figure_latex_metadata$subtitle, multipanel_metadata$subtitle))
stopifnot(any(
  multipanel_result$checks$check_id == "figure_latex_metadata_panels" &
    multipanel_result$checks$status == "pass"
))

invalid_multipanel = multipanel_metadata
invalid_multipanel$study[[1L]] = ""
invalid_multipanel$figure_id[[2L]] = invalid_multipanel$figure_id[[1L]]
invalid_multipanel$pdf_path[[2L]] = "../outside.pdf"
invalid_multipanel$png_path[[2L]] = "figures/unlisted.png"
invalid_multipanel$panel_order[[2L]] = 3L
invalid_multipanel$group_title = ""
.self_test_write_figure_metadata(invalid_multipanel, multipanel_run)
invalid_multipanel_result = audit_publication_output(multipanel_run, min_png_pixels = 250L, min_png_dpi = 299)
expected_metadata_failures = c(
  "figure_latex_metadata_required_values",
  "figure_latex_metadata_identifiers",
  "figure_latex_metadata_paths",
  "figure_latex_metadata_pairing",
  "figure_latex_metadata_coverage",
  "figure_latex_metadata_panels"
)
stopifnot(all(expected_metadata_failures %in% invalid_multipanel_result$checks$check_id[
  invalid_multipanel_result$checks$status == "fail"
]))
stopifnot(identical(invalid_multipanel_result$summary$structural_status[[1L]], "FAIL"))

invalid_schema = multipanel_metadata[, setdiff(names(multipanel_metadata), "group_caption"), drop = FALSE]
.self_test_write_figure_metadata(invalid_schema, multipanel_run)
invalid_schema_result = audit_publication_output(multipanel_run, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(any(
  invalid_schema_result$checks$check_id == "figure_latex_metadata_schema" &
    invalid_schema_result$checks$status == "fail"
))

bad = file.path(root, "bad run")
dir.create(file.path(bad, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(bad, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(bad, "objects"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  data.frame(ci_low = c(0.4, NA), ci_high = c(0.2, 0.5)),
  file.path(bad, "tables", "reversed.csv"),
  row.names = FALSE
)
writeLines(c("a,b", '1,"unterminated'), file.path(bad, "tables", "invalid.csv"), useBytes = TRUE)
utils::write.csv(
  data.frame(life_satisfaction = "content", gender = "Woman", age = 35L, A2 = 1L),
  file.path(bad, "tables", "raw_rows.csv"),
  row.names = FALSE
)
writeLines('{"status":', file.path(bad, "invalid.json"), useBytes = TRUE)
writeLines('{"task":{"model":"private"}}', file.path(bad, "fitted_object.json"), useBytes = TRUE)
writeLines("metadata", file.path(bad, ".DS_Store"), useBytes = TRUE)
saveRDS(list(private = TRUE), file.path(bad, "objects", "model.rds"))
.self_test_write_figure(file.path(bad, "figures", "two_page"), pages = 2L, pixels = 100L, dpi = 100)
.self_test_write_figure(
  file.path(bad, "figures", "transparent"),
  pixels = 300L,
  dpi = 300,
  transparent = TRUE
)
grDevices::pdf(file.path(bad, "figures", "pdf_only.pdf"), width = 4, height = 4)
graphics::plot.new()
invisible(grDevices::dev.off())

bad_result = audit_publication_output(bad, min_png_pixels = 250L, min_png_dpi = 299)
stopifnot(identical(bad_result$summary$overall_status[[1L]], "FAIL"))
stopifnot(identical(bad_result$summary$structural_status[[1L]], "FAIL"))
stopifnot(identical(bad_result$summary$release_eligibility[[1L]], "NOT_ELIGIBLE"))
stopifnot(any(bad_result$checks$check_id == "csv_parse" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "interval_order" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "json_parse" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "figure_pair" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "pdf_pages" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "png_specification" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "png_visibility" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "forbidden_private_artifact" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "raw_analysis_table" & bad_result$checks$status == "fail"))
stopifnot(any(bad_result$checks$check_id == "sensitive_json_payload" & bad_result$checks$status == "fail"))

# Transactional finalization -------------------------------------------------

.self_test_finalization_audit = function(run_dir, min_png_pixels = 1500L, min_png_dpi = 299) {
  checks = data.frame(
    check_id = "synthetic",
    category = "test",
    path = "",
    status = "pass",
    severity = "info",
    message = "Synthetic structural audit passed.",
    stringsAsFactors = FALSE
  )
  summary = data.frame(
    structural_status = "PASS",
    release_eligibility = "ELIGIBLE",
    eligibility_blockers = "",
    stringsAsFactors = FALSE
  )
  .run_contract_write_csv(checks, file.path(run_dir, "provenance", "output_audit.csv"))
  .run_contract_write_csv(summary, file.path(run_dir, "provenance", "output_audit_summary.csv"))
  .run_contract_write_csv(
    data.frame(figure_id = character(), stringsAsFactors = FALSE),
    file.path(run_dir, "provenance", "figure_inventory.csv")
  )
  writeLines("# Synthetic publication output audit", file.path(run_dir, "OUTPUT_AUDIT.md"), useBytes = TRUE)
  invisible(list(summary = summary, checks = checks))
}

.self_test_new_finalization_run = function(root, run_id) {
  run = create_run_contract(
    study = "shild",
    output_root = file.path(root, "runs"),
    run_id = run_id,
    mode = "publication",
    seed = 20260714L,
    expected_artifacts = "tables/summary.csv"
  )
  .run_contract_write_csv(
    data.frame(metric = "auc", estimate = 0.91),
    file.path(run$dirs$tables, "summary.csv")
  )
  write_run_checkpoint(run, "analysis", "started", list(mode = "PUBLICATION"))
  run
}

.self_test_checkpoint_files = function(root, checkpoint, status) {
  files = list.files(file.path(root, "checkpoints"), full.names = TRUE)
  pattern = paste0("_", checkpoint, "_", status, "\\.csv$")
  files[grepl(pattern, basename(files))]
}

original_audit_publication_output = audit_publication_output
original_write_upload_completion_checkpoints = .write_upload_completion_checkpoints
had_package_source_assertion = exists(".assert_package_source_snapshot", inherits = FALSE)
if (had_package_source_assertion) {
  original_package_source_assertion = .assert_package_source_snapshot
}
audit_publication_output = .self_test_finalization_audit
.assert_package_source_snapshot = function(snapshot, project_root, context = "analysis run") {
  invisible(TRUE)
}

finalization_root = file.path(root, "transactional finalization")
dir.create(finalization_root, recursive = TRUE, showWarnings = FALSE)
successful = .self_test_new_finalization_run(finalization_root, "successful")
upload_dir = .finalize_study_output(
  run = successful,
  upload_expected = successful$expected_artifacts,
  package_source = list(),
  project_root = finalization_root,
  analysis_checkpoint = "analysis",
  finalization_checkpoint = "finalization",
  label = "synthetic"
)

stopifnot(dir.exists(upload_dir))
verify_sanitized_upload_bundle(upload_dir, "shild")
verify_recursive_manifest(upload_dir, file.path(upload_dir, "UPLOAD_MANIFEST.csv"))

upload_checkpoints = read_run_checkpoints(upload_dir)
upload_tail = tail(upload_checkpoints, 2L)
stopifnot(identical(upload_tail$checkpoint, c("analysis", "finalization")))
stopifnot(identical(upload_tail$status, c("completed", "completed")))
stopifnot(all(grepl("upload_verified=TRUE", upload_tail$details, fixed = TRUE)))
stopifnot(all(grepl("publication_transaction=committed", upload_tail$details, fixed = TRUE)))
stopifnot(!any(grepl("upload_dir=", upload_tail$details, fixed = TRUE)))
stopifnot(!any(grepl(normalizePath(finalization_root, winslash = "/"), upload_tail$details, fixed = TRUE)))
stopifnot(length(.self_test_checkpoint_files(upload_dir, "analysis", "completed")) == 1L)
stopifnot(length(.self_test_checkpoint_files(upload_dir, "finalization", "completed")) == 1L)

upload_manifest = utils::read.csv(
  file.path(upload_dir, "UPLOAD_MANIFEST.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stopifnot("checkpoints/checkpoints.csv" %in% upload_manifest$path)
stopifnot(any(grepl("_analysis_completed\\.csv$", upload_manifest$path)))
stopifnot(any(grepl("_finalization_completed\\.csv$", upload_manifest$path)))
stopifnot(!"UPLOAD_MANIFEST.csv" %in% upload_manifest$path)

private_checkpoints = read_run_checkpoints(successful)
private_tail = tail(private_checkpoints, 2L)
stopifnot(identical(private_tail$checkpoint, c("analysis", "finalization")))
stopifnot(identical(private_tail$status, c("completed", "completed")))
stopifnot(all(grepl("upload_verified=TRUE", private_tail$details, fixed = TRUE)))
stopifnot(all(grepl("publication_transaction=committed", private_tail$details, fixed = TRUE)))
stopifnot(!any(grepl("upload_dir=", private_tail$details, fixed = TRUE)))
stopifnot(!any(grepl(normalizePath(finalization_root, winslash = "/"), private_tail$details, fixed = TRUE)))

post_commit = .self_test_new_finalization_run(finalization_root, "post-commit-bookkeeping")
post_commit_checkpoint_state = new.env(parent = emptyenv())
post_commit_checkpoint_state$calls = 0L
.write_upload_completion_checkpoints = function(...) {
  post_commit_checkpoint_state$calls = post_commit_checkpoint_state$calls + 1L
  if (post_commit_checkpoint_state$calls == 2L) {
    stop("Synthetic private post-commit bookkeeping failure.", call. = FALSE)
  }
  original_write_upload_completion_checkpoints(...)
}
post_commit_upload = suppressMessages(.finalize_study_output(
  run = post_commit,
  upload_expected = post_commit$expected_artifacts,
  package_source = list(),
  project_root = finalization_root,
  analysis_checkpoint = "analysis",
  finalization_checkpoint = "finalization",
  label = "synthetic"
))
stopifnot(post_commit_checkpoint_state$calls == 2L, dir.exists(post_commit_upload))
verify_sanitized_upload_bundle(post_commit_upload, "shild")
verify_recursive_manifest(post_commit_upload, file.path(post_commit_upload, "UPLOAD_MANIFEST.csv"))
post_commit_upload_tail = tail(read_run_checkpoints(post_commit_upload), 2L)
stopifnot(
  identical(post_commit_upload_tail$checkpoint, c("analysis", "finalization")),
  identical(post_commit_upload_tail$status, c("completed", "completed")),
  all(grepl("publication_transaction=committed", post_commit_upload_tail$details, fixed = TRUE))
)
post_commit_private = read_run_checkpoints(post_commit)
stopifnot(!any(post_commit_private$checkpoint == "finalization" & post_commit_private$status == "failed"))
post_commit_bookkeeping = post_commit_private[
  post_commit_private$checkpoint == "finalization_private_bookkeeping",
  ,
  drop = FALSE
]
stopifnot(
  nrow(post_commit_bookkeeping) == 1L,
  identical(post_commit_bookkeeping$status, "failed"),
  grepl("publication_transaction=committed", post_commit_bookkeeping$details, fixed = TRUE)
)
verify_recursive_manifest(post_commit$private_dir, file.path(post_commit$private_dir, "MANIFEST.csv"))
.write_upload_completion_checkpoints = original_write_upload_completion_checkpoints

failed = .self_test_new_finalization_run(finalization_root, "failed")
failed_upload = paste0(failed$private_dir, "_upload")
failure = tryCatch(
  {
    .finalize_study_output(
      run = failed,
      upload_expected = failed$expected_artifacts,
      package_source = list(),
      project_root = finalization_root,
      analysis_checkpoint = "analysis",
      finalization_checkpoint = "finalization",
      label = "synthetic",
      before_publish = function() stop("Synthetic pre-publication failure.", call. = FALSE)
    )
    NULL
  },
  error = function(error) error
)
stopifnot(inherits(failure, "error"))
stopifnot(grepl("Synthetic pre-publication failure", conditionMessage(failure), fixed = TRUE))
stopifnot(!dir.exists(failed_upload), !file.exists(failed_upload))
stopifnot(!length(list.files(
  dirname(failed$private_dir),
  pattern = "^[.]shild-release-",
  all.files = TRUE
)))
failed_checkpoints = read_run_checkpoints(failed)
stopifnot(!any(failed_checkpoints$checkpoint == "analysis" & failed_checkpoints$status == "completed"))
stopifnot(!any(failed_checkpoints$checkpoint == "finalization" & failed_checkpoints$status == "completed"))
stopifnot(any(failed_checkpoints$checkpoint == "finalization" & failed_checkpoints$status == "failed"))

audit_publication_output = original_audit_publication_output
if (had_package_source_assertion) {
  .assert_package_source_snapshot = original_package_source_assertion
} else {
  rm(.assert_package_source_snapshot)
}

cat("publication_output_audit self-test: OK\n")
