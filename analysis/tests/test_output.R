#!/usr/bin/env Rscript
# Standalone self-test for analysis/shared/output.R ----------------------------

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/tests/test_output.R"
}
analysis_dir = dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
source(file.path(analysis_dir, "shared", "output.R"), local = FALSE)

.self_test_expect_error = function(code, pattern) {
  error = tryCatch(
    {
      force(code)
      NULL
    },
    error = function(error) error
  )
  if (is.null(error)) {
    stop("Expected an error matching: ", pattern, call. = FALSE)
  }
  if (!grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    stop(
      "Unexpected error. Expected `", pattern, "`, received `", conditionMessage(error), "`.",
      call. = FALSE
    )
  }
  invisible(error)
}

.self_test_write = function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

root = tempfile("run-contract-self-test-")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

first = create_run_contract(
  study = "shild",
  output_root = file.path(root, "runs"),
  run_id = "fixed-run",
  mode = "publication",
  seed = 20260714L,
  expected_artifacts = c(
    "tables/summary.csv", "tables/fold_assignment.csv", "tables/g3_calibration_curve.csv",
    "tables/g3_calibration_plot_source.csv"
  )
)
second = create_run_contract(
  study = "shild",
  output_root = file.path(root, "runs"),
  run_id = "fixed-run",
  mode = "publication",
  seed = 20260714L
)
stopifnot(first$private_dir != second$private_dir)
stopifnot(grepl("-001$", second$private_dir))

summary = data.frame(metric = c("auc", "prauc"), estimate = c(0.91, 0.62))
.run_contract_write_csv(summary, file.path(first$dirs$tables, "summary.csv"))
fold_ids = 101:108
fold_hashes = vapply(
  fold_ids,
  .run_contract_hash_id,
  character(1L),
  column = "row_id",
  run_salt = first$run_salt
)
input_order = order(fold_hashes, decreasing = TRUE)
folds = data.frame(
  row_id = fold_ids[input_order],
  respondent_id = fold_ids[input_order],
  cluster = rep(c("school-a", "school-b", "school-c", "school-d"), each = 2L)[input_order],
  fold = rep(1:2, length.out = length(fold_ids))[input_order],
  stratum = rep(c("content", "discontent"), length.out = length(fold_ids))[input_order]
)
.run_contract_write_csv(folds, file.path(first$dirs$tables, "fold_assignment.csv"))
.run_contract_write_csv(
  data.frame(
    bin = c("[0, 0.5)", "[0.5, 1]"),
    n = c(40L, 60L),
    predicted = c(0.2, 0.7),
    observed = c(0.1, 0.8)
  ),
  file.path(first$dirs$tables, "g3_calibration_curve.csv")
)
.run_contract_write_csv(
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
  file.path(first$dirs$tables, "g3_calibration_plot_source.csv")
)
.run_contract_write_csv(
  data.frame(row_id = 101:103, truth = c(0, 1, 0), prediction = c(0.1, 0.8, 0.2)),
  file.path(first$dirs$tables, "oof_predictions.csv")
)
.run_contract_write_csv(
  data.frame(truth = c(0, 1, 0), probability = c(0.1, 0.8, 0.2)),
  file.path(first$dirs$audit_bundle, "gates", "G3", "observation_level.csv")
)
.run_contract_write_csv(
  data.frame(
    life_satisfaction = "content",
    gender = "Woman",
    age = 35L,
    A2 = 1,
    D2 = 2,
    D8 = 3,
    M8 = 4
  ),
  file.path(first$private_dir, "raw_shild.csv")
)
saveRDS(list(task = "private", backend = summary), file.path(first$dirs$objects, "full_result.rds"))
.self_test_write("serialized payload", file.path(first$dirs$tables, "disguised.rds.txt"))
.self_test_write("compressed payload", file.path(first$dirs$tables, "aggregate.csv.gz"))
.self_test_write(
  '{"task":{"backend":{"row_id":101}},"predictions":[0.2]}',
  file.path(first$private_dir, "fitted_object.json")
)
.self_test_write(
  '{"observation_level":{"truth":[0,1,0],"probability":[0.1,0.8,0.2]}}',
  file.path(first$dirs$audit_bundle, "gates", "G3", "observation_level.json")
)
.self_test_write(
  '{"train_sets":[[1,2]],"test_sets":[[3]],"row_map":{"1":"school-a"}}',
  file.path(first$dirs$audit_bundle, "split_membership.json")
)
.self_test_write(
  c(
    "analysis started",
    "input=/Users/example/private/shild.csv",
    "framework=/Library/Frameworks/R.framework/Resources/lib/libRblas.dylib",
    "tool=/opt/local/bin/tool",
    "volume=/Volumes/Study/data.csv",
    "windows=Z:\\Research\\data.csv",
    "unc=\\\\server\\share\\data.csv",
    "uri=file://server/share/data.csv",
    "ordinary=Keep input/output, /optional, /homeostasis, and https://example.org/a/b unchanged."
  ),
  file.path(first$dirs$logs, "private_path.log")
)
.self_test_write("mode: publication", file.path(first$private_dir, "config.yml"))
.self_test_write("%PDF-1.4 synthetic figure", file.path(first$dirs$figures, "figure.pdf"))
binary_path_probe = c(
  charToRaw("%PDF-1.4"),
  as.raw(0L),
  charToRaw("metadata=/Users/example/private/figure-source.pdf")
)
stopifnot(.run_contract_binary_has_local_path(binary_path_probe))
stopifnot(.run_contract_binary_has_local_path(charToRaw("metadata=Z:\\private\\figure-source.pdf")))
stopifnot(.run_contract_binary_has_local_path(charToRaw("metadata=/Library/Frameworks/R.framework/libR.dylib")))
stopifnot(.run_contract_binary_has_local_path(charToRaw("metadata=\\\\server\\share\\figure.pdf")))
stopifnot(.run_contract_binary_has_local_path(charToRaw("metadata=file://server/share/figure.pdf")))
compressed_prefix_probe = c(
  as.raw(c(0xc5, 0xdc, 0x82, 0x8f)),
  charToRaw("t:/]"),
  as.raw(c(0xba, 0x5c, 0x2e, 0xcf))
)
stopifnot(!.run_contract_binary_has_local_path(compressed_prefix_probe))

original_handlers = globalCallingHandlers()
previous_warning_state = new.env(parent = emptyenv())
previous_warning_state$count = 0L
previous_warning_handler = function(condition) {
  previous_warning_state$count = previous_warning_state$count + 1L
  invisible(NULL)
}
.run_contract_set_global_handlers(c(
  list(warning = previous_warning_handler),
  original_handlers[!names(original_handlers) %in% "warning"]
))
handlers_before_logging = globalCallingHandlers()

run_logging = start_run_logging(first, failure_checkpoint = "analysis")
cat("synthetic stdout\n")
message("synthetic message")
warning("synthetic warning", call. = FALSE, immediate. = TRUE)
tryCatch(
  stop("synthetic handled error", call. = FALSE),
  error = function(error) invisible(NULL)
)
stopifnot(stop_run_logging(run_logging), !stop_run_logging(run_logging))
stopifnot(
  previous_warning_state$count == 1L,
  identical(globalCallingHandlers(), handlers_before_logging)
)

.run_contract_set_global_handlers(original_handlers)

console = readLines(file.path(first$dirs$logs, "console.log"), warn = FALSE)
conditions = utils::read.csv(
  file.path(first$dirs$logs, "conditions.csv"),
  stringsAsFactors = FALSE
)
stopifnot(any(grepl("synthetic stdout", console, fixed = TRUE)))
stopifnot(all(c("message", "warning") %in% conditions$type), !"error" %in% conditions$type)

child_root = file.path(root, "uncaught-error-runs")
child_script = file.path(root, "uncaught-error.R")
output_source = normalizePath(
  file.path(analysis_dir, "shared", "output.R"),
  winslash = "/",
  mustWork = TRUE
)
writeLines(c(
  paste0("source(", deparse(output_source), ", local = FALSE)"),
  paste0("output_root = ", deparse(child_root)),
  "run = create_run_contract(",
  "  study = \"shild\", output_root = output_root, run_id = \"uncaught-run\",",
  "  mode = \"publication\", seed = 1L",
  ")",
  "run_logging = start_run_logging(run, failure_checkpoint = \"analysis\")",
  "write_run_checkpoint(run, \"analysis\", \"started\")",
  "cat(\"child stdout\\n\")",
  "message(\"child message\")",
  "warning(\"child warning\", call. = FALSE, immediate. = TRUE)",
  "stop(\"synthetic uncaught error\", call. = FALSE)"
), child_script, useBytes = TRUE)
child_output = suppressWarnings(system2(
  file.path(R.home("bin"), "Rscript"),
  shQuote(child_script),
  stdout = TRUE,
  stderr = TRUE
))
child_status = attr(child_output, "status")
stopifnot(length(child_status) == 1L, child_status != 0L)

child_private = file.path(child_root, "uncaught-run_shild")
child_conditions = utils::read.csv(
  file.path(child_private, "logs", "conditions.csv"),
  stringsAsFactors = FALSE
)
child_checkpoints = read_run_checkpoints(child_private)
stopifnot(
  sum(child_conditions$type == "error") == 1L,
  identical(child_conditions$message[child_conditions$type == "error"], "synthetic uncaught error"),
  sum(child_checkpoints$checkpoint == "analysis" & child_checkpoints$status == "failed") == 1L,
  any(grepl("child stdout", readLines(
    file.path(child_private, "logs", "console.log"),
    warn = FALSE
  ), fixed = TRUE))
)

broken_logger_root = file.path(root, "broken-logger-runs")
broken_logger_state = file.path(root, "broken-logger-state.csv")
broken_logger_script = file.path(root, "broken-logger.R")
writeLines(c(
  paste0("source(", deparse(output_source), ", local = FALSE)"),
  paste0("output_root = ", deparse(broken_logger_root)),
  paste0("state_path = ", deparse(broken_logger_state)),
  "run = create_run_contract(",
  "  study = \"shild\", output_root = output_root, run_id = \"broken-logger\",",
  "  mode = \"publication\", seed = 1L",
  ")",
  "logging = NULL",
  "original_handlers = globalCallingHandlers()",
  "previous_error_handler = function(condition) {",
  "  .run_contract_write_csv(data.frame(",
  "    active = logging$active,",
  "    connection_open = tryCatch(isOpen(logging$connection), error = function(error) FALSE),",
  "    handlers_restored = TRUE",
  "  ), state_path)",
  "}",
  ".run_contract_set_global_handlers(c(",
  "  list(error = previous_error_handler),",
  "  original_handlers[!names(original_handlers) %in% \"error\"]",
  "))",
  "handlers_before = globalCallingHandlers()",
  "logging = start_run_logging(run, failure_checkpoint = \"analysis\")",
  "sink(type = \"output\")",
  "stop_run_logging(logging)"
), broken_logger_script, useBytes = TRUE)
broken_logger_output = suppressWarnings(system2(
  file.path(R.home("bin"), "Rscript"),
  shQuote(broken_logger_script),
  stdout = TRUE,
  stderr = TRUE
))
broken_logger_status = attr(broken_logger_output, "status")
stopifnot(length(broken_logger_status) == 1L, broken_logger_status != 0L)
broken_logger_result = utils::read.csv(broken_logger_state, stringsAsFactors = FALSE)
stopifnot(
  !broken_logger_result$active,
  !broken_logger_result$connection_open,
  broken_logger_result$handlers_restored
)

input = .self_test_write("a,b\n1,2", file.path(root, "synthetic_input.csv"))
write_run_provenance(
  first,
  source_files = c(output = file.path(analysis_dir, "shared", "output.R")),
  input_files = c(synthetic = input),
  extra = list(test_case = "synthetic")
)
source_manifest = utils::read.csv(
  file.path(first$dirs$provenance, "source_files.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
input_manifest = utils::read.csv(
  file.path(first$dirs$provenance, "input_files.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stopifnot(all(c("md5", "sha256") %in% names(source_manifest)))
stopifnot(all(c("md5", "sha256") %in% names(input_manifest)))
stopifnot(all(grepl("^[0-9a-f]{64}$", source_manifest$sha256)))
stopifnot(all(grepl("^[0-9a-f]{64}$", input_manifest$sha256)))
write_run_checkpoint(first, "analysis", "started", list(stage = 1L))
write_run_checkpoint(first, "analysis", "completed", list(stage = 2L))
stopifnot(nrow(read_run_checkpoints(first)) == 2L)

validate_expected_artifacts(first$private_dir, first$expected_artifacts)
.self_test_expect_error(
  validate_expected_artifacts(first$private_dir, "tables/does_not_exist.csv"),
  "Missing expected artifact"
)
private_manifest = file.path(first$private_dir, "MANIFEST.csv")
private_manifest_table = write_recursive_manifest(first$private_dir, private_manifest)
stopifnot(all(c("md5", "sha256") %in% names(private_manifest_table)))
stopifnot(all(grepl("^[0-9a-f]{64}$", private_manifest_table$sha256)))
verify_recursive_manifest(first$private_dir, private_manifest)
altered_manifest = private_manifest_table
first_sha = altered_manifest$sha256[[1L]]
replacement = if (substr(first_sha, 1L, 1L) == "0") "1" else "0"
altered_manifest$sha256[[1L]] = paste0(replacement, substring(first_sha, 2L))
.run_contract_write_csv(altered_manifest, private_manifest)
.self_test_expect_error(
  verify_recursive_manifest(first$private_dir, private_manifest),
  "Manifest checksum mismatch"
)
.run_contract_write_csv(private_manifest_table, private_manifest)
verify_recursive_manifest(first$private_dir, private_manifest)

upload_one = file.path(root, "upload-one")
upload_two = file.path(root, "upload-two")
create_sanitized_upload_bundle(
  private_dir = first$private_dir,
  upload_dir = upload_one,
  study = "shild",
  run_salt = first$run_salt,
  expected_artifacts = first$expected_artifacts
)
create_sanitized_upload_bundle(
  private_dir = first$private_dir,
  upload_dir = upload_two,
  study = "shild",
  run_salt = first$run_salt,
  expected_artifacts = first$expected_artifacts
)

stopifnot(!file.exists(file.path(upload_one, "objects", "full_result.rds")))
stopifnot(!file.exists(file.path(upload_one, "tables", "disguised.rds.txt")))
stopifnot(!file.exists(file.path(upload_one, "tables", "aggregate.csv.gz")))
stopifnot(!file.exists(file.path(upload_one, "fitted_object.json")))
stopifnot(!file.exists(file.path(upload_one, "raw_shild.csv")))
stopifnot(!file.exists(file.path(upload_one, "tables", "oof_predictions.csv")))
stopifnot(!file.exists(file.path(upload_one, "audit_bundle", "gates", "G3", "observation_level.csv")))
stopifnot(!file.exists(file.path(upload_one, "audit_bundle", "gates", "G3", "observation_level.json")))
stopifnot(!file.exists(file.path(upload_one, "audit_bundle", "split_membership.json")))
stopifnot(file.exists(file.path(upload_one, "tables", "summary.csv")))
stopifnot(file.exists(file.path(upload_one, "tables", "g3_calibration_curve.csv")))
stopifnot(file.exists(file.path(upload_one, "tables", "g3_calibration_plot_source.csv")))
redacted = readLines(file.path(upload_one, "logs", "private_path.log"), warn = FALSE)
stopifnot(any(grepl("<REDACTED_LOCAL_PATH>", redacted, fixed = TRUE)))
stopifnot(!any(vapply(
  c(
    "/Users/example", "/Library/Frameworks", "/opt/local", "/Volumes/Study", "Z:\\Research",
    "\\\\server\\share", "file://server/share"
  ),
  function(path) any(grepl(path, redacted, fixed = TRUE)),
  logical(1L)
)))
stopifnot(any(grepl(
  "ordinary=Keep input/output, /optional, /homeostasis, and https://example.org/a/b unchanged.",
  redacted,
  fixed = TRUE
)))
upload_readme = readLines(file.path(upload_one, "UPLOAD_README.md"), warn = FALSE, encoding = "UTF-8")
stopifnot(any(grepl("do not by themselves make results manuscript-ready", upload_readme, fixed = TRUE)))

fold_one = utils::read.csv(
  file.path(upload_one, "tables", "fold_assignment.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
fold_two = utils::read.csv(
  file.path(upload_two, "tables", "fold_assignment.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stopifnot(all(grepl("^id_[0-9a-f]{20}$", fold_one$row_id)))
stopifnot(all(grepl("^id_[0-9a-f]{20}$", fold_one$cluster)))
stopifnot(!any(as.character(fold_ids) %in% fold_one$row_id))
stopifnot(!"respondent_id" %in% names(fold_one))
stopifnot(!"stratum" %in% names(fold_one))
stopifnot(identical(fold_one$row_id, sort(fold_one$row_id)))
stopifnot(!identical(fold_one$row_id, fold_hashes[input_order]))
stopifnot(identical(fold_one, fold_two))

verify_sanitized_upload_bundle(upload_one, "shild")
verify_recursive_manifest(upload_one, file.path(upload_one, "UPLOAD_MANIFEST.csv"))
local_path_probe = file.path(upload_one, "logs", "local_path_probe.log")
.self_test_write("BLAS: /Library/Frameworks/R.framework/lib/libRblas.dylib", local_path_probe)
.self_test_expect_error(
  verify_sanitized_upload_bundle(upload_one, "shild"),
  "Sanitized upload bundle contains an absolute local path"
)
unlink(local_path_probe)
verify_sanitized_upload_bundle(upload_one, "shild")
upload_manifest = utils::read.csv(
  file.path(upload_one, "UPLOAD_MANIFEST.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stopifnot(all(c("md5", "sha256") %in% names(upload_manifest)))
stopifnot(all(grepl("^[0-9a-f]{64}$", upload_manifest$sha256)))
.self_test_write("# Synthetic output audit", file.path(upload_one, "OUTPUT_AUDIT.md"))
.self_test_expect_error(
  verify_recursive_manifest(upload_one, file.path(upload_one, "UPLOAD_MANIFEST.csv")),
  "Manifest file set differs"
)
write_recursive_manifest(upload_one, file.path(upload_one, "UPLOAD_MANIFEST.csv"))
verify_recursive_manifest(upload_one, file.path(upload_one, "UPLOAD_MANIFEST.csv"))
.self_test_write("tampered", file.path(upload_two, "tables", "summary.csv"))
.self_test_expect_error(
  verify_recursive_manifest(upload_two, file.path(upload_two, "UPLOAD_MANIFEST.csv")),
  "Manifest checksum mismatch"
)

cat("analysis/shared/output.R self-test passed\n")
