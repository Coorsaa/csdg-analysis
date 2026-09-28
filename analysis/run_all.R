#!/usr/bin/env Rscript

# Execute the complete analysis sequence with one collision-safe run identifier.

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/run_all.R"
}
analysis_dir = dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
project_root = normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)
rscript = file.path(R.home("bin"), "Rscript")

quick = identical(Sys.getenv("CSDG_QUICK", unset = "0"), "1")
run_id = Sys.getenv("CSDG_RUN_ID", unset = "")
if (!nzchar(run_id)) {
  run_id = paste0("analysis-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
}
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
  stop("CSDG_RUN_ID may contain only letters, digits, periods, underscores, and hyphens.", call. = FALSE)
}

artifact_root = file.path(project_root, "paper_artifacts")
current_root = file.path(artifact_root, "current")
staging_root = file.path(artifact_root, paste0(".current-", run_id, "-staging"))
shild_private = file.path(artifact_root, paste0(run_id, "_shild"))
pisa_private = file.path(artifact_root, paste0(run_id, "_pisa2018_reading"))
synthesis_private = file.path(artifact_root, paste0(run_id, "_evidence_synthesis"))
shild_upload = paste0(shild_private, "_upload")
pisa_upload = paste0(pisa_private, "_upload")
synthesis_upload = paste0(synthesis_private, "_upload")

expected_new_paths = c(shild_private, shild_upload, pisa_private, pisa_upload)
if (!quick) {
  expected_new_paths = c(expected_new_paths, synthesis_private, synthesis_upload)
}
existing = expected_new_paths[file.exists(expected_new_paths) | dir.exists(expected_new_paths)]
if (!quick) {
  canonical_paths = c(current_root, staging_root)
  existing = c(existing, canonical_paths[file.exists(canonical_paths) | dir.exists(canonical_paths)])
}
if (length(existing)) {
  stop(
    "The requested run identifier already has output paths:\n",
    paste(existing, collapse = "\n"),
    call. = FALSE
  )
}

base_environment = c(
  paste0("CSDG_RUN_ID=", run_id),
  paste0("CSDG_QUICK=", if (quick) "1" else "0"),
  paste0("CSDG_ANALYSIS_SEED=", Sys.getenv("CSDG_ANALYSIS_SEED", unset = "20260714")),
  paste0("CSDG_THREADS=", Sys.getenv("CSDG_THREADS", unset = "1")),
  paste0("CSDG_ALE_BOOTSTRAP_WORKERS=", Sys.getenv("CSDG_ALE_BOOTSTRAP_WORKERS", unset = "4"))
)

run_script = function(relative_path, environment = base_environment, script_arguments = character()) {
  path = file.path(project_root, relative_path)
  message("\n=== ", relative_path, " ===")
  status = system2(rscript, shQuote(c(path, script_arguments)), env = environment)
  if (!identical(status, 0L)) {
    stop(relative_path, " failed with exit status ", status, ".", call. = FALSE)
  }
  invisible(TRUE)
}

run_script("analysis/preflight.R")
run_script("analysis/shild/analysis.R")
run_script("analysis/pisa/analysis.R")

if (quick) {
  message("\nQuick-mode SHILD and PISA smoke tests completed. These outputs are not eligible for manuscript use.")
  quit(save = "no", status = 0L)
}

synthesis_environment = c(
  base_environment,
  paste0("CSDG_SHILD_PARENT=", shild_private),
  paste0("CSDG_PISA_PARENT=", pisa_private),
  paste0("CSDG_G4_WORKERS=", Sys.getenv("CSDG_G4_WORKERS", unset = "3"))
)
run_script("analysis/evidence_synthesis.R", environment = synthesis_environment)

move_run_directory = function(source, destination) {
  if (!dir.exists(source)) {
    stop("Expected completed run directory is missing: ", source, call. = FALSE)
  }
  if (file.exists(destination) || dir.exists(destination)) {
    stop("Result staging destination already exists: ", destination, call. = FALSE)
  }
  parent = dirname(destination)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create result staging directory: ", parent, call. = FALSE)
  }
  if (!file.rename(source, destination)) {
    stop("Could not move completed run directory to: ", destination, call. = FALSE)
  }
  invisible(destination)
}

run_directories = data.frame(
  source = c(shild_private, shild_upload, pisa_private, pisa_upload, synthesis_private, synthesis_upload),
  destination = file.path(
    staging_root,
    c("shild", "shild", "pisa", "pisa", "evidence", "evidence"),
    c("private", "upload", "private", "upload", "private", "upload")
  ),
  stringsAsFactors = FALSE
)
moved = rep(FALSE, nrow(run_directories))

tryCatch({
  for (index in seq_len(nrow(run_directories))) {
    move_run_directory(run_directories$source[[index]], run_directories$destination[[index]])
    moved[[index]] = TRUE
  }

  staging_delivery_root = file.path(staging_root, "delivery")
  staging_delivery_dir = file.path(staging_delivery_root, "CSDG_publication_results_upload")
  staging_delivery_zip = paste0(staging_delivery_dir, ".zip")
  run_script(
    "analysis/build_delivery.R",
    script_arguments = c(
      paste0("--shild-upload=", file.path(staging_root, "shild", "upload")),
      paste0("--pisa-upload=", file.path(staging_root, "pisa", "upload")),
      paste0("--evidence-upload=", file.path(staging_root, "evidence", "upload")),
      paste0("--output-dir=", staging_delivery_dir),
      paste0("--zip=", staging_delivery_zip)
    )
  )

  if (!file.rename(staging_root, current_root)) {
    stop("Could not atomically publish the complete current-result directory.", call. = FALSE)
  }
}, error = function(error) {
  rollback_failures = character()
  for (index in rev(which(moved))) {
    source = run_directories$source[[index]]
    destination = run_directories$destination[[index]]
    restored = dir.exists(destination) && !file.exists(source) && !dir.exists(source) &&
      file.rename(destination, source)
    if (!isTRUE(restored)) {
      rollback_failures = c(rollback_failures, destination)
    }
  }
  if (!length(rollback_failures) && dir.exists(staging_root)) {
    unlink(staging_root, recursive = TRUE, force = TRUE)
  }
  rollback_note = if (length(rollback_failures)) {
    paste0(" Rollback was incomplete for: ", paste(rollback_failures, collapse = ", "), ".")
  } else {
    " Completed study runs were restored to their original run-id paths."
  }
  stop(conditionMessage(error), rollback_note, call. = FALSE)
})

delivery_dir = file.path(current_root, "delivery", "CSDG_publication_results_upload")
delivery_zip = paste0(delivery_dir, ".zip")

message("\nComplete analysis-results delivery finished.")
message("Delivery: ", delivery_dir)
message("ZIP: ", delivery_zip)
message("SHA-256: ", paste0(delivery_zip, ".sha256"))
