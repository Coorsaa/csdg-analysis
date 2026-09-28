#!/usr/bin/env Rscript

# Build a self-contained analysis-results delivery from manifest-checked study bundles.
#
# The script copies each result bundle byte for byte, verifies that its recorded producer sources match this checkout,
# snapshots those sources, collects figure assembly metadata, and verifies a ZIP round-trip before committing output.

.delivery_stop = function(...) {
  stop(..., call. = FALSE)
}

.delivery_message = function(...) {
  message("[delivery] ", ...)
}

.delivery_require_package = function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .delivery_stop("Package `", package, "` is required.")
  }
  invisible(TRUE)
}

.delivery_script_path = function() {
  arguments = commandArgs(trailingOnly = FALSE)
  file_argument = grep("^--file=", arguments, value = TRUE)
  path = if (length(file_argument)) sub("^--file=", "", file_argument[[1L]]) else "analysis/build_delivery.R"
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.delivery_default_project_root = function() {
  dirname(dirname(.delivery_script_path()))
}

.delivery_scalar_string = function(value, name) {
  valid = is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  if (!valid) {
    .delivery_stop("`", name, "` must be one non-empty character value.")
  }
  invisible(TRUE)
}

.delivery_safe_relative_path = function(path, name) {
  .delivery_scalar_string(path, name)
  normalized = gsub("\\\\", "/", path)
  parts = strsplit(normalized, "/", fixed = TRUE)[[1L]]
  invalid = startsWith(normalized, "/") || grepl("^[A-Za-z]:/", normalized) ||
    grepl("[[:cntrl:]]", normalized) || any(parts %in% c("", ".", ".."))
  if (invalid) {
    .delivery_stop("Unsafe relative path in `", name, "`: ", path)
  }
  normalized
}

.delivery_safe_output_name = function(path) {
  name = basename(path)
  valid = grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", name) && !name %in% c(".", "..")
  if (!valid) {
    .delivery_stop("The output directory name may contain only letters, numbers, '.', '_', and '-'.")
  }
  name
}

.delivery_existing_directory = function(path, name) {
  .delivery_scalar_string(path, name)
  if (!dir.exists(path)) {
    .delivery_stop("`", name, "` does not exist or is not a directory: ", path)
  }
  if (nzchar(Sys.readlink(path))) {
    .delivery_stop("`", name, "` must not be a symbolic link: ", path)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.delivery_new_path = function(path, name, create_parent = TRUE) {
  .delivery_scalar_string(path, name)
  parent = dirname(path)
  if (create_parent && !dir.exists(parent)) {
    created = dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(parent)) {
      .delivery_stop("Could not create the parent directory for `", name, "`: ", parent)
    }
  }
  parent = normalizePath(parent, winslash = "/", mustWork = TRUE)
  file.path(parent, basename(path))
}

.delivery_path_within = function(path, root) {
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

.delivery_paths_overlap = function(left, right) {
  .delivery_path_within(left, right) || .delivery_path_within(right, left)
}

.delivery_relative_path = function(path, root) {
  root = normalizePath(root, winslash = "/", mustWork = TRUE)
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix = paste0(root, "/")
  if (!startsWith(path, prefix)) {
    .delivery_stop("A file resolved outside its declared root: ", path)
  }
  substring(path, nchar(prefix) + 1L)
}

.delivery_files = function(root) {
  files = list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  if (!length(files)) {
    return(character())
  }
  files[file.info(files)$isdir %in% FALSE]
}

.delivery_assert_no_symlinks = function(root, name) {
  root = .delivery_existing_directory(root, name)
  entries = unique(c(
    list.dirs(root, recursive = TRUE, full.names = TRUE),
    list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ))
  links = entries[nzchar(Sys.readlink(entries))]
  if (length(links)) {
    relative = vapply(links, .delivery_relative_path, character(1L), root = root)
    .delivery_stop(name, " contains symbolic links: ", paste(relative, collapse = ", "))
  }
  invisible(TRUE)
}

.delivery_sha256 = function(paths) {
  .delivery_require_package("digest")
  vapply(
    paths,
    function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE),
    character(1L)
  )
}

.delivery_tree_state = function(root, exclude = character()) {
  root = .delivery_existing_directory(root, "tree root")
  .delivery_assert_no_symlinks(root, "Tree root")
  files = .delivery_files(root)
  if (!length(files)) {
    return(data.frame(path = character(), bytes = numeric(), sha256 = character()))
  }
  relative = vapply(files, .delivery_relative_path, character(1L), root = root)
  keep = !relative %in% gsub("\\\\", "/", exclude)
  files = files[keep]
  relative = relative[keep]
  index = order(relative)
  files = files[index]
  relative = relative[index]
  data.frame(
    path = relative,
    bytes = as.numeric(file.info(files)$size),
    sha256 = unname(.delivery_sha256(files)),
    stringsAsFactors = FALSE
  )
}

.delivery_tree_hash = function(state) {
  lines = paste(state$path, format(state$bytes, scientific = FALSE, trim = TRUE), state$sha256, sep = "\t")
  digest::digest(paste(lines, collapse = "\n"), algo = "sha256", serialize = FALSE)
}

.delivery_project_source_state = function(project_root) {
  project_root = .delivery_existing_directory(project_root, "project root")
  relative = c("DESCRIPTION", "NAMESPACE", "_pkgdown.yml")
  rules = list(
    R = "r",
    inst = c("r", "md", "yml", "yaml", "json", "csv"),
    "tests/testthat" = "r",
    analysis = c("r", "md", "yml", "yaml", "example")
  )
  for (root in names(rules)) {
    directory = file.path(project_root, root)
    if (!dir.exists(directory)) {
      next
    }
    paths = .delivery_files(directory)
    paths = paths[tolower(tools::file_ext(paths)) %in% rules[[root]]]
    entries = vapply(paths, .delivery_relative_path, character(1L), root = directory)
    relative = c(relative, file.path(root, entries))
  }
  relative = unique(gsub("\\\\", "/", relative))
  relative = relative[!basename(relative) %in% c(".DS_Store", "Rplots.pdf")]
  paths = file.path(project_root, relative)
  keep = file.exists(paths) & file.info(paths)$isdir %in% FALSE
  relative = relative[keep]
  paths = paths[keep]
  index = order(relative)
  data.frame(
    path = relative[index],
    bytes = as.numeric(file.info(paths[index])$size),
    sha256 = unname(.delivery_sha256(paths[index])),
    stringsAsFactors = FALSE
  )
}

.delivery_validate_producer_sources = function(root, bundle_id, project_source) {
  path = file.path(root, "provenance", "package_source_manifest.csv")
  recorded = .delivery_read_csv(path, paste0(bundle_id, " producer-source manifest"))
  .delivery_assert_columns(recorded, c("path", "bytes", "sha256"), "Producer-source manifest")
  recorded$path = vapply(recorded$path, .delivery_safe_relative_path, character(1L), name = "source path")
  recorded$bytes = suppressWarnings(as.numeric(recorded$bytes))
  recorded$sha256 = tolower(recorded$sha256)
  if (anyNA(recorded$bytes) || anyNA(recorded$sha256) || anyDuplicated(recorded$path) ||
      any(!grepl("^[0-9a-f]{64}$", recorded$sha256))) {
    .delivery_stop(bundle_id, " producer-source manifest is malformed.")
  }
  recorded = recorded[order(recorded$path), c("path", "bytes", "sha256")]
  if (!.delivery_states_identical(recorded, project_source)) {
    comparison = merge(
      recorded[, c("path", "sha256")],
      project_source[, c("path", "sha256")],
      by = "path",
      all = TRUE,
      suffixes = c("_recorded", "_current")
    )
    changed = comparison[
      is.na(comparison$sha256_recorded) | is.na(comparison$sha256_current) |
        comparison$sha256_recorded != comparison$sha256_current,
      "path"
    ]
    detail = paste(head(changed, 10L), collapse = ", ")
    if (length(changed) > 10L) detail = paste0(detail, ", ...")
    .delivery_stop(
      bundle_id,
      " was not produced by the current package and analysis sources. Changed paths: ",
      detail
    )
  }
  .delivery_tree_hash(recorded)
}

.delivery_states_identical = function(left, right) {
  identical(left$path, right$path) && identical(left$bytes, right$bytes) && identical(left$sha256, right$sha256)
}

.delivery_copy_tree = function(source, destination) {
  source = .delivery_existing_directory(source, "copy source")
  .delivery_assert_no_symlinks(source, "Copy source")
  if (file.exists(destination) || dir.exists(destination)) {
    .delivery_stop("Copy destination already exists: ", destination)
  }
  if (!dir.create(destination, recursive = TRUE, showWarnings = FALSE)) {
    .delivery_stop("Could not create copy destination: ", destination)
  }

  directories = list.dirs(source, recursive = TRUE, full.names = TRUE)
  directories = directories[directories != source]
  if (length(directories)) {
    relative = vapply(directories, .delivery_relative_path, character(1L), root = source)
    created = vapply(
      file.path(destination, relative),
      dir.create,
      logical(1L),
      recursive = TRUE,
      showWarnings = FALSE
    )
    if (any(!created & !dir.exists(file.path(destination, relative)))) {
      .delivery_stop("Could not reproduce the directory structure at: ", destination)
    }
  }

  files = .delivery_files(source)
  if (length(files)) {
    relative = vapply(files, .delivery_relative_path, character(1L), root = source)
    copied = file.copy(
      from = files,
      to = file.path(destination, relative),
      overwrite = FALSE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
    if (!all(copied)) {
      .delivery_stop("Could not copy every file into: ", destination)
    }
  }
  invisible(destination)
}

.delivery_read_csv = function(path, name) {
  if (!file.exists(path)) {
    .delivery_stop("Missing ", name, ": ", path)
  }
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(error) .delivery_stop("Could not read ", name, ": ", conditionMessage(error))
  )
}

.delivery_assert_columns = function(data, columns, name) {
  missing = setdiff(columns, names(data))
  if (length(missing)) {
    .delivery_stop(name, " is missing columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

.delivery_boolean = function(value, name) {
  if (length(value) != 1L || is.na(value)) {
    .delivery_stop("`", name, "` must contain exactly one explicit TRUE or FALSE value.")
  }
  normalized = toupper(trimws(as.character(value)))
  if (!normalized %in% c("TRUE", "FALSE")) {
    .delivery_stop("`", name, "` must contain exactly one explicit TRUE or FALSE value.")
  }
  identical(normalized, "TRUE")
}

.delivery_release_status = function(
    structural_status,
    run_mode,
    manuscript_ready,
    release_eligibility,
    eligibility_blockers,
    name) {
  fields = list(structural_status, run_mode, release_eligibility, eligibility_blockers)
  valid_fields = vapply(
    fields,
    function(value) length(value) == 1L && !is.na(value) && nzchar(trimws(as.character(value))),
    logical(1L)
  )
  if (!all(valid_fields)) {
    .delivery_stop(name, " has missing or non-scalar release-status values.")
  }
  structural_status = toupper(trimws(as.character(structural_status)))
  run_mode = toupper(trimws(as.character(run_mode)))
  release_eligibility = toupper(trimws(as.character(release_eligibility)))
  eligibility_blockers = trimws(as.character(eligibility_blockers))
  manuscript_ready = .delivery_boolean(manuscript_ready, paste0(name, " manuscript_ready"))

  if (!identical(structural_status, "PASS")) {
    .delivery_stop(name, " must record structural_status=PASS, received ", structural_status, ".")
  }
  if (!run_mode %in% c("PUBLICATION", "SMOKE_TEST")) {
    .delivery_stop(name, " has an invalid run_mode: ", run_mode, ".")
  }
  if (!release_eligibility %in% c("ELIGIBLE", "NOT_ELIGIBLE")) {
    .delivery_stop(name, " has an invalid release_eligibility: ", release_eligibility, ".")
  }
  if (identical(release_eligibility, "ELIGIBLE") &&
      (!manuscript_ready || !identical(run_mode, "PUBLICATION") || tolower(eligibility_blockers) != "none")) {
    .delivery_stop(
      name,
      " records ELIGIBLE without publication mode, manuscript_ready=TRUE, and eligibility_blockers=none."
    )
  }
  if (identical(release_eligibility, "NOT_ELIGIBLE") && tolower(eligibility_blockers) == "none") {
    .delivery_stop(name, " records NOT_ELIGIBLE without explicit eligibility blockers.")
  }

  list(
    structural_status = structural_status,
    run_mode = run_mode,
    manuscript_ready = manuscript_ready,
    release_eligibility = release_eligibility,
    eligibility_blockers = eligibility_blockers
  )
}

.delivery_validate_output_audit_summary = function(root, bundle_id) {
  path = file.path(root, "provenance", "output_audit_summary.csv")
  summary = .delivery_read_csv(path, paste0(bundle_id, " output-audit summary"))
  required = c(
    "structural_status", "run_mode", "manuscript_ready", "release_eligibility", "eligibility_blockers"
  )
  .delivery_assert_columns(summary, required, paste0(bundle_id, " output-audit summary"))
  if (nrow(summary) != 1L || anyDuplicated(names(summary))) {
    .delivery_stop(bundle_id, " output-audit summary must contain one row with unique column names.")
  }
  .delivery_release_status(
    structural_status = summary$structural_status[[1L]],
    run_mode = summary$run_mode[[1L]],
    manuscript_ready = summary$manuscript_ready[[1L]],
    release_eligibility = summary$release_eligibility[[1L]],
    eligibility_blockers = summary$eligibility_blockers[[1L]],
    name = paste0(bundle_id, " output-audit summary")
  )
}

.delivery_validate_upload_manifest = function(root) {
  manifest_path = file.path(root, "UPLOAD_MANIFEST.csv")
  manifest = .delivery_read_csv(manifest_path, "upload manifest")
  .delivery_assert_columns(manifest, c("path", "bytes", "sha256"), "Upload manifest")
  if (!nrow(manifest)) {
    .delivery_stop("Upload manifest is empty: ", manifest_path)
  }
  manifest$path = vapply(manifest$path, .delivery_safe_relative_path, character(1L), name = "manifest path")
  if (anyDuplicated(manifest$path)) {
    .delivery_stop("Upload manifest contains duplicate paths: ", manifest_path)
  }
  if (anyNA(manifest$sha256) || any(!grepl("^[0-9A-Fa-f]{64}$", manifest$sha256))) {
    .delivery_stop("Upload manifest contains invalid SHA-256 values: ", manifest_path)
  }
  paths = file.path(root, manifest$path)
  missing = manifest$path[!file.exists(paths)]
  if (length(missing)) {
    .delivery_stop("Upload manifest references missing files: ", paste(missing, collapse = ", "))
  }
  if (any(file.info(paths)$isdir %in% TRUE)) {
    .delivery_stop("Upload manifest must contain files only: ", manifest_path)
  }
  actual_bytes = as.numeric(file.info(paths)$size)
  expected_bytes = suppressWarnings(as.numeric(manifest$bytes))
  if (anyNA(expected_bytes) || any(actual_bytes != expected_bytes)) {
    .delivery_stop("Upload manifest byte counts do not match the input bundle: ", root)
  }
  actual_sha256 = unname(.delivery_sha256(paths))
  if (any(actual_sha256 != tolower(manifest$sha256))) {
    .delivery_stop("Upload manifest SHA-256 values do not match the input bundle: ", root)
  }

  actual_relative = .delivery_tree_state(root)$path
  expected_relative = sort(c(manifest$path, "UPLOAD_MANIFEST.csv"))
  if (!identical(actual_relative, expected_relative)) {
    .delivery_stop("Upload manifest does not cover the complete input bundle: ", root)
  }
  manifest
}

.delivery_validate_input_bundle = function(root, bundle_id, project_source) {
  root = .delivery_existing_directory(root, paste0(bundle_id, " upload"))
  .delivery_assert_no_symlinks(root, paste0(bundle_id, " upload"))
  required = c("figures", "tables", "provenance")
  missing_directories = required[!dir.exists(file.path(root, required))]
  if (length(missing_directories)) {
    .delivery_stop(bundle_id, " upload is missing directories: ", paste(missing_directories, collapse = ", "))
  }
  metadata_path = file.path(root, "provenance", "figure_latex_metadata.csv")
  if (!file.exists(metadata_path)) {
    .delivery_stop(bundle_id, " upload is missing figure metadata: ", metadata_path)
  }
  manifest = .delivery_validate_upload_manifest(root)
  audit_status = .delivery_validate_output_audit_summary(root, bundle_id)
  state = .delivery_tree_state(root)
  producer_source_hash = .delivery_validate_producer_sources(root, bundle_id, project_source)
  list(
    root = root,
    manifest = manifest,
    state = state,
    tree_hash = .delivery_tree_hash(state),
    manifest_sha256 = unname(.delivery_sha256(file.path(root, "UPLOAD_MANIFEST.csv"))),
    producer_source_hash = producer_source_hash,
    audit_status = audit_status
  )
}

.delivery_metadata_value = function(data, name, default = "") {
  if (name %in% names(data)) data[[name]] else rep(default, nrow(data))
}

.delivery_prefix_paths = function(values, prefix) {
  vapply(
    values,
    function(value) {
      if (is.na(value) || !nzchar(trimws(value))) {
        return("")
      }
      parts = trimws(strsplit(value, "|", fixed = TRUE)[[1L]])
      parts = parts[nzchar(parts)]
      parts = vapply(parts, .delivery_safe_relative_path, character(1L), name = "source table path")
      paste(file.path(prefix, parts), collapse = " | ")
    },
    character(1L)
  )
}

.delivery_collect_figure_metadata = function(bundle, bundle_id, destination) {
  source_path = file.path(bundle$root, "provenance", "figure_latex_metadata.csv")
  data = .delivery_read_csv(source_path, paste0(bundle_id, " figure metadata"))
  required = c("figure_id", "latex_group_id", "panel_order", "pdf_path", "png_path", "source_tables")
  .delivery_assert_columns(data, required, paste0(bundle_id, " figure metadata"))
  if (!nrow(data)) {
    .delivery_stop(bundle_id, " figure metadata is empty.")
  }
  if (anyNA(data$figure_id) || any(!nzchar(data$figure_id)) || anyDuplicated(data$figure_id)) {
    .delivery_stop(bundle_id, " figure IDs must be non-empty and unique.")
  }
  if (anyNA(data$latex_group_id) || any(!nzchar(data$latex_group_id))) {
    .delivery_stop(bundle_id, " LaTeX group IDs must be non-empty.")
  }

  pdf_path = vapply(data$pdf_path, .delivery_safe_relative_path, character(1L), name = "PDF path")
  png_path = vapply(data$png_path, .delivery_safe_relative_path, character(1L), name = "PNG path")
  if (any(tolower(tools::file_ext(pdf_path)) != "pdf") || any(tolower(tools::file_ext(png_path)) != "png")) {
    .delivery_stop(bundle_id, " figure metadata must link one PDF and one PNG per component.")
  }
  linked = c(pdf_path, png_path)
  missing_figures = linked[!file.exists(file.path(bundle$root, linked))]
  if (length(missing_figures)) {
    .delivery_stop(bundle_id, " figure metadata references missing files: ", paste(missing_figures, collapse = ", "))
  }

  source_tables = .delivery_metadata_value(data, "source_tables")
  table_parts = unique(trimws(unlist(strsplit(source_tables, "|", fixed = TRUE))))
  table_parts = table_parts[nzchar(table_parts)]
  if (length(table_parts)) {
    table_parts = vapply(table_parts, .delivery_safe_relative_path, character(1L), name = "source table path")
    missing_tables = table_parts[!file.exists(file.path(bundle$root, table_parts))]
    if (length(missing_tables)) {
      .delivery_stop(
        bundle_id,
        " figure metadata references missing source tables: ",
        paste(missing_tables, collapse = ", ")
      )
    }
  }

  panel_order = suppressWarnings(as.integer(data$panel_order))
  if (anyNA(panel_order) || any(panel_order < 1L)) {
    .delivery_stop(bundle_id, " panel_order must contain positive integers.")
  }
  if (anyDuplicated(paste(data$latex_group_id, panel_order, sep = "::"))) {
    .delivery_stop(bundle_id, " panel_order must be unique within each LaTeX group.")
  }
  output = data.frame(
    component_id = paste(tolower(bundle_id), data$figure_id, sep = "__"),
    bundle_id = bundle_id,
    study = .delivery_metadata_value(data, "study", bundle_id),
    figure_id = data$figure_id,
    latex_group_id = data$latex_group_id,
    panel_id = .delivery_metadata_value(data, "panel_id"),
    panel_order = panel_order,
    panel_heading = .delivery_metadata_value(data, "panel_heading"),
    pdf_path = file.path(destination, pdf_path),
    png_path = file.path(destination, png_path),
    title = .delivery_metadata_value(data, "title"),
    subtitle = .delivery_metadata_value(data, "subtitle"),
    caption = .delivery_metadata_value(data, "caption"),
    group_title = .delivery_metadata_value(data, "group_title"),
    group_subtitle = .delivery_metadata_value(data, "group_subtitle"),
    group_caption = .delivery_metadata_value(data, "group_caption"),
    source_tables = .delivery_prefix_paths(source_tables, destination),
    width_in = .delivery_metadata_value(data, "width_in", NA_real_),
    height_in = .delivery_metadata_value(data, "height_in", NA_real_),
    role = .delivery_metadata_value(data, "role"),
    estimand_or_scope = .delivery_metadata_value(data, "estimand_or_scope"),
    uncertainty_semantics = .delivery_metadata_value(data, "uncertainty_semantics"),
    privacy_scope = .delivery_metadata_value(data, "privacy_scope"),
    latex_assembly_note = .delivery_metadata_value(data, "latex_assembly_note"),
    source_metadata = file.path(destination, "provenance", "figure_latex_metadata.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  output
}

.delivery_code_files = function(project_root) {
  exact = c(
    ".Rbuildignore",
    "DESCRIPTION",
    "LICENSE",
    "NAMESPACE",
    "NEWS.md",
    "README.Rmd",
    "README.md",
    "_pkgdown.yml",
    "tests/testthat.R",
    "inst/CITATION"
  )
  rules = list(
    R = c("r"),
    analysis = c("r", "md", "yml", "yaml", "example"),
    "tests/testthat" = c("r"),
    inst = c("r", "md", "yml", "yaml", "json"),
    man = c("rd"),
    vignettes = c("rmd")
  )

  relative = exact[file.exists(file.path(project_root, exact))]
  for (directory in names(rules)) {
    root = file.path(project_root, directory)
    if (!dir.exists(root)) {
      next
    }
    files = .delivery_files(root)
    extensions = tolower(tools::file_ext(files))
    files = files[extensions %in% rules[[directory]]]
    if (length(files)) {
      relative = c(relative, vapply(files, .delivery_relative_path, character(1L), root = project_root))
    }
  }
  sort(unique(gsub("\\\\", "/", relative)))
}

.delivery_snapshot_code = function(project_root, destination) {
  project_root = .delivery_existing_directory(project_root, "project root")
  required = "DESCRIPTION"
  missing = required[!file.exists(file.path(project_root, required))]
  if (length(missing)) {
    .delivery_stop("Project root is missing required package files: ", paste(missing, collapse = ", "))
  }
  relative = .delivery_code_files(project_root)
  if (!length(relative)) {
    .delivery_stop("No package or analysis source files were found.")
  }
  if (!dir.create(destination, recursive = TRUE, showWarnings = FALSE)) {
    .delivery_stop("Could not create code snapshot directory: ", destination)
  }
  source = file.path(project_root, relative)
  source_state = data.frame(
    source_path = relative,
    bytes = as.numeric(file.info(source)$size),
    sha256 = unname(.delivery_sha256(source)),
    stringsAsFactors = FALSE
  )
  parents = unique(dirname(file.path(destination, relative)))
  invisible(vapply(parents, dir.create, logical(1L), recursive = TRUE, showWarnings = FALSE))
  copied = file.copy(source, file.path(destination, relative), overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
  if (!all(copied)) {
    .delivery_stop("Could not copy every package and analysis source file.")
  }
  copied_sha256 = unname(.delivery_sha256(file.path(destination, relative)))
  if (any(copied_sha256 != source_state$sha256)) {
    .delivery_stop("Code snapshot differs from the current source tree.")
  }
  write.csv(source_state, file.path(destination, "CODE_MANIFEST.csv"), row.names = FALSE, na = "")
  source_state
}

.delivery_bundle_ledger = function(specifications, validated) {
  rows = lapply(
    seq_along(specifications),
    function(index) {
      specification = specifications[[index]]
      bundle = validated[[index]]
      data.frame(
        bundle_id = specification$id,
        destination = specification$destination,
        structural_status = bundle$audit_status$structural_status,
        run_mode = bundle$audit_status$run_mode,
        manuscript_ready = bundle$audit_status$manuscript_ready,
        release_eligibility = bundle$audit_status$release_eligibility,
        eligibility_blockers = bundle$audit_status$eligibility_blockers,
        file_count = nrow(bundle$state),
        bytes = sum(bundle$state$bytes),
        tree_sha256 = bundle$tree_hash,
        upload_manifest_sha256 = bundle$manifest_sha256,
        producer_source_tree_sha256 = bundle$producer_source_hash,
        stringsAsFactors = FALSE
      )
    }
  )
  do.call(rbind, rows)
}

.delivery_bundle_status_lines = function(bundle_ledger, markdown = FALSE) {
  vapply(seq_len(nrow(bundle_ledger)), function(index) {
    row = bundle_ledger[index, , drop = FALSE]
    prefix = if (markdown) paste0("- `", row$bundle_id, "`: ") else paste0(row$bundle_id, ": ")
    paste0(
      prefix,
      row$release_eligibility,
      "; run_mode=", row$run_mode,
      "; manuscript_ready=", toupper(as.character(row$manuscript_ready)),
      "; blockers=", row$eligibility_blockers
    )
  }, character(1L))
}

.delivery_readme = function(timestamp, has_evidence, bundle_ledger) {
  evidence_line = if (has_evidence) {
    "- `EVIDENCE_SYNTHESIS/` contains the complementary evidence synthesis."
  } else {
    "- No complementary evidence synthesis was supplied for this delivery."
  }
  review_required = any(bundle_ledger$release_eligibility == "NOT_ELIGIBLE")
  title = if (review_required) {
    "# Analysis results and review delivery"
  } else {
    "# Analysis results delivery"
  }
  status_notice = if (review_required) {
    c(
      "**Analyst review is required. At least one included bundle is explicitly `NOT_ELIGIBLE`.**",
      "This delivery is intended for analysis review and paper updating; it is not a manuscript-release package."
    )
  } else {
    "**Every included bundle explicitly records `ELIGIBLE`; consult each audit before manuscript use.**"
  }
  c(
    title,
    "",
    paste0("Built at ", timestamp, "."),
    "",
    status_notice,
    "",
    "## Bundle release status",
    "",
    .delivery_bundle_status_lines(bundle_ledger, markdown = TRUE),
    "",
    paste(
      "Checksum, manifest, source-identity, and archive checks establish file integrity only; they do not certify",
      "manuscript readiness, scientific validity, or resolution of the blockers listed above."
    ),
    "",
    "## Contents",
    "",
    "- `STUDY_RESULTS/SHILD/` contains the structurally audited SHILD analysis output.",
    "- `STUDY_RESULTS/PISA/` contains the structurally audited PISA analysis output.",
    evidence_line,
    paste(
      "- `ANALYSIS_CODE/` is a checksum-indexed snapshot of the package and analysis sources verified against",
      "every supplied result bundle."
    ),
    "- `FIGURE_LATEX_METADATA.csv` maps every figure component to its files, source tables, and LaTeX text.",
    "- `INPUT_BUNDLES.csv` records the identity of each supplied result bundle without local filesystem paths.",
    "- `DELIVERY_QA.csv`, `DELIVERY_MANIFEST.csv`, and `SHA256SUMS.txt` record integrity checks.",
    "",
    "## Figure assembly",
    "",
    "Figures are stored as separate PDF and PNG components. Rows sharing a `latex_group_id` belong to one LaTeX",
    "figure; arrange them by `panel_order` and use the recorded headings and captions outside the graphics.",
    "",
    "## Integrity",
    "",
    "Each supplied result tree is copied byte for byte and checked against its upload manifest. The delivery manifest",
    "covers every regular file except itself and `SHA256SUMS.txt`; the checksum file also records the manifest hash.",
    "The delivery directory is committed only after the ZIP archive extracts to a byte-identical directory tree.",
    "That round-trip confirms archive integrity, not scientific or manuscript-release eligibility."
  )
}

.delivery_category = function(path) {
  result = if (startsWith(path, "STUDY_RESULTS/SHILD/")) {
    "SHILD"
  } else if (startsWith(path, "STUDY_RESULTS/PISA/")) {
    "PISA"
  } else if (startsWith(path, "EVIDENCE_SYNTHESIS/")) {
    "EVIDENCE_SYNTHESIS"
  } else if (startsWith(path, "ANALYSIS_CODE/")) {
    "ANALYSIS_CODE"
  } else {
    "DELIVERY"
  }
  result
}

.delivery_write_manifest = function(root) {
  exclusions = c("DELIVERY_MANIFEST.csv", "SHA256SUMS.txt")
  state = .delivery_tree_state(root, exclude = exclusions)
  state$category = vapply(state$path, .delivery_category, character(1L))
  state = state[, c("path", "category", "bytes", "sha256")]
  write.csv(state, file.path(root, "DELIVERY_MANIFEST.csv"), row.names = FALSE, na = "")

  manifest_hash = unname(.delivery_sha256(file.path(root, "DELIVERY_MANIFEST.csv")))
  sums = rbind(
    state[, c("path", "sha256")],
    data.frame(path = "DELIVERY_MANIFEST.csv", sha256 = manifest_hash, stringsAsFactors = FALSE)
  )
  sums = sums[order(sums$path), , drop = FALSE]
  writeLines(paste0(sums$sha256, "  ", sums$path), file.path(root, "SHA256SUMS.txt"), useBytes = TRUE)
  state
}

.delivery_validate_checksum_file = function(root) {
  path = file.path(root, "SHA256SUMS.txt")
  lines = readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines) || any(!grepl("^[0-9a-f]{64}  .+$", lines))) {
    .delivery_stop("SHA256SUMS.txt has an invalid format.")
  }
  expected_sha256 = substring(lines, 1L, 64L)
  relative = substring(lines, 67L)
  relative = vapply(relative, .delivery_safe_relative_path, character(1L), name = "checksum path")
  if (anyDuplicated(relative)) {
    .delivery_stop("SHA256SUMS.txt contains duplicate paths.")
  }
  actual_paths = file.path(root, relative)
  if (any(!file.exists(actual_paths))) {
    .delivery_stop("SHA256SUMS.txt references missing files.")
  }
  actual_sha256 = unname(.delivery_sha256(actual_paths))
  if (any(actual_sha256 != expected_sha256)) {
    .delivery_stop("SHA256SUMS.txt does not match the delivery files.")
  }
  invisible(TRUE)
}

.delivery_validate_bundle_ledger = function(bundle_ledger) {
  required = c(
    "bundle_id", "destination", "structural_status", "run_mode", "manuscript_ready",
    "release_eligibility", "eligibility_blockers"
  )
  .delivery_assert_columns(bundle_ledger, required, "Input-bundle ledger")
  if (!nrow(bundle_ledger) || anyDuplicated(names(bundle_ledger)) || anyDuplicated(bundle_ledger$bundle_id)) {
    .delivery_stop("Input-bundle ledger must contain uniquely named columns and unique bundle rows.")
  }
  if (anyNA(bundle_ledger$bundle_id) || any(!nzchar(trimws(bundle_ledger$bundle_id)))) {
    .delivery_stop("Input-bundle ledger contains a missing bundle ID.")
  }
  invisible(vapply(bundle_ledger$destination, .delivery_safe_relative_path, character(1L), name = "destination"))
  for (index in seq_len(nrow(bundle_ledger))) {
    .delivery_release_status(
      structural_status = bundle_ledger$structural_status[[index]],
      run_mode = bundle_ledger$run_mode[[index]],
      manuscript_ready = bundle_ledger$manuscript_ready[[index]],
      release_eligibility = bundle_ledger$release_eligibility[[index]],
      eligibility_blockers = bundle_ledger$eligibility_blockers[[index]],
      name = paste0(bundle_ledger$bundle_id[[index]], " input-bundle ledger row")
    )
  }
  invisible(TRUE)
}

.delivery_validate_directory = function(root) {
  root = .delivery_existing_directory(root, "delivery directory")
  .delivery_assert_no_symlinks(root, "Delivery directory")
  required = c(
    "README.md",
    "INPUT_BUNDLES.csv",
    "FIGURE_LATEX_METADATA.csv",
    "DELIVERY_QA.csv",
    "DELIVERY_MANIFEST.csv",
    "SHA256SUMS.txt"
  )
  missing = required[!file.exists(file.path(root, required))]
  if (length(missing)) {
    .delivery_stop("Delivery is missing required files: ", paste(missing, collapse = ", "))
  }

  manifest = .delivery_read_csv(file.path(root, "DELIVERY_MANIFEST.csv"), "delivery manifest")
  .delivery_assert_columns(manifest, c("path", "category", "bytes", "sha256"), "Delivery manifest")
  manifest$path = vapply(manifest$path, .delivery_safe_relative_path, character(1L), name = "delivery path")
  if (anyDuplicated(manifest$path)) {
    .delivery_stop("Delivery manifest contains duplicate paths.")
  }
  if (anyNA(manifest$sha256) || any(!grepl("^[0-9A-Fa-f]{64}$", manifest$sha256))) {
    .delivery_stop("Delivery manifest contains invalid SHA-256 values.")
  }
  expected = .delivery_tree_state(root, exclude = c("DELIVERY_MANIFEST.csv", "SHA256SUMS.txt"))
  manifest = manifest[order(manifest$path), , drop = FALSE]
  if (!identical(manifest$path, expected$path)) {
    .delivery_stop("Delivery manifest does not cover the complete delivery tree.")
  }
  if (any(as.numeric(manifest$bytes) != expected$bytes) || any(tolower(manifest$sha256) != expected$sha256)) {
    .delivery_stop("Delivery manifest values do not match the delivery tree.")
  }

  bundle_ledger = .delivery_read_csv(file.path(root, "INPUT_BUNDLES.csv"), "input-bundle ledger")
  .delivery_validate_bundle_ledger(bundle_ledger)

  metadata = .delivery_read_csv(file.path(root, "FIGURE_LATEX_METADATA.csv"), "global figure metadata")
  .delivery_assert_columns(metadata, c("component_id", "pdf_path", "png_path", "source_tables"), "Figure metadata")
  if (!nrow(metadata) || anyDuplicated(metadata$component_id)) {
    .delivery_stop("Global figure metadata must contain unique components.")
  }
  figure_paths = c(metadata$pdf_path, metadata$png_path)
  figure_paths = vapply(figure_paths, .delivery_safe_relative_path, character(1L), name = "figure path")
  if (any(!file.exists(file.path(root, figure_paths)))) {
    .delivery_stop("Global figure metadata references missing figure files.")
  }
  .delivery_validate_checksum_file(root)
  invisible(TRUE)
}

.delivery_validate_archive_names = function(zip_path, output_name) {
  archive = utils::unzip(zip_path, list = TRUE)
  if (!nrow(archive)) {
    .delivery_stop("ZIP archive is empty.")
  }
  names = gsub("\\\\", "/", archive$Name)
  invalid = startsWith(names, "/") | grepl("^[A-Za-z]:/", names) | grepl("[[:cntrl:]]", names)
  parts = strsplit(sub("/$", "", names), "/", fixed = TRUE)
  invalid = invalid | vapply(parts, function(value) any(value %in% c("", ".", "..")), logical(1L))
  correct_root = names == output_name | startsWith(names, paste0(output_name, "/"))
  if (any(invalid) || any(!correct_root)) {
    .delivery_stop("ZIP archive contains unsafe paths or files outside its canonical top-level directory.")
  }
  invisible(TRUE)
}

.delivery_validate_archive_round_trip = function(zip_path, source_root, output_name) {
  .delivery_validate_archive_names(zip_path, output_name)
  extraction_root = tempfile("delivery-extract-")
  if (!dir.create(extraction_root, recursive = TRUE, showWarnings = FALSE)) {
    .delivery_stop("Could not create temporary archive extraction directory.")
  }
  on.exit(unlink(extraction_root, recursive = TRUE, force = TRUE), add = TRUE)
  zip::unzip(zip_path, exdir = extraction_root)
  extracted = file.path(extraction_root, output_name)
  .delivery_assert_no_symlinks(extracted, "Extracted delivery")
  source_state = .delivery_tree_state(source_root)
  extracted_state = .delivery_tree_state(extracted)
  if (!.delivery_states_identical(source_state, extracted_state)) {
    .delivery_stop("ZIP archive does not round-trip to a byte-identical delivery tree.")
  }
  invisible(TRUE)
}

.delivery_build = function(shild_upload, pisa_upload, evidence_upload = NULL, output_dir, zip_path, project_root) {
  .delivery_require_package("digest")
  .delivery_require_package("zip")
  project_root = .delivery_existing_directory(project_root, "project root")
  project_source = .delivery_project_source_state(project_root)
  output_dir = .delivery_new_path(output_dir, "output_dir")
  output_name = .delivery_safe_output_name(output_dir)
  zip_path = .delivery_new_path(zip_path, "zip")
  checksum_path = paste0(zip_path, ".sha256")
  if (tolower(tools::file_ext(zip_path)) != "zip") {
    .delivery_stop("`zip` must end in .zip: ", zip_path)
  }
  occupied = c(output_dir, zip_path, checksum_path)
  if (any(file.exists(occupied) | dir.exists(occupied))) {
    .delivery_stop("Output paths must not already exist: ", paste(occupied, collapse = ", "))
  }
  if (.delivery_path_within(zip_path, output_dir) || .delivery_path_within(checksum_path, output_dir)) {
    .delivery_stop("ZIP and checksum paths must be outside the delivery directory.")
  }

  specifications = list(
    list(id = "SHILD", source = shild_upload, destination = "STUDY_RESULTS/SHILD"),
    list(id = "PISA", source = pisa_upload, destination = "STUDY_RESULTS/PISA")
  )
  if (!is.null(evidence_upload) && nzchar(evidence_upload)) {
    specifications[[length(specifications) + 1L]] = list(
      id = "EVIDENCE_SYNTHESIS",
      source = evidence_upload,
      destination = "EVIDENCE_SYNTHESIS"
    )
  }
  validated = Map(
    function(specification) {
      .delivery_validate_input_bundle(specification$source, specification$id, project_source)
    },
    specifications
  )
  input_roots = vapply(validated, `[[`, character(1L), "root")
  if (anyDuplicated(input_roots)) {
    .delivery_stop("Each input bundle must have a distinct directory.")
  }
  if (any(vapply(input_roots, .delivery_paths_overlap, logical(1L), right = output_dir))) {
    .delivery_stop("The delivery output must not overlap an input bundle.")
  }

  staging_parent = tempfile(".delivery-stage-", tmpdir = dirname(output_dir))
  if (!dir.create(staging_parent, recursive = TRUE, showWarnings = FALSE)) {
    .delivery_stop("Could not create the staging directory: ", staging_parent)
  }
  stage = file.path(staging_parent, output_name)
  if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE)) {
    .delivery_stop("Could not create the staged delivery: ", stage)
  }
  temporary_zip = file.path(staging_parent, paste0(output_name, ".zip"))
  published = character()
  success = FALSE
  on.exit({
    if (!success && length(published)) {
      for (path in rev(published)) {
        unlink(path, recursive = dir.exists(path), force = TRUE)
      }
    }
    if (dir.exists(staging_parent)) {
      unlink(staging_parent, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  .delivery_message("Copying manifest-checked analysis bundles")
  metadata = vector("list", length(specifications))
  for (index in seq_along(specifications)) {
    specification = specifications[[index]]
    destination = file.path(stage, specification$destination)
    .delivery_copy_tree(validated[[index]]$root, destination)
    copied_state = .delivery_tree_state(destination)
    if (!.delivery_states_identical(validated[[index]]$state, copied_state)) {
      .delivery_stop(specification$id, " copy differs from its verified input tree.")
    }
    metadata[[index]] = .delivery_collect_figure_metadata(
      validated[[index]],
      specification$id,
      specification$destination
    )
  }

  figure_metadata = do.call(rbind, metadata)
  if (anyDuplicated(figure_metadata$component_id)) {
    .delivery_stop("Global figure component IDs are not unique.")
  }
  group_keys = paste(figure_metadata$bundle_id, figure_metadata$latex_group_id, sep = "::")
  group_sizes = table(group_keys)
  figure_metadata$latex_group_is_multipanel = unname(group_sizes[group_keys]) > 1L
  leading = c("component_id", "bundle_id", "study", "figure_id", "latex_group_id", "latex_group_is_multipanel")
  figure_metadata = figure_metadata[, c(leading, setdiff(names(figure_metadata), leading)), drop = FALSE]
  figure_metadata = figure_metadata[
    order(figure_metadata$bundle_id, figure_metadata$latex_group_id, figure_metadata$panel_order),
    ,
    drop = FALSE
  ]
  write.csv(figure_metadata, file.path(stage, "FIGURE_LATEX_METADATA.csv"), row.names = FALSE, na = "")
  bundle_ledger = .delivery_bundle_ledger(specifications, validated)
  write.csv(
    bundle_ledger,
    file.path(stage, "INPUT_BUNDLES.csv"),
    row.names = FALSE
  )

  .delivery_message("Snapshotting package and analysis sources")
  code_manifest = .delivery_snapshot_code(project_root, file.path(stage, "ANALYSIS_CODE"))
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  writeLines(
    .delivery_readme(
      timestamp,
      any(vapply(specifications, `[[`, character(1L), "id") == "EVIDENCE_SYNTHESIS"),
      bundle_ledger
    ),
    file.path(stage, "README.md"),
    useBytes = TRUE
  )

  current_inputs = Map(
    function(specification) {
      .delivery_validate_input_bundle(specification$source, specification$id, project_source)
    },
    specifications
  )
  unchanged = vapply(
    seq_along(validated),
    function(index) .delivery_states_identical(validated[[index]]$state, current_inputs[[index]]$state),
    logical(1L)
  )
  if (!all(unchanged)) {
    .delivery_stop("An input bundle changed while the delivery was being assembled.")
  }

  group_count = length(unique(paste(figure_metadata$bundle_id, figure_metadata$latex_group_id, sep = "::")))
  review_required = any(bundle_ledger$release_eligibility == "NOT_ELIGIBLE")
  release_status = if (review_required) "REVIEW_REQUIRED" else "PASS"
  release_detail = paste(.delivery_bundle_status_lines(bundle_ledger), collapse = " | ")
  qa = data.frame(
    check = c(
      "required_inputs",
      "input_manifests",
      "bundle_structural_audits",
      "bundle_release_eligibility",
      "integrity_scope",
      "input_copy_identity",
      "figure_metadata",
      "figure_files",
      "analysis_code_snapshot",
      "input_immutability",
      "delivery_manifest",
      "archive_round_trip"
    ),
    status = c("PASS", "PASS", "PASS", release_status, "INFORMATION", rep("PASS", 7L)),
    detail = c(
      paste(length(specifications), "analysis bundles were supplied."),
      paste(sum(vapply(validated, function(bundle) nrow(bundle$manifest), integer(1L))), "manifest rows verified."),
      "Every bundle records structural_status=PASS in a one-row output-audit summary.",
      release_detail,
      "Checksum and archive integrity do not certify manuscript readiness or scientific release eligibility.",
      "Every copied result tree is byte-identical to its verified input tree.",
      paste(nrow(figure_metadata), "unique components in", group_count, "LaTeX groups."),
      "Every metadata row links an existing PDF, PNG, and source table.",
      paste(nrow(code_manifest), "current package and analysis source files copied and checksummed."),
      "Every input tree was reverified immediately before archive construction.",
      "The recursive manifest and checksum index cover the complete declared delivery scope.",
      "The delivery archive safely extracts to a byte-identical directory tree."
    ),
    stringsAsFactors = FALSE
  )
  write.csv(qa, file.path(stage, "DELIVERY_QA.csv"), row.names = FALSE)
  .delivery_write_manifest(stage)
  .delivery_validate_directory(stage)

  .delivery_message("Creating and verifying the ZIP archive")
  zip::zipr(
    zipfile = temporary_zip,
    files = output_name,
    root = staging_parent,
    include_directories = TRUE,
    compression_level = 9L
  )
  .delivery_validate_archive_round_trip(temporary_zip, stage, output_name)

  if (!file.rename(stage, output_dir)) {
    .delivery_stop("Could not commit the integrity-checked delivery directory: ", output_dir)
  }
  published = c(published, output_dir)
  if (!file.copy(temporary_zip, zip_path, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)) {
    .delivery_stop("Could not commit the integrity-checked ZIP archive: ", zip_path)
  }
  published = c(published, zip_path)
  zip_sha256 = unname(.delivery_sha256(zip_path))
  published = c(published, checksum_path)
  writeLines(paste0(zip_sha256, "  ", basename(zip_path)), checksum_path, useBytes = TRUE)

  .delivery_validate_directory(output_dir)
  .delivery_validate_archive_round_trip(zip_path, output_dir, output_name)
  checksum_line = readLines(checksum_path, warn = FALSE)
  expected_line = paste0(zip_sha256, "  ", basename(zip_path))
  if (!identical(checksum_line, expected_line)) {
    .delivery_stop("External ZIP checksum is invalid.")
  }
  success = TRUE
  .delivery_message("Committed delivery directory: ", output_dir)
  .delivery_message("Archive: ", zip_path)
  .delivery_message("SHA-256: ", zip_sha256)
  invisible(list(output_dir = output_dir, zip = zip_path, sha256 = zip_sha256))
}

.delivery_synthetic_audit_summary = function(
    structural_status = "PASS",
    run_mode = "PUBLICATION",
    manuscript_ready = TRUE,
    release_eligibility = "ELIGIBLE",
    eligibility_blockers = "none") {
  data.frame(
    structural_status = structural_status,
    run_mode = run_mode,
    manuscript_ready = manuscript_ready,
    release_eligibility = release_eligibility,
    eligibility_blockers = eligibility_blockers,
    stringsAsFactors = FALSE
  )
}

.delivery_write_synthetic_bundle = function(
    root,
    study,
    figure_id,
    producer_source,
    audit_summary = .delivery_synthetic_audit_summary()) {
  directories = file.path(root, c("figures", "tables", "provenance"))
  invisible(vapply(directories, dir.create, logical(1L), recursive = TRUE, showWarnings = FALSE))
  pdf_path = file.path(root, "figures", paste0(figure_id, ".pdf"))
  png_path = file.path(root, "figures", paste0(figure_id, ".png"))
  writeBin(charToRaw("%PDF-1.4\n%%EOF\n"), pdf_path)
  writeBin(as.raw(c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)), png_path)
  table_path = file.path(root, "tables", paste0(figure_id, "_source.csv"))
  write.csv(data.frame(estimate = 0.5), table_path, row.names = FALSE)
  metadata = data.frame(
    study = study,
    figure_id = figure_id,
    latex_group_id = figure_id,
    panel_id = "a",
    panel_order = 1L,
    panel_heading = "Synthetic component",
    pdf_path = paste0("figures/", figure_id, ".pdf"),
    png_path = paste0("figures/", figure_id, ".png"),
    title = "Synthetic figure",
    subtitle = "",
    caption = "Synthetic caption.",
    group_title = "Synthetic group",
    group_subtitle = "",
    group_caption = "Synthetic group caption.",
    source_tables = paste0("tables/", figure_id, "_source.csv"),
    width_in = 5,
    height_in = 4,
    role = "test",
    estimand_or_scope = "synthetic",
    uncertainty_semantics = "none",
    privacy_scope = "synthetic",
    latex_assembly_note = "Single component.",
    stringsAsFactors = FALSE
  )
  write.csv(metadata, file.path(root, "provenance", "figure_latex_metadata.csv"), row.names = FALSE)
  write.csv(
    producer_source,
    file.path(root, "provenance", "package_source_manifest.csv"),
    row.names = FALSE
  )
  if (!is.null(audit_summary)) {
    write.csv(
      audit_summary,
      file.path(root, "provenance", "output_audit_summary.csv"),
      row.names = FALSE
    )
  }
  writeLines("Synthetic analysis-result bundle.", file.path(root, "UPLOAD_README.md"))
  state = .delivery_tree_state(root)
  paths = file.path(root, state$path)
  manifest = data.frame(
    path = state$path,
    bytes = state$bytes,
    md5 = unname(tools::md5sum(paths)),
    sha256 = state$sha256,
    stringsAsFactors = FALSE
  )
  write.csv(manifest, file.path(root, "UPLOAD_MANIFEST.csv"), row.names = FALSE)
  invisible(root)
}

.delivery_write_synthetic_project = function(root) {
  directories = file.path(root, c("R", "analysis", "tests/testthat", "inst/schema", "man", "vignettes"))
  invisible(vapply(directories, dir.create, logical(1L), recursive = TRUE, showWarnings = FALSE))
  writeLines(c("Package: synthetic", "Version: 0.0.1"), file.path(root, "DESCRIPTION"))
  writeLines("export(synthetic)", file.path(root, "NAMESPACE"))
  writeLines("synthetic = function() TRUE", file.path(root, "R", "synthetic.R"))
  writeLines("value = synthetic()", file.path(root, "analysis", "example.R"))
  writeLines("testthat::expect_true(TRUE)", file.path(root, "tests/testthat", "test-synthetic.R"))
  writeLines("{}", file.path(root, "inst/schema", "synthetic.json"))
  writeLines("Synthetic documentation.", file.path(root, "man", "synthetic.Rd"))
  writeLines("# Synthetic vignette", file.path(root, "vignettes", "synthetic.Rmd"))
  invisible(root)
}

.delivery_self_test = function() {
  root = tempfile("delivery-self-test-")
  if (!dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
    .delivery_stop("Could not create the self-test directory.")
  }
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  project = .delivery_write_synthetic_project(file.path(root, "project"))
  producer_source = .delivery_project_source_state(project)
  expect_error = function(code, pattern) {
    error = tryCatch({
      force(code)
      NULL
    }, error = function(error) error)
    if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
      .delivery_stop("Self-test expected an error containing: ", pattern)
    }
    invisible(error)
  }
  shild = .delivery_write_synthetic_bundle(
    file.path(root, "shild"),
    "SHILD",
    "shild_test",
    producer_source,
    audit_summary = .delivery_synthetic_audit_summary(
      manuscript_ready = FALSE,
      release_eligibility = "NOT_ELIGIBLE",
      eligibility_blockers = "analyst verification not signed"
    )
  )
  pisa = .delivery_write_synthetic_bundle(
    file.path(root, "pisa"),
    "PISA",
    "pisa_test",
    producer_source
  )
  evidence = .delivery_write_synthetic_bundle(
    file.path(root, "evidence"),
    "SYNTHESIS",
    "evidence_test",
    producer_source
  )
  missing_audit = .delivery_write_synthetic_bundle(
    file.path(root, "missing_audit"),
    "MISSING_AUDIT",
    "missing_audit_test",
    producer_source,
    audit_summary = NULL
  )
  expect_error(
    .delivery_validate_input_bundle(missing_audit, "MISSING_AUDIT", producer_source),
    "Missing MISSING_AUDIT output-audit summary"
  )
  malformed_audit = .delivery_write_synthetic_bundle(
    file.path(root, "malformed_audit"),
    "MALFORMED_AUDIT",
    "malformed_audit_test",
    producer_source,
    audit_summary = rbind(.delivery_synthetic_audit_summary(), .delivery_synthetic_audit_summary())
  )
  expect_error(
    .delivery_validate_input_bundle(malformed_audit, "MALFORMED_AUDIT", producer_source),
    "output-audit summary must contain one row"
  )
  failed_structural_audit = .delivery_write_synthetic_bundle(
    file.path(root, "failed_structural_audit"),
    "FAILED_STRUCTURAL_AUDIT",
    "failed_structural_audit_test",
    producer_source,
    audit_summary = .delivery_synthetic_audit_summary(
      structural_status = "FAIL",
      manuscript_ready = FALSE,
      release_eligibility = "NOT_ELIGIBLE",
      eligibility_blockers = "structural audit failed"
    )
  )
  expect_error(
    .delivery_validate_input_bundle(failed_structural_audit, "FAILED_STRUCTURAL_AUDIT", producer_source),
    "must record structural_status=PASS"
  )
  output = file.path(root, "canonical_delivery")
  archive = file.path(root, "canonical_delivery.zip")
  result = .delivery_build(shild, pisa, evidence, output, archive, project)
  metadata = read.csv(file.path(result$output_dir, "FIGURE_LATEX_METADATA.csv"), stringsAsFactors = FALSE)
  if (nrow(metadata) != 3L || !all(c("SHILD", "PISA", "EVIDENCE_SYNTHESIS") %in% metadata$bundle_id)) {
    .delivery_stop("Self-test did not preserve all three synthetic figure components.")
  }
  if (!file.exists(result$zip) || !file.exists(paste0(result$zip, ".sha256"))) {
    .delivery_stop("Self-test did not commit the ZIP archive and checksum.")
  }
  bundle_ledger = read.csv(file.path(result$output_dir, "INPUT_BUNDLES.csv"), stringsAsFactors = FALSE)
  shild_status = bundle_ledger[bundle_ledger$bundle_id == "SHILD", , drop = FALSE]
  if (nrow(shild_status) != 1L || shild_status$release_eligibility != "NOT_ELIGIBLE" ||
      isTRUE(shild_status$manuscript_ready) || shild_status$eligibility_blockers != "analyst verification not signed") {
    .delivery_stop("Self-test did not preserve SHILD's explicit review status and blocker.")
  }
  readme = readLines(file.path(result$output_dir, "README.md"), warn = FALSE, encoding = "UTF-8")
  if (!any(grepl("Analysis results and review delivery", readme, fixed = TRUE)) ||
      !any(grepl("SHILD", readme, fixed = TRUE) & grepl("analyst verification not signed", readme, fixed = TRUE)) ||
      !any(grepl("do not certify manuscript readiness", readme, fixed = TRUE))) {
    .delivery_stop("Self-test delivery README did not state review status, blockers, and integrity scope.")
  }
  eligible_ledger = bundle_ledger
  eligible_ledger$manuscript_ready = TRUE
  eligible_ledger$release_eligibility = "ELIGIBLE"
  eligible_ledger$eligibility_blockers = "none"
  eligible_readme = .delivery_readme("synthetic", TRUE, eligible_ledger)
  if (!identical(eligible_readme[[1L]], "# Analysis results delivery") ||
      any(grepl("Analyst review is required", eligible_readme, fixed = TRUE))) {
    .delivery_stop("Self-test delivery README did not distinguish an all-eligible delivery.")
  }
  qa = read.csv(file.path(result$output_dir, "DELIVERY_QA.csv"), stringsAsFactors = FALSE)
  release_qa = qa[qa$check == "bundle_release_eligibility", , drop = FALSE]
  if (nrow(release_qa) != 1L || release_qa$status != "REVIEW_REQUIRED" ||
      !grepl("analyst verification not signed", release_qa$detail, fixed = TRUE)) {
    .delivery_stop("Self-test delivery QA did not preserve the explicit review requirement.")
  }
  core_output = file.path(root, "canonical_core_delivery")
  core_archive = file.path(root, "canonical_core_delivery.zip")
  core_result = .delivery_build(shild, pisa, output_dir = core_output, zip_path = core_archive, project_root = project)
  core_metadata = read.csv(
    file.path(core_result$output_dir, "FIGURE_LATEX_METADATA.csv"),
    stringsAsFactors = FALSE
  )
  if (nrow(core_metadata) != 2L || dir.exists(file.path(core_result$output_dir, "EVIDENCE_SYNTHESIS"))) {
    .delivery_stop("Self-test did not handle the optional evidence bundle correctly.")
  }
  .delivery_message("Self-test PASS")
  invisible(TRUE)
}

.delivery_usage = function() {
  c(
    "Usage:",
    "  Rscript analysis/build_delivery.R \\",
    "    --shild-upload=/path/to/shild_upload \\",
    "    --pisa-upload=/path/to/pisa_upload \\",
    "    --evidence-upload=/path/to/evidence_upload \\",
    "    --output-dir=/path/to/canonical_delivery \\",
    "    --zip=/path/to/canonical_delivery.zip",
    "",
    "Options:",
    "  --project-root=/path/to/package  Override the automatically detected package root.",
    "  --evidence-upload=PATH           Omit this option when no evidence synthesis is supplied.",
    "  --self-test                      Run a complete synthetic three-bundle build.",
    "  --help                           Print this usage text."
  )
}

.delivery_parse_arguments = function(arguments) {
  values = list()
  flags = character()
  for (argument in arguments) {
    if (argument %in% c("--self-test", "--help")) {
      flags = c(flags, substring(argument, 3L))
      next
    }
    if (!grepl("^--[A-Za-z0-9-]+=.+$", argument)) {
      .delivery_stop("Invalid argument: ", argument, "\n", paste(.delivery_usage(), collapse = "\n"))
    }
    separator = regexpr("=", argument, fixed = TRUE)[[1L]]
    key = substring(argument, 3L, separator - 1L)
    key = gsub("-", "_", key, fixed = TRUE)
    value = substring(argument, separator + 1L)
    if (!is.null(values[[key]])) {
      .delivery_stop("Argument supplied more than once: --", gsub("_", "-", key, fixed = TRUE))
    }
    values[[key]] = value
  }
  values$flags = unique(flags)
  values
}

.delivery_main = function() {
  options = .delivery_parse_arguments(commandArgs(trailingOnly = TRUE))
  if ("help" %in% options$flags) {
    cat(paste(.delivery_usage(), collapse = "\n"), "\n")
    return(invisible(TRUE))
  }
  if ("self-test" %in% options$flags) {
    return(.delivery_self_test())
  }
  required = c("shild_upload", "pisa_upload", "output_dir", "zip")
  missing = required[vapply(required, function(name) is.null(options[[name]]), logical(1L))]
  if (length(missing)) {
    .delivery_stop(
      "Missing required arguments: ",
      paste(missing, collapse = ", "),
      "\n",
      paste(.delivery_usage(), collapse = "\n")
    )
  }
  allowed = c(required, "evidence_upload", "project_root", "flags")
  unknown = setdiff(names(options), allowed)
  if (length(unknown)) {
    .delivery_stop("Unknown arguments: ", paste(unknown, collapse = ", "))
  }
  project_root = options$project_root
  if (is.null(project_root)) {
    project_root = .delivery_default_project_root()
  }
  .delivery_build(
    shild_upload = options$shild_upload,
    pisa_upload = options$pisa_upload,
    evidence_upload = options$evidence_upload,
    output_dir = options$output_dir,
    zip_path = options$zip,
    project_root = project_root
  )
}

if (sys.nframe() == 0L) {
  .delivery_main()
}
