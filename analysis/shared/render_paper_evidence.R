# Regenerate evidence-synthesis figure components from their archived aggregate tables.

.paper_evidence_renderer_path = local({
  source_path = tryCatch(sys.frame(1L)$ofile, error = function(condition) "")
  if (is.null(source_path) || !length(source_path) || is.na(source_path) || !nzchar(source_path)) {
    source_path = file.path("analysis", "shared", "render_paper_evidence.R")
  }
  normalizePath(source_path, winslash = "/", mustWork = FALSE)
})

.paper_evidence_source_tables = c(
  "tables/pisa_evidence_boundaries.csv",
  "tables/pisa_performance_explanation_sensitivity.csv",
  "tables/shild_g4_local_fidelity_bandwidth_cases.csv",
  "tables/shild_g4_local_fidelity_cases.csv",
  "tables/shild_performance_explanation_sensitivity.csv"
)

.paper_evidence_quantity_ids = c(
  "performance_tolerance_used",
  "practical_pair_reversals",
  "importance_weighted_reversals",
  "top_five_overlap"
)

.paper_evidence_figure_ids = c(
  paste0(
    "shild_g4_systematic_local_audit__",
    c(
      "apparent_vs_cross_fitted",
      "relative_fidelity_by_prediction",
      "absolute_fidelity_by_prediction",
      "locality_curve"
    )
  ),
  unlist(lapply(c("shild", "pisa"), function(study) {
    paste0("performance_explanation_sensitivity__", study, "__", .paper_evidence_quantity_ids)
  }), use.names = FALSE),
  "pisa_evidence_boundaries"
)

.paper_evidence_project_root = function() {
  source_root = dirname(dirname(dirname(.paper_evidence_renderer_path)))
  candidates = unique(normalizePath(
    c(source_root, ".", "..", "../.."),
    winslash = "/",
    mustWork = FALSE
  ))
  is_root = vapply(candidates, function(path) {
    file.exists(file.path(path, "DESCRIPTION")) &&
      file.exists(file.path(path, "analysis", "shared", "study_helpers.R")) &&
      file.exists(file.path(path, "analysis", "shared", "evidence_helpers.R"))
  }, logical(1L))
  if (!any(is_root)) {
    stop("Could not locate the package root for the evidence renderer.", call. = FALSE)
  }
  candidates[is_root][[1L]]
}

.paper_evidence_assert_directory_argument = function(path, name, must_work) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop(sprintf("`%s` must be one non-empty directory path.", name), call. = FALSE)
  }
  if (must_work && !dir.exists(path)) {
    stop(sprintf("`%s` does not exist: %s", name, path), call. = FALSE)
  }
  invisible(TRUE)
}

.paper_evidence_pisa_boundary_plot = function(boundaries, helper) {
  plot = helper$.evidence_pisa_boundary_plot(boundaries)
  suppressMessages(
    plot +
      scale_fill_manual(
        values = c(Outcome = "#F2F2F2", Model = "#BDBDBD", Setting = "#595959"),
        guide = "none"
      ) +
      scale_color_manual(
        values = c(Outcome = "#111111", Model = "#111111", Setting = "#FFFFFF"),
        guide = "none"
      )
  )
}

.paper_evidence_assert_monochrome = function(plot, figure_id) {
  built = ggplot_build(plot)
  channels = unlist(lapply(built$data, function(layer) {
    columns = intersect(c("colour", "fill"), names(layer))
    unlist(layer[, columns, drop = FALSE], use.names = FALSE)
  }), use.names = FALSE)
  channels = unique(as.character(channels[!is.na(channels)]))
  channels = channels[nzchar(channels) & channels != "transparent"]
  if (!length(channels)) return(invisible(TRUE))
  rgb = tryCatch(
    grDevices::col2rgb(channels),
    error = function(condition) {
      stop("Could not validate the rendered color channels for ", figure_id, ".", call. = FALSE)
    }
  )
  if (any(rgb[1L, ] != rgb[2L, ] | rgb[2L, ] != rgb[3L, ])) {
    stop("Figure is not achromatic: ", figure_id, ".", call. = FALSE)
  }
  invisible(TRUE)
}

.paper_evidence_metadata_sources = function(metadata) {
  source_tables = trimws(unlist(strsplit(metadata$source_tables, "|", fixed = TRUE), use.names = FALSE))
  sort(unique(sub(";.*$", "", source_tables)))
}

.paper_evidence_common_metadata = function(metadata) {
  fields = c(
    "study", "figure_group_id", "is_grouped_figure", "latex_group_id", "group_title",
    "group_subtitle", "group_caption", "generator_script", "generator_function", "latex_assembly_note"
  )
  result = lapply(fields, function(field) {
    values = unique(metadata[[field]])
    values = values[!is.na(values)]
    if (!length(values)) NA else values[[1L]]
  })
  names(result) = fields
  result
}

render_paper_evidence = function(parent_dir, output_dir) {
  .paper_evidence_assert_directory_argument(parent_dir, "parent_dir", must_work = TRUE)
  .paper_evidence_assert_directory_argument(output_dir, "output_dir", must_work = FALSE)

  required_packages = c("data.table", "devtools", "ggplot2")
  missing_packages = required_packages[
    !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(missing_packages)) {
    stop(
      "Install the required renderer packages: ",
      paste(missing_packages, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  project_root = .paper_evidence_project_root()
  library(mlr3autoiml)
  suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
  })

  helper = new.env(parent = globalenv())
  sys.source(file.path(project_root, "analysis", "shared", "study_helpers.R"), envir = helper)
  sys.source(file.path(project_root, "analysis", "shared", "evidence_helpers.R"), envir = helper)
  helper$.analysis_palette = function() autoiml_palette(style = "monochrome")

  parent_dir = normalizePath(parent_dir, winslash = "/", mustWork = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir = normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  if (identical(parent_dir, output_dir)) {
    stop("`output_dir` must differ from the archived evidence input directory.", call. = FALSE)
  }

  metadata_path = file.path(parent_dir, "provenance", "figure_latex_metadata.csv")
  if (!file.exists(metadata_path)) {
    stop("Evidence figure metadata are missing: ", metadata_path, call. = FALSE)
  }
  metadata = fread(metadata_path)
  missing_metadata_columns = setdiff(helper$.analysis_figure_metadata_columns, names(metadata))
  if (length(missing_metadata_columns)) {
    stop(
      "Evidence figure metadata are missing columns: ",
      paste(missing_metadata_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (nrow(metadata) != length(.paper_evidence_figure_ids) || anyNA(metadata$figure_id) ||
      anyDuplicated(metadata$figure_id) || !setequal(metadata$figure_id, .paper_evidence_figure_ids)) {
    stop("Evidence metadata must register the expected 13 unique figure components.", call. = FALSE)
  }
  if (any(!is.finite(metadata$width_in)) || any(metadata$width_in <= 0) ||
      any(!is.finite(metadata$height_in)) || any(metadata$height_in <= 0)) {
    stop("Evidence metadata contain invalid component dimensions.", call. = FALSE)
  }

  source_tables = .paper_evidence_metadata_sources(metadata)
  if (!identical(source_tables, sort(.paper_evidence_source_tables))) {
    stop("Evidence metadata do not reference the expected five aggregate source tables.", call. = FALSE)
  }
  source_paths = file.path(parent_dir, source_tables)
  helper$.require_files(source_paths)
  tables = setNames(lapply(source_paths, fread), sub("\\.csv$", "", basename(source_paths)))

  performance_source = rbindlist(list(
    tables$shild_performance_explanation_sensitivity,
    tables$pisa_performance_explanation_sensitivity
  ), use.names = TRUE, fill = TRUE)
  systematic_panels = helper$.evidence_systematic_g4_panels(
    tables$shild_g4_local_fidelity_cases,
    tables$shild_g4_local_fidelity_bandwidth_cases
  )
  performance_plots = setNames(lapply(c("SHILD", "PISA"), function(study_name) {
    helper$.evidence_performance_explanation_plot(performance_source, study_name)
  }), c("SHILD", "PISA"))
  pisa_boundary_plot = .paper_evidence_pisa_boundary_plot(tables$pisa_evidence_boundaries, helper)
  plots = c(systematic_panels, performance_plots, list(pisa_evidence_boundaries = pisa_boundary_plot))
  invisible(Map(.paper_evidence_assert_monochrome, plots, names(plots)))

  figures_dir = file.path(output_dir, "figures")
  helper$.analysis_reset_figure_registry()
  helper$.analysis_clear_figure_files(figures_dir)

  systematic_metadata = metadata[
    figure_group_id == "shild_g4_systematic_local_audit"
  ][order(panel_order)]
  if (nrow(systematic_metadata) != 4L ||
      !identical(systematic_metadata$figure_id, paste0(
        "shild_g4_systematic_local_audit__",
        names(systematic_panels)
      ))) {
    stop("Systematic local-audit metadata do not define the expected four panels.", call. = FALSE)
  }
  for (index in seq_len(nrow(systematic_metadata))) {
    row = systematic_metadata[index]
    helper$.save_plot(
      systematic_panels[[index]],
      file.path(figures_dir, row$figure_id),
      width = row$width_in,
      height = row$height_in,
      metadata = as.list(row[, setdiff(names(row), c("figure_id", "pdf_path", "png_path")), with = FALSE])
    )
  }

  for (study_name in c("SHILD", "PISA")) {
    study_key = tolower(study_name)
    study_metadata = metadata[
      study == study_name & figure_group_id == "performance_explanation_sensitivity"
    ][order(panel_order)]
    if (nrow(study_metadata) != 4L || length(unique(study_metadata$width_in)) != 1L ||
        length(unique(study_metadata$height_in)) != 1L) {
      stop(study_name, " performance-explanation metadata do not define four equal components.", call. = FALSE)
    }
    helper$.save_plot(
      performance_plots[[study_name]],
      file.path(figures_dir, paste0("performance_explanation_sensitivity__", study_key)),
      width = sum(study_metadata$width_in),
      height = unique(study_metadata$height_in),
      metadata = .paper_evidence_common_metadata(study_metadata),
      min_component_width = unique(study_metadata$width_in),
      min_component_height = unique(study_metadata$height_in),
      align_component_widths = FALSE
    )
  }

  boundary_metadata = metadata[figure_id == "pisa_evidence_boundaries"]
  helper$.save_plot(
    pisa_boundary_plot,
    file.path(figures_dir, "pisa_evidence_boundaries"),
    width = boundary_metadata$width_in,
    height = boundary_metadata$height_in,
    metadata = as.list(
      boundary_metadata[, setdiff(names(boundary_metadata), c("figure_id", "pdf_path", "png_path")), with = FALSE]
    )
  )

  rendered = helper$.analysis_figure_registry_table()
  if (nrow(rendered) != nrow(metadata) || anyDuplicated(rendered$figure_id) ||
      !setequal(rendered$figure_id, metadata$figure_id)) {
    stop("The evidence renderer did not reproduce all 13 registered component IDs.", call. = FALSE)
  }
  rendered_order = match(metadata$figure_id, rendered$figure_id)
  if (anyNA(rendered_order) ||
      any(abs(metadata$width_in - rendered$width_in[rendered_order]) > 1e-10) ||
      any(abs(metadata$height_in - rendered$height_in[rendered_order]) > 1e-10)) {
    stop("Rendered evidence component dimensions differ from the archived metadata.", call. = FALSE)
  }

  text_columns = c("title", "subtitle", "caption", "group_title", "group_subtitle", "group_caption")
  for (column in c("pdf_path", "png_path", text_columns)) {
    set(metadata, j = column, value = rendered[[column]][rendered_order])
  }
  metadata[, `:=`(
    generator_script = "analysis/shared/render_paper_evidence.R",
    generator_function = "render_paper_evidence"
  )]
  presentation_columns = c(text_columns, "latex_assembly_note")
  presentation_text = tolower(apply(metadata[, ..presentation_columns], 1L, paste, collapse = " "))
  if (any(grepl("\\b(red|blue)\\b", presentation_text))) {
    stop("Evidence metadata contain color-specific presentation wording.", call. = FALSE)
  }

  pdf_files = list.files(figures_dir, pattern = "\\.pdf$", full.names = TRUE)
  png_files = list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
  expected_stems = sort(metadata$figure_id)
  if (!identical(sort(sub("\\.pdf$", "", basename(pdf_files))), expected_stems) ||
      !identical(sort(sub("\\.png$", "", basename(png_files))), expected_stems) ||
      any(file.info(c(pdf_files, png_files))$size <= 0)) {
    stop("The evidence renderer did not write 13 non-empty PDF/PNG component pairs.", call. = FALSE)
  }

  metadata_path = file.path(output_dir, "provenance", "figure_latex_metadata.csv")
  helper$.analysis_write_figure_latex_metadata(metadata, metadata_path, run_dir = output_dir)
  metadata = fread(metadata_path)
  metadata[]
}
