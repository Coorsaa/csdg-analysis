#!/usr/bin/env Rscript

# Regenerate the public ALE and ICE components without reading individual records or fitting models.

render_supplement_figures = function(project_root = getwd()) {
  project_root = normalizePath(project_root, mustWork = TRUE)
  stopifnot(file.exists(file.path(project_root, "DESCRIPTION")))
  library(mlr3autoiml)
  suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
  })
  helper = new.env(parent = globalenv())
  sys.source(file.path(project_root, "analysis/shared/study_helpers.R"), envir = helper)
  sys.source(file.path(project_root, "analysis/shild/prepare_data.R"), envir = helper)
  sys.source(file.path(project_root, "analysis/pisa/prepare_data.R"), envir = helper)
  if (!identical(helper$.analysis_plot_font_family(), "Arial")) {
    stop("Arial must be available before rendering supplementary figures.", call. = FALSE)
  }

  supplement = file.path(project_root, "supplement")
  metadata = fread(file.path(supplement, "FIGURE_MANIFEST.csv"))
  stopifnot(nrow(metadata) == 30L, !anyDuplicated(metadata$figure_id))
  hash_file = function(path) {
    result = system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE)
    stopifnot(is.null(attr(result, "status")), length(result) == 1L)
    sub("[[:space:]].*$", "", result)
  }
  for (extension in c("pdf", "png")) {
    original_column = paste0("retained_", extension, "_sha256")
    if (!original_column %in% names(metadata)) {
      metadata[, (original_column) := get(paste0(extension, "_sha256"))]
    }
  }

  rendered_ids = character()
  source_hashes = list()
  for (study in c("shild", "pisa")) {
    feature_labels = if (study == "shild") {
      helper$.analysis_shild_feature_labels()
    } else {
      helper$.analysis_pisa_feature_labels()
    }
    for (kind in c("ale", "ice")) {
      table_stem = paste0(study, if (kind == "ale") "_rich_ale_1d" else "_rich_ice_quantiles")
      source_path = file.path(supplement, study, "tables", paste0(table_stem, ".csv"))
      source_hash_before = hash_file(source_path)
      data = fread(source_path)
      y_label = if (kind == "ale") {
        if (study == "shild") "Accumulated local\nprobability difference" else {
          "Accumulated local\nprediction difference"
        }
      } else if (study == "shild") {
        "Fitted probability of discontent\n(logit scale)"
      } else {
        "Predicted reading score"
      }
      options = list(
        x = data,
        type = kind,
        feature_labels = feature_labels,
        y_label = y_label,
        base_size = 14,
        axis_text_size = 14,
        axis_title_size = 14,
        style = "monochrome"
      )
      if (kind == "ice" && study == "shild") {
        options$y_limits = c(0.003, 0.95)
        options$y_scale = "logit"
        options$y_breaks = c(0.005, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50, 0.90)
      } else if (kind == "ice") {
        options$y_limits = range(pretty(range(c(data$q05_prediction, data$q95_prediction)), n = 6L))
      }
      plot = do.call(csdg_plot_data, options)
      common_y_limits = plot$scales$get_scales("y")$limits
      components = helper$.analysis_plot_components(plot, table_stem)
      stopifnot(length(components) == 4L)
      for (component in components) {
        row_index = match(component$figure_id, metadata$figure_id)
        stopifnot(!is.na(row_index))
        row = metadata[row_index]
        component_feature = unique(component$plot$data$feature)
        stopifnot(length(component_feature) == 1L)
        component_options = options
        component_options$x = data[feature == component_feature]
        panel = do.call(csdg_plot_data, component_options) + facet_null()
        if (kind == "ale") {
          panel = suppressMessages(panel + scale_y_continuous(limits = common_y_limits))
        }
        panel = helper$.analysis_raise_point_layers(panel) +
          labs(x = NULL, y = y_label, title = NULL, subtitle = NULL, caption = NULL) +
          theme(
            axis.text.y = element_text(family = "Arial", size = 14, color = "black"),
            axis.text.x = element_text(family = "Arial", size = 14, color = "black"),
            axis.title.y = element_text(family = "Arial", size = 14, margin = margin(r = 8)),
            strip.text = element_blank(),
            legend.position = "none",
            plot.margin = margin(8, 12, 8, 8)
          )
        built = ggplot_build(panel)
        stopifnot(length(built$layout$panel_params) == 1L)
        stopifnot(length(built$layout$panel_params[[1L]]$y$get_labels()) >= 2L)
        grob = ggplotGrob(panel)
        y_title = grob$grobs[[which(grob$layout$name == "ylab-l")]]
        stopifnot(inherits(y_title, "titleGrob"), identical(y_title$children[[1L]]$label, y_label))
        ggsave(
          file.path(supplement, row$pdf_path), plot = panel,
          width = row$width_in, height = row$height_in, units = "in", device = cairo_pdf, bg = "white"
        )
        ggsave(
          file.path(supplement, row$png_path), plot = panel,
          width = row$width_in, height = row$height_in, units = "in",
          dpi = ceiling(max(300, 1500 / row$width_in, 1500 / row$height_in)), bg = "white"
        )
        metadata[row_index, `:=`(
          pdf_sha256 = hash_file(file.path(supplement, row$pdf_path)),
          png_sha256 = hash_file(file.path(supplement, row$png_path)),
          generator_script = "analysis/render_supplement_figures.R",
          generator_function = "render_supplement_figures",
          derivation_scope = paste(
            "Presentation-only re-render from public aggregate tables; complete y-axis labels in every component;",
            "shared within-study ranges retained; no empirical rerun"
          ),
          latex_assembly_note = paste(
            "Each component includes a complete y-axis; assemble in panel_order with panel headings from metadata.",
            "For ICE include the retained shared quantile legend once."
          )
        )]
        rendered_ids = c(rendered_ids, component$figure_id)
      }
      stopifnot(identical(source_hash_before, hash_file(source_path)))
      source_hashes[[table_stem]] = data.table(
        source_table = file.path(study, "tables", paste0(table_stem, ".csv")),
        source_sha256 = source_hash_before,
        numerical_input_unchanged = TRUE,
        empirical_analysis_rerun = FALSE,
        rendered_components = length(components)
      )
    }
  }
  stopifnot(length(rendered_ids) == 16L, !anyDuplicated(rendered_ids))
  # EFFORT2 asks about hypothetical effort, not the effort actually invested in the test.
  source_path = file.path(supplement, "pisa/tables/pisa_rich_ale_2d.csv")
  source_hash_before = hash_file(source_path)
  surface = fread(source_path)
  surface_id = "pisa_rich_ale_2d"
  row_index = match(surface_id, metadata$figure_id)
  row = metadata[row_index]
  panel = csdg_plot_data(surface, type = "ale_2d", style = "monochrome", base_size = 14,
    x_label = "Joy/like reading (WLE)\n(JOYREAD)",
    y_label = "Hypothetical test effort\n(EFFORT2)") + facet_null() +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme(axis.text = element_text(family = "Arial", size = 14),
      axis.title = element_text(family = "Arial", size = 14), plot.margin = margin(8, 12, 8, 8))
  stopifnot(identical(panel$labels$y, "Hypothetical test effort\n(EFFORT2)"))
  for (extension in c("pdf", "png")) {
    path = file.path(supplement, row[[paste0(extension, "_path")]])
    ggsave(path, panel, width = row$width_in, height = row$height_in, units = "in",
      device = if (extension == "pdf") cairo_pdf else "png", dpi = 320, bg = "white")
    metadata[row_index, (paste0(extension, "_sha256")) := hash_file(path)]
  }
  metadata[row_index, `:=`(
    generator_script = "analysis/render_supplement_figures.R",
    generator_function = "render_supplement_figures",
    derivation_scope = "Public aggregate re-render; EFFORT2 label corrected to hypothetical effort; no numerical change",
    latex_assembly_note = "Complete axes; caption and interpretation metadata remain external"
  )]
  stopifnot(identical(source_hash_before, hash_file(source_path)))
  source_hashes[[surface_id]] = data.table(source_table = "pisa/tables/pisa_rich_ale_2d.csv",
    source_sha256 = source_hash_before, numerical_input_unchanged = TRUE,
    empirical_analysis_rerun = FALSE, rendered_components = 1L)
  rendered_ids = c(rendered_ids, surface_id)
  fwrite(metadata, file.path(supplement, "FIGURE_MANIFEST.csv"))
  fwrite(rbindlist(source_hashes), file.path(supplement, "PUBLIC_RENDER_PROVENANCE.csv"))
  message("Rendered 17 ALE/ICE components from five unchanged public aggregate tables.")
  invisible(rendered_ids)
}

if (sys.nframe() == 0L) render_supplement_figures()
