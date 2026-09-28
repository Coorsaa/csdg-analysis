#!/usr/bin/env Rscript
# Render the shareable PISA paper figures from aggregate publication tables.

.render_pisa_renderer_path = local({
  source_path = tryCatch(sys.frame(1L)$ofile, error = function(condition) "")
  if (is.null(source_path) || !length(source_path) || is.na(source_path) || !nzchar(source_path)) {
    source_path = file.path("analysis", "shared", "render_paper_pisa.R")
  }
  normalizePath(source_path, winslash = "/", mustWork = FALSE)
})

.render_pisa_project_root = function(start = dirname(.render_pisa_renderer_path)) {
  candidate = normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(candidate, "DESCRIPTION")) &&
        file.exists(file.path(candidate, "analysis", "shared", "study_helpers.R"))) {
      return(candidate)
    }
    parent = dirname(candidate)
    if (identical(parent, candidate)) {
      stop("Could not locate the mlr3autoiml project root.", call. = FALSE)
    }
    candidate = parent
  }
}

.render_pisa_read_table = function(tables_dir, filename) {
  if (!grepl("^[A-Za-z0-9._-]+\\.csv$", filename)) {
    stop("Invalid PISA shareable-table filename: ", filename, call. = FALSE)
  }
  path = file.path(tables_dir, filename)
  if (!file.exists(path)) {
    stop("Required PISA shareable table is missing: ", path, call. = FALSE)
  }
  data.table::fread(path, na.strings = c("", "NA", "NaN"))
}

.render_pisa_neutralize_text = function(values) {
  values = gsub("[Rr]ed points", "Points", values)
  values = gsub("[Rr]ed point", "point", values)
  values = gsub("[Bb]lue whiskers", "Whiskers", values)
  values = gsub("[Bb]lue whisker", "whisker", values)
  values = gsub("[Bb]lue ranges", "Ranges", values)
  values = gsub("[Bb]lue range", "range", values)
  values = gsub("[Rr]ed markers", "Markers", values)
  values = gsub("[Rr]ed marker", "marker", values)
  values = gsub("[Rr]ed line", "median line", values)
  values = gsub("[Bb]lue line", "line", values)
  values = gsub("light-blue", "light-gray", values, ignore.case = TRUE)
  values = gsub("inner blue", "inner dark-gray", values, ignore.case = TRUE)
  values = gsub("\\bRed\\b", "Dark-gray", values, perl = TRUE)
  values = gsub("\\bred\\b", "dark-gray", values, perl = TRUE)
  values = gsub("\\bBlue\\b", "Gray", values, perl = TRUE)
  values = gsub("\\bblue\\b", "gray", values, perl = TRUE)
  values = gsub("\\bColor\\b", "Grayscale tone", values, perl = TRUE)
  values = gsub("\\bcolor\\b", "grayscale tone", values, perl = TRUE)
  values = gsub("\\bColour\\b", "Grayscale tone", values, perl = TRUE)
  values = gsub("\\bcolour\\b", "grayscale tone", values, perl = TRUE)
  values
}

.render_pisa_strip_text = function(plot) {
  plot + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
}

.render_pisa_save_component = function(plot, metadata_row, figures_dir) {
  figure_id = metadata_row$figure_id[[1L]]
  width = as.numeric(metadata_row$width_in[[1L]])
  height = as.numeric(metadata_row$height_in[[1L]])
  if (!is.finite(width) || width <= 0 || !is.finite(height) || height <= 0) {
    stop("Invalid dimensions for PISA figure component: ", figure_id, call. = FALSE)
  }
  plot = .render_pisa_strip_text(plot)
  pdf_path = file.path(figures_dir, paste0(figure_id, ".pdf"))
  png_path = file.path(figures_dir, paste0(figure_id, ".png"))
  dpi = ceiling(max(300, 1500 / width, 1500 / height))
  ggplot2::ggsave(
    pdf_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf,
    bg = "white"
  )
  ggplot2::ggsave(
    png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white"
  )
  invisible(c(pdf_path, png_path))
}

.render_pisa_matched_control_contrast_plots = function(data) {
  data = data.table::copy(data.table::as.data.table(data))
  required = c(
    "country", "size_matched_exclusion_penalty_Cbar_minus_A", "matched_control_penalty_minimum",
    "matched_control_penalty_maximum", "unrestricted_vs_size_matched_training_size_contrast_B_minus_Cbar",
    "n_matched_control_repetitions", "descriptive_only"
  )
  missing = setdiff(required, names(data))
  if (length(missing)) {
    stop("The PISA matched-control plot source is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(data) != 32L || anyNA(data[, ..required]) || anyDuplicated(data$country) ||
      any(data$n_matched_control_repetitions != 50L) || any(!data$descriptive_only)) {
    stop("The PISA matched-control plots require 32 complete descriptive 50-control records.", call. = FALSE)
  }
  numeric_columns = setdiff(required, c("country", "descriptive_only"))
  if (any(!vapply(data[, ..numeric_columns], function(column) all(is.finite(column)), logical(1L)))) {
    stop("The PISA matched-control plot source contains nonfinite values.", call. = FALSE)
  }
  if (!identical(.analysis_plot_font_family(), "Arial")) {
    stop("Arial must be available before rendering the PISA matched-control figure.", call. = FALSE)
  }

  data.table::setorder(data, size_matched_exclusion_penalty_Cbar_minus_A, country)
  data[, country_label := factor(country, levels = country)]
  common_theme = .analysis_theme(base_size = 13, line_base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 13, color = "black"),
      axis.title = ggplot2::element_text(size = 14, color = "black"),
      plot.margin = ggplot2::margin(8, 12, 10, 8)
    )

  size_matched_exclusion = ggplot2::ggplot(data, ggplot2::aes(y = country_label)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey40", linewidth = 0.45) +
    ggplot2::geom_segment(
      ggplot2::aes(x = matched_control_penalty_minimum, xend = matched_control_penalty_maximum, yend = country_label),
      color = "grey45",
      linewidth = 0.65
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = size_matched_exclusion_penalty_Cbar_minus_A),
      shape = 21L,
      size = 2.1,
      stroke = 0.65,
      color = "black",
      fill = "white"
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::labs(
      x = "Size-matched penalty, Cbar - A\n(RMSE points)",
      y = "Held-out country"
    ) +
    common_theme

  contrast_limit = ceiling(
    max(abs(data$unrestricted_vs_size_matched_training_size_contrast_B_minus_Cbar)) * 1.08 * 20
  ) / 20
  training_size_contrast = ggplot2::ggplot(data, ggplot2::aes(y = country_label)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey35", linetype = "dashed", linewidth = 0.55) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = unrestricted_vs_size_matched_training_size_contrast_B_minus_Cbar,
        yend = country_label
      ),
      color = "grey55",
      linewidth = 0.65
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = unrestricted_vs_size_matched_training_size_contrast_B_minus_Cbar),
      shape = 21L,
      size = 2.1,
      stroke = 0.65,
      color = "black",
      fill = "white"
    ) +
    ggplot2::scale_x_continuous(labels = function(value) sprintf("%+.1f", value), expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(-contrast_limit, contrast_limit), clip = "off") +
    ggplot2::labs(
      x = "Training-size contrast, B - Cbar\n(RMSE points)",
      y = "Held-out country"
    ) +
    common_theme

  list(
    size_matched_exclusion = size_matched_exclusion,
    training_size_contrast = training_size_contrast
  )
}

#' Render the archived PISA paper figures in monochrome
#'
#' @param parent_dir Directory containing `tables/` and `provenance/figure_latex_metadata.csv`.
#' @param output_dir Directory in which `figures/` and neutralized provenance metadata are written.
#' @return The updated 31-row figure-component metadata table.
render_paper_pisa = function(parent_dir, output_dir) {
  if (!is.character(parent_dir) || length(parent_dir) != 1L || is.na(parent_dir) || !nzchar(parent_dir)) {
    stop("`parent_dir` must be one non-empty directory path.", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    stop("`output_dir` must be one non-empty directory path.", call. = FALSE)
  }
  parent_dir = normalizePath(parent_dir, winslash = "/", mustWork = TRUE)
  tables_dir = file.path(parent_dir, "tables")
  catalog_path = file.path(parent_dir, "provenance", "figure_catalog.csv")
  metadata_path = file.path(parent_dir, "provenance", "figure_latex_metadata.csv")
  if (!dir.exists(tables_dir) || !file.exists(catalog_path) || !file.exists(metadata_path)) {
    stop(
      "The PISA parent must contain tables/ and the figure catalog and metadata provenance files.",
      call. = FALSE
    )
  }

  project_root = .render_pisa_project_root()
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package `devtools` is required to render the PISA paper figures.", call. = FALSE)
  }
  source(file.path(project_root, "analysis", "shared", "study_helpers.R"), local = TRUE)
  source(file.path(project_root, "analysis", "pisa", "prepare_data.R"), local = TRUE)
  library(mlr3autoiml)
  suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
  })

  catalog = fread(catalog_path, na.strings = c("", "NA", "NaN"))
  metadata = fread(metadata_path, na.strings = c("", "NA", "NaN"))
  required_metadata = c(
    .analysis_figure_metadata_columns,
    "width_in",
    "height_in"
  )
  missing_metadata = setdiff(required_metadata, names(metadata))
  if (length(missing_metadata)) {
    stop("PISA figure metadata is missing columns: ", paste(missing_metadata, collapse = ", "), call. = FALSE)
  }
  if (!"figure_id" %in% names(catalog) || anyNA(catalog$figure_id) || any(!nzchar(catalog$figure_id)) ||
      anyDuplicated(catalog$figure_id)) {
    stop("The PISA figure catalog must contain unique, non-empty component IDs.", call. = FALSE)
  }
  expected_ids = catalog$figure_id
  if (anyNA(metadata$figure_id) || any(!nzchar(metadata$figure_id)) || anyDuplicated(metadata$figure_id) ||
      !setequal(metadata$figure_id, expected_ids)) {
    stop("PISA figure metadata must match the authoritative figure catalog exactly.", call. = FALSE)
  }
  if (any(!is.finite(metadata$width_in) | metadata$width_in <= 0) ||
      any(!is.finite(metadata$height_in) | metadata$height_in <= 0)) {
    stop("PISA figure metadata contains invalid component dimensions.", call. = FALSE)
  }
  metadata[, width_in := as.numeric(width_in)]
  metadata[figure_group_id == "pisa_model_importance_ranks", width_in := 7.5]

  source_paths = trimws(unlist(strsplit(metadata$source_tables, "|", fixed = TRUE), use.names = FALSE))
  source_paths = sort(unique(source_paths[nzchar(source_paths)]))
  if (!length(source_paths) || any(!grepl("^tables/[A-Za-z0-9._-]+\\.csv$", source_paths))) {
    stop("PISA figure metadata contains invalid shareable source-table paths.", call. = FALSE)
  }
  source_filenames = basename(source_paths)
  source_tables = setNames(
    lapply(source_filenames, function(filename) .render_pisa_read_table(tables_dir, filename)),
    source_filenames
  )
  table = function(filename) {
    result = source_tables[[filename]]
    if (is.null(result)) {
      stop("The PISA renderer requested an unregistered source table: ", filename, call. = FALSE)
    }
    data.table::copy(result)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir = normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  figures_dir = file.path(output_dir, "figures")
  provenance_dir = file.path(output_dir, "provenance")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  .analysis_clear_figure_files(figures_dir)

  plots = new.env(parent = emptyenv())
  add_plot = function(figure_id, plot) {
    if (!figure_id %in% metadata$figure_id || figure_id %in% names(plots)) {
      stop("Invalid or duplicate PISA figure component: ", figure_id, call. = FALSE)
    }
    plots[[figure_id]] = plot
    invisible(TRUE)
  }
  add_components = function(figure_group_id, plot) {
    components = .analysis_plot_components(plot, figure_group_id)
    for (component in components) add_plot(component$figure_id, component$plot)
    invisible(TRUE)
  }

  feature_labels = .analysis_pisa_feature_labels()
  plot_feature_labels = .analysis_pisa_plot_feature_labels()
  audit_labels = .analysis_pisa_audit_labels()
  subgroup_labels = .analysis_pisa_subgroup_labels()

  report_card = table("pisa_report_card.csv")
  add_plot(
    "pisa_gate_overview",
    csdg_plot_data(
      report_card,
      type = "gate_status",
      gate_order = report_card$gate_id,
      style = "monochrome"
    )
  )

  performance = table("pisa_gate_g1__performance__summary.csv")
  performance_plot = csdg_plot(
    list(performance = list(summary = performance)),
    gate = "G1",
    base_size = 16,
    style = "monochrome"
  ) +
    theme(axis.title.x = element_text(size = 13.5))
  add_components("pisa_g1_performance", performance_plot)

  dependence = table("pisa_gate_g2__pairwise.csv")
  add_plot(
    "pisa_g2_dependence",
    csdg_plot_data(
      dependence,
      type = "dependence",
      feature_labels = plot_feature_labels,
      top_n = 15L,
      base_size = 16,
      style = "monochrome",
      axis_text_size = 12.5,
      legend_text_size = 12.5,
      legend_title_size = 12.5
    )
  )

  calibration = table("pisa_g3_calibration_plot_source.csv")
  add_plot(
    "pisa_g3_calibration",
    csdg_plot_data(
      calibration,
      type = "calibration",
      y_label = "Mean held-out PV1 score",
      base_size = 14,
      axis_text_size = 12.5,
      axis_title_size = 13,
      style = "monochrome"
    )
  )

  pfi = table("pisa_gate_g5__pfi__summary.csv")
  add_plot(
    "pisa_g5_pfi",
    csdg_plot_data(
      pfi,
      type = "pfi",
      feature_labels = plot_feature_labels,
      x_label = "Increase in held-out RMSE",
      base_size = 16,
      direct_label_size = 4.2,
      style = "monochrome"
    )
  )

  model_ranks = table("pisa_model_ranks.csv")
  fold_performance = table("pisa_all_pv_fold_performance.csv")
  add_plot(
    "pisa_g6_model_sensitivity",
    csdg_plot_data(
      model_ranks,
      type = "model_comparison",
      x_label = "Mean held-out RMSE difference from focal XGBoost",
      fold_scores = fold_performance,
      reference_learner = "xgboost",
      base_size = 15,
      direct_label_size = 4.2,
      limit_label_size = 4.1,
      axis_text_size = 12.5,
      style = "monochrome"
    )
  )

  importance_ranks = table("pisa_model_importance_ranks.csv")
  add_components(
    "pisa_model_importance_ranks",
    csdg_plot_data(
      importance_ranks,
      type = "importance_comparison",
      top_n = 10L,
      feature_labels = plot_feature_labels,
      learner_order = c("xgboost", "random_forest", "ridge"),
      base_size = 16,
      x_label = "Held-out RMSE increase",
      style = "monochrome",
      axis_text_size = 12.5,
      axis_title_size = 13.5,
      strip_text_size = 13
    )
  )

  subgroup_metrics = table("pisa_pv1_technical_subgroup_metrics.csv")
  subgroup_uncertainty = table("pisa_pv1_technical_subgroup_uncertainty.csv")
  subgroup_contrasts = table("pisa_pv1_technical_subgroup_contrasts.csv")
  subgroup_source = list(metrics = subgroup_metrics, uncertainty = subgroup_uncertainty)
  subgroup_plot = csdg_plot_data(
    subgroup_source,
    type = "subgroup",
    metric = "rmse",
    audit_labels = audit_labels,
    subgroup_labels = subgroup_labels,
    subgroup_order = c("Female", "Male", "Native", "Second-generation", "First-generation"),
    x_limits = c(64, 84),
    base_size = 15,
    direct_label_size = 4,
    style = "monochrome",
    axis_text_size = 13,
    axis_title_size = 13
  )
  add_components("pisa_g7_subgroups", subgroup_plot)

  add_plot(
    "pisa_g7_subgroup_contrasts",
    csdg_plot_data(
      subgroup_contrasts,
      type = "subgroup_contrast",
      metric = "rmse",
      audit_labels = audit_labels,
      subgroup_labels = subgroup_labels,
      x_label = "RMSE difference (first - second)",
      base_size = 15,
      direct_label_size = 4.3,
      style = "monochrome",
      axis_text_size = 13
    )
  )

  pooled_performance = table("pisa_all_pv_pooled_performance.csv")
  benchmark_data = pooled_performance[measure_id %in% c("regr.rmse", "regr.rsq")]
  benchmark_plot = csdg_plot_data(
    benchmark_data,
    type = "measurement_trajectory",
    x_label = "Plausible value",
    y_label = "Pooled out-of-fold estimate",
    base_size = 15,
    style = "monochrome",
    axis_text_size = 13,
    axis_title_size = 13,
    legend_text_size = 13,
    legend_title_size = 13,
    strip_text_size = 13
  ) +
    labs(color = "Model", shape = "Model", linetype = "Model")
  benchmark_built = ggplot_build(benchmark_plot)
  if (uniqueN(benchmark_built$data[[1L]]$linetype) < 2L ||
      uniqueN(benchmark_built$data[[2L]]$shape) < 2L) {
    stop("PISA model trajectories must retain redundant line types and point shapes.", call. = FALSE)
  }
  add_components("pisa_all_pv_benchmark", benchmark_plot + theme(legend.position = "none"))
  add_plot("pisa_all_pv_benchmark__legend", csdg_plot_legend(benchmark_plot))

  pfi_all_pv = table("pisa_all_pv_pfi_descriptive_summary.csv")
  add_plot(
    "pisa_all_pv_pfi",
    .analysis_pisa_pfi_plot(
      pfi_all_pv,
      feature_labels = feature_labels,
      top_n = 15L,
      base_size = 17,
      direct_label_size = 4.5
    )
  )

  loco_penalty = table("pisa_loco_country_penalty_summary.csv")
  add_plot(
    "pisa_g6_transport",
    csdg_plot_data(
      loco_penalty,
      type = "setting_penalty",
      x_label = "Country-exclusion penalty in RMSE (LOCO - common-split OOF)",
      y_label = "Held-out country",
      base_size = 14,
      style = "monochrome",
      axis_text_size = 12,
      axis_title_size = 12.5
    )
  )

  ale_1d = table("pisa_rich_ale_1d.csv")
  ale_plot = csdg_plot_data(
    ale_1d,
    type = "ale",
    title = "PISA PV1 dependence-aware one-dimensional ALE",
    subtitle = paste(
      "Ribbons are pointwise 95% country-stratified school-bootstrap percentile intervals from complete learner",
      "refits; they are not simultaneous and condition on PV1 and the fixed learner specification. A ribbon is",
      "omitted where fewer than 80% of refits meet the interval-support threshold."
    ),
    feature_labels = feature_labels,
    base_size = 15,
    style = "monochrome",
    axis_text_size = 13,
    axis_title_size = 13
  )
  add_components("pisa_rich_ale_1d", ale_plot)

  ice = table("pisa_rich_ice_quantiles.csv")
  ice_range = range(c(ice$q05_prediction, ice$q95_prediction), finite = TRUE)
  if (length(ice_range) != 2L || any(!is.finite(ice_range)) || diff(ice_range) <= 0) {
    stop("PISA ICE quantiles do not define a finite nonzero prediction range.", call. = FALSE)
  }
  ice_limits = range(pretty(ice_range, n = 6L))
  ice_plot = csdg_plot_data(
    ice,
    type = "ice",
    feature_labels = feature_labels,
    y_label = "Predicted reading score",
    y_limits = ice_limits,
    base_size = 15,
    style = "monochrome",
    axis_text_size = 13,
    axis_title_size = 13
  )
  add_components("pisa_rich_ice_quantiles", ice_plot + theme(legend.position = "none"))
  add_plot("pisa_rich_ice_quantiles__legend", csdg_plot_legend(ice_plot))

  interactions = table("pisa_rich_interaction_screen.csv")
  add_plot(
    "pisa_rich_interaction_screen",
    csdg_plot_data(
      interactions,
      type = "interaction",
      feature_labels = feature_labels,
      base_size = 15,
      direct_label_size = 4.2,
      style = "monochrome"
    ) +
      theme(
        axis.text.y = element_text(margin = margin(r = 6)),
        plot.margin = margin(10, 18, 12, 24)
      )
  )

  ale_2d = table("pisa_rich_ale_2d.csv")
  add_plot(
    "pisa_rich_ale_2d",
    csdg_plot_data(
      ale_2d,
      type = "ale_2d",
      feature_labels = plot_feature_labels,
      base_size = 13,
      style = "monochrome"
    )
  )

  multiplicity_bins = table("pisa_multiplicity_bins.csv")
  multiplicity_quantiles = table("pisa_multiplicity_distribution.csv")
  add_plot(
    "pisa_prediction_multiplicity",
    csdg_plot_data(
      multiplicity_bins,
      type = "multiplicity",
      quantiles = multiplicity_quantiles,
      x_label = "Within-student OOF prediction range (reading-score points)",
      base_size = 15,
      direct_label_size = 4,
      style = "monochrome"
    )
  )

  ale_1d_semantics = .analysis_ale_semantics("PISA", "1d", support_threshold = 20L)
  ale_2d_semantics = .analysis_ale_semantics(
    "PISA",
    "2d",
    support_threshold = 20L,
    available = "ale2d" %in% names(ale_2d) && any(is.finite(ale_2d$ale2d))
  )
  metadata[figure_group_id == "pisa_rich_ale_1d", `:=`(
    estimand_or_scope = ale_1d_semantics$estimand_or_scope,
    uncertainty_semantics = ale_1d_semantics$uncertainty_semantics
  )]
  metadata[figure_group_id == "pisa_rich_ale_2d", `:=`(
    estimand_or_scope = ale_2d_semantics$estimand_or_scope,
    uncertainty_semantics = ale_2d_semantics$uncertainty_semantics
  )]

  metadata[figure_group_id == "pisa_g1_performance", group_subtitle := paste(
    "Points are fold means; whiskers are descriptive fold 10th-90th percentiles, not confidence intervals.",
    "Each metric panel uses a labeled data-adaptive axis."
  )]
  metadata[figure_group_id == "pisa_g7_subgroups", group_subtitle := paste(
    "School-clustered 95% percentile-bootstrap intervals are stratified within country and conditional on OOF",
    "predictions. The common RMSE axis spans 64-84 points; this is not a population subgroup claim."
  )]
  metadata[figure_group_id == "pisa_rich_ice_quantiles", group_subtitle := paste(
    "The outer light-gray band is pointwise q05-q95, the inner dark-gray band is q25-q75, and the line is the",
    sprintf(
      paste(
        "pointwise median. All panels use the same labeled %.0f-%.0f reading-score scale;",
        "these are curve quantiles, not CIs."
      ),
      ice_limits[[1L]],
      ice_limits[[2L]]
    )
  )]
  metadata[figure_id == "pisa_rich_interaction_screen", subtitle := paste(
    "Friedman-Popescu H is bounded from zero to one; the horizontal axis is restricted to the displayed values.",
    "The screen is descriptive and dependence-sensitive, not a causal interaction analysis."
  )]
  metadata[figure_id == "pisa_prediction_multiplicity", subtitle := paste(
    "Stepwise cumulative OOF summary from shareable fixed-width bin counts, with exact aggregate p50, p90, and",
    "p95 anchors. The steps do not imply continuous or row-level resolution; row-level ranges remain private."
  )]

  if (!setequal(names(plots), metadata$figure_id)) {
    stop("The PISA renderer does not cover every registered figure component.", call. = FALSE)
  }
  for (figure_id in metadata$figure_id) {
    metadata_index = match(figure_id, metadata[["figure_id"]])
    metadata_row = metadata[metadata_index]
    .render_pisa_save_component(
      plots[[figure_id]],
      metadata_row,
      figures_dir
    )
  }

  text_columns = names(metadata)[vapply(metadata, is.character, logical(1L))]
  metadata[, (text_columns) := lapply(.SD, .render_pisa_neutralize_text), .SDcols = text_columns]
  metadata[, `:=`(
    pdf_path = paste0("figures/", figure_id, ".pdf"),
    png_path = paste0("figures/", figure_id, ".png"),
    generator_script = "analysis/shared/render_paper_pisa.R",
    generator_function = "render_paper_pisa"
  )]
  metadata_text = unlist(metadata[, ..text_columns], use.names = FALSE)
  color_word = grepl("\\b(red|blue|color|colour)\\b", metadata_text, ignore.case = TRUE)
  if (any(color_word, na.rm = TRUE)) {
    stop(
      "PISA output metadata retains color-dependent wording: ",
      paste(unique(metadata_text[color_word]), collapse = " | "),
      call. = FALSE
    )
  }
  output_metadata = file.path(provenance_dir, "figure_latex_metadata.csv")
  fwrite(metadata, output_metadata)

  pdf_ids = sub("\\.pdf$", "", list.files(figures_dir, pattern = "\\.pdf$"))
  png_ids = sub("\\.png$", "", list.files(figures_dir, pattern = "\\.png$"))
  if (length(pdf_ids) != length(expected_ids) || length(png_ids) != length(expected_ids) ||
      !setequal(pdf_ids, metadata$figure_id) || !setequal(png_ids, metadata$figure_id)) {
    stop("PISA rendering did not produce exactly one PDF and PNG for every registered component.", call. = FALSE)
  }
  metadata[]
}
