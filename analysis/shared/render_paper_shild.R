# Regenerate the SHILD paper figures from the archived shareable aggregate tables.

.paper_shild_renderer_path = local({
  source_path = tryCatch(sys.frame(1L)$ofile, error = function(condition) "")
  if (is.null(source_path) || !length(source_path) || is.na(source_path) || !nzchar(source_path)) {
    source_path = file.path("analysis", "shared", "render_paper_shild.R")
  }
  normalizePath(source_path, winslash = "/", mustWork = FALSE)
})

.paper_shild_project_root = function() {
  root = dirname(dirname(dirname(.paper_shild_renderer_path)))
  required = c(
    file.path(root, "DESCRIPTION"),
    file.path(root, "analysis", "shared", "study_helpers.R"),
    file.path(root, "analysis", "shild", "prepare_data.R")
  )
  if (!all(file.exists(required))) {
    stop("Could not locate the package root for the SHILD paper renderer.", call. = FALSE)
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

.paper_shild_read_table = function(tables_dir, name) {
  path = file.path(tables_dir, name)
  if (!file.exists(path)) {
    stop("Required SHILD shareable table is missing: ", path, call. = FALSE)
  }
  data = fread(path)
  if (!nrow(data)) {
    stop("Required SHILD shareable table is empty: ", path, call. = FALSE)
  }
  data[]
}

.paper_shild_fold_scores = function(tables_dir) {
  learners = c("xgboost", "ridge_logistic", "random_forest", "decision_tree", "featureless")
  file = "shild_g1_benchmark_fold_scores.csv"
  scores = .paper_shild_read_table(tables_dir, file)
  required = c("learner_id", "measure_id", "iteration", "repetition", "fold", "estimate")
  .analysis_require_columns(scores, required, file)
  result = scores[learner_id %in% learners, .(
    learner_id = as.character(learner_id),
    measure_id = as.character(measure_id),
    iteration = as.integer(iteration),
    repetition = as.integer(repetition),
    fold = as.integer(fold),
    estimate = as.numeric(estimate)
  )]
  if (!setequal(unique(result$learner_id), learners)) {
    stop("The archived SHILD benchmark fold scores do not contain every comparison learner.", call. = FALSE)
  }
  if (any(!is.finite(result$estimate))) {
    stop("The archived SHILD model-comparison fold scores contain non-finite estimates.", call. = FALSE)
  }
  result[]
}

.paper_shild_categorical_profiles = function(profiles, feature_labels, semantic_levels) {
  .analysis_require_columns(
    profiles,
    c("feature", "value", "median_prediction", "q10_prediction", "q90_prediction"),
    "SHILD optional categorical profiles"
  )
  profiles = copy(profiles)
  profiles[, feature_label := .analysis_feature_labels(feature, feature_labels)]
  profiles = .analysis_order_profile_values(profiles, "feature", "value", semantic_levels)
  ggplot(profiles, aes(x = value_key, y = median_prediction)) +
    geom_errorbar(
      aes(ymin = q10_prediction, ymax = q90_prediction),
      color = "grey35",
      width = 0.14,
      linewidth = 0.8
    ) +
    geom_point(color = "black", fill = "white", shape = 21L, size = 2.2, stroke = 0.65) +
    facet_wrap(~ feature_label, scales = "free_x") +
    scale_x_discrete(labels = function(value) sub("^[^:]+::", "", value)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    labs(
      x = NULL,
      y = "Fitted probability of discontent (0-1)",
      title = "Optional SHILD full-fit model queries (descriptive and non-causal)",
      subtitle = .analysis_wrap_text(paste(
        "Categories follow questionnaire order; open points are medians, and whiskers are descriptive",
        "10th-90th percentiles over reference rows, not confidence intervals."
      ))
    ) +
    .analysis_theme(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    .analysis_typography_theme(axis_text_size = 11, axis_title_size = 9.5)
}

.paper_shild_interaction_profiles = function(profiles, feature_labels, semantic_levels) {
  .analysis_require_columns(
    profiles,
    c("feature_1", "feature_2", "first_value", "second_value", "mean_prediction"),
    "SHILD optional interaction profiles"
  )
  profiles = copy(profiles)
  profiles[, pair_label := paste(
    .analysis_feature_labels(feature_1, feature_labels),
    .analysis_feature_labels(feature_2, feature_labels),
    sep = " x "
  )]
  first = .analysis_order_profile_values(profiles, "feature_1", "first_value", semantic_levels)
  second = .analysis_order_profile_values(
    profiles,
    "feature_2",
    "second_value",
    semantic_levels,
    reverse = TRUE
  )
  profiles[, `:=`(first_key = first$value_key, second_key = second$value_key)]
  ggplot(profiles, aes(x = first_key, y = second_key, fill = mean_prediction)) +
    geom_tile(color = "white", linewidth = 0.35) +
    facet_wrap(~ pair_label, scales = "free") +
    scale_x_discrete(labels = function(value) sub("^[^:]+::", "", value)) +
    scale_y_discrete(labels = function(value) sub("^[^:]+::", "", value)) +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1), na.value = "grey90") +
    labs(
      x = "First feature value",
      y = "Second feature value",
      fill = "Fitted probability\nof discontent (0-1)",
      title = "Optional SHILD two-feature model queries (descriptive and non-causal)",
      subtitle = .analysis_wrap_text(paste(
        "One panel per queried pair; categories follow questionnaire order. Synthetic two-feature partial-",
        "dependence queries can leave observed joint support and are descriptive, not causal."
      ))
    ) +
    .analysis_theme(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    .analysis_typography_theme(
      axis_text_size = 11,
      legend_text_size = 11,
      legend_title_size = 11
    )
}

.paper_shild_neutralize_text = function(value) {
  if (!is.character(value)) {
    return(value)
  }
  replacements = c(
    "Each red point" = "Each point",
    "Red points" = "Points",
    "red points" = "points",
    "red point" = "point",
    "red markers" = "quantile markers",
    "red quantiles" = "exact quantiles",
    "red line" = "median line",
    "blue whiskers" = "interval whiskers",
    "Blue bars" = "Interval bars",
    "blue bars" = "interval bars",
    "blue line" = "solid line",
    "outer light-blue band" = "outer lighter-gray band",
    "inner blue band" = "inner darker-gray band",
    "light-blue" = "lighter-gray"
  )
  result = value
  for (pattern in names(replacements)) {
    result = gsub(pattern, replacements[[pattern]], result, fixed = TRUE)
  }
  result = gsub("\\bBlue\\b", "Gray", result, perl = TRUE)
  result = gsub("\\bblue\\b", "gray", result, perl = TRUE)
  result
}

.paper_shild_output_metadata = function(parent_metadata, registry, output_dir, ale_2d_available) {
  expected_ids = parent_metadata$figure_id
  if (!setequal(expected_ids, registry$figure_id) || length(expected_ids) != nrow(registry)) {
    stop("Rendered SHILD figure IDs do not match the 37-component parent registry.", call. = FALSE)
  }
  dimensions = merge(
    parent_metadata[, .(figure_id, parent_width = width_in, parent_height = height_in)],
    registry[, .(figure_id, rendered_width = width_in, rendered_height = height_in)],
    by = "figure_id",
    all = TRUE
  )
  if (any(abs(dimensions$parent_width - dimensions$rendered_width) > 1e-8) ||
      any(abs(dimensions$parent_height - dimensions$rendered_height) > 1e-8)) {
    stop("Rendered SHILD figure dimensions differ from the parent metadata.", call. = FALSE)
  }
  metadata = copy(parent_metadata)
  ale_1d_semantics = .analysis_ale_semantics("SHILD", "1d", support_threshold = 20L)
  ale_2d_semantics = .analysis_ale_semantics(
    "SHILD",
    "2d",
    support_threshold = 20L,
    available = ale_2d_available
  )
  metadata[figure_group_id == "shild_rich_ale_1d", `:=`(
    estimand_or_scope = ale_1d_semantics$estimand_or_scope,
    uncertainty_semantics = ale_1d_semantics$uncertainty_semantics
  )]
  metadata[figure_group_id == "shild_rich_ale_2d", `:=`(
    estimand_or_scope = ale_2d_semantics$estimand_or_scope,
    uncertainty_semantics = ale_2d_semantics$uncertainty_semantics
  )]
  metadata[figure_group_id == "shild_g1_performance", group_subtitle := paste(
    "Points are fold means; whiskers are descriptive fold 10th-90th percentiles, not confidence intervals.",
    "Each metric panel uses a labeled data-adaptive axis."
  )]
  metadata[figure_id == "shild_rich_interaction_screen", subtitle := paste(
    "Friedman-Popescu H is bounded from zero to one; the horizontal axis is restricted to the displayed values.",
    "The screen is descriptive and dependence-sensitive, not a causal interaction analysis."
  )]
  metadata[figure_id == "shild_prediction_multiplicity", subtitle := paste(
    "Stepwise cumulative OOF summary from shareable fixed-width bin counts, with exact aggregate p50, p90, and",
    "p95 anchors. The steps do not imply continuous or row-level resolution; labeled ranges are not CIs."
  )]
  text_columns = intersect(
    c(
      "title", "subtitle", "caption", "group_title", "group_subtitle", "group_caption",
      "latex_assembly_note"
    ),
    names(metadata)
  )
  for (column in text_columns) {
    set(metadata, j = column, value = .paper_shild_neutralize_text(metadata[[column]]))
  }
  metadata[, `:=`(
    pdf_path = paste0("figures/", figure_id, ".pdf"),
    png_path = paste0("figures/", figure_id, ".png"),
    generator_script = "analysis/shared/render_paper_shild.R",
    generator_function = "render_paper_shild"
  )]
  remaining = unlist(metadata[, ..text_columns], use.names = FALSE)
  if (any(grepl("\\b(?:red|blue)\\b", remaining, ignore.case = TRUE, perl = TRUE), na.rm = TRUE)) {
    stop("Neutralized SHILD metadata still contains hue-dependent wording.", call. = FALSE)
  }
  setkey(metadata, figure_id)
  metadata = metadata[J(expected_ids)]
  setkey(metadata, NULL)
  provenance_dir = file.path(output_dir, "provenance")
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  .analysis_write_figure_latex_metadata(
    metadata,
    file.path(provenance_dir, "figure_latex_metadata.csv"),
    run_dir = output_dir
  )
  metadata[]
}

#' Render the archived SHILD paper figures in monochrome
#'
#' @param parent_dir Directory containing `tables/` and `provenance/figure_latex_metadata.csv`.
#' @param output_dir Directory in which `figures/` and neutralized provenance metadata are written.
#' @return The updated 37-row figure metadata table, invisibly.
render_paper_shild = function(parent_dir, output_dir) {
  if (!is.character(parent_dir) || length(parent_dir) != 1L || is.na(parent_dir) || !nzchar(parent_dir)) {
    stop("`parent_dir` must be one non-empty directory path.", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    stop("`output_dir` must be one non-empty directory path.", call. = FALSE)
  }
  root = .paper_shild_project_root()
  parent_dir = normalizePath(parent_dir, winslash = "/", mustWork = TRUE)
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package `devtools` is required to render the SHILD paper figures.", call. = FALSE)
  }
  library(mlr3autoiml)
  suppressPackageStartupMessages(library("data.table", character.only = TRUE))
  suppressPackageStartupMessages(library("ggplot2", character.only = TRUE))
  renderer_environment = environment(render_paper_shild)
  sys.source(file.path(root, "analysis", "shared", "study_helpers.R"), envir = renderer_environment)
  sys.source(file.path(root, "analysis", "shild", "prepare_data.R"), envir = renderer_environment)

  tables_dir = file.path(parent_dir, "tables")
  metadata_path = file.path(parent_dir, "provenance", "figure_latex_metadata.csv")
  if (!dir.exists(tables_dir) || !file.exists(metadata_path)) {
    stop("The SHILD parent directory must contain shareable tables and figure metadata.", call. = FALSE)
  }
  parent_metadata = fread(metadata_path)
  .analysis_require_columns(
    parent_metadata,
    c(.analysis_figure_metadata_columns, "figure_id", "width_in", "height_in"),
    "SHILD parent figure metadata"
  )
  if (nrow(parent_metadata) != 37L || anyDuplicated(parent_metadata$figure_id)) {
    stop("The SHILD parent metadata must register exactly 37 unique figure components.", call. = FALSE)
  }
  source_paths = trimws(unlist(strsplit(parent_metadata$source_tables, "|", fixed = TRUE)))
  if (!length(source_paths) || any(!grepl("^tables/[A-Za-z0-9._-]+\\.csv$", source_paths))) {
    stop("Every SHILD paper figure must derive only from registered shareable CSV tables.", call. = FALSE)
  }
  .require_files(file.path(parent_dir, source_paths))

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir = normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  figures_dir = file.path(output_dir, "figures")
  .analysis_reset_figure_registry()
  .analysis_clear_figure_files(figures_dir)

  read_shild = function(name) .paper_shild_read_table(tables_dir, name)
  feature_labels = .analysis_shild_feature_labels()
  semantic_levels = .analysis_shild_semantic_levels()
  audit_labels = c(audit_gender = "Gender", audit_age_group = "Age group")
  subgroup_labels = c(
    "audit_gender::Man" = "Men",
    "audit_gender::Woman" = "Women",
    "audit_age_group::16-24" = "16-24",
    "audit_age_group::25-34" = "25-34",
    "audit_age_group::35-44" = "35-44",
    "audit_age_group::45-54" = "45-54",
    "audit_age_group::55-64" = "55-64"
  )

  report_card = read_shild("shild_report_card.csv")
  overview_plot = csdg_plot_data(
    report_card,
    type = "gate_status",
    title = "SHILD claim-scoped diagnostic report card",
    gate_order = report_card$gate_id,
    style = "monochrome"
  )
  .save_plot(
    overview_plot,
    file.path(figures_dir, "shild_gate_overview"),
    width = 8,
    height = 6
  )

  performance_plot = csdg_plot_data(
    read_shild("shild_gate_g1__performance__summary.csv"),
    type = "performance",
    title = "SHILD focal XGBoost held-out predictive performance",
    base_size = 12,
    style = "monochrome"
  )
  .save_plot(
    performance_plot,
    file.path(figures_dir, "shild_g1_performance"),
    width = 9,
    height = 6
  )

  dependence_plot = csdg_plot_data(
    read_shild("shild_gate_g2__pairwise.csv"),
    type = "dependence",
    title = "SHILD mixed-type pairwise dependence strength",
    subtitle = .analysis_wrap_text(paste(
      "Lower triangle for the 15 strongest variables. Cells combine absolute Spearman correlation,",
      "Cramer's V, and eta-squared; these descriptive magnitudes are not directly equivalent."
    )),
    feature_labels = feature_labels,
    top_n = 15L,
    base_size = 15,
    style = "monochrome"
  )
  .save_plot(
    dependence_plot,
    file.path(figures_dir, "shild_g2_dependence"),
    width = 11,
    height = 9
  )

  calibration_plot = csdg_plot_data(
    read_shild("shild_g3_calibration_plot_source.csv"),
    type = "calibration",
    title = "SHILD out-of-fold calibration",
    subtitle = paste(
      "Each point is one quantile-derived bin: x is its mean out-of-fold probability, and y is its",
      "observed event fraction. Interval bars are conditional 95% Wilson intervals excluding model-training",
      "variation; the solid line only guides the bin sequence, and the diagonal denotes perfect calibration."
    ),
    x_label = "Mean held-out probability of discontent",
    y_label = "Observed discontent fraction",
    base_size = 14,
    axis_text_size = 12.5,
    axis_title_size = 13,
    style = "monochrome"
  )
  .save_plot(
    calibration_plot,
    file.path(figures_dir, "shild_g3_calibration"),
    width = 8,
    height = 6
  )

  fidelity_plot = csdg_plot_data(
    read_shild("shild_g4_local_fidelity_source.csv"),
    type = "local_fidelity",
    title = "SHILD cross-fitted local surrogate fidelity",
    subtitle = paste(
      "Cases 1 and 2 follow the declared 10th- and 90th-percentile focal out-of-fold prediction-score order.",
      "The dashed line is the prespecified minimum of 0.80; points are diagnostics, not confidence intervals."
    ),
    style = "monochrome"
  )
  .save_plot(
    fidelity_plot,
    file.path(figures_dir, "shild_g4_local_fidelity"),
    width = 8,
    height = 5
  )

  pfi_plot = csdg_plot_data(
    read_shild("shild_gate_g5__pfi__summary.csv"),
    type = "pfi",
    title = "SHILD foldwise held-out permutation importance",
    subtitle = paste(
      "Top 10 features by mean importance; whiskers show descriptive fold 10th-90th percentiles,",
      "not confidence intervals."
    ),
    feature_labels = feature_labels,
    top_n = 10L,
    x_label = "Increase in held-out log loss",
    base_size = 16,
    direct_label_size = 4.2,
    style = "monochrome"
  )
  .save_plot(
    pfi_plot,
    file.path(figures_dir, "shild_g5_pfi"),
    width = 10,
    height = 7
  )

  model_plot = csdg_plot_data(
    read_shild("shild_gate_g6a__model_generalization__candidates.csv"),
    type = "model_comparison",
    title = "SHILD near-equivalent model sensitivity screen",
    x_label = "Mean held-out log loss",
    x_limits = c(0, 0.25),
    fold_scores = .paper_shild_fold_scores(tables_dir),
    base_size = 15,
    direct_label_size = 4.2,
    limit_label_size = 4.1,
    axis_text_size = 12.5,
    legend_text_size = 12,
    style = "monochrome"
  )
  .save_plot(
    model_plot,
    file.path(figures_dir, "shild_g6_model_sensitivity"),
    width = 9,
    height = 6
  )

  subgroup_source = list(
    metrics = read_shild("shild_gate_g7a__subgroup_audit__metrics.csv"),
    uncertainty = read_shild("shild_gate_g7a__subgroup_audit__uncertainty.csv")
  )
  subgroup_plot = csdg_plot_data(
    subgroup_source,
    type = "subgroup",
    metric = "auc",
    title = "SHILD focal-model subgroup ROC AUC audit",
    subtitle = paste(
      "Row-bootstrap 95% intervals conditional on out-of-fold predictions; direct labels show AUC and n.",
      "The full 0.5-1.0 AUC scale avoids magnifying small absolute gaps."
    ),
    audit_labels = audit_labels,
    subgroup_labels = subgroup_labels,
    subgroup_order = c("16-24", "25-34", "35-44", "45-54", "55-64", "Women", "Men"),
    x_limits = c(0.5, 1),
    base_size = 15,
    direct_label_size = 4,
    axis_text_size = 13,
    axis_title_size = 13,
    style = "monochrome"
  )
  .save_plot(
    subgroup_plot,
    file.path(figures_dir, "shild_g7_subgroups"),
    width = 10,
    height = 4.6
  )

  contrast_plot = csdg_plot_data(
    read_shild("shild_gate_g7a__subgroup_audit__contrasts.csv"),
    type = "subgroup_contrast",
    metric = "auc",
    title = "SHILD pairwise subgroup AUC contrasts",
    subtitle = paste(
      "Points are first-minus-second differences; whiskers are unadjusted conditional 95% bootstrap",
      "intervals. Model-training variation is excluded, and no significance claim is made."
    ),
    audit_labels = audit_labels,
    subgroup_labels = subgroup_labels,
    x_label = "ROC AUC difference (first - second)",
    base_size = 16,
    direct_label_size = 4.5,
    axis_text_size = 13,
    style = "monochrome"
  )
  .save_plot(
    contrast_plot,
    file.path(figures_dir, "shild_g7_subgroup_contrasts"),
    width = 10,
    height = 7
  )

  importance_plot = csdg_plot_data(
    read_shild("shild_gate_g6a__model_generalization__explanation_agreement__ranks.csv"),
    type = "rank_heatmap",
    title = "SHILD held-out PFI ranks across near-equivalent models",
    subtitle = "Union of each model's top ten; rank values are printed in every cell.",
    top_n = 10L,
    feature_labels = feature_labels,
    learner_order = c("xgboost", "ridge_logistic", "random_forest"),
    base_size = 15,
    cell_label_size = 3.8,
    axis_text_size = 12,
    legend_text_size = 12,
    legend_title_size = 12,
    style = "monochrome"
  )
  .save_plot(
    importance_plot,
    file.path(figures_dir, "shild_model_importance_ranks"),
    width = 10,
    height = 7
  )

  benchmark_plot = csdg_plot_data(
    read_shild("shild_g1_benchmark_summary.csv"),
    type = "performance",
    title = "SHILD common-split learner sensitivity benchmark",
    base_size = 16,
    direct_label_size = 4.2,
    axis_text_size = 13.5,
    axis_title_size = 12.5,
    style = "monochrome"
  ) +
    theme(
      axis.text.y = element_text(margin = margin(r = 6)),
      plot.margin = margin(6, 14, 6, 6)
    )
  .save_plot(
    benchmark_plot,
    file.path(figures_dir, "shild_g1_benchmark"),
    width = 10,
    height = 7
  )

  ale_plot = csdg_plot_data(
    read_shild("shild_rich_ale_1d.csv"),
    type = "ale",
    title = "SHILD dependence-aware numeric ALE",
    subtitle = paste(
      "Ribbons are pointwise 95% respondent-bootstrap percentile intervals from complete learner refits;",
      "they are not simultaneous bands and condition on the fixed learner specification."
    ),
    feature_labels = feature_labels,
    y_label = "Accumulated local probability difference",
    base_size = 14,
    axis_text_size = 13,
    axis_title_size = 12.5,
    style = "monochrome"
  )
  .save_plot(
    ale_plot,
    file.path(figures_dir, "shild_rich_ale_1d"),
    width = 9,
    height = 6
  )

  ice_plot = csdg_plot_data(
    read_shild("shild_rich_ice_quantiles.csv"),
    type = "ice",
    title = "SHILD aggregate ICE model queries",
    subtitle = paste(
      "The outer lighter-gray band is pointwise q05-q95, the inner darker-gray band is q25-q75, and the",
      "median line is the pointwise median across row-level ICE curves; these are curve quantiles, not CIs.",
      "A common logit probability scale keeps low medians and broad upper quantiles visible."
    ),
    feature_labels = feature_labels,
    y_label = "Fitted probability of discontent (logit scale)",
    y_limits = c(0.003, 0.95),
    y_scale = "logit",
    y_breaks = c(0.005, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50, 0.90),
    base_size = 14,
    axis_text_size = 13,
    axis_title_size = 12.5,
    style = "monochrome"
  )
  ice_legend_plot = csdg_plot_legend(ice_plot)
  .save_plot(
    ice_plot + theme(legend.position = "none"),
    file.path(figures_dir, "shild_rich_ice_quantiles"),
    width = 9,
    height = 6,
    metadata = list(
      latex_assembly_note = paste(
        "Assemble the four ICE panels in panel_order as a 2-row by 2-column figure, then place the shared",
        "quantile legend once beneath them with LaTeX."
      )
    )
  )
  .save_plot(
    ice_legend_plot,
    file.path(figures_dir, "shild_rich_ice_quantiles__legend"),
    width = 9,
    height = 1.25,
    metadata = list(
      figure_group_id = "shild_rich_ice_quantiles",
      is_grouped_figure = TRUE,
      latex_group_id = "shild_rich_ice_quantiles",
      panel_id = "shared_legend",
      panel_order = 5L,
      panel_heading = "Shared ICE quantile legend",
      group_title = ice_plot$labels$title,
      group_subtitle = ice_plot$labels$subtitle,
      latex_assembly_note = paste(
        "Shared legend for the four ICE panels; place it once beneath the 2-row by 2-column panel assembly",
        "with LaTeX."
      )
    )
  )

  interaction_plot = csdg_plot_data(
    read_shild("shild_rich_interaction_screen.csv"),
    type = "interaction",
    title = "SHILD grid-based interaction screen",
    subtitle = "Grid-based H approximation; dependence-sensitive model diagnostic, not a causal interaction.",
    feature_labels = feature_labels,
    base_size = 15,
    direct_label_size = 4.2,
    style = "monochrome"
  ) +
    theme(
      axis.text.y = element_text(margin = margin(r = 6)),
      plot.margin = margin(6, 14, 6, 6)
    )
  .save_plot(
    interaction_plot,
    file.path(figures_dir, "shild_rich_interaction_screen"),
    width = 8,
    height = 5
  )

  ale_2d_data = read_shild("shild_rich_ale_2d.csv")
  shild_ale_2d_available = "ale2d" %in% names(ale_2d_data) && any(is.finite(ale_2d_data$ale2d))
  ale_2d_plot = csdg_plot_data(
    ale_2d_data,
    type = "ale_2d",
    title = "SHILD second-order ALE interaction surface",
    subtitle = paste(
      "First-order effects are removed; irregular rectangles follow empirical cells, and unsupported cells",
      "are shaded uniformly. Model-fitting uncertainty is not represented."
    ),
    feature_labels = feature_labels,
    style = "monochrome"
  )
  .save_plot(
    ale_2d_plot,
    file.path(figures_dir, "shild_rich_ale_2d"),
    width = 8,
    height = 6
  )

  multiplicity_distribution = read_shild("shild_multiplicity_distribution.csv")
  multiplicity_quantiles = multiplicity_distribution[
    statistic %in% c("p50", "p90", "p95"),
    .(statistic, prediction_range)
  ]
  multiplicity_plot = csdg_plot_data(
    read_shild("shild_multiplicity_bins.csv"),
    type = "multiplicity",
    quantiles = multiplicity_quantiles,
    title = "SHILD prediction multiplicity across AUC-near-equivalent models",
    subtitle = paste(
      "Stepwise cumulative OOF summary from shareable fixed-width bin counts, with exact aggregate p50, p90, and",
      "p95 anchors. The steps do not imply continuous or row-level resolution; labeled ranges are not CIs."
    ),
    x_label = "Within-respondent probability range",
    base_size = 14,
    direct_label_size = 4,
    style = "monochrome"
  )
  .save_plot(
    multiplicity_plot,
    file.path(figures_dir, "shild_prediction_multiplicity"),
    width = 8,
    height = 6
  )

  decision_plot = csdg_plot_data(
    read_shild("shild_gate_g3b__decision_curve.csv"),
    type = "decision_curve",
    title = "SHILD out-of-fold decision-curve quantities",
    subtitle = paste(
      "Illustrative threshold-level utility quantities without confidence intervals; not evidence of",
      "real-world benefit. The shareable figure contains no row-level probability marks."
    ),
    base_size = 14,
    axis_text_size = 12.5,
    axis_title_size = 13,
    style = "monochrome"
  )
  .save_plot(
    decision_plot,
    file.path(figures_dir, "shild_g3_decision_curve"),
    width = 8,
    height = 6
  )

  optional_profiles = read_shild("shild_optional_model_query_profiles.csv")
  optional_profiles = optional_profiles[!is.finite(value_numeric)]
  profile_plot = .paper_shild_categorical_profiles(optional_profiles, feature_labels, semantic_levels)
  .save_plot(
    profile_plot,
    file.path(figures_dir, "shild_optional_model_query_categorical"),
    width = 9,
    height = 6
  )

  interaction_profiles = read_shild("shild_optional_model_query_interactions.csv")
  interaction_profile_plot = .paper_shild_interaction_profiles(
    interaction_profiles,
    feature_labels,
    semantic_levels
  )
  .save_plot(
    interaction_profile_plot,
    file.path(figures_dir, "shild_optional_model_query_interactions"),
    width = 9,
    height = 6
  )

  registry = .analysis_figure_registry_table()
  metadata = .paper_shild_output_metadata(
    parent_metadata,
    registry,
    output_dir,
    ale_2d_available = shild_ale_2d_available
  )
  figure_paths = c(metadata$pdf_path, metadata$png_path)
  .require_files(file.path(output_dir, figure_paths))
  invisible(metadata[])
}
