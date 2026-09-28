#!/usr/bin/env Rscript

arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "analysis/tests/test_study_plots.R"
}
analysis_dir = dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
project_root = normalizePath(file.path(analysis_dir, ".."), winslash = "/", mustWork = TRUE)

required = c("data.table", "devtools", "ggplot2", "png")
missing = required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) {
  stop("Missing study-plot test packages: ", paste(missing, collapse = ", "), ".", call. = FALSE)
}

source(file.path(analysis_dir, "shared", "study_helpers.R"), local = FALSE)
source(file.path(analysis_dir, "pisa", "prepare_data.R"), local = FALSE)
source(file.path(analysis_dir, "shild", "prepare_data.R"), local = FALSE)
source(file.path(analysis_dir, "shared", "render_paper_shild.R"), local = FALSE)
library(mlr3autoiml)
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

# The package draws all reusable table-backed diagnostics.
calibration = data.table(
  predicted_value = c(0.02, 0.08, 0.42),
  observed_value = c(0.01, 0.09, 0.45),
  n = c(400L, 400L, 400L)
)
calibration_plot = csdg_plot_data(calibration, type = "calibration", base_size = 13)
calibration_layers = ggplot_build(calibration_plot)$data
stopifnot(
  inherits(calibration_plot, "ggplot"),
  length(calibration_layers) == 3L,
  inherits(calibration_plot$layers[[1L]]$geom, "GeomAbline"),
  inherits(calibration_plot$layers[[2L]]$geom, "GeomErrorbar"),
  inherits(calibration_plot$layers[[3L]]$geom, "GeomPoint"),
  identical(calibration_plot$theme$text$family, "Arial")
)

layer_order_plot = ggplot(data.table(x = 1, y = 1, label = "A"), aes(x = x, y = y)) +
  geom_point() +
  geom_text(aes(label = label))
layer_order_plot = .analysis_raise_point_layers(layer_order_plot)
stopifnot(inherits(tail(layer_order_plot$layers, 1L)[[1L]]$geom, "GeomPoint"))

# The SHILD paper renderer consumes the current long-form benchmark score table.
fold_score_dir = tempfile("shild-fold-scores-")
dir.create(fold_score_dir)
fold_score_learners = c("xgboost", "ridge_logistic", "random_forest", "decision_tree", "featureless")
fold_scores = CJ(
  learner_id = fold_score_learners,
  measure_id = c("classif.auc", "classif.logloss"),
  fold = 1:2
)
fold_scores[, `:=`(
  outcome_id = "life_satisfaction",
  iteration = fold,
  repetition = 1L,
  estimate = ifelse(measure_id == "classif.auc", 0.90, 0.12)
)]
fwrite(fold_scores, file.path(fold_score_dir, "shild_g1_benchmark_fold_scores.csv"))
render_fold_scores = .paper_shild_fold_scores(fold_score_dir)
stopifnot(
  nrow(render_fold_scores) == 20L,
  setequal(render_fold_scores$learner_id, fold_score_learners),
  setequal(render_fold_scores$measure_id, c("classif.auc", "classif.logloss")),
  all(is.finite(render_fold_scores$estimate))
)

# Measurement-realization trajectories and their shared legends are package-owned.
all_pv = CJ(
  outcome_id = paste0("PV", 1:10, "READ"),
  learner_id = c("xgboost", "ridge", "random_forest"),
  measure_id = c("regr.rmse", "regr.rsq")
)
all_pv[, estimate := fifelse(
  measure_id == "regr.rmse",
  72 + match(learner_id, c("xgboost", "ridge", "random_forest")) +
    as.integer(gsub("[^0-9]", "", outcome_id)) / 10,
  0.72 - match(learner_id, c("xgboost", "ridge", "random_forest")) / 100
)]
all_pv_plot = csdg_plot_data(
  all_pv,
  type = "measurement_trajectory",
  title = "Synthetic all-PV benchmark",
  base_size = 13,
  axis_text_size = 11,
  legend_text_size = 11
)
legend_plot = csdg_plot_legend(all_pv_plot)
stopifnot(inherits(all_pv_plot, "ggplot"), inherits(legend_plot, "ggplot"))

# Separately saved ICE panels retain one common y scale and one exact shared legend.
ice = CJ(feature = paste0("feature_", 1:4), x = 1:3)
ice[, `:=`(
  q05_prediction = 0.10 + 0.02 * x,
  q25_prediction = 0.20 + 0.02 * x,
  median_prediction = 0.30 + 0.02 * x,
  q75_prediction = 0.40 + 0.02 * x,
  q95_prediction = 0.50 + 0.02 * x,
  n_curves = 100L
)]
ice_plot = csdg_plot_data(
  ice,
  type = "ice",
  title = "Synthetic ICE queries",
  subtitle = "Pointwise descriptive curve quantiles, not confidence intervals",
  y_limits = c(0, 1)
)
ice_components = .analysis_plot_components(
  ice_plot + theme(legend.position = "none"),
  tempfile("ice-components-")
)
ice_y_ranges = lapply(ice_components, function(component) {
  ggplot_build(component$plot)$layout$panel_params[[1L]]$y.range
})
stopifnot(
  length(ice_components) == 4L,
  all(vapply(ice_components, function(component) inherits(component$plot, "ggplot"), logical(1L))),
  all(vapply(ice_y_ranges[-1L], identical, logical(1L), ice_y_ranges[[1L]])),
  inherits(csdg_plot_legend(ice_plot), "ggplot")
)

# Matched setting-exclusion penalties use the package's descriptive-range plot.
loco = data.table(
  held_out_group = c("AAA", "BBB", "CCC"),
  mean_penalty = c(1.2, 2.8, 4.1),
  minimum_penalty = c(0.5, 1.9, 3.2),
  maximum_penalty = c(2.0, 3.7, 5.0),
  mean_loco_rmse = c(75.2, 76.8, 78.1),
  mean_common_split_oof_rmse = c(74, 74, 74)
)
loco_plot = csdg_plot_data(loco, type = "setting_penalty", base_size = 13, axis_text_size = 11)
stopifnot(inherits(loco_plot, "ggplot"), nrow(ggplot_build(loco_plot)$data[[3L]]) == 3L)

# The overlapping PISA socioeconomic block is a visibly separate alternative query, not an atomic-rank row.
pfi_source = data.table(
  feature_group = c(paste0("feature_", 1:15), "socioeconomic_joint_block"),
  mean_across_pv = c(seq(3, 1.6, length.out = 15L), 2.4),
  minimum_across_pv = c(seq(2.9, 1.5, length.out = 15L), 2.2),
  maximum_across_pv = c(seq(3.1, 1.7, length.out = 15L), 2.6)
)
pisa_pfi_plot = .analysis_pisa_pfi_plot(pfi_source, top_n = 15L)
stopifnot(
  inherits(pisa_pfi_plot, "ggplot"),
  nrow(pisa_pfi_plot$data) == 15L,
  sum(pisa_pfi_plot$data$feature_group == "socioeconomic_joint_block") == 1L,
  identical(
    pisa_pfi_plot$data[feature_group == "socioeconomic_joint_block", query_scope],
    "Alternative overlapping joint query"
  ),
  " " %in% levels(pisa_pfi_plot$data$display_label),
  inherits(pisa_pfi_plot$layers[[2L]]$geom, "GeomHline")
)

legacy_calibration = data.table(
  estimand = "integrated_absolute_calibration_error",
  estimate = 0.02,
  definition = "Legacy label"
)
canonical_calibration = .analysis_standardize_calibration_metric(legacy_calibration)
stopifnot(
  identical(canonical_calibration$estimand, "uniform_grid_mean_absolute_calibration_error"),
  grepl("Arithmetic mean", canonical_calibration$definition, fixed = TRUE),
  .analysis_calibration_metric_aliases()[alias == "integrated_absolute_calibration_error", alias_status] ==
    "deprecated_alias"
)

canonical_name = "uniform_grid_mean_absolute_calibration_error"
deprecated_name = "integrated_absolute_calibration_error"
mixed_calibration = list(
  summary = data.table(
    estimand = c(canonical_name, deprecated_name),
    estimate = c(0.02, 0.99),
    lower = c(0.01, 0.98),
    upper = c(0.03, 1.00),
    canonical_estimand = canonical_name,
    estimand_status = c("canonical", "deprecated_alias")
  ),
  bootstrap_summary = data.table(
    uniform_grid_mean_absolute_calibration_error = c(0.01, 0.02),
    integrated_absolute_calibration_error = c(0.98, 0.99)
  ),
  metric_aliases = .analysis_calibration_metric_aliases()
)
standardized_calibration = .analysis_standardize_calibration_metric(mixed_calibration)
stopifnot(
  nrow(standardized_calibration$summary) == 1L,
  identical(standardized_calibration$summary$estimand, canonical_name),
  identical(standardized_calibration$summary$estimate, 0.02),
  identical(standardized_calibration$summary$estimand_status, "canonical"),
  canonical_name %in% names(standardized_calibration$bootstrap_summary),
  !(deprecated_name %in% names(standardized_calibration$bootstrap_summary)),
  identical(
    standardized_calibration$bootstrap_summary[[canonical_name]],
    c(0.01, 0.02)
  ),
  standardized_calibration$metric_aliases[alias == deprecated_name, alias_status] == "deprecated_alias"
)

# Paper components are saved separately, while their outer text is retained in metadata.
temporary_root = tempfile("csdg-study-plot-test-")
dir.create(temporary_root, recursive = TRUE)
on.exit(unlink(temporary_root, recursive = TRUE, force = TRUE), add = TRUE)
.analysis_reset_figure_registry()
.save_plot(
  ice_plot + theme(legend.position = "none"),
  file.path(temporary_root, "ice"),
  width = 9,
  height = 6,
  metadata = list(latex_assembly_note = "Assemble four panels and place the shared legend beneath them.")
)
.save_plot(
  csdg_plot_legend(ice_plot),
  file.path(temporary_root, "ice__legend"),
  width = 9,
  height = 1.25,
  metadata = list(
    figure_group_id = "ice",
    is_grouped_figure = TRUE,
    latex_group_id = "ice",
    panel_id = "shared_legend",
    panel_order = 5L,
    panel_heading = "Shared ICE quantile legend",
    group_title = ice_plot$labels$title,
    group_subtitle = ice_plot$labels$subtitle,
    latex_assembly_note = "Place the shared legend beneath the four panels."
  )
)
ice_registry = .analysis_figure_registry_table()
stopifnot(
  nrow(ice_registry) == 5L,
  identical(ice_registry$panel_order, 1:5),
  all(ice_registry$figure_group_id == "ice"),
  all(ice_registry$is_grouped_figure),
  all(ice_registry$group_title == ice_plot$labels$title),
  all(ice_registry$group_subtitle == ice_plot$labels$subtitle)
)
.analysis_reset_figure_registry()
faceted = ggplot(
  data.table(
    panel = c("A", "B"),
    x = c(1, 2),
    category = c("A deliberately long category label", "Short label")
  ),
  aes(x = x, y = category)
) +
  geom_point() +
  facet_wrap(~ panel, ncol = 1L, scales = "free_y") +
  labs(title = "Group title", subtitle = "Group subtitle", caption = "Group caption") +
  theme_minimal(base_size = 12)
components = .analysis_plot_components(faceted, file.path(temporary_root, "alignment_check"))
unaligned_grobs = lapply(components, function(component) ggplotGrob(component$plot))
aligned_components = .analysis_align_component_widths(components)
stopifnot(
  !identical(unaligned_grobs[[1L]]$widths, unaligned_grobs[[2L]]$widths),
  identical(aligned_components[[1L]]$plot$widths, aligned_components[[2L]]$plot$widths)
)
.save_plot(
  faceted,
  file.path(temporary_root, "faceted"),
  width = 7,
  height = 4.7,
  min_component_height = 2.35,
  align_component_widths = TRUE,
  metadata = list(
    study = "synthetic",
    role = "test",
    estimand_or_scope = "synthetic",
    uncertainty_semantics = "none",
    privacy_scope = "public",
    source_tables = "tables/synthetic.csv"
  )
)
registry = .analysis_figure_registry_table()
stopifnot(
  nrow(registry) == 2L,
  all(file.exists(file.path(temporary_root, paste0(registry$figure_id, ".pdf")))),
  all(file.exists(file.path(temporary_root, paste0(registry$figure_id, ".png")))),
  all(registry$group_title == "Group title"),
  all(registry$group_subtitle == "Group subtitle"),
  all(registry$height_in == 2.35),
  all(registry$group_caption == "Group caption"),
  all(registry$is_grouped_figure)
)

pisa_catalog = .analysis_pisa_figure_catalog()
ale_semantics = .analysis_ale_semantics_table()
stopifnot(
  nrow(pisa_catalog) == 17L,
  !anyDuplicated(pisa_catalog$figure_id),
  all(nzchar(pisa_catalog$source_tables)),
  all(nzchar(pisa_catalog$uncertainty_semantics)),
  grepl(
    "Selected focal XGBoost regression pipeline",
    pisa_catalog[figure_id == "pisa_rich_ale_1d", estimand_scope],
    fixed = TRUE
  ),
  grepl(
    "Schools sampled with replacement independently within country",
    pisa_catalog[figure_id == "pisa_rich_ale_1d", uncertainty_semantics],
    fixed = TRUE
  ),
  nrow(ale_semantics) == 4L,
  all(nzchar(ale_semantics$selected_learner)),
  all(nzchar(ale_semantics$fit_scope)),
  all(nzchar(ale_semantics$plausible_value_scope)),
  all(nzchar(ale_semantics$analytic_distribution)),
  all(nzchar(ale_semantics$bootstrap_refit_status)),
  all(nzchar(ale_semantics$omitted_uncertainty)),
  all(ale_semantics[ale_dimension == "1d", bootstrap_replicates_requested] == 500L),
  all(ale_semantics[ale_dimension == "1d", bootstrap_refits_successful] == 500L),
  all(!ale_semantics[ale_dimension == "1d", simultaneous_interval]),
  all(ale_semantics$support_threshold == 20L),
  all(grepl("stripped from PDF and PNG", ale_semantics$plot_outer_text, fixed = TRUE))
)

cat("Study-specific plot tests passed.\n")
