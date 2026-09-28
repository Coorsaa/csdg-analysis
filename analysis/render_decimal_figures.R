#!/usr/bin/env Rscript

# Reformat selected current figures from frozen plot inputs; no models or diagnostics are fitted.
# Usage: Rscript analysis/render_decimal_figures.R article_dir reference_dir [private_case_table]

arguments = commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) %in% c(2L, 3L))
article_dir = normalizePath(arguments[[1L]], mustWork = TRUE)
reference_dir = normalizePath(arguments[[2L]], mustWork = TRUE)
case_path = if (length(arguments) == 3L) normalizePath(arguments[[3L]], mustWork = TRUE) else NULL
root = getwd()
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
library(mlr3autoiml)
source("analysis/shared/study_helpers.R")
source("analysis/shared/evidence_helpers.R")
source("analysis/shared/decimal_labels.R")
.analysis_palette = function() autoiml_palette(style = "monochrome")

records = new.env(parent = emptyenv())
records$values = list()
hash = function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)

export = function(plot, path, width, height, axes, annotations = FALSE, inputs) {
  stem = tools::file_path_sans_ext(basename(path))
  reference = file.path(reference_dir, paste0(stem, "__before.pdf"))
  # Building on the same Cairo device retains the original font metrics and panel spacing.
  grDevices::cairo_pdf(reference, width = width, height = height, bg = "white")
  result = .analysis_decimal_grob(plot, axes, annotations)
  grid::grid.draw(result$original)
  grDevices::dev.off()
  stopifnot(length(result$changes) > 0L)
  ggsave(path, result$revised, width = width, height = height, device = cairo_pdf, bg = "white")
  png = sub("\\.pdf$", ".png", path)
  ggsave(png, result$revised, width = width, height = height, dpi = 320, bg = "white")
  records$values[[stem]] = list(
    figure_id = stem, pdf = path, pdf_sha256 = hash(path), png_sha256 = hash(png),
    input_hashes = lapply(inputs, hash), inputs = inputs,
    label_changes = result$changes, geometry_unchanged = TRUE, empirical_execution = FALSE
  )
}

# Capture the existing aggregate-backed local-comparison generators without publishing other figures.
captured = new.env(parent = emptyenv())
captured$plots = list()
renderer = new.env(parent = globalenv())
renderer$ggsave = function(filename, plot, ...) {
  captured$plots[[tools::file_path_sans_ext(basename(filename))]] = plot
  invisible(filename)
}
renderer$fwrite = function(...) invisible(NULL)
sys.source("analysis/targeted/render_targeted_figures.R", envir = renderer)
renderer$render_targeted_figures(root)
main_figures = file.path(article_dir, "v4/analysis_figs")
for (id in c("shild_common_probability_weighted_r2", "shild_convergence_weighted_r2")) {
  input = if (grepl("convergence", id)) "shild_convergence_summary.csv" else "shild_common_scale_summary.csv"
  export(captured$plots[[id]], file.path(main_figures, paste0(id, ".pdf")), 4, 3.2, "y",
    inputs = file.path("analysis/targeted/results/shild", input))
}

# The existing private case table is read only after its identity matches the published input contract.
# Neither that table nor case identifiers are added to the upload.
if (!is.null(case_path)) {
  contract = fread("supplement/PRIVATE_INPUT_PROVENANCE.csv")
  stopifnot(identical(hash(case_path), contract[input_name == basename(case_path), sha256]))
  cases = fread(case_path)
  stopifnot(nrow(cases) == 300L, all(cases$n_perturbation_replicates == 20L))
  bandwidth_case_path = file.path(dirname(case_path), "shild_g4_local_fidelity_bandwidth_cases.csv")
  stopifnot(identical(hash(bandwidth_case_path), contract[input_name == basename(bandwidth_case_path), sha256]))
  panels = .evidence_systematic_g4_panels(cases, fread(bandwidth_case_path))
  id = "shild_g4_systematic_local_audit__apparent_vs_cross_fitted"
  panel = .analysis_plot_components(panels$apparent_vs_cross_fitted, id)[[1L]]$plot
  export(panel, file.path(main_figures, paste0(id, ".pdf")), 5, 4.2, c("x", "y"), inputs = case_path)
  rm(cases, panels)
} else {
  message("The verified paired-case scatter is retained; its private plotting table was not supplied.")
}

deciles_path = "supplement/plot_data/shild_g4_decile_plot_summary.csv"
bandwidth_path = "supplement/plot_data/shild_g4_bandwidth_plot_summary.csv"
deciles = fread(deciles_path)
bandwidths = fread(bandwidth_path)
for (data in list(deciles, bandwidths)) {
  columns = grep("weighted_", names(data), value = TRUE)
  setnames(data, columns, sub("weighted_", "", columns, fixed = TRUE))
}
panel = .evidence_systematic_g4_summary_panels(deciles, bandwidths)$relative_fidelity_by_prediction
id = "shild_g4_systematic_local_audit__relative_fidelity_by_prediction"
panel = .analysis_plot_components(panel, id)[[1L]]$plot
export(panel, file.path(main_figures, paste0(id, ".pdf")), 5, 4.2, "y", inputs = c(deciles_path, bandwidth_path))

table_dir = "supplement/shild/tables"
read_table = function(name) fread(file.path(table_dir, name))
save_supplement = function(plot, stem, width, height, axes, annotations = FALSE, tables) {
  components = .analysis_plot_components(plot, stem)
  for (component in components) {
    path = file.path("supplement/shild/figures", paste0(component$figure_id, ".pdf"))
    panel = .analysis_raise_point_layers(component$plot)
    component_width = if (component$is_grouped_figure) max(5, width * component$width_fraction) else width
    component_height = if (component$is_grouped_figure) max(4, height * component$height_fraction) else height
    export(panel, path, component_width, component_height, axes, annotations, file.path(table_dir, tables))
  }
}

calibration_input = "shild_g3_calibration_plot_source.csv"
curve = read_table(calibration_input)
stopifnot(!"n" %in% names(curve), all(c("lower", "upper") %in% names(curve)))
plot = csdg_plot_data(curve, type = "calibration", x_label = "Mean held-out probability of discontent",
  y_label = "Observed discontent fraction", base_size = 14, axis_text_size = 12.5,
  axis_title_size = 13, style = "monochrome")
save_supplement(plot, "shild_g3_calibration", 8, 6, c("x", "y"), tables = calibration_input)

audit_labels = c(audit_gender = "Gender", audit_age_group = "Age group")
subgroup_labels = c("audit_gender::Man" = "Men", "audit_gender::Woman" = "Women",
  "audit_age_group::16-24" = "16-24", "audit_age_group::25-34" = "25-34", "audit_age_group::35-44" = "35-44",
  "audit_age_group::45-54" = "45-54", "audit_age_group::55-64" = "55-64")
subgroup_inputs = c("shild_gate_g7a__subgroup_audit__metrics.csv", "shild_gate_g7a__subgroup_audit__uncertainty.csv")
plot = csdg_plot_data(list(metrics = read_table(subgroup_inputs[[1L]]),
  uncertainty = read_table(subgroup_inputs[[2L]])), type = "subgroup", metric = "auc",
  audit_labels = audit_labels, subgroup_labels = subgroup_labels,
  subgroup_order = c("16-24", "25-34", "35-44", "45-54", "55-64", "Women", "Men"),
  x_limits = c(0.5, 1), base_size = 15, direct_label_size = 4, axis_text_size = 13,
  axis_title_size = 13, style = "monochrome")
save_supplement(plot, "shild_g7_subgroups", 10, 4.6, "x", TRUE, subgroup_inputs)

contrast_input = "shild_gate_g7a__subgroup_audit__contrasts.csv"
plot = csdg_plot_data(read_table(contrast_input), type = "subgroup_contrast", metric = "auc",
  audit_labels = audit_labels, subgroup_labels = subgroup_labels, x_label = "ROC AUC difference (first - second)",
  base_size = 16, direct_label_size = 4.5, axis_text_size = 13, style = "monochrome")
save_supplement(plot, "shild_g7_subgroup_contrasts", 10, 7, "x", TRUE, contrast_input)

distribution_input = "shild_multiplicity_distribution.csv"
bins_input = "shild_multiplicity_bins.csv"
quantiles = read_table(distribution_input)[statistic %in% c("p50", "p90", "p95"), .(statistic, prediction_range)]
plot = csdg_plot_data(read_table(bins_input), type = "multiplicity", quantiles = quantiles,
  x_label = "Within-respondent probability range", base_size = 14, direct_label_size = 4, style = "monochrome")
save_supplement(plot, "shild_prediction_multiplicity", 8, 6, "x", TRUE, c(distribution_input, bins_input))

stopifnot(length(records$values) == if (is.null(case_path)) 8L else 9L)
jsonlite::write_json(records$values, file.path(reference_dir, "FIGURE_DECIMAL_AUDIT.json"), pretty = TRUE, auto_unbox = TRUE)
message("Reformatted ", length(records$values), " figure components; numerical inputs and drawing instructions preserved.")
