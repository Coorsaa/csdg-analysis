#!/usr/bin/env Rscript

# Recreate retained displays from frozen public aggregates, without fitting models or diagnostics.
# The apparent-versus-cross-fitted scatter remains a verified render because its case pairs are restricted.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
library(mlr3autoiml)
source("analysis/shared/study_helpers.R")
source("analysis/shared/evidence_helpers.R")
source("analysis/pisa/prepare_data.R")
source("analysis/shared/render_paper_pisa.R")
source("analysis/shared/render_matched_controls.R")
.analysis_palette = function() autoiml_palette(style = "monochrome")

evidence_dir = "final_docs/EVIDENCE_FIGURES/figures"
figure_dir = "final_docs/figures"
.analysis_reset_figure_registry()

# Performance differences and PFI order changes use distinct, explicitly labeled scales.
sensitivity = rbindlist(lapply(c("SHILD", "PISA"), function(study) {
  fread(file.path("final_docs", paste0(study, "_PERFORMANCE_EXPLANATION_SENSITIVITY.csv")))
}), fill = TRUE)
for (study in c("SHILD", "PISA")) {
  .save_plot(
    .evidence_performance_explanation_plot(sensitivity, study),
    file.path(evidence_dir, paste0("performance_explanation_sensitivity__", tolower(study))),
    width = 10.2, height = 2.65, min_component_width = 2.55, min_component_height = 2.65,
    metadata = list(study = study, uncertainty_semantics = "Descriptive accepted-model comparison; no interval")
  )
}

# Descriptive medians and type-8 quartiles across within-case replicate medians.
# The decile panels form Figure S1 of the online supplement; the locality curve (kernel width) is not in the article.
deciles = fread("supplement/plot_data/shild_g4_decile_plot_summary.csv")
bandwidths = fread("supplement/plot_data/shild_g4_bandwidth_plot_summary.csv")
for (data in list(deciles, bandwidths)) {
  columns = grep("weighted_", names(data), value = TRUE)
  setnames(data, columns, sub("weighted_", "", columns, fixed = TRUE))
}
systematic = .evidence_systematic_g4_summary_panels(deciles, bandwidths)
for (panel in names(systematic)) {
  .save_plot(
    systematic[[panel]], file.path(evidence_dir, paste0("shild_g4_systematic_local_audit__", panel)),
    width = 5, height = 4.2,
    metadata = list(
      study = "SHILD", group_caption = .evidence_systematic_g4_group_text()$caption,
      uncertainty_semantics = "Median and 25th-75th percentiles across within-case medians; not confidence intervals"
    )
  )
}

# Training-size-matched country-exclusion contrasts retain their frozen values and interpretation.
contrasts = fread("final_docs/PISA_LOCO_MATCHED_CONTROL_CONTRASTS.csv")
invisible(.save_loco_contrast_plots(
  .render_pisa_matched_control_contrast_plots(contrasts), figure_dir
))
message("Regenerated retained aggregate-backed figures; paired case scatter retained from its verified PDF.")
