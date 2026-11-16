# Figure for the PISA application: importance of individual predictors (panel a) and of socioeconomic information
# under different blocks and designs (panel b), for the four learners, from the aggregates written by
# analysis/pisa/ses_reanalysis.R. Points are means over the 10 plausible values; ranges are minimum and maximum
# across plausible values (not confidence intervals).
# Usage (from the repository root): Rscript analysis/pisa/render_ses_figure.R [stage]
# Needs ggplot2 4.0.0 or later (argument reverse of position_dodge()).

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
args = commandArgs(trailingOnly = TRUE)
stage = if (length(args)) args[[1L]] else "exploratory"
result_dir = file.path("analysis/pisa/results/ses_reanalysis", stage)
pfi = fread(file.path(result_dir, "pfi_summary.csv"))

learner_levels = c("xgboost", "random_forest", "ridge_fixed", "ridge_cv")
learner_labels = c("XGBoost", "Random forest", "Linear regression", "Ridge regression")
labels = c(METASPAM = "Assessing credibility", METASUM = "Summarizing strategies",
  SCREADCOMP = "Perceived reading competence", SCREADDIFF = "Perceived reading difficulty",
  JOYREAD = "Enjoyment of reading", UNDREM = "Understanding and remembering",
  ESCS = "Index of economic, social, and cultural status", HOMEPOS = "Home possessions", WEALTH = "Family wealth",
  ST001D01T = "Grade", LANGN = "Language at home")

summarize = function(table) {
  table[, .(mean = mean(mean_importance), low = min(mean_importance), high = max(mean_importance)),
    by = .(learner, feature_group)]
}
blocks = c("socioeconomic_joint_block", "ses_family", "homepos_family")
individual = summarize(pfi[design == "full" & !feature_group %in% blocks])
individual[, rank := frank(-mean, ties.method = "first"), by = learner]
shown = unique(individual[rank <= 5, feature_group])
stopifnot(all(shown %in% names(labels)))
individual = individual[feature_group %in% shown]
order_groups = individual[learner == "xgboost"][order(-mean), feature_group]
order_groups = c(order_groups[order_groups %in% shown], setdiff(shown, order_groups))
individual[, label := factor(paste0(labels[feature_group], " (", feature_group, ")"),
  levels = rev(paste0(labels[order_groups], " (", order_groups, ")")))]

socio = rbindlist(list(
  summarize(pfi[design == "full" & feature_group == "socioeconomic_joint_block"])[,
    label := "Block: ESCS, parental occupation, home possessions"],
  summarize(pfi[design == "full" & feature_group == "ses_family"])[,
    label := "All eight socioeconomic indices"],
  summarize(pfi[design == "composite_only" & feature_group == "ESCS"])[,
    label := "ESCS only (refit)"],
  summarize(pfi[design == "components_only" & feature_group == "ses_components"])[,
    label := "Six component indices only (refit)"]
))
socio[, label := factor(label, levels = rev(unique(label)))]

style = function(plot, limit) {
  plot + scale_shape_manual(values = c(16L, 22L, 17L, 25L), labels = learner_labels, drop = FALSE) +
    scale_x_continuous(limits = c(0, limit), expand = expansion(mult = c(0.015, 0.02))) +
    labs(x = "Increase in held-out RMSE (reading-score points)", y = NULL, shape = NULL) +
    theme_minimal(base_size = 10, base_family = "Arial") +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      axis.text = element_text(color = "black", size = 9),
      legend.position = "bottom", legend.location = "plot", legend.text = element_text(size = 9),
      legend.key.spacing.x = unit(10, "pt"), legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(6, "pt"),
      plot.margin = margin(4, 10, 4, 4)) +
    guides(shape = guide_legend(nrow = 1L))
}
panel = function(data, limit) {
  data[, learner := factor(learner, levels = learner_levels)]
  # Learners are offset within each row in legend order (XGBoost on top); lines separate the rows.
  dodge = position_dodge(width = 0.7, reverse = TRUE)
  style(ggplot(data, aes(x = mean, y = label, shape = learner, group = learner)) +
    geom_hline(yintercept = seq_len(nlevels(data$label) - 1L) + 0.5, colour = "grey85", linewidth = 0.3) +
    geom_errorbar(aes(xmin = low, xmax = high), position = dodge, width = 0, linewidth = 0.45) +
    geom_point(position = dodge, size = 2, stroke = 0.5, fill = "white"), limit)
}
limit_a = ceiling(max(individual$high)) + 0.5
limit_b = ceiling(max(socio$high)) + 0.5
for (name in c("a", "b")) {
  plot = if (name == "a") panel(individual, limit_a) else panel(socio, limit_b)
  height = if (name == "a") 0.26 * length(shown) + 0.55 else 1.6
  ggsave(file.path(result_dir, paste0("pisa_ses_figure_", name, ".pdf")), plot, width = 6.5, height = height,
    device = cairo_pdf, bg = "white")
  ggsave(file.path(result_dir, paste0("pisa_ses_figure_", name, ".png")), plot, width = 6.5, height = height,
    dpi = 300, bg = "white")
}
fwrite(rbind(individual[, .(panel = "a", learner, feature_group, label = as.character(label), mean, low, high)],
  socio[, .(panel = "b", learner, feature_group, label = gsub("\n", " ", as.character(label)), mean, low, high)]),
  file.path(result_dir, "pisa_ses_figure_source.csv"))
