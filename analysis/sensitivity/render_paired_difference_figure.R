# Paired probability-scale RMSE difference by prediction-score decile (Figure S2b of the methods supplement).
# Reads the public aggregate table of the targeted SHILD extension; no respondent-level data are used.

suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

# Run from the analysis repository root:
#   Rscript analysis/sensitivity/render_paired_difference_figure.R [OUTPUT_PDF]
# The default output is written to analysis/sensitivity/results/; copy it to the manuscript's
# v5/analysis_figs/ folder.
args = commandArgs(trailingOnly = TRUE)
root = getwd()
stopifnot(file.exists(file.path(root, "analysis", "sensitivity", "render_paired_difference_figure.R")))
source_csv = file.path(root, "analysis", "targeted", "results", "shild",
  "shild_common_scale_paired_by_decile.csv")
output_pdf = if (length(args)) args[1L] else file.path(root, "analysis", "sensitivity", "results",
  "shild_common_probability_paired_rmse_difference.pdf")

data = fread(source_csv)[metric == "weighted_rmse"]
stopifnot(nrow(data) == 10L, all(data$n_cases == 10L), all(data$q75 < 0))

theme = theme_minimal(base_size = 12, base_family = "Arial") + theme(
  panel.grid.minor = element_blank(), plot.margin = margin(7, 9, 7, 7),
  axis.title.y = element_text(margin = margin(r = 7)), axis.title.x = element_text(margin = margin(t = 7)))

plot = ggplot(data, aes(x = factor(prediction_score_decile), y = median)) +
  geom_hline(yintercept = 0, linewidth = .5, linetype = 2) +
  geom_errorbar(aes(ymin = q25, ymax = q75), width = .25, linewidth = .5) +
  geom_point(shape = 21, fill = "white", color = "black", size = 2.4, stroke = .65) +
  scale_y_continuous(limits = c(-.035, .005), breaks = seq(-.03, 0, .01)) +
  labs(x = "Decile of predicted probability", y = "RMSE difference\n(log-odds minus probability)") + theme

ggsave(output_pdf, plot, width = 4, height = 3.2, device = cairo_pdf, bg = "white")
