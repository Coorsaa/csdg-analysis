# Public aggregate-only graphics for the targeted SHILD extension and controlled illustration.

render_targeted_figures = function(project_root = getwd()) {
  suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
  output = file.path(project_root, "analysis/targeted/results/shild")
  rows = new.env(parent = emptyenv())
  rows$values = list()
  theme = theme_minimal(base_size = 12, base_family = "Arial") + theme(
    panel.grid.minor = element_blank(), legend.position = "bottom", legend.text = element_text(size = 10),
    plot.margin = margin(7, 9, 7, 7), axis.title.y = element_text(margin = margin(r = 7)),
    axis.title.x = element_text(margin = margin(t = 7)))
  export = function(plot, id, title, caption, source, width = 4, height = 3.2) {
    ggsave(file.path(output, paste0(id, ".pdf")), plot, width = width, height = height,
      device = cairo_pdf, bg = "white")
    ggsave(file.path(output, paste0(id, ".png")), plot, width = width, height = height,
      dpi = 400, bg = "white")
    rows$values[[length(rows$values) + 1L]] = data.table(figure_id = id, title = title, caption = caption,
      source_table = source, pdf = paste0(id, ".pdf"), png = paste0(id, ".png"),
      width_in = width, height_in = height, font = "Arial", color = "monochrome",
      embedded_title_caption = FALSE, interval = "Across-case interquartile range; not a confidence interval",
      generator = "analysis/targeted/render_targeted_figures.R")
  }
  common = fread(file.path(output, "shild_common_scale_summary.csv"))
  for (measure in c("weighted_rmse", "weighted_r2")) {
    data = common[metric == measure]
    data[, model_label := factor(surrogate_scale, levels = c("response", "link"),
      labels = c("Probability", "Log-odds"))]
    label = if (measure == "weighted_rmse") "Weighted RMSE\n(probability scale)" else {
      "Weighted R-squared\n(probability scale)"
    }
    p = ggplot(data, aes(x = model_label, y = median)) +
      geom_errorbar(aes(ymin = q25, ymax = q75), width = .09, linewidth = .5) +
      geom_point(shape = 21, fill = "white", color = "black", size = 2.6, stroke = .65) +
      scale_y_continuous(limits = c(0, if (measure == "weighted_rmse") .030 else 1),
        breaks = if (measure == "weighted_rmse") seq(0, .03, .01) else seq(0, 1, .25),
        expand = expansion(mult = c(0, .04))) + labs(x = "Scale of the surrogate", y = label) + theme
    export(p, paste0("shild_common_probability_", measure), "Shared probability-scale evaluation",
      paste("Identical held-out neighbors, weights, and five surrogate cross-fit folds for the 100 original",
        "method-sensitivity cases and five seeds. Logit-fit predictions were transformed pointwise to probability.",
        "Points are across-case medians of within-case seed medians; whiskers are across-case 25th-75th percentiles,",
        "not confidence intervals. Probability-fit predictions were not clipped."), "shild_common_scale_summary.csv")
  }
  convergence = fread(file.path(output, "shild_convergence_summary.csv"))
  for (measure in c("weighted_rmse", "weighted_r2")) {
    data = convergence[metric == measure & design_role == "size"]
    data[, n_label := factor(n_training, levels = c(250, 500, 1000, 2000))]
    data[, model_label := factor(surrogate_scale, levels = c("response", "link"),
      labels = c("Probability", "Log-odds"))]
    dodge = position_dodge(width = .16)
    p = ggplot(data, aes(n_label, median, group = model_label, shape = model_label, linetype = model_label)) +
      geom_errorbar(aes(ymin = q25, ymax = q75), width = .1, linewidth = .45, position = dodge,
        show.legend = FALSE) +
      geom_line(linewidth = .55, position = dodge) +
      geom_point(size = 2.3, stroke = .6, fill = "white", position = dodge) +
      scale_shape_manual(values = c(21, 24), name = NULL) + scale_linetype_manual(values = c(1, 2), name = NULL) +
      scale_y_continuous(limits = c(0, if (measure == "weighted_rmse") .04 else 1),
        breaks = if (measure == "weighted_rmse") seq(0, .04, .01) else seq(0, 1, .25),
        expand = expansion(mult = c(0, .04))) +
      labs(x = "Number of perturbed points", y = if (measure == "weighted_rmse") {
        "Weighted RMSE\n(probability scale)"
      } else "Weighted R-squared\n(probability scale)") + theme
    export(p, paste0("shild_convergence_", measure), "Fixed-evaluation neighborhood learning curve",
      paste("Thirty deterministically balanced cases; five training draws; one independent 1000-neighbor evaluation",
        "set held fixed per case. Nested training sizes 250/500/1000/2000; normalized standardized-ridge penalty",
        "lambda=.001; kernel width .75. Points and whiskers are across-case medians and interquartile ranges after",
        "within-case seed aggregation, not confidence intervals. Both fits are scored on the probability scale."),
      "shild_convergence_summary.csv", height = 3.2)
  }
  contribution = fread(file.path(output, "shild_contribution_stability_summary.csv"))
  label_helper = new.env(parent = globalenv())
  sys.source(file.path(project_root, "analysis/shild/prepare_data.R"), envir = label_helper)
  feature_labels = label_helper$.analysis_shild_feature_labels()
  for (scale in c("response", "link")) {
    data = contribution[surrogate_scale == scale]
    data[, feature := factor(feature, levels = data$feature[order(data$median_contribution_rms)])]
    long = melt(data, id.vars = "feature",
      measure.vars = c("median_contribution_rms", "median_pairwise_contribution_rms_difference"),
      variable.name = "quantity", value.name = "value")
    long[, quantity := factor(quantity, labels = c("Contribution magnitude", "Between-seed difference"))]
    p = ggplot(long, aes(x = value, y = feature, shape = quantity)) +
      geom_point(position = position_dodge(width = .45), size = 2.1, fill = "white") +
      scale_shape_manual(values = c(21, 24), name = NULL) +
      scale_y_discrete(labels = feature_labels) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(.02, .05))) +
      labs(x = paste("Weighted RMS contribution",
        if (scale == "response") "(probability units)" else "(logit units)"),
        y = "Original predictor") + theme
    export(p, paste0("shild_contributions_", scale), "Contribution-sensitive stability",
      paste("All 27 original predictors; encoded terms summed within original variable. A common weighted",
        "1000-neighbor reference set centers all five seed-specific surrogates per case. Symbols show across-case",
        "medians of contribution RMS magnitude and mean pairwise RMS difference. No relevance cutoff is applied.",
        if (scale == "link") "Contributions are additive in logits, not probabilities." else {
          "Contributions are on the probability-fit scale."
        }), "shild_contribution_stability_summary.csv", width = 8.5, height = 8.6)
    rows$values[[length(rows$values)]]$interval = "No interval displayed"
  }
  fwrite(rbindlist(rows$values), file.path(output, "FIGURE_METADATA.csv"))
  controlled = file.path(project_root, "analysis/targeted/results/controlled")
  fwrite(data.table(figure_id = paste0("controlled_pfi_model_", 1:2), model = c("f1(X)=X1", "f2(X)=X2"),
    title = "Redundancy and measurement-error replacement questions",
    caption = paste("Analytical population PFI for the used indicator and the joint block under four declared",
      "Gaussian reliability settings. Independent marginal replacement, true conditional replacement, and joint",
      "replacement preserve different quantities. Unused-indicator PFI is zero and retained in the results table.",
      "Monte Carlo estimates and standard errors independently verify the displayed analytic values;",
      "no confidence interval is displayed."),
    width_in = 7.4, height_in = 3.5, source_table = "controlled_pfi_results.csv",
    interval = "None; analytical population expectations",
    generator = "analysis/targeted/controlled_examples.R"),
    file.path(controlled, "FIGURE_METADATA.csv"))
  invisible(rows$values)
}

if (sys.nframe() == 0L) render_targeted_figures()
