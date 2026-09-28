# Model and plausible-value interpretation of the existing PISA importance estimates.
# No predictive fitting or new perturbation is performed here.

verify_pisa_aggregate = function(candidate, baseline, agreement) {
  keys = c("outcome_id", "feature_group")
  stopifnot(nrow(candidate) == 1260L, nrow(baseline) == 420L)
  stopifnot(setequal(unique(candidate$learner_name), c("xgboost", "random_forest", "ridge")))
  stopifnot(setequal(unique(candidate$outcome_id), paste0("PV", seq_len(10L), "READ")))
  stopifnot(!anyDuplicated(candidate[, c("learner_name", keys), with = FALSE]))
  selected = data.table::copy(candidate[learner_name == "xgboost"])
  baseline = data.table::copy(baseline)
  data.table::setorderv(selected, keys)
  data.table::setorderv(baseline, keys)
  # The original aggregate ranked an overlapping joint query with its components.
  # Only the corrected canonical atomic ranks are used; all underlying values must match.
  fields = setdiff(intersect(names(selected), names(baseline)), "rank")
  stopifnot(isTRUE(all.equal(
    selected[, ..fields], baseline[, ..fields], tolerance = 1e-12, check.attributes = FALSE
  )))
  averaged = candidate[, .(importance = mean(mean_importance)), by = .(learner_name, feature_group)]
  comparisons = lapply(c("random_forest", "ridge"), function(model) {
    result = mlr3autoiml::csdg_importance_agreement(
      averaged[learner_name == "xgboost", .(feature_group, importance)],
      averaged[learner_name == model, .(feature_group, importance)],
      top_k = 5L,
      practical_tolerances = sort(unique(agreement$practical_tolerance)),
      exclude_groups = "socioeconomic_joint_block"
    )
    computed = result$tolerance_sensitivity
    expected = agreement[learner_1 == "xgboost" & learner_2 == model]
    data.table::setorder(computed, practical_tolerance)
    data.table::setorder(expected, practical_tolerance)
    stopifnot(isTRUE(all.equal(
      computed, expected[, names(computed), with = FALSE], tolerance = 1e-10, check.attributes = FALSE
    )))
    stopifnot(all(abs(expected$top_k_jaccard - result$top_k$jaccard) < 1e-12))
    computed[, learner_name := model]
    computed
  })
  data.table::rbindlist(comparisons)
}

pisa_content_synthesis = function(root = ".", additional_aggregate = NULL) {
  result_dir = file.path(root, "analysis/targeted/results/pisa")
  resource_dir = file.path(root, "analysis/targeted/resources")
  dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(resource_dir, recursive = TRUE, showWarnings = FALSE)
  saved = file.path(resource_dir, "pisa_model_pfi_all_pv.csv")
  source_path = if (is.null(additional_aggregate)) saved else additional_aggregate
  if (!file.exists(source_path)) stop("Supply the frozen model-by-PV PFI aggregate; do not refit predictors.")
  values = data.table::fread(source_path)
  canonical_files = c(
    "baseline_evidence/STUDY_RESULTS/PISA/tables/pisa_all_pv_pfi_summary.csv",
    "baseline_evidence/STUDY_RESULTS/PISA/tables/pisa_model_importance_agreement.csv"
  )
  canonical = data.table::fread(file.path(root, canonical_files[[1L]]))
  agreement = data.table::fread(file.path(root, canonical_files[[2L]]))
  verified_agreement = verify_pisa_aggregate(values, canonical, agreement)
  if (normalizePath(source_path) != normalizePath(saved, mustWork = FALSE)) {
    if (file.exists(saved)) {
      stopifnot(identical(digest::digest(file = saved, algo = "sha256"),
        digest::digest(file = source_path, algo = "sha256")))
    } else {
      stopifnot(file.copy(source_path, saved, overwrite = FALSE))
    }
  }
  values[, atomic := feature_group != "socioeconomic_joint_block"]
  values[atomic == TRUE, atomic_rank := data.table::frank(-mean_importance, ties.method = "average"),
    by = .(learner_name, outcome_id)]
  summary = values[, .(
    importance_mean = mean(mean_importance),
    pv_min = min(mean_importance), pv_max = max(mean_importance),
    top_five_pv_count = if (all(atomic)) sum(atomic_rank <= 5) else NA_integer_,
    rank_min = if (all(atomic)) min(atomic_rank) else NA_real_,
    rank_max = if (all(atomic)) max(atomic_rank) else NA_real_,
    n_plausible_values = data.table::uniqueN(outcome_id),
    atomic = all(atomic)
  ), by = .(learner_name, feature_group)]
  summary[atomic == TRUE, mean_scale_rank := data.table::frank(-importance_mean, ties.method = "average"),
    by = learner_name]
  selected_groups = unique(summary[atomic == TRUE & mean_scale_rank <= 5, feature_group])
  labels = data.table::data.table(
    feature_group = c("METASPAM", "METASUM", "SCREADCOMP", "SCREADDIFF", "JOYREAD", "UNDREM",
      "ESCS", "HOMEPOS", "WEALTH", "ST001D01T"),
    content_label = c("Assessing credibility", "Summarizing strategies", "Perceived reading competence",
      "Perceived reading difficulty", "Enjoyment of reading", "Understanding and remembering",
      "Economic, social, and cultural status", "Home possessions", "Family wealth", "Grade"),
    content_family = c(rep("Reading-related indicator", 6L), rep("Family-background indicator", 3L),
      "Schooling indicator"),
    interpretation = "Reported indicator label; not a newly constructed latent scale or a causal effect",
    source = "OECD PISA 2018 code labels; OECD 21st-Century Readers, 2021"
  )
  stopifnot(setequal(selected_groups, labels$feature_group))
  displayed = merge(summary[feature_group %in% selected_groups], labels, by = "feature_group")
  order_groups = summary[learner_name == "xgboost" & feature_group %in% selected_groups][
    order(importance_mean), feature_group]
  displayed[, plot_label := factor(
    paste0(content_label, "\n(", feature_group, ")"),
    levels = paste0(labels[match(order_groups, feature_group), content_label], "\n(", order_groups, ")")
  )]
  displayed[, model := factor(learner_name,
    levels = c("xgboost", "random_forest", "ridge"),
    labels = c("XGBoost", "Random forest", "Ridge regression"))]
  dodge = ggplot2::position_dodge(width = 0.58)
  plot = ggplot2::ggplot(displayed, ggplot2::aes(
    x = importance_mean, y = plot_label, shape = model, group = model
  )) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = pv_min, xmax = pv_max),
      orientation = "y", width = 0.08, linewidth = 0.38, position = dodge) +
    ggplot2::scale_shape_manual(values = c(21L, 22L, 24L)) +
    ggplot2::scale_x_continuous(limits = c(0, 8.3), breaks = seq(0, 8, 2),
      expand = ggplot2::expansion(mult = c(0.015, 0.01))) +
    ggplot2::labs(x = "Increase in held-out RMSE (reading-score points)", y = NULL, shape = NULL) +
    ggplot2::theme_minimal(base_size = 12, base_family = "Arial") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "black", size = 11),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 8), lineheight = 0.98),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 9)),
      legend.position = "bottom", legend.text = ggplot2::element_text(size = 11),
      plot.margin = ggplot2::margin(8, 16, 8, 8)
    ) +
    ggplot2::geom_point(position = dodge, size = 2.05, stroke = 0.5, fill = "white")
  ggplot2::ggsave(file.path(result_dir, "pisa_model_pv_content.pdf"), plot,
    device = grDevices::cairo_pdf, width = 7.8, height = 6.2, units = "in", bg = "white")
  ggplot2::ggsave(file.path(result_dir, "pisa_model_pv_content.png"), plot,
    width = 7.8, height = 6.2, units = "in", dpi = 240, bg = "white")
  data.table::fwrite(summary, file.path(result_dir, "pisa_model_pv_content_summary.csv"))
  data.table::fwrite(displayed, file.path(result_dir, "pisa_model_pv_figure_data.csv"))
  data.table::fwrite(labels, file.path(result_dir, "pisa_content_label_dictionary.csv"))
  data.table::fwrite(verified_agreement, file.path(result_dir, "pisa_verified_model_agreement.csv"))
  provenance = list(
    run_id = "targeted_pisa_aggregate_20260910", design_status = "retrospective_exploratory_synthesis",
    source = "analysis/targeted/resources/pisa_model_pfi_all_pv.csv",
    source_sha256 = digest::digest(file = saved, algo = "sha256"),
    canonical_basis = list(
      description = "Archived study outputs of the primary PISA run",
      files = canonical_files,
      sha256 = vapply(file.path(root, canonical_files), digest::digest, character(1L),
        file = TRUE, algo = "sha256", USE.NAMES = FALSE)
    ),
    identity_checks = c("All 420 selected-model numeric and descriptive rows match canonical evidence",
      "Model-class agreement at all three tolerances exactly reconciles with canonical aggregate evidence"),
    outcome_definition = "Ten separately analyzed reading proficiency plausible values",
    variation = list(varied_component = "Outcome realization and learner",
      held_constant = "Analytic rows, folds, RMSE loss, and within-country/school-aware marginal perturbation",
      same_estimand = FALSE,
      rationale = "Each learner and outcome realization defines a different fitted-model reliance quantity",
      invariance_claimed = "Leading content areas, not exact coefficients or complete ranks",
      relevance_to_proposition = "Tests whether the selected-model indicator interpretation recurs in the evaluated set"),
    joint_pfi_decision = "No new joint query is needed for the indicator-level question; overlapping block retained separately",
    uncertainty = "Across-PV min-max ranges are descriptive, not sampling confidence intervals or independent replication",
    models_refit = FALSE, new_pfi_queries = FALSE, raw_data_reproduced = FALSE
  )
  jsonlite::write_json(provenance, file.path(result_dir, "PISA_SYNTHESIS_PROVENANCE.json"),
    pretty = TRUE, auto_unbox = TRUE)
  invisible(list(summary = summary, displayed = displayed, plot = plot, provenance = provenance))
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(data.table))
  library(mlr3autoiml)
  arguments = commandArgs(trailingOnly = TRUE)
  supplied = if (length(arguments)) arguments[[1L]] else NULL
  pisa_content_synthesis(additional_aggregate = supplied)
}
