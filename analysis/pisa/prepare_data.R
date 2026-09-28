# PISA labels, audit summaries, setting diagnostics, and complete-case bookkeeping.

.analysis_pisa_feature_labels = function(manifest = NULL) {
  result = c(
    DIRINS = "Teacher-directed instruction (WLE)\n(DIRINS)",
    EFFORT1 = "How much effort did you put into this test?\n(EFFORT1)",
    EFFORT2 = "How much effort would you have invested?\n(EFFORT2)",
    ESCS = "Index of economic, social and cultural status\n(ESCS)",
    HISEI = "Index highest parental occupational status\n(HISEI)",
    HOMEPOS = "Home possessions (WLE)\n(HOMEPOS)",
    ISCEDO = "ISCED orientation\n(ISCEDO)",
    JOYREAD = "Joy/Like reading (WLE)\n(JOYREAD)",
    LANGN = "Language at home (3-digit code)\n(LANGN)",
    METASPAM = "Meta-cognition: assess credibility\n(METASPAM)",
    METASUM = "Meta-cognition: summarizing\n(METASUM)",
    PARED = "Index highest parental education in years of schooling\n(PARED)",
    SCREADCOMP = "Self-concept of reading: Perception of competence (WLE)\n(SCREADCOMP)",
    SCREADDIFF = "Self-concept of reading: Perception of difficulty (WLE)\n(SCREADDIFF)",
    ST001D01T = "Student International Grade (Derived)\n(ST001D01T)",
    UNDREM = "Meta-cognition: understanding and remembering\n(UNDREM)",
    WEALTH = "Family wealth (WLE)\n(WEALTH)"
  )
  if (!is.null(manifest)) {
    manifest = as.data.table(manifest)
    if (all(c("variable_out", "label") %in% names(manifest))) {
      manifest_labels = setNames(
        paste0(manifest$label, "\n(", manifest$variable_out, ")"),
        manifest$variable_out
      )
      result[names(manifest_labels)] = manifest_labels
    }
  }
  result[["socioeconomic_joint_block"]] = "Socioeconomic block\n(ESCS + HISEI + HOMEPOS)"
  result
}

.analysis_pisa_compact_feature_labels = function() {
  c(
    ADAPTIVITY = "Adaptive instruction",
    CULTPOSS = "Cultural possessions",
    DIRINS = "Teacher-led instruction",
    EFFORT1 = "Self-reported test effort",
    EFFORT2 = "Hypothetical test effort",
    ESCS = "Socioeconomic status",
    HEDRES = "Home educational resources",
    HISEI = "Parental occupational status",
    HOMEPOS = "Home possessions",
    ICTRES = "ICT resources",
    ISCEDD = "Education designation",
    ISCEDL = "Education level",
    ISCEDO = "Education orientation",
    LANGN = "Language at home",
    PARED = "Parental education",
    ST001D01T = "Student grade",
    STIMREAD = "Reading stimulation",
    TEACHINT = "Teacher interest",
    TEACHSUP = "Teacher support",
    WEALTH = "Family wealth"
  )
}

.analysis_pisa_plot_feature_labels = function(manifest = NULL) {
  labels = .analysis_pisa_feature_labels(manifest)
  compact_labels = .analysis_pisa_compact_feature_labels()
  labels[names(compact_labels)] = paste0(compact_labels, "\n(", names(compact_labels), ")")
  labels
}

.analysis_pisa_pfi_plot = function(
    data,
    feature_labels = NULL,
    top_n = 15L,
    base_size = 15,
    direct_label_size = 3.8) {
  data = copy(as.data.table(data))
  .analysis_require_columns(
    data,
    c("feature_group", "mean_across_pv", "minimum_across_pv", "maximum_across_pv"),
    "PISA all-plausible-value PFI table"
  )
  top_n = as.integer(top_n)
  if (length(top_n) != 1L || is.na(top_n) || top_n < 2L) {
    stop("`top_n` must be one integer of at least two.", call. = FALSE)
  }
  joint = data[feature_group == "socioeconomic_joint_block"]
  atomic = data[feature_group != "socioeconomic_joint_block"]
  if (nrow(joint) != 1L || !nrow(atomic)) {
    stop("PISA PFI requires one socioeconomic joint-block row and at least one atomic row.", call. = FALSE)
  }
  setorder(atomic, -mean_across_pv, feature_group)
  atomic = head(atomic, top_n - 1L)
  plot_data = rbindlist(list(atomic, joint), use.names = TRUE, fill = TRUE)
  plot_data[, query_scope := fifelse(
    feature_group == "socioeconomic_joint_block",
    "Alternative overlapping joint query",
    "Atomic feature group"
  )]
  labels = plot_data$feature_group
  if (!is.null(feature_labels)) {
    mapped = unname(feature_labels[plot_data$feature_group])
    labels[!is.na(mapped)] = mapped[!is.na(mapped)]
  }
  labels[plot_data$feature_group == "socioeconomic_joint_block"] = paste(
    labels[plot_data$feature_group == "socioeconomic_joint_block"],
    "alternative overlapping query; not additive",
    sep = "\n"
  )
  plot_data[, display_label := labels]
  atomic_labels = plot_data[query_scope == "Atomic feature group", display_label]
  joint_label = plot_data[query_scope == "Alternative overlapping joint query", display_label]
  display_levels = c(joint_label, " ", rev(atomic_labels))
  plot_data[, display_label := factor(display_label, levels = display_levels)]
  plot_data[, direct_label := sprintf("%.2f", mean_across_pv)]

  ggplot(plot_data, aes(x = mean_across_pv, y = display_label)) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.45) +
    geom_hline(yintercept = 2, color = "grey25", linewidth = 0.65) +
    geom_errorbar(
      aes(xmin = minimum_across_pv, xmax = maximum_across_pv),
      orientation = "y",
      width = 0.18,
      linewidth = 0.6,
      color = "grey25"
    ) +
    geom_point(
      aes(shape = query_scope),
      size = 2.8,
      stroke = 0.65,
      color = "black",
      fill = "white"
    ) +
    geom_text(
      aes(x = maximum_across_pv, label = direct_label),
      hjust = -0.25,
      size = direct_label_size,
      family = .analysis_plot_font_family()
    ) +
    scale_shape_manual(
      values = c("Atomic feature group" = 21, "Alternative overlapping joint query" = 24),
      guide = "none"
    ) +
    scale_y_discrete(drop = FALSE, limits = display_levels) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.16))) +
    labs(
      x = "Increase in held-out RMSE",
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    .analysis_theme(base_size = base_size, line_base_size = base_size) +
    .analysis_typography_theme(
      axis_text_size = base_size - 2,
      axis_title_size = base_size,
      legend_text_size = base_size - 2,
      legend_title_size = base_size - 1
    ) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(5.5, 28, 5.5, 5.5)
    )
}

.analysis_pisa_audit_labels = function() {
  c(audit_gender = "Gender", audit_immigration = "Immigration background")
}

.analysis_pisa_subgroup_labels = function() {
  c(
    "audit_gender::1" = "Female",
    "audit_gender::2" = "Male",
    "audit_immigration::1" = "Native",
    "audit_immigration::2" = "Second-generation",
    "audit_immigration::3" = "First-generation"
  )
}

.analysis_pisa_subgroup_missingness = function(audit_data) {
  variables = c("audit_gender", "audit_immigration")
  .analysis_require_columns(audit_data, variables, "PISA subgroup audit data")
  rbindlist(lapply(variables, function(variable) {
    values = audit_data[[variable]]
    data.table(
      audit_variable = variable,
      n_total = length(values),
      n_observed = sum(!is.na(values)),
      n_missing = sum(is.na(values)),
      missing_fraction = mean(is.na(values)),
      interpretation = "Missing audit values are excluded only from the corresponding subgroup diagnostic"
    )
  }))
}

.analysis_pisa_public_subgroup_tables = function(subgroup_audit, subgroup_missingness) {
  required_tables = c("metrics", "uncertainty", "contrasts")
  if (!is.list(subgroup_audit) || !all(required_tables %in% names(subgroup_audit))) {
    stop("The PISA subgroup audit does not contain metrics, uncertainty, and contrasts tables.", call. = FALSE)
  }
  audit_labels = .analysis_pisa_audit_labels()
  subgroup_labels = .analysis_pisa_subgroup_labels()

  add_audit_label = function(data, table_label) {
    data = copy(as.data.table(data))
    .analysis_require_columns(data, "audit_variable", table_label)
    mapped = unname(audit_labels[data$audit_variable])
    if (anyNA(mapped)) {
      stop(table_label, " contains an unmapped PISA audit variable.", call. = FALSE)
    }
    data[, audit_label := mapped]
    data
  }
  add_subgroup_label = function(data, code_column, label_column, table_label) {
    .analysis_require_columns(data, code_column, table_label)
    keys = paste(data$audit_variable, data[[code_column]], sep = "::")
    mapped = unname(subgroup_labels[keys])
    if (anyNA(mapped)) {
      stop(table_label, " contains an unmapped PISA subgroup code.", call. = FALSE)
    }
    data[, (label_column) := mapped]
    data
  }

  metrics = add_audit_label(subgroup_audit$metrics, "PISA subgroup metrics")
  metrics = add_subgroup_label(metrics, "subgroup", "subgroup_label", "PISA subgroup metrics")
  setcolorder(metrics, c("audit_variable", "audit_label", "subgroup", "subgroup_label", setdiff(
    names(metrics),
    c("audit_variable", "audit_label", "subgroup", "subgroup_label")
  )))

  uncertainty = add_audit_label(subgroup_audit$uncertainty, "PISA subgroup uncertainty")
  uncertainty = add_subgroup_label(
    uncertainty,
    "subgroup",
    "subgroup_label",
    "PISA subgroup uncertainty"
  )
  setcolorder(uncertainty, c("audit_variable", "audit_label", "subgroup", "subgroup_label", setdiff(
    names(uncertainty),
    c("audit_variable", "audit_label", "subgroup", "subgroup_label")
  )))

  contrasts = add_audit_label(subgroup_audit$contrasts, "PISA subgroup contrasts")
  contrasts = add_subgroup_label(
    contrasts,
    "subgroup_1",
    "subgroup_1_label",
    "PISA subgroup contrasts"
  )
  contrasts = add_subgroup_label(
    contrasts,
    "subgroup_2",
    "subgroup_2_label",
    "PISA subgroup contrasts"
  )
  setcolorder(contrasts, c(
    "audit_variable", "audit_label", "subgroup_1", "subgroup_1_label", "subgroup_2", "subgroup_2_label",
    setdiff(names(contrasts), c(
      "audit_variable", "audit_label", "subgroup_1", "subgroup_1_label", "subgroup_2", "subgroup_2_label"
    ))
  ))

  missingness = add_audit_label(subgroup_missingness, "PISA subgroup missingness")
  setcolorder(missingness, c("audit_variable", "audit_label", setdiff(
    names(missingness),
    c("audit_variable", "audit_label")
  )))

  list(metrics = metrics, uncertainty = uncertainty, contrasts = contrasts, missingness = missingness)
}

.analysis_pisa_dependence_evidence = function(dependence) {
  result = dependence
  pairwise = copy(as.data.table(result$pairwise))
  .analysis_require_columns(pairwise, c("feature_1", "feature_2", "association"), "PISA dependence table")
  pairwise[, `:=`(
    signed_association = association,
    association_strength = abs(association),
    display_interpretation = paste(
      "Absolute association strength is used for ordering and color; signed Spearman estimates remain in",
      "signed_association"
    )
  )]
  result$pairwise = pairwise
  result$limitations = unique(c(
    result$limitations,
    "Figure ordering and color use absolute association magnitude; signed Spearman values remain tabulated."
  ))
  result
}

.analysis_pisa_loco_penalty_tables = function(focal_oof, loco_scores, loco_predictions = NULL) {
  if (!is.list(focal_oof) || !length(focal_oof) || is.null(names(focal_oof)) || any(!nzchar(names(focal_oof)))) {
    stop("The focal OOF archive must be a named, non-empty list of plausible-value results.", call. = FALSE)
  }
  common_split_rows = rbindlist(lapply(names(focal_oof), function(outcome_id) {
    oof = focal_oof[[outcome_id]]
    predictions = copy(as.data.table(oof$predictions))
    .analysis_require_columns(
      predictions,
      c("row_id", "truth", "response"),
      paste("PISA common-split OOF predictions for", outcome_id)
    )
    if (!nrow(predictions) || any(!is.finite(predictions$truth)) || any(!is.finite(predictions$response))) {
      stop("The PISA common-split OOF predictions must be non-empty and finite.", call. = FALSE)
    }
    if (anyDuplicated(predictions$row_id)) {
      stop("The PISA common-split comparator requires one OOF prediction per sampled row.", call. = FALSE)
    }
    row_ids = oof$task$row_ids
    country = copy(as.data.table(oof$task$data(rows = row_ids, cols = "audit_country")))
    .analysis_require_columns(country, "audit_country", paste("PISA task country map for", outcome_id))
    if (nrow(country) != length(row_ids) || anyDuplicated(row_ids)) {
      stop("The PISA task country map is not one-to-one with task row IDs.", call. = FALSE)
    }
    country[, `:=`(row_id = row_ids, held_out_group = as.character(audit_country))]
    if (anyNA(country$held_out_group) || any(!nzchar(country$held_out_group))) {
      stop("The PISA task country map contains a missing country.", call. = FALSE)
    }
    predictions = merge(
      predictions,
      country[, .(row_id, held_out_group)],
      by = "row_id",
      all.x = TRUE,
      sort = FALSE
    )
    if (anyNA(predictions$held_out_group)) {
      stop("At least one PISA common-split OOF prediction has no country mapping.", call. = FALSE)
    }
    predictions[, .(
      outcome_id = outcome_id,
      held_out_group,
      row_id,
      common_split_truth = truth,
      common_split_response = response
    )]
  }), use.names = TRUE)
  if (anyDuplicated(common_split_rows[, .(outcome_id, held_out_group, row_id)])) {
    stop("The PISA common-split OOF archive contains duplicate country/PV/row keys.", call. = FALSE)
  }
  common_split = common_split_rows[, .(
    common_split_oof_rmse = sqrt(mean((common_split_truth - common_split_response)^2)),
    n_common_split_assessment = .N
  ), by = .(outcome_id, held_out_group)]

  loco_scores = copy(as.data.table(loco_scores))
  .analysis_require_columns(
    loco_scores,
    c("outcome_id", "held_out_group", "regr.rmse", "n_assessment"),
    "PISA LOCO scores"
  )
  if (anyDuplicated(loco_scores[, .(outcome_id, held_out_group)])) {
    stop("The PISA LOCO scores contain duplicate plausible-value/country keys.", call. = FALSE)
  }
  loco = loco_scores[, .(
    outcome_id = as.character(outcome_id),
    held_out_group = as.character(held_out_group),
    loco_rmse = as.numeric(`regr.rmse`),
    n_loco_assessment = as.integer(n_assessment)
  )]
  comparison = merge(
    loco,
    common_split,
    by = c("outcome_id", "held_out_group"),
    all = TRUE,
    sort = FALSE
  )
  if (nrow(comparison) != nrow(loco) || anyNA(comparison) ||
      any(!is.finite(comparison$loco_rmse)) || any(!is.finite(comparison$common_split_oof_rmse))) {
    stop("The PISA LOCO and common-split OOF country/PV keys do not match exactly.", call. = FALSE)
  }
  if (any(comparison$n_loco_assessment != comparison$n_common_split_assessment)) {
    stop("The matched PISA LOCO and common-split OOF country sample sizes differ.", call. = FALSE)
  }
  row_match_verified = FALSE
  if (!is.null(loco_predictions)) {
    loco_predictions = copy(as.data.table(loco_predictions))
    .analysis_require_columns(
      loco_predictions,
      c("outcome_id", "held_out_group", "row_id", "truth", "response"),
      "PISA LOCO predictions"
    )
    loco_predictions[, `:=`(
      outcome_id = as.character(outcome_id),
      held_out_group = as.character(held_out_group)
    )]
    if (anyDuplicated(loco_predictions[, .(outcome_id, held_out_group, row_id)])) {
      stop("The PISA LOCO predictions contain duplicate country/PV/row keys.", call. = FALSE)
    }
    matched_rows = merge(
      common_split_rows,
      loco_predictions[, .(
        outcome_id,
        held_out_group,
        row_id,
        loco_truth = truth,
        loco_response = response
      )],
      by = c("outcome_id", "held_out_group", "row_id"),
      all = TRUE,
      sort = FALSE
    )
    if (nrow(matched_rows) != nrow(common_split_rows) || anyNA(matched_rows) ||
        any(abs(matched_rows$common_split_truth - matched_rows$loco_truth) > 1e-10)) {
      stop("The PISA LOCO and common-split prediction rows or truths do not match exactly.", call. = FALSE)
    }
    recomputed_loco = matched_rows[, .(
      recomputed_loco_rmse = sqrt(mean((loco_truth - loco_response)^2)),
      n_matched_rows = .N
    ), by = .(outcome_id, held_out_group)]
    score_check = merge(
      comparison[, .(outcome_id, held_out_group, loco_rmse, n_loco_assessment)],
      recomputed_loco,
      by = c("outcome_id", "held_out_group"),
      all = TRUE
    )
    if (nrow(score_check) != nrow(comparison) || anyNA(score_check) ||
        any(abs(score_check$loco_rmse - score_check$recomputed_loco_rmse) > 1e-8) ||
        any(score_check$n_loco_assessment != score_check$n_matched_rows)) {
      stop("The PISA LOCO score table disagrees with the row-matched LOCO predictions.", call. = FALSE)
    }
    row_match_verified = TRUE
  }
  comparison[, `:=`(
    country_exclusion_penalty = loco_rmse - common_split_oof_rmse,
    comparator = "Same-country common-split OOF XGBoost RMSE",
    direction = "Positive values indicate worse RMSE after excluding the country from model training",
    limitation = paste(
      "LOCO and common-split OOF use different training sizes and compositions; the matched difference is",
      "an operational boundary diagnostic, not a causal effect"
    ),
    uncertainty_semantics = "One matched descriptive country/PV difference; no confidence interval",
    row_match_validation = if (row_match_verified) {
      "Exact country/PV/row match and truth equality verified against private predictions"
    } else {
      "Country/PV keys and assessment counts matched; row-level LOCO predictions were not supplied"
    }
  )]
  outcome_order = names(focal_oof)
  comparison[, outcome_order__ := match(outcome_id, outcome_order)]
  setorder(comparison, outcome_order__, held_out_group)
  comparison[, outcome_order__ := NULL]

  country_summary = comparison[, .(
    n_pv = uniqueN(outcome_id),
    n_assessment_sizes = uniqueN(n_loco_assessment),
    n_assessment_per_pv = min(n_loco_assessment),
    mean_penalty = mean(country_exclusion_penalty),
    sd_penalty = sd(country_exclusion_penalty),
    minimum_penalty = min(country_exclusion_penalty),
    median_penalty = median(country_exclusion_penalty),
    maximum_penalty = max(country_exclusion_penalty),
    mean_loco_rmse = mean(loco_rmse),
    mean_common_split_oof_rmse = mean(common_split_oof_rmse),
    interpretation = paste(
      "Mean matched country-exclusion penalty across plausible values; observed countries only,",
      "and positive values indicate worse prediction after exclusion"
    ),
    limitation = paste(
      "LOCO and common-split OOF use different training sizes and compositions; the matched difference is",
      "an operational boundary diagnostic, not a causal effect"
    ),
    uncertainty_semantics = "Plausible-value minima and maxima are descriptive ranges, not confidence intervals"
  ), by = held_out_group]
  if (any(country_summary$n_assessment_sizes != 1L)) {
    stop("The PISA country/PV comparison does not have one assessment size per country.", call. = FALSE)
  }
  country_summary[, `:=`(n_assessment_sizes = NULL, n_assessment_per_pv = as.integer(n_assessment_per_pv))]
  setorder(country_summary, mean_penalty, held_out_group)
  list(comparison = comparison[], country_summary = country_summary[])
}

.analysis_pisa_matched_control_contrasts = function(loco_comparison, matched_summary) {
  loco = copy(as.data.table(loco_comparison))
  matched = copy(as.data.table(matched_summary))
  .analysis_require_columns(
    loco,
    c(
      "outcome_id", "held_out_group", "loco_rmse", "common_split_oof_rmse", "country_exclusion_penalty",
      "n_loco_assessment", "n_common_split_assessment"
    ),
    "PISA country/PV LOCO comparison"
  )
  .analysis_require_columns(
    matched,
    c(
      "held_out_group", "n_matched_repetitions", "n_reference_analysis", "n_candidate_other_setting",
      "n_matched_exclusion_analysis", "n_assessment", "reference_oof_loss", "mean_matched_exclusion_loss",
      "standard_deviation_matched_exclusion_loss", "minimum_matched_exclusion_loss",
      "maximum_matched_exclusion_loss", "mean_matched_exclusion_penalty",
      "standard_deviation_matched_exclusion_penalty", "minimum_matched_exclusion_penalty",
      "maximum_matched_exclusion_penalty"
    ),
    "PISA matched-size country-exclusion summary"
  )
  if (anyDuplicated(loco[, .(outcome_id, held_out_group)]) || anyDuplicated(matched$held_out_group)) {
    stop("PISA LOCO or matched-size country keys are duplicated.", call. = FALSE)
  }
  matched = matched[, .(
    held_out_group,
    n_matched_control_repetitions = as.integer(n_matched_repetitions),
    reference_training_size = as.integer(n_reference_analysis),
    unrestricted_loco_training_size = as.integer(n_candidate_other_setting),
    size_matched_exclusion_training_size = as.integer(n_matched_exclusion_analysis),
    assessment_size = as.integer(n_assessment),
    matched_reference_oof_rmse = reference_oof_loss,
    Cbar_size_matched_exclusion_rmse = mean_matched_exclusion_loss,
    matched_control_rmse_sd = standard_deviation_matched_exclusion_loss,
    matched_control_rmse_minimum = minimum_matched_exclusion_loss,
    matched_control_rmse_maximum = maximum_matched_exclusion_loss,
    size_matched_exclusion_penalty_Cbar_minus_A = mean_matched_exclusion_penalty,
    matched_control_penalty_sd = standard_deviation_matched_exclusion_penalty,
    matched_control_penalty_minimum = minimum_matched_exclusion_penalty,
    matched_control_penalty_maximum = maximum_matched_exclusion_penalty
  )]
  if (nrow(matched) != 32L || any(matched$n_matched_control_repetitions < 2L) || anyNA(matched)) {
    stop("PISA matched-size controls must cover 32 countries with complete summaries.", call. = FALSE)
  }

  all_pv_context = merge(loco, matched, by = "held_out_group", all.x = TRUE, sort = FALSE)
  if (nrow(all_pv_context) != nrow(loco) || anyNA(all_pv_context$n_matched_control_repetitions)) {
    stop("PISA matched-size controls do not cover every LOCO country.", call. = FALSE)
  }
  all_pv_context[, matched_control_available := outcome_id == "PV1READ"]
  control_columns = c(
    "n_matched_control_repetitions", "reference_training_size", "unrestricted_loco_training_size",
    "size_matched_exclusion_training_size", "assessment_size", "matched_reference_oof_rmse",
    "Cbar_size_matched_exclusion_rmse", "matched_control_rmse_sd", "matched_control_rmse_minimum",
    "matched_control_rmse_maximum", "size_matched_exclusion_penalty_Cbar_minus_A",
    "matched_control_penalty_sd", "matched_control_penalty_minimum", "matched_control_penalty_maximum"
  )
  all_pv_context[matched_control_available == FALSE, (control_columns) := NA]
  all_pv_context[, `:=`(
    raw_loco_penalty_B_minus_A = country_exclusion_penalty,
    unrestricted_vs_size_matched_training_size_contrast_B_minus_Cbar = fifelse(
      matched_control_available,
      loco_rmse - Cbar_size_matched_exclusion_rmse,
      NA_real_
    ),
    descriptive_only = TRUE,
    plausible_value_scope = fifelse(
      matched_control_available,
      "PV1READ_only",
      "matched_controls_not_computed"
    ),
    interpretation = paste(
      "For PV1READ, Cbar-A is the training-size-matched observed-country exclusion/composition diagnostic,",
      "and B-Cbar compares unrestricted with size-matched no-target-country training."
    ),
    limitation = paste(
      "Matched controls are available only for PV1READ. The diagnostics are descriptive and do not identify a",
      "causal country effect, transport, or performance in a genuinely new country."
    )
  )]
  setnames(all_pv_context, "held_out_group", "country")
  setorder(all_pv_context, country, outcome_id)

  contrasts = all_pv_context[outcome_id == "PV1READ", .(
    country,
    plausible_value_scope = "PV1READ_only",
    A_reference_oof_rmse = common_split_oof_rmse,
    B_unrestricted_loco_rmse = loco_rmse,
    Cbar_size_matched_exclusion_rmse,
    raw_loco_penalty_B_minus_A,
    size_matched_exclusion_penalty_Cbar_minus_A,
    unrestricted_vs_size_matched_training_size_contrast_B_minus_Cbar,
    n_matched_control_repetitions,
    matched_control_penalty_sd,
    matched_control_penalty_minimum,
    matched_control_penalty_maximum,
    matched_control_rmse_sd,
    matched_control_rmse_minimum,
    matched_control_rmse_maximum,
    reference_training_size,
    unrestricted_loco_training_size,
    size_matched_exclusion_training_size,
    assessment_size,
    descriptive_only,
    interpretation,
    limitation
  )]
  if (nrow(contrasts) != 32L || anyNA(contrasts) || anyDuplicated(contrasts$country)) {
    stop("PISA PV1 matched-control contrasts must contain 32 complete country records.", call. = FALSE)
  }
  if (any(contrasts$assessment_size != all_pv_context[outcome_id == "PV1READ", n_loco_assessment]) ||
      any(contrasts$assessment_size != all_pv_context[outcome_id == "PV1READ", n_common_split_assessment])) {
    stop("PISA LOCO, common-split OOF, and matched-control assessment counts differ.", call. = FALSE)
  }
  if (any(abs(contrasts$A_reference_oof_rmse -
      all_pv_context[outcome_id == "PV1READ", matched_reference_oof_rmse]) > 1e-12)) {
    stop("PISA common-split OOF RMSE does not match the matched-control reference loss.", call. = FALSE)
  }
  if (any(abs(
    contrasts$raw_loco_penalty_B_minus_A -
      contrasts$size_matched_exclusion_penalty_Cbar_minus_A -
      contrasts$unrestricted_vs_size_matched_training_size_contrast_B_minus_Cbar
  ) > 1e-12)) {
    stop("The PISA PV1 matched-control contrast identity does not hold.", call. = FALSE)
  }
  setorder(contrasts, size_matched_exclusion_penalty_Cbar_minus_A, country)
  list(contrasts = contrasts[], all_pv_context = all_pv_context[])
}

.analysis_pisa_figure_catalog = function(
    support_threshold = 20L, reference_n = 1500L, bootstrap_replicates = 500L) {
  ale_1d_semantics = .analysis_ale_semantics(
    "PISA",
    "1d",
    support_threshold = support_threshold,
    reference_n = reference_n,
    bootstrap_replicates = bootstrap_replicates
  )
  ale_2d_semantics = .analysis_ale_semantics(
    "PISA",
    "2d",
    support_threshold = support_threshold,
    reference_n = reference_n,
    bootstrap_replicates = bootstrap_replicates
  )
  catalog = data.table(
    figure_id = c(
      "pisa_gate_overview", "pisa_g1_performance", "pisa_g2_dependence", "pisa_g3_calibration",
      "pisa_g5_pfi", "pisa_g6_transport", "pisa_g6_model_sensitivity", "pisa_g7_subgroups",
      "pisa_g7_subgroup_contrasts", "pisa_model_importance_ranks", "pisa_all_pv_benchmark",
      "pisa_all_pv_pfi", "pisa_rich_ale_1d", "pisa_rich_ice_quantiles",
      "pisa_rich_interaction_screen", "pisa_rich_ale_2d", "pisa_prediction_multiplicity"
    ),
    source_tables = c(
      "tables/pisa_report_card.csv | tables/pisa_claim_support.csv",
      "tables/pisa_gate_g1__performance__summary.csv",
      "tables/pisa_gate_g2__pairwise.csv | tables/pisa_gate_g2__support.csv",
      paste(
        "tables/pisa_g3_calibration_plot_source.csv | tables/pisa_gate_g3a__calibration__summary.csv |",
        "tables/pisa_all_pv_calibration_curve.csv"
      ),
      "tables/pisa_gate_g5__pfi__summary.csv",
      "tables/pisa_loco_country_penalty_summary.csv | tables/pisa_loco_country_pv_comparison.csv",
      "tables/pisa_model_ranks.csv | tables/pisa_all_pv_fold_performance.csv",
      paste(
        "tables/pisa_pv1_technical_subgroup_metrics.csv |",
        "tables/pisa_pv1_technical_subgroup_uncertainty.csv |",
        "tables/pisa_pv1_technical_subgroup_missingness.csv"
      ),
      paste(
        "tables/pisa_pv1_technical_subgroup_contrasts.csv |",
        "tables/pisa_pv1_technical_subgroup_missingness.csv"
      ),
      "tables/pisa_model_importance_ranks.csv",
      "tables/pisa_all_pv_pooled_performance.csv",
      "tables/pisa_all_pv_pfi_descriptive_summary.csv",
      "tables/pisa_rich_ale_1d.csv",
      "tables/pisa_rich_ice_quantiles.csv",
      "tables/pisa_rich_interaction_screen.csv",
      "tables/pisa_rich_ale_2d.csv",
      "tables/pisa_multiplicity_distribution.csv | tables/pisa_multiplicity_bins.csv"
    ),
    estimand_scope = c(
      "Administrative gate-status overview for the all-plausible-value selected-model claim",
      "Focal-model PV1 out-of-fold predictive performance",
      "Mixed-type association magnitude in the sampled predictor distribution",
      "Natural cubic regression spline with four degrees of freedom for focal-model PV1 out-of-fold calibration",
      "Focal-model PV1 held-out permutation importance",
      "Matched operational observed-country exclusion penalty relative to same-country common-split OOF RMSE",
      "All-plausible-value mean-RMSE model-class sensitivity screen",
      "Focal-model PV1 out-of-fold RMSE by technical subgroup",
      "Pairwise technical-subgroup differences in focal-model PV1 out-of-fold RMSE",
      "Held-out importance values across ten plausible values and near-equivalent models",
      "Common-split held-out performance across 10 plausible values",
      "Held-out permutation importance across 10 plausible values",
      ale_1d_semantics$estimand_or_scope,
      "Full-fit PV1 aggregate ICE intervention queries",
      "Full-fit PV1 Friedman-Popescu H interaction screen",
      ale_2d_semantics$estimand_or_scope,
      "Binned empirical CDF of within-row PV1 OOF prediction ranges across near-equivalent models"
    ),
    uncertainty_semantics = c(
      "No statistical uncertainty interval",
      "Blue ranges are fold 10th-90th percentiles, not confidence intervals",
      "No statistical uncertainty interval",
      paste(
        "Pointwise 95% school-within-country bootstrap percentile interval conditional on supplied OOF predictions;",
        "type-8 quantiles; not simultaneous and not model-training or plausible-value uncertainty"
      ),
      "Blue ranges are fold 10th-90th percentiles, not confidence intervals",
      "Blue ranges are plausible-value minima and maxima, not confidence intervals",
      paste(
        "Blue whiskers are descriptive matched-fold 10th-90th percentiles, not confidence intervals;",
        "the acceptance tolerance is not a significance test"
      ),
      "School-clustered percentile-bootstrap 95% intervals stratified within country; no model refitting",
      "School-clustered percentile-bootstrap 95% intervals stratified within country; no model refitting",
      "No statistical uncertainty interval",
      "Across-plausible-value variation is descriptive and not Rubin-pooled uncertainty",
      "Blue ranges are plausible-value minima and maxima, not confidence intervals",
      ale_1d_semantics$uncertainty_semantics,
      "Ribbons are empirical ICE curve quantiles, not confidence intervals",
      "No statistical uncertainty interval",
      ale_2d_semantics$uncertainty_semantics,
      "Empirical distribution and quantiles; no statistical uncertainty interval"
    ),
    role = c(
      "claim-governance overview", "focal evidence", "support diagnostic", "focal evidence",
      "focal explanation", "technical boundary diagnostic", "technical boundary diagnostic",
      "technical boundary diagnostic", "technical boundary diagnostic", "model-sensitivity diagnostic",
      "measurement sensitivity", "measurement sensitivity",
      "complementary model description", "complementary model description",
      "complementary model description", "complementary model description", "model-sensitivity diagnostic"
    ),
    privacy = rep("Shareable aggregate; no row-level outcomes or predictions", 17L)
  )
  catalog[, `:=`(
    pdf_path = file.path("figures", paste0(figure_id, ".pdf")),
    png_path = file.path("figures", paste0(figure_id, ".png"))
  )]
  setcolorder(
    catalog,
    c(
      "figure_id", "pdf_path", "png_path", "source_tables", "estimand_scope", "uncertainty_semantics",
      "role", "privacy"
    )
  )
  catalog[]
}

.analysis_pisa_complete_case_retained_rows = function(primary_task, complete_case_oof) {
  primary_row_ids = primary_task$row_ids
  target_columns = primary_task$target_names
  feature_columns = primary_task$feature_names
  if (length(target_columns) != 1L || !length(feature_columns) || anyDuplicated(primary_row_ids)) {
    stop("The primary PISA task cannot define a one-outcome complete-case mask.", call. = FALSE)
  }
  verification_columns = c(target_columns, feature_columns, "audit_country", "audit_school")
  primary_values = as.data.table(primary_task$data(
    rows = primary_row_ids,
    cols = verification_columns
  ))
  if (nrow(primary_values) != length(primary_row_ids)) {
    stop("The primary PISA task data are not row-aligned with its row IDs.", call. = FALSE)
  }
  retained = complete.cases(primary_values[, c(target_columns, feature_columns), with = FALSE])
  retained_rows = data.table(
    row_id = primary_row_ids[retained],
    audit_country = as.character(primary_values$audit_country[retained])
  )
  if (!nrow(retained_rows) || anyNA(retained_rows) || anyDuplicated(retained_rows$row_id)) {
    stop("The recomputed PISA complete-case membership is empty, incomplete, or non-unique.", call. = FALSE)
  }

  complete_task = complete_case_oof$task
  complete_row_ids = complete_task$row_ids
  complete_values = as.data.table(complete_task$data(
    rows = complete_row_ids,
    cols = verification_columns
  ))
  if (nrow(complete_values) != length(complete_row_ids) || nrow(complete_values) != nrow(retained_rows)) {
    stop("The archived complete-case task size differs from the recomputed primary-task mask.", call. = FALSE)
  }
  primary_retained_values = primary_values[retained, ..verification_columns]
  values_match = identical(names(primary_retained_values), names(complete_values)) &&
    all(vapply(
      verification_columns,
      function(column) identical(primary_retained_values[[column]], complete_values[[column]]),
      logical(1L)
    ))
  if (!values_match) {
    stop(
      "The archived complete-case task is not the row-ordered subset defined by the primary-task mask.",
      call. = FALSE
    )
  }
  complete_folds = rbindlist(lapply(seq_along(complete_case_oof$test_sets), function(fold) {
    data.table(complete_case_row_id = complete_case_oof$test_sets[[fold]], complete_case_fold = fold)
  }))
  if (nrow(complete_folds) != length(complete_row_ids) ||
      anyDuplicated(complete_folds$complete_case_row_id) ||
      !setequal(complete_folds$complete_case_row_id, complete_row_ids)) {
    stop("The archived complete-case assessment folds do not partition the complete-case task.", call. = FALSE)
  }
  fold_index = match(complete_row_ids, complete_folds$complete_case_row_id)
  if (anyNA(fold_index)) {
    stop("The archived complete-case fold map cannot be aligned to task-row order.", call. = FALSE)
  }
  retained_rows[, complete_case_fold := complete_folds$complete_case_fold[fold_index]]
  retained_rows[]
}

.analysis_pisa_complete_case_membership = function(fold_assignment, retained_rows, country_counts) {
  fold_assignment = copy(as.data.table(fold_assignment))
  retained_rows = copy(as.data.table(retained_rows))
  country_counts = copy(as.data.table(country_counts))
  .analysis_require_columns(
    fold_assignment,
    c("row_id", "stratum", "fold"),
    "PISA primary fold assignment"
  )
  .analysis_require_columns(
    retained_rows,
    c("row_id", "audit_country", "complete_case_fold"),
    "PISA retained complete-case rows"
  )
  .analysis_require_columns(
    country_counts,
    c("CNT", "n_complete_case"),
    "PISA complete-case country counts"
  )
  if (anyDuplicated(fold_assignment$row_id) || anyDuplicated(retained_rows$row_id) ||
      anyDuplicated(country_counts$CNT)) {
    stop("The PISA complete-case membership inputs contain duplicate identifiers.", call. = FALSE)
  }
  membership = fold_assignment[row_id %in% retained_rows$row_id]
  if (nrow(membership) != nrow(retained_rows) ||
      !setequal(membership$row_id, retained_rows$row_id)) {
    stop("The PISA complete-case membership does not exactly match the retained primary row IDs.", call. = FALSE)
  }
  retained_index = match(membership$row_id, retained_rows$row_id)
  if (anyNA(retained_index) ||
      any(as.character(membership$stratum) != as.character(retained_rows$audit_country[retained_index])) ||
      any(as.integer(membership$fold) != as.integer(retained_rows$complete_case_fold[retained_index]))) {
    stop("The PISA complete-case country/fold membership disagrees with the archived sensitivity task.", call. = FALSE)
  }
  observed_counts = membership[, .(observed_n_complete_case = .N), by = .(CNT = as.character(stratum))]
  expected_counts = country_counts[, .(
    CNT = as.character(CNT),
    expected_n_complete_case = as.integer(n_complete_case)
  )]
  count_check = merge(expected_counts, observed_counts, by = "CNT", all = TRUE)
  count_check[is.na(expected_n_complete_case), expected_n_complete_case := 0L]
  count_check[is.na(observed_n_complete_case), observed_n_complete_case := 0L]
  if (any(count_check$expected_n_complete_case != count_check$observed_n_complete_case)) {
    stop("The PISA complete-case membership disagrees with the public country counts.", call. = FALSE)
  }
  setorder(membership, row_id)
  membership[]
}
