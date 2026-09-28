# SHILD data preparation, labels, and study-specific model-query graphics.

.analysis_shild_feature_labels = function() {
  c(
    gender = "Gender",
    age = "Age",
    A2 = "General health (A2)",
    C1 = "Long-term health condition (C1)",
    D2 = "Depressed or dispirited (D2)",
    D4 = "Relaxed and handles stress (D4)",
    D6 = "Tension (D6)",
    D8 = "Worry frequency (D8)",
    D10 = "Emotional stability (D10)",
    D11 = "Task persistence (D11)",
    D15 = "Planning and implementation (D15)",
    D16 = "Nervousness (D16)",
    D17 = "Distractibility (D17)",
    E1 = "Height, cm (E1)",
    E2 = "Weight, kg (E2)",
    E5_a = "Other practitioner consultation (E5a)",
    E17 = "Personal-problem confidant (E17)",
    F15 = "Employment-related rating, 0-10 (F15)",
    G1 = "Partner status (G1)",
    J2 = "Time with other relatives (J2)",
    J4 = "Time with acquaintances (J4)",
    J9 = "Cultural activity frequency (J9)",
    J14 = "Newspaper-reading frequency (J14)",
    J17 = "Trips abroad in past year (J17)",
    M2 = "Primary income source (M2)",
    M6 = "Medicine and supplement spending (M6)",
    M8 = "Current finances (M8)",
    socioeconomic_joint_block = "Socioeconomic block"
  )
}

.analysis_shild_complete_case_membership = function(complete_case, fold_assignment) {
  if (!is.list(complete_case) || is.null(complete_case$membership) || is.null(complete_case$retained)) {
    stop("The SHILD complete-case result does not contain its row-membership bridge.", call. = FALSE)
  }
  membership = copy(as.data.table(complete_case$membership))
  fold_assignment = copy(as.data.table(fold_assignment))
  .analysis_require_columns(
    membership,
    c("complete_case_row_id", "source_row_id", "fold"),
    "SHILD complete-case row bridge"
  )
  .analysis_require_columns(
    fold_assignment,
    c("row_id", "fold", "stratum", "respondent_id"),
    "SHILD primary fold assignment"
  )
  expected_source_rows = which(complete_case$retained)
  if (anyDuplicated(membership$complete_case_row_id) || anyDuplicated(membership$source_row_id) ||
      anyDuplicated(fold_assignment$row_id) ||
      !identical(membership$complete_case_row_id, seq_len(nrow(membership))) ||
      !identical(membership$source_row_id, as.integer(expected_source_rows))) {
    stop("The SHILD complete-case row bridge is incomplete or out of order.", call. = FALSE)
  }
  source_index = match(membership$source_row_id, fold_assignment$row_id)
  if (anyNA(source_index) ||
      any(as.integer(membership$fold) != as.integer(fold_assignment$fold[source_index]))) {
    stop("The SHILD complete-case bridge disagrees with the primary fold assignment.", call. = FALSE)
  }
  result = data.table(
    complete_case_row_id = as.integer(membership$complete_case_row_id),
    row_id = as.integer(membership$source_row_id),
    fold = as.integer(membership$fold),
    stratum = as.character(fold_assignment$stratum[source_index]),
    respondent_id = fold_assignment$respondent_id[source_index]
  )
  if (anyNA(result)) {
    stop("The SHILD complete-case membership contains missing identifiers.", call. = FALSE)
  }
  result[]
}

.recode_domain_sentinels = function(data, sentinel_map) {
  records = list()
  for (variable in names(sentinel_map)) {
    if (!variable %in% names(data)) {
      stop("Sentinel-map variable is absent: ", variable, call. = FALSE)
    }
    for (code in sentinel_map[[variable]]) {
      mask = !is.na(data[[variable]]) & data[[variable]] == code
      records[[length(records) + 1L]] = data.table(
        variable = variable,
        code = as.numeric(code),
        n_recoded = sum(mask),
        basis = "prespecified domain-audited high code; dictionary has no value labels"
      )
      if (any(mask)) {
        set(data, i = which(mask), j = variable, value = NA)
      }
    }
  }
  list(data = data, audit = rbindlist(records))
}

.shild_predictors = function() {
  c(
    "gender", "age", "A2", "C1", "D2", "D4", "D6", "D8", "D10", "D11", "D15", "D16", "D17",
    "E1", "E2", "E5_a", "E17", "F15", "G1", "J2", "J4", "J9", "J14", "J17", "M2", "M6", "M8"
  )
}

.shild_factor_specs = function() {
  frequency = c(
    `1` = "Always", `2` = "Often", `3` = "Sometimes", `4` = "Rarely", `5` = "Never"
  )
  social_frequency = c(
    `1` = "Daily", `2` = "Several times a week", `3` = "Once a week",
    `4` = "Several times a month", `5` = "Once a month", `6` = "Less frequently", `7` = "Never"
  )
  list(
    gender = list(labels = c(`1` = "Man", `2` = "Woman"), missing_codes = character()),
    A2 = list(
      labels = c(`1` = "Very well", `2` = "Well", `3` = "Average", `4` = "Poor", `5` = "Very poor"),
      missing_codes = character()
    ),
    C1 = list(labels = c(`1` = "Yes", `2` = "No"), missing_codes = "9"),
    D2 = list(labels = frequency, missing_codes = c("8", "9")),
    D4 = list(labels = frequency, missing_codes = c("8", "9")),
    D6 = list(labels = frequency, missing_codes = c("8", "9")),
    D8 = list(labels = frequency, missing_codes = c("8", "9")),
    D10 = list(labels = frequency, missing_codes = c("8", "9")),
    D11 = list(labels = frequency, missing_codes = c("8", "9")),
    D15 = list(labels = frequency, missing_codes = c("8", "9")),
    D16 = list(labels = frequency, missing_codes = c("8", "9")),
    D17 = list(labels = frequency, missing_codes = c("8", "9")),
    E5_a = list(labels = c(`1` = "Yes", `2` = "No"), missing_codes = "9"),
    E17 = list(
      labels = c(
        `1` = "Partner/spouse/boyfriend/girlfriend", `2` = "Children", `3` = "Parents", `4` = "Siblings",
        `5` = "Other family", `6` = "Friends/colleagues", `7` = "Staff",
        `8` = "I do not share this with anyone", `9` = "Others"
      ),
      missing_codes = c("98", "99")
    ),
    G1 = list(labels = c(`1` = "Yes", `2` = "No"), missing_codes = c("8", "9")),
    J2 = list(labels = social_frequency, missing_codes = c("8", "99")),
    J4 = list(labels = social_frequency, missing_codes = c("8", "99")),
    J9 = list(labels = social_frequency, missing_codes = c("8", "99")),
    J14 = list(labels = c(social_frequency, `9` = "Don't know"), missing_codes = c("8", "99")),
    M2 = list(
      labels = c(
        `1` = "Salary or fee income", `2` = "Self-employment income", `3` = "Incapacity benefit",
        `4` = "Early retirement or retirement pension", `5` = "Pension schemes", `6` = "Social security",
        `7` = "Unemployment benefits", `8` = "Other welfare", `9` = "Black money",
        `10` = "Interest income", `11` = "Trading bonds, shares, or real estate", `12` = "Inheritance",
        `13` = "Other", `99` = "Don't know"
      ),
      missing_codes = character()
    ),
    M8 = list(
      labels = c(
        `1` = "Very good", `2` = "Good", `3` = "Average", `4` = "Bad", `5` = "Very bad",
        `9` = "Don't know"
      ),
      missing_codes = character()
    )
  )
}

.shild_value_audit = function(values, variable, labels, missing_codes, population) {
  codes = as.character(values)
  codes[is.na(values)] = "<NA>"
  audit = data.table(source_code = codes)[, .(n = .N), by = source_code][order(source_code)]
  audit[, `:=`(
    variable = variable,
    mapped_value = unname(labels[source_code]),
    action = fifelse(
      source_code == "<NA>",
      "retain source missing",
      fifelse(source_code %in% names(labels), "map to labeled factor level", "set to missing")
    ),
    population = population
  )]
  audit[source_code %in% missing_codes, action := "set documented non-substantive code to missing"]
  audit[source_code == "<NA>", mapped_value := NA_character_]
  setcolorder(audit, c("variable", "source_code", "n", "mapped_value", "action", "population"))
  audit[]
}

.prepare_shild_raw = function(source_data) {
  source_data = copy(as.data.table(source_data))
  predictors = .shild_predictors()
  required = c("A1", predictors)
  .assert_expected_columns(source_data, required, "raw SHILD input")
  if (length(predictors) != 27L) {
    stop("The locked SHILD predictor set must contain exactly 27 variables.", call. = FALSE)
  }
  unexpected_outcomes = setdiff(unique(source_data$A1[!is.na(source_data$A1)]), c(1L, 2L, 3L, 4L, 5L, 8L, 9L))
  if (length(unexpected_outcomes)) {
    stop("Unexpected raw SHILD A1 code(s): ", paste(unexpected_outcomes, collapse = ", "), ".", call. = FALSE)
  }

  source_data[, source_row := .I]
  substantive_target = source_data$A1 %in% c(1L, 2L, 4L, 5L)
  age_in_scope = is.finite(source_data$age) & source_data$age >= 16 & source_data$age <= 64
  analytic_mask = substantive_target & age_in_scope
  raw_analytic = source_data[analytic_mask, c("source_row", "A1", predictors), with = FALSE]
  raw_predictors = copy(raw_analytic[, ..predictors])

  target_labels = c(`1` = "content", `2` = "content", `4` = "discontent", `5` = "discontent")
  target_audit = .shild_value_audit(
    source_data$A1,
    "A1",
    target_labels,
    c("3", "8", "9"),
    "all source rows"
  )
  target_audit[source_code %in% c("3", "8", "9"), action := "exclude non-substantive outcome"]

  factor_specs = .shild_factor_specs()
  value_audit = c(list(target_audit), lapply(names(factor_specs), function(variable) {
    spec = factor_specs[[variable]]
    .shild_value_audit(
      raw_analytic[[variable]],
      variable,
      spec$labels,
      spec$missing_codes,
      "substantive-outcome, age-eligible rows"
    )
  }))

  analytic = raw_analytic[, c("source_row", predictors), with = FALSE]
  setnames(analytic, "source_row", "respondent_id")
  analytic[, life_satisfaction := factor(
    unname(target_labels[as.character(raw_analytic$A1)]),
    levels = c("content", "discontent")
  )]
  for (variable in names(factor_specs)) {
    spec = factor_specs[[variable]]
    mapped = unname(spec$labels[as.character(analytic[[variable]])])
    set(analytic, j = variable, value = factor(mapped, levels = unname(spec$labels)))
  }

  sentinel_map = list(
    E1 = 999L,
    E2 = c(998L, 999L),
    F15 = c(98L, 99L),
    J17 = 999L,
    M6 = c(99998L, 99999L)
  )
  recoded = .recode_domain_sentinels(analytic, sentinel_map)
  analytic = recoded$data
  sentinel_audit = recoded$audit
  sentinel_audit[, basis := fifelse(
    variable == "F15",
    "outside the documented 0-10 response range; verified by the responsible co-authors",
    "prespecified high code inferred from the supplied data; verified by the responsible co-authors"
  )]

  domain_audit = rbindlist(list(
    raw_analytic[, .(
      variable = "E1", flagged_range = "value = 0 cm", n_flagged = sum(E1 == 0, na.rm = TRUE),
      minimum = suppressWarnings(min(E1[E1 == 0], na.rm = TRUE)),
      maximum = suppressWarnings(max(E1[E1 == 0], na.rm = TRUE)),
      action = "recode to missing as physically impossible adult height",
      basis = "dictionary specifies centimeters; analytic respondents are aged 16-64"
    )],
    raw_analytic[, .(
      variable = "E1", flagged_range = "0 < value < 100 cm", n_flagged = sum(E1 > 0 & E1 < 100, na.rm = TRUE),
      minimum = suppressWarnings(min(E1[E1 > 0 & E1 < 100], na.rm = TRUE)),
      maximum = suppressWarnings(max(E1[E1 > 0 & E1 < 100], na.rm = TRUE)),
      action = "recode to missing as implausible adult height; do not infer or convert units",
      basis = "dictionary specifies centimeters; analytic respondents are aged 16-64"
    )],
    raw_analytic[, .(
      variable = "E2", flagged_range = "value = 0 kg", n_flagged = sum(E2 == 0, na.rm = TRUE),
      minimum = suppressWarnings(min(E2[E2 == 0], na.rm = TRUE)),
      maximum = suppressWarnings(max(E2[E2 == 0], na.rm = TRUE)),
      action = "recode to missing as physically impossible adult weight",
      basis = "dictionary specifies kilograms; analytic respondents are aged 16-64"
    )],
    raw_analytic[, .(
      variable = "E2", flagged_range = "0 < value < 20 kg", n_flagged = sum(E2 > 0 & E2 < 20, na.rm = TRUE),
      minimum = suppressWarnings(min(E2[E2 > 0 & E2 < 20], na.rm = TRUE)),
      maximum = suppressWarnings(max(E2[E2 > 0 & E2 < 20], na.rm = TRUE)),
      action = "recode to missing as implausible adult weight; do not infer or convert units",
      basis = "dictionary specifies kilograms; analytic respondents are aged 16-64"
    )]
  ), use.names = TRUE)
  domain_audit[, n_recoded := n_flagged]
  domain_audit[!is.finite(minimum), `:=`(minimum = NA_real_, maximum = NA_real_)]
  minimum_plausible = c(E1 = 100, E2 = 20)
  for (variable in names(minimum_plausible)) {
    implausible_rows = which(
      !is.na(analytic[[variable]]) & analytic[[variable]] < minimum_plausible[[variable]]
    )
    if (length(implausible_rows)) {
      set(analytic, i = implausible_rows, j = variable, value = NA)
    }
  }
  sentinel_audit = rbindlist(list(
    sentinel_audit,
    data.table(
      variable = c("E1", "E2"),
      code = 0,
      n_recoded = c(sum(raw_analytic$E1 == 0, na.rm = TRUE), sum(raw_analytic$E2 == 0, na.rm = TRUE)),
      basis = c(
        "physically impossible adult height; verified by the responsible co-authors",
        "physically impossible adult weight; verified by the responsible co-authors"
      )
    )
  ), use.names = TRUE)

  sample_flow = data.table(
    stage = c(
      "raw source rows",
      "rows with substantive extreme A1 outcome (1, 2, 4, or 5)",
      "analytic rows with substantive outcome and age 16-64"
    ),
    n = c(nrow(source_data), sum(substantive_target), sum(analytic_mask))
  )
  sample_flow[, exclusion_from_previous := c(0L, head(n, -1L) - tail(n, -1L))]

  numeric_rules = c(
    age = "retain numeric years; restrict analytic sample to 16-64",
    E1 = paste(
      "retain centimeters; recode 999 and values below 100 to missing;",
      "do not infer or convert alternative units"
    ),
    E2 = paste(
      "retain kilograms; recode 998, 999, and values below 20 to missing;",
      "do not infer or convert alternative units"
    ),
    F15 = "retain the documented 0-10 numeric rating; recode 98 and 99 to missing",
    J17 = "retain numeric count; recode 999 to missing",
    M6 = "retain numeric amount; recode 99998 and 99999 to missing"
  )
  factor_rules = vapply(factor_specs, function(spec) {
    mapping = paste(paste(names(spec$labels), unname(spec$labels), sep = "="), collapse = " | ")
    missing = if (length(spec$missing_codes)) paste(spec$missing_codes, collapse = ", ") else "none"
    paste0("map codes to factor levels: ", mapping, "; non-substantive/missing codes: ", missing)
  }, character(1L))
  variable_spec = data.table(
    variable = c("A1", predictors),
    role = c("outcome source", rep("predictor", length(predictors))),
    transformation = c(
      "1/2 = content; 4/5 = discontent; exclude 3, 8, and 9",
      unname(c(factor_rules, numeric_rules)[predictors])
    ),
    verification_status = "verified_by_responsible_coauthors on 2026-09-09"
  )

  list(
    data = analytic[],
    predictors = predictors,
    sample_flow = sample_flow[],
    value_recode_audit = rbindlist(value_audit, use.names = TRUE, fill = TRUE),
    variable_recode_spec = variable_spec[],
    sentinel_audit = sentinel_audit[],
    domain_value_audit = domain_audit[],
    missingness_before = .missingness_table(
      raw_predictors,
      predictors,
      "before documented categorical, sentinel, and implausible domain-value recoding"
    ),
    missingness_after = .missingness_table(
      analytic,
      predictors,
      "after documented categorical, sentinel, and implausible domain-value recoding"
    )
  )
}

.analysis_shild_semantic_levels = function() {
  list(
    gender = c("Man", "Woman"),
    A2 = c("Very well", "Well", "Average", "Poor", "Very poor"),
    C1 = c("Yes", "No"),
    D2 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D4 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D6 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D8 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D10 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D11 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D15 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D16 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    D17 = c("Always", "Often", "Sometimes", "Rarely", "Never"),
    E5_a = c("Yes", "No"),
    E17 = c(
      "Partner/spouse/boyfriend/girlfriend", "Children", "Parents", "Siblings", "Other family",
      "Friends/colleagues", "Staff", "I do not share this with anyone", "Others"
    ),
    G1 = c("Yes", "No"),
    J2 = c("Daily", "Several times a week", "Once a week", "Several times a month", "Once a month",
      "Less frequently", "Never"),
    J4 = c("Daily", "Several times a week", "Once a week", "Several times a month", "Once a month",
      "Less frequently", "Never"),
    J9 = c("Daily", "Several times a week", "Once a week", "Several times a month", "Once a month",
      "Less frequently", "Never"),
    J14 = c("Daily", "Several times a week", "Once a week", "Several times a month", "Once a month",
      "Less frequently", "Never", "Don't know"),
    M2 = c(
      "Salary or fee income", "Self-employment income", "Incapacity benefit",
      "Early retirement or retirement pension",
      "Pension schemes", "Social security", "Unemployment benefits", "Other welfare", "Black money",
      "Interest income", "Trading bonds, shares, or real estate", "Inheritance", "Other", "Don't know"
    ),
    M8 = c("Very good", "Good", "Average", "Bad", "Very bad", "Don't know")
  )
}

.analysis_order_profile_values = function(profiles, feature_column, value_column, level_orders, reverse = FALSE) {
  profiles = copy(as.data.table(profiles))
  profiles[, value_key := paste(get(feature_column), get(value_column), sep = "::")]
  ordered_keys = unlist(lapply(unique(profiles[[feature_column]]), function(feature) {
    observed = unique(as.character(profiles[get(feature_column) == feature, get(value_column)]))
    requested = level_orders[[feature]] %or% character()
    values = c(intersect(requested, observed), setdiff(observed, requested))
    if (isTRUE(reverse)) {
      values = rev(values)
    }
    paste(feature, values, sep = "::")
  }), use.names = FALSE)
  profiles[, value_key := factor(value_key, levels = unique(ordered_keys))]
  profiles
}

.plot_effect_profiles = function(
    profiles,
    title,
    feature_labels = .analysis_shild_feature_labels(),
    level_orders = .analysis_shild_semantic_levels(),
    base_size = 11,
    axis_text_size = NULL,
    axis_title_size = NULL,
    legend_text_size = NULL,
    legend_title_size = NULL,
    strip_text_size = NULL) {
  profiles = copy(as.data.table(profiles))
  .analysis_require_columns(
    profiles,
    c("feature", "value", "value_numeric", "median_prediction", "q10_prediction", "q90_prediction"),
    "Fitted-effect profiles"
  )
  profiles[, feature_label := .analysis_feature_labels(feature, feature_labels)]
  numeric = profiles[is.finite(value_numeric)]
  categorical = profiles[!is.finite(value_numeric)]
  plots = list()
  palette = .analysis_palette()
  if (nrow(numeric)) {
    plots$numeric = ggplot(numeric, aes(x = value_numeric, y = median_prediction)) +
      geom_ribbon(
        aes(ymin = q10_prediction, ymax = q90_prediction),
        fill = palette$metric[["primary"]],
        alpha = 0.2
      ) +
      geom_line(color = palette$metric[["secondary"]], linewidth = 0.8) +
      geom_point(color = palette$metric[["secondary"]], size = 2.1) +
      facet_wrap(~ feature_label, scales = "free_x") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      labs(
        x = NULL,
        y = "Fitted probability of discontent (0-1)",
        title = title,
        subtitle = .analysis_wrap_text(paste(
          "Red lines are medians; blue ribbons are descriptive 10th-90th percentiles over reference rows,",
          "not confidence intervals"
        ))
      ) +
      .analysis_theme(base_size = base_size) +
      theme(legend.position = "none") +
      .analysis_typography_theme(
        axis_text_size = axis_text_size,
        axis_title_size = axis_title_size,
        strip_text_size = strip_text_size
      )
  }
  if (nrow(categorical)) {
    categorical = .analysis_order_profile_values(categorical, "feature", "value", level_orders)
    plots$categorical = ggplot(categorical, aes(x = value_key, y = median_prediction)) +
      geom_errorbar(
        aes(ymin = q10_prediction, ymax = q90_prediction),
        color = palette$metric[["primary"]],
        width = 0.14,
        linewidth = 0.9
      ) +
      geom_point(color = palette$metric[["secondary"]], size = 2.5) +
      facet_wrap(~ feature_label, scales = "free_x") +
      scale_x_discrete(labels = function(value) sub("^[^:]+::", "", value)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      labs(
        x = NULL,
        y = "Fitted probability of discontent (0-1)",
        title = title,
        subtitle = .analysis_wrap_text(paste(
          "Categories follow questionnaire order; red points are medians, and blue whiskers are descriptive",
          "10th-90th percentiles over reference rows, not confidence intervals"
        ))
      ) +
      .analysis_theme(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      .analysis_typography_theme(
        axis_text_size = axis_text_size,
        axis_title_size = axis_title_size,
        legend_text_size = legend_text_size,
        legend_title_size = legend_title_size,
        strip_text_size = strip_text_size
      )
  }
  plots
}

.plot_interaction_profiles = function(
    profiles,
    title,
    feature_labels = .analysis_shild_feature_labels(),
    level_orders = .analysis_shild_semantic_levels(),
    base_size = 11,
    axis_text_size = NULL,
    axis_title_size = NULL,
    legend_text_size = NULL,
    legend_title_size = NULL,
    strip_text_size = NULL) {
  profiles = copy(as.data.table(profiles))
  .analysis_require_columns(
    profiles,
    c("feature_1", "feature_2", "first_value", "second_value", "mean_prediction"),
    "Fitted interaction profiles"
  )
  profiles[, pair_label := paste(
    .analysis_feature_labels(feature_1, feature_labels),
    .analysis_feature_labels(feature_2, feature_labels),
    sep = " x "
  )]
  first = .analysis_order_profile_values(profiles, "feature_1", "first_value", level_orders)
  profiles[, first_key := first$value_key]
  second = .analysis_order_profile_values(profiles, "feature_2", "second_value", level_orders, reverse = TRUE)
  profiles[, second_key := second$value_key]
  palette = .analysis_palette()

  ggplot(profiles, aes(x = first_key, y = second_key, fill = mean_prediction)) +
    geom_tile(color = "white", linewidth = 0.35) +
    facet_wrap(~ pair_label, scales = "free") +
    scale_x_discrete(labels = function(value) sub("^[^:]+::", "", value)) +
    scale_y_discrete(labels = function(value) sub("^[^:]+::", "", value)) +
    scale_fill_gradient(
      low = palette$gradient[["low"]],
      high = palette$gradient[["high"]],
      limits = c(0, 1)
    ) +
    labs(
      x = "First feature value",
      y = "Second feature value",
      fill = "Fitted probability\nof discontent (0-1)",
      title = title,
      subtitle = .analysis_wrap_text(paste(
        "One panel per queried pair; categories follow questionnaire order. Synthetic two-feature partial-",
        "dependence queries can leave observed joint support and are descriptive, not causal."
      ))
    ) +
    .analysis_theme(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    .analysis_typography_theme(
      axis_text_size = axis_text_size,
      axis_title_size = axis_title_size,
      legend_text_size = legend_text_size,
      legend_title_size = legend_title_size,
      strip_text_size = strip_text_size
    )
}
