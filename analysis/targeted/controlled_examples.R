# Known PFI properties under exact redundancy and declared Gaussian measurement error.
# This simulation contains no SHILD/PISA records and does not evaluate human users of CSDG.
# Model j predicts from predictor j alone, either unchanged (f_j(x) = x_j, as in the main-text example) or as the
# best predictor of Y from X_j (f_j(x) = sqrt(r_j) x_j). With slope c, marginal and grouped PFI equal 2 c sqrt(r_j)
# and conditional PFI 2 c sqrt(r_j) (1 - r_k).

controlled_slope = function(r1, r2, model, model_form) {
  if (model_form == "identity") 1 else sqrt(c(r1, r2)[model])
}

controlled_pfi_expectation = function(r1, r2, model, intervention, feature = model, model_form = "identity") {
  stopifnot(r1 >= 0, r1 <= 1, r2 >= 0, r2 <= 1, model %in% 1:2,
    intervention %in% c("marginal", "conditional", "joint_block"), feature %in% 1:2,
    model_form %in% c("identity", "optimal"))
  reliability = c(r1, r2)
  if (intervention != "joint_block" && feature != model) return(0)
  slope = controlled_slope(r1, r2, model, model_form)
  2 * slope * sqrt(reliability[model]) * if (intervention == "conditional") (1 - reliability[3L - model]) else 1
}

controlled_conditional_draw = function(other, r1, r2, noise) {
  rho = sqrt(r1 * r2)
  rho * other + sqrt(max(0, 1 - rho^2)) * noise
}

run_controlled_examples = function(project_root = getwd(), n = 200000L, seed = 20260910L, render = TRUE) {
  stopifnot(n >= 1000L, is.logical(render), length(render) == 1L)
  suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
  set.seed(seed)
  settings = data.table(scenario = c("Exact redundancy", "Equal reliability", "X1 more reliable", "X2 more reliable"),
    r1 = c(1, .8, .9, .4), r2 = c(1, .8, .4, .9))
  results = list()
  for (scenario_index in seq_len(nrow(settings))) {
    setting = settings[scenario_index]
    z = rnorm(n)
    x1 = sqrt(setting$r1) * z + sqrt(1 - setting$r1) * rnorm(n)
    x2 = sqrt(setting$r2) * z + sqrt(1 - setting$r2) * rnorm(n)
    x = list(x1, x2)
    independent_z = rnorm(n)
    independent_block = list(
      sqrt(setting$r1) * independent_z + sqrt(1 - setting$r1) * rnorm(n),
      sqrt(setting$r2) * independent_z + sqrt(1 - setting$r2) * rnorm(n)
    )
    for (model_form in c("identity", "optimal")) for (model in 1:2) {
      slope = controlled_slope(setting$r1, setting$r2, model, model_form)
      baseline_loss = (z - slope * x[[model]])^2
      for (intervention in c("marginal", "conditional", "joint_block")) {
        features = if (intervention == "joint_block") model else 1:2
        for (feature in features) {
          replacement = if (intervention == "conditional") {
            controlled_conditional_draw(x[[3L - feature]], setting$r1, setting$r2, rnorm(n))
          } else if (intervention == "joint_block") independent_block[[model]] else rnorm(n)
          perturbed_prediction = slope * if (intervention == "joint_block" || feature == model) replacement else x[[model]]
          difference = (z - perturbed_prediction)^2 - baseline_loss
          expected = controlled_pfi_expectation(setting$r1, setting$r2, model, intervention, feature, model_form)
          estimate = mean(difference)
          monte_carlo_se = sd(difference) / sqrt(n)
          stopifnot(abs(estimate - expected) <= 6 * monte_carlo_se + 1e-12)
          results[[length(results) + 1L]] = data.table(
            scenario = setting$scenario, r1 = setting$r1, r2 = setting$r2, model_form = model_form,
            model = if (model_form == "identity") paste0("f", model, "(X) = X", model) else
              paste0("f", model, "(X) = sqrt(r", model, ") X", model),
            model_index = model, intervention = intervention,
            queried_feature = if (intervention == "joint_block") "X1 + X2" else paste0("X", feature),
            model_used_feature = feature == model, n_monte_carlo = n, seed = seed,
            expected_baseline_risk = 1 - 2 * slope * sqrt(c(setting$r1, setting$r2)[model]) + slope^2,
            analytical_population_pfi = expected, monte_carlo_pfi = estimate, monte_carlo_se = monte_carlo_se,
            absolute_mc_error = abs(estimate - expected), passed_six_se_check = TRUE,
            replacement_law = if (intervention == "conditional") "True Gaussian conditional law" else {
              "Independent population replacement; dependence preserved within joint block"
            },
            evidence_kind = "controlled_simulation_not_human_or_application_data"
          )
        }
      }
    }
  }
  output = file.path(project_root, "analysis/targeted/results/controlled")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  table = rbindlist(results)
  fwrite(table, file.path(output, "controlled_pfi_results.csv"))
  # A finite permutation is deliberately separate from the independent-replacement population calculation.
  set.seed(seed + 1L)
  finite_z = rnorm(1000L)
  permutation = sample.int(length(finite_z))
  finite = data.table(n = length(finite_z), seed = seed + 1L,
    realized_permutation_loss = mean((finite_z - finite_z[permutation])^2),
    expected_random_permutation_conditional_on_sample = 2 * mean((finite_z - mean(finite_z))^2),
    population_independent_replacement_loss = 2,
    interpretation = "Finite empirical permutation is not required to equal its population expectation")
  fwrite(finite, file.path(output, "finite_permutation_example.csv"))
  if (render) {
    for (model_id in 1:2) {
      plot_data = copy(table[model_form == "identity" & model_index == model_id & model_used_feature])
      plot_data[, scenario_label := sprintf("%s\nr1 = %.1f; r2 = %.1f", scenario, r1, r2)]
      plot_data[, scenario_label := factor(scenario_label, levels = rev(unique(scenario_label)))]
      plot_data[, intervention := factor(intervention, levels = c("marginal", "conditional", "joint_block"),
        labels = c("Marginal indicator", "Conditional indicator", "Joint block"))]
      p = ggplot(plot_data, aes(x = analytical_population_pfi, y = scenario_label, shape = intervention)) +
        geom_point(position = position_dodge(width = .55), size = 2.8, color = "black", fill = "white") +
        scale_shape_manual(values = c(21, 24, 22), name = NULL) +
        scale_x_continuous(limits = c(0, 2.08), breaks = c(0, .5, 1, 1.5, 2),
          expand = expansion(mult = c(.025, .025))) +
        labs(x = "Population PFI (increase in squared-error risk)", y = NULL) +
        theme_minimal(base_size = 11, base_family = "Arial") +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom", plot.margin = margin(8, 12, 8, 8))
      ggsave(file.path(output, paste0("controlled_pfi_model_", model_id, ".pdf")), p,
        width = 7.4, height = 3.5, device = cairo_pdf, bg = "white")
      ggsave(file.path(output, paste0("controlled_pfi_model_", model_id, ".png")), p,
        width = 7.4, height = 3.5, dpi = 300, bg = "white")
    }
  }
  jsonlite::write_json(list(seed = seed, monte_carlo_rows_per_scenario = n, scenarios = 4L,
    result_rows = nrow(table), maximum_absolute_monte_carlo_error = max(table$absolute_mc_error),
    analytical_checks = sprintf("All %d loss-contrast estimates within six Monte Carlo standard errors", nrow(table)),
    model_functions = c("f1(X) = X1", "f2(X) = X2", "f1(X) = sqrt(r1) X1", "f2(X) = sqrt(r2) X2"),
    human_or_application_data = FALSE,
    status = "executed_and_analytically_checked"), file.path(output, "controlled_run_metadata.json"),
    pretty = TRUE, auto_unbox = TRUE)
  invisible(table)
}

if (sys.nframe() == 0L) run_controlled_examples()
