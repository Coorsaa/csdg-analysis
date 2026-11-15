# Simulation: operating characteristics of the decision criteria for claims about permutation feature importance.
#
# How often does each criterion declare a claim "met" when the learner-level property is false, at its boundary,
# or true with increasing margin? Two claims are assessed:
#   * dominance: "the learner relies mainly on A and B", that is, the PFI of A and of B is each at least twice the
#     PFI of every other predictor (the binding comparison is A against C);
#   * ordering: "A has larger PFI than B".
# Criteria: (1) the rule holds in every one of the K cross-validation folds; (2) for every comparison of the claim,
# the lower bound of the one-sided 95% corrected resampled t interval (Nadeau and Bengio, 2003) for the mean over
# folds of the difference exceeds 0; (3) every such mean exceeds 0 (point estimate only). The uncorrected t interval
# is recorded as a reference but not shown in the figure.
# Data: y = X b + e with eight standard normal predictors (A, B, C, and five noise predictors with small effects),
# pairwise correlation 0.3, and standard normal errors. The learner is ordinary least squares (chosen for speed).
# PFI is the held-out increase in mean squared error after marginal permutation, averaged over 10 permutations per
# fold. The true learner-level value of predictor j is the population PFI, 2 b_j^2; it is reproduced with OLS fitted
# on 10^6 draws and evaluated on 10^6 independent draws.
#
# Only aggregate tables and one figure are written. Usage (from the repository root):
#   Rscript analysis/simulation/criteria_simulation.R --workers=3
#   Rscript analysis/simulation/criteria_simulation.R --quick   # smoke test

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(parallel)
})

args = commandArgs(trailingOnly = TRUE)
arg_value = function(name, default = NULL) {
  hit = grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}
quick = "--quick" %in% args
workers = as.integer(arg_value("workers", "1"))
root = normalizePath(arg_value("root", "."), mustWork = TRUE)
output_dir = arg_value("output-dir", file.path(root, "analysis/simulation/results"))
if (quick) output_dir = paste0(output_dir, "_quick")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

settings = list(
  seed = 20260928L,
  replications = if (quick) 40L else 500L,
  n = c(1000L, 5000L),
  folds = c(5L, 10L),
  permutations = 10L,
  correlation = 0.3,
  error_sd = 1,
  alpha = 0.05,
  population_n = if (quick) 1e5 else 1e6,
  margins = round(seq(0.5, 3, by = 0.025), 3)
)

# ---- Design: coefficients for each claim and true margin ----

# The margin is the true PFI of A divided by the value it must exceed: 2 * PFI(C) for dominance and PFI(B) for
# ordering. The property holds if the margin exceeds 1; with marginal permutation of standardized predictors the
# population PFI of predictor j is 2 b_j^2, so the grid is set through b_C (dominance) or b_B (ordering).
predictors = c("A", "B", "C", paste0("N", 1:5))
p = length(predictors)
base_coefficients = c(A = 0.5, B = 0.625, C = 0.3, N1 = 0.1, N2 = 0.08, N3 = 0.06, N4 = 0.04, N5 = 0.02)
grid = CJ(claim = c("dominance", "ordering"), margin = settings$margins)
coefficient_matrix = t(vapply(seq_len(nrow(grid)), function(i) {
  b = base_coefficients
  if (grid$claim[[i]] == "dominance") b[["C"]] = b[["A"]] / sqrt(2 * grid$margin[[i]])
  else b[["B"]] = b[["A"]] / sqrt(grid$margin[[i]])
  b
}, numeric(p)))
colnames(coefficient_matrix) = predictors

# Every comparison a claim makes; the dominance claim holds only if all twelve differences are positive.
comparisons = rbind(
  CJ(claim = "dominance", first = c("A", "B"), second = setdiff(predictors, c("A", "B")), factor = 2),
  data.table(claim = "ordering", first = "A", second = "B", factor = 1)
)
binding = data.table(claim = c("dominance", "ordering"), first = "A", second = c("C", "B"), factor = c(2, 1))

sigma = matrix(settings$correlation, p, p)
diag(sigma) = 1
chol_sigma = chol(sigma)
draw_predictors = function(n) matrix(rnorm(n * p), n, p) %*% chol_sigma

# ---- Held-out PFI for OLS ----

# Permuting x_j changes each prediction of a linear model by beta_j * delta with delta = x_j[perm] - x_j. With the
# held-out residuals r, the PFI is beta_j^2 * mean(delta^2) - 2 * beta_j * mean(r * delta), averaged over the
# permutations. OLS is equivariant: for y = X b + e the fitted slopes are b + d, where d is the OLS fit of e on X,
# and the residuals do not depend on b. One fit per fold therefore serves every claim and every margin.
fold_moments = function(x, e, test) {
  d = lm.fit(cbind(1, x[-test, , drop = FALSE]), e[-test])$coefficients
  x_test = x[test, , drop = FALSE]
  r = e[test] - d[[1L]] - drop(x_test %*% d[-1L])
  m = length(test)
  moments = vapply(seq_len(p), function(j) {
    delta = matrix(x_test[, j][replicate(settings$permutations, sample.int(m))], m) - x_test[, j]
    c(mean(delta^2), mean(r * delta))
  }, numeric(2L))
  list(d = d[-1L], s2 = moments[1L, ], s1 = moments[2L, ])
}
fold_pfi = function(moments, coefficients = coefficient_matrix) {
  beta = sweep(coefficients, 2L, moments$d, "+")
  sweep(beta^2, 2L, moments$s2, "*") - 2 * sweep(beta, 2L, moments$s1, "*")
}

# The shortcut must equal PFI computed by refitting on y and predicting with the permuted column.
check_shortcut = function() {
  set.seed(settings$seed - 1L)
  x = draw_predictors(400L)
  e = rnorm(400L, sd = settings$error_sd)
  test = 301:400
  b = coefficient_matrix[1L, ]
  seed_state = .Random.seed
  shortcut = fold_pfi(fold_moments(x, e, test), coefficient_matrix[1L, , drop = FALSE])
  .Random.seed <<- seed_state
  y = drop(x %*% b) + e
  fit = lm.fit(cbind(1, x[-test, ]), y[-test])$coefficients
  predict_ols = function(z) drop(cbind(1, z) %*% fit)
  mse = mean((y[test] - predict_ols(x[test, ]))^2)
  explicit = vapply(seq_len(p), function(j) {
    perms = replicate(settings$permutations, sample.int(length(test)))
    mean(apply(perms, 2L, function(perm) {
      z = x[test, ]
      z[, j] = z[perm, j]
      mean((y[test] - predict_ols(z))^2) - mse
    }))
  }, numeric(1L))
  stopifnot(isTRUE(all.equal(unname(shortcut[1L, ]), explicit, tolerance = 1e-10)))
}
check_shortcut()

# ---- True learner-level values ----

set.seed(settings$seed)
population_n = settings$population_n
x_fit = draw_predictors(population_n)
d_population = lm.fit(cbind(1, x_fit), rnorm(population_n, sd = settings$error_sd))$coefficients
rm(x_fit)
x_eval = draw_predictors(population_n)
r_eval = rnorm(population_n, sd = settings$error_sd) - d_population[[1L]] - drop(x_eval %*% d_population[-1L])
population_moments = vapply(seq_len(p), function(j) {
  delta = x_eval[sample.int(population_n), j] - x_eval[, j]
  c(mean(delta^2), mean(r_eval * delta))
}, numeric(2L))
rm(x_eval, r_eval)
pfi_population = fold_pfi(list(d = d_population[-1L], s2 = population_moments[1L, ], s1 = population_moments[2L, ]))
pfi_analytic = 2 * coefficient_matrix^2

# Expected PFI of OLS fitted on n_train draws: 2 b_j^2 + 2 c ((Sigma^-1)_jj - 1) with c = sigma_e^2 / (n_train - p - 2),
# from E[(X'X)^-1] for a Gaussian design with intercept. It shows how far the population value is from the
# learner-level value at the training size used in each fold.
pfi_training_size = function(n_train) {
  c_value = settings$error_sd^2 / (n_train - p - 2)
  sweep(pfi_analytic, 2L, 2 * c_value * (diag(solve(sigma)) - 1), "+")
}

binding_value = function(pfi, statistic) {
  vapply(seq_len(nrow(grid)), function(i) {
    row = binding[claim == grid$claim[[i]]]
    first = pfi[i, row$first]
    second = row$factor * pfi[i, row$second]
    if (statistic == "difference") first - second else first / second
  }, numeric(1L))
}
truth = copy(grid)
truth[, `:=`(b_A = coefficient_matrix[, "A"], b_B = coefficient_matrix[, "B"], b_C = coefficient_matrix[, "C"],
  pfi_A = pfi_analytic[, "A"], pfi_B = pfi_analytic[, "B"], pfi_C = pfi_analytic[, "C"],
  pfi_A_population = pfi_population[, "A"], pfi_B_population = pfi_population[, "B"],
  pfi_C_population = pfi_population[, "C"],
  difference = binding_value(pfi_analytic, "difference"),
  difference_population = binding_value(pfi_population, "difference"),
  margin_population = binding_value(pfi_population, "ratio"))]
stopifnot(max(abs(truth$margin_population / truth$margin - 1)) < if (quick) 0.1 else 0.03)
training_truth = rbindlist(lapply(settings$n, function(n) rbindlist(lapply(settings$folds, function(k) {
  pfi = pfi_training_size(n - n / k)
  data.table(grid, n = n, folds = k, difference_training_size = binding_value(pfi, "difference"),
    margin_training_size = binding_value(pfi, "ratio"))
}))))

# ---- Replications ----

# "interval_and_share" combines the corrected interval with the requirement that the comparison holds in at least
# 90% of the folds (all folds when K = 5, 9 of 10 when K = 10). "interval_two_sided" and "interval_two_sided_and_share"
# use the lower limit of the two-sided 95% corrected interval (a one-sided 97.5% bound), as the applications and
# mlr3autoiml do.
criteria = c("every_fold", "interval", "mean_only", "uncorrected_interval", "interval_and_share", "interval_two_sided",
  "interval_two_sided_and_share")

assess = function(pfi, k, n) {
  n_test = n / k
  correction = 1 / k + n_test / (n - n_test)
  critical = qt(1 - settings$alpha, df = k - 1L)
  critical_two_sided = qt(1 - settings$alpha / 2, df = k - 1L)
  out = copy(grid)
  out[, (criteria) := TRUE]
  for (claim_name in unique(grid$claim)) {
    rows = which(grid$claim == claim_name)
    for (i in which(comparisons$claim == claim_name)) {
      cmp = comparisons[i]
      difference = pfi[rows, cmp$first, ] - cmp$factor * pfi[rows, cmp$second, ]
      mean_difference = rowMeans(difference)
      fold_variance = apply(difference, 1L, var)
      out[rows, `:=`(
        every_fold = every_fold & apply(difference >= 0, 1L, all),
        interval = interval & (mean_difference - critical * sqrt(correction * fold_variance) > 0),
        mean_only = mean_only & (mean_difference > 0),
        uncorrected_interval = uncorrected_interval & (mean_difference - critical * sqrt(fold_variance / k) > 0),
        interval_and_share = interval_and_share & (mean_difference - critical * sqrt(correction * fold_variance) > 0) &
          (rowMeans(difference >= 0) >= 0.9),
        interval_two_sided = interval_two_sided &
          (mean_difference - critical_two_sided * sqrt(correction * fold_variance) > 0),
        interval_two_sided_and_share = interval_two_sided_and_share &
          (mean_difference - critical_two_sided * sqrt(correction * fold_variance) > 0) &
          (rowMeans(difference >= 0) >= 0.9)
      )]
      if (cmp$first == "A" && cmp$second == binding[claim == claim_name]$second) {
        out[rows, `:=`(estimate = mean_difference, variance_corrected = correction * fold_variance,
          variance_uncorrected = fold_variance / k)]
      }
    }
  }
  out
}

run_replication = function(n, replication) {
  set.seed(settings$seed + 100000L * match(n, settings$n) + replication)
  x = draw_predictors(n)
  e = rnorm(n, sd = settings$error_sd)
  rbindlist(lapply(settings$folds, function(k) {
    fold = sample(rep_len(seq_len(k), n))
    pfi = simplify2array(lapply(seq_len(k), function(f) fold_pfi(fold_moments(x, e, which(fold == f)))))
    assess(pfi, k, n)[, `:=`(n = n, folds = k, replication = replication)]
  }))
}

jobs = CJ(n = settings$n, replication = seq_len(settings$replications))
started = Sys.time()
results = rbindlist(mclapply(seq_len(nrow(jobs)), function(i) run_replication(jobs$n[[i]], jobs$replication[[i]]),
  mc.cores = workers, mc.preschedule = TRUE))
stopifnot(nrow(results) == nrow(jobs) * length(settings$folds) * nrow(grid), !anyNA(results))
message(sprintf("Replications finished in %.1f minutes.", as.numeric(difftime(Sys.time(), started, units = "mins"))))

# ---- Aggregates ----

rates = melt(results[, lapply(.SD, mean), by = .(claim, margin, n, folds), .SDcols = criteria],
  id.vars = c("claim", "margin", "n", "folds"), variable.name = "criterion", value.name = "met")
rates[, `:=`(criterion = as.character(criterion), mc_se = sqrt(met * (1 - met) / settings$replications),
  replications = settings$replications,
  status = fifelse(abs(margin - 1) < 1e-9, "boundary", fifelse(margin < 1, "false", "true")))]
rates = merge(rates, truth[, .(claim, margin, margin_population)], by = c("claim", "margin"))
setcolorder(rates, c("claim", "margin", "margin_population", "status", "n", "folds", "criterion", "met", "mc_se",
  "replications"))
setorder(rates, claim, criterion, n, folds, margin)

estimates = results[, .(mean_estimate = mean(estimate), empirical_sd = sd(estimate),
  corrected_se = sqrt(mean(variance_corrected)), uncorrected_se = sqrt(mean(variance_uncorrected))),
  by = .(claim, margin, n, folds)]
estimates = merge(estimates, truth[, .(claim, margin, difference, difference_population)], by = c("claim", "margin"))
estimates = merge(estimates, training_truth, by = c("claim", "margin", "n", "folds"))
estimates[, `:=`(bias = mean_estimate - difference, variance_ratio_corrected = corrected_se^2 / empirical_sd^2,
  variance_ratio_uncorrected = uncorrected_se^2 / empirical_sd^2)]
setcolorder(estimates, c("claim", "margin", "n", "folds", "difference", "difference_population",
  "difference_training_size", "margin_training_size", "mean_estimate", "bias", "empirical_sd", "corrected_se",
  "uncorrected_se", "variance_ratio_corrected", "variance_ratio_uncorrected"))
setorder(estimates, claim, n, folds, margin)

# Key numbers: rates at the boundary, the largest rate at or below it, the smallest margin at which each criterion
# reaches 80% "met", and the interval variance relative to the empirical variance of the mean at the boundary.
summary_table = rates[, .(
  met_at_boundary = met[status == "boundary"],
  mc_se_at_boundary = mc_se[status == "boundary"],
  max_met_at_or_below_boundary = max(met[status != "true"]),
  met_at_margin_1.25 = met[abs(margin - 1.25) < 1e-9],
  met_at_margin_1.5 = met[abs(margin - 1.5) < 1e-9],
  met_at_margin_2 = met[abs(margin - 2) < 1e-9],
  margin_for_80_percent = if (any(met >= 0.8)) min(margin[met >= 0.8]) else NA_real_
), by = .(claim, criterion, n, folds)]
summary_table = merge(summary_table, estimates[abs(margin - 1) < 1e-9,
  .(claim, n, folds, variance_ratio_corrected, variance_ratio_uncorrected)], by = c("claim", "n", "folds"))
setcolorder(summary_table, c("claim", "criterion", "n", "folds"))
setorder(summary_table, claim, criterion, n, folds)

fwrite(rates, file.path(output_dir, "criteria_rates.csv"))
fwrite(estimates, file.path(output_dir, "criteria_estimates.csv"))
fwrite(truth, file.path(output_dir, "criteria_truth.csv"))
fwrite(summary_table, file.path(output_dir, "criteria_summary.csv"))

# ---- Figure ----

plot_data = rates[criterion %in% c("interval_and_share", "interval", "every_fold", "mean_only")]
plot_data[, `:=`(
  criterion = factor(criterion, levels = c("interval_and_share", "interval", "every_fold", "mean_only"),
    labels = c("Corrected interval and 90% of folds", "Corrected interval", "Every fold", "Mean only")),
  panel_label = factor(sprintf("paste(italic(n), ' = %s, ', italic(K), ' = %d')", format(n, big.mark = ","), folds),
    levels = CJ(n = settings$n, k = settings$folds)[,
      sprintf("paste(italic(n), ' = %s, ', italic(K), ' = %d')", format(n, big.mark = ","), k)]),
  claim_label = factor(claim, levels = c("dominance", "ordering"),
    labels = c("Relies-mainly claim:\nPFI(A) / [2 PFI(C)]", "Order claim:\nPFI(A) / PFI(B)"))
)]
plot = ggplot(plot_data, aes(x = margin, y = met, linetype = criterion)) +
  geom_hline(yintercept = c(settings$alpha, 0.5), linewidth = 0.3, color = "gray70") +
  geom_vline(xintercept = 1, linewidth = 0.3, color = "gray45") +
  geom_line(linewidth = 0.55, color = "black") +
  scale_linetype_manual(values = c("solid", "twodash", "longdash", "dotted")) +
  scale_x_continuous(breaks = 1:3, limits = range(settings$margins)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".50", ".75", "1")) +
  facet_grid(claim_label ~ panel_label, labeller = labeller(panel_label = label_parsed, claim_label = label_value)) +
  labs(x = "True learner-level margin (PFI of A divided by the value it must exceed)",
    y = "Proportion of replications with claim met", linetype = NULL) +
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(color = "black", size = 9.5),
    strip.text = element_text(color = "black", size = 10), legend.position = "bottom",
    legend.text = element_text(size = 10), legend.key.width = unit(2.2, "lines"), panel.spacing = unit(0.7, "lines"),
    plot.margin = margin(6, 6, 6, 6))
ggsave(file.path(output_dir, "criteria_simulation.pdf"), plot, width = 7.2, height = 4.6, device = cairo_pdf,
  bg = "white")
ggsave(file.path(output_dir, "criteria_simulation.png"), plot, width = 7.2, height = 4.6, dpi = 300, bg = "white")
message("Results written to ", output_dir)
