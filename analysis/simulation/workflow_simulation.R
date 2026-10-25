# Simulation: the CSDG workflow for claims about permutation feature importance, from the exploratory claim to its
# test on new data, compared with standard practice.
#
# The researcher inspects held-out permutation feature importance (PFI) of a learner refitted in each fold of a
# stratified 10-fold cross-validation, names the two predictors with the largest fold-averaged PFI (top1, top2), and
# states two claims:
#   R: "the learner relies mainly on top1 and top2" (each has at least twice the PFI of every other predictor);
#   O: "the learner relies more on top1 than on top2".
# Standard practice reports R if PFI(top2) is at least twice PFI(third) on the fold averages and always reports O.
# CSDG checks each claim with the default criteria of the article and of mlr3autoiml 0.3.0 (csdg_check() and
# csdg_assess() with the claims of claim_relies_mainly() and claim_order(), worded "Under marginal permutation"):
# content on the fold averages with the corrected resampled t interval rule (Nadeau and Bengio, 2003) for every
# comparison the claim requires (R: each named predictor against each of the 8 other predictors), minimum importance
# (1% of the improvement over the model without predictors), selection stability (9 of 10 folds), and the Monte Carlo
# rule. A met claim is exploratory; it is tested with the same fixed criteria on an independent sample of the same
# size (csdg_confirm()). The components of the rule are also evaluated alone in the same workflow: the content check,
# content and minimum importance, and selection stability. The functions below reimplement these rules;
# --mode=verify compares them with the package.
# Data: binary outcome, 10 predictors. Latent Z1, Z2, Z3 ~ N(0, 1); X1 and X2 are items of Z1 with reliabilities r1
# and r2, X3 an item of Z2 with reliability .8, X4 = Z3, X5 ~ N(0, 1), X6 = .6 X3 + .8 e (correlated with X3, no
# effect), X7 to X10 noise. logit P(Y = 1) = b0 + bA Z1 + bB Z2 + 0.3 Z3 + 0.3 (X5^2 - 1), with b0 for prevalence .20.
# Scenario S1 ("clear"): r1 = .9, r2 = .6, bA = 1.2, bB = 1.0; S2 ("ambiguous"): r1 = r2 = .8, bA = bB = 0.9.
# Learner: XGBoost (xgboost package, 100 rounds, depth 3, eta 0.1); loss: log loss; PFI: mean increase in held-out log
# loss over 10 permutations per fold and predictor. Sample sizes 300, 1,000, and 5,000; 500 replications each.
# Truth: the learner's expected PFI and improvement at the training size of a fold, from 100 fits on independent
# training samples, each evaluated on 20,000 independent test rows with 5 permutations. Claim R is true if each named
# predictor has at least twice the true PFI of every other predictor and at least 1% of the true improvement; claim O
# is true if PFI(top1) - PFI(top2) is at least 1% of the true improvement, false if it is at most 0, and boundary
# otherwise. The claims are also assessed with the package's default wording, under which a named predictor whose
# R^2 on the other predictors is at least .50 leaves the claim unresolved (reported as a sensitivity result).
#
# Only aggregate tables, one figure, and the session information are written. The full run takes about 15 minutes
# with 6 workers. Usage (from the repository root):
#   Rscript analysis/simulation/workflow_simulation.R --workers=6
#   Rscript analysis/simulation/workflow_simulation.R --quick         # smoke test, writes results/workflow_quick
#   Rscript analysis/simulation/workflow_simulation.R --mode=timing   # times 5 replications per n and scenario
#   Rscript analysis/simulation/workflow_simulation.R --mode=figure   # redraws the figure from the result tables
#   R_LIBS=<library with mlr3autoiml 0.3.0> Rscript analysis/simulation/workflow_simulation.R --mode=verify
# Options: --cache=<file.rds> keeps the replication results outside the repository and resumes an interrupted run;
# --replications, --root, --output-dir.

Sys.setenv(OMP_NUM_THREADS = "1")
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(parallel)
  library(xgboost)
})

args = commandArgs(trailingOnly = TRUE)
arg_value = function(name, default = NULL) {
  hit = grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}
quick = "--quick" %in% args
mode = arg_value("mode", "run")
workers = as.integer(arg_value("workers", "1"))
root = normalizePath(arg_value("root", "."), mustWork = TRUE)
output_dir = arg_value("output-dir", file.path(root, "analysis/simulation/results/workflow"))
if (quick) output_dir = paste0(output_dir, "_quick")
cache_file = arg_value("cache")
stopifnot(mode %in% c("run", "verify", "timing", "figure", "none"), workers >= 1L)

settings = list(
  seed = 20261024L,
  replications = as.integer(arg_value("replications", if (quick) "12" else "500")),
  n = c(300L, 1000L, 5000L),
  folds = 10L,
  permutations = 10L,
  prevalence = 0.20,
  truth_fits = if (quick) 10L else 100L,
  truth_test_n = if (quick) 5000L else 20000L,
  truth_permutations = 5L,
  nrounds = 100L,
  xgb_params = list(objective = "binary:logistic", max_depth = 3L, eta = 0.1, subsample = 1, colsample_bytree = 1,
    nthread = 1L),
  clip = 1e-15,
  # Defaults of csdg_check() in mlr3autoiml 0.3.0 and the factor of claim_relies_mainly().
  min_importance = 0.01,
  stability = 0.9,
  factor = 2,
  level = 0.95,
  max_r2 = 0.5
)
scenarios = data.table(
  scenario = c("S1", "S2"),
  label = c("clear", "ambiguous"),
  r1 = c(0.9, 0.8),
  r2 = c(0.6, 0.8),
  b_A = c(1.2, 0.9),
  b_B = c(1.0, 0.9)
)
predictors = paste0("X", 1:10)
p = length(predictors)

# ---- Data-generating process ----

# Intercept for the target prevalence. The linear part b_A Z1 + b_B Z2 + 0.3 Z3 is normal with variance
# b_A^2 + b_B^2 + 0.09 and independent of X5, so the prevalence is a two-dimensional normal integral (fine grid).
intercept_for_prevalence = function(b_A, b_B, prevalence = settings$prevalence) {
  grid = seq(-8, 8, by = 0.02)
  weight = dnorm(grid) / sum(dnorm(grid))
  linear = sqrt(b_A^2 + b_B^2 + 0.3^2) * grid
  quadratic = 0.3 * (grid^2 - 1)
  share = function(b0) sum(outer(weight, weight) * plogis(b0 + outer(linear, quadratic, "+")))
  uniroot(function(b0) share(b0) - prevalence, c(-10, 5), tol = 1e-12)$root
}
scenarios[, b0 := mapply(intercept_for_prevalence, b_A, b_B)]
scenario_list = lapply(seq_len(nrow(scenarios)), function(s) as.list(scenarios[s]))

simulate_data = function(n, sc) {
  z = matrix(rnorm(3L * n), n)
  e = matrix(rnorm(9L * n), n)
  x = cbind(
    X1 = sqrt(sc$r1) * z[, 1L] + sqrt(1 - sc$r1) * e[, 1L],
    X2 = sqrt(sc$r2) * z[, 1L] + sqrt(1 - sc$r2) * e[, 2L],
    X3 = sqrt(0.8) * z[, 2L] + sqrt(0.2) * e[, 3L],
    X4 = z[, 3L],
    X5 = e[, 4L],
    X6 = 0,
    X7 = e[, 6L], X8 = e[, 7L], X9 = e[, 8L], X10 = e[, 9L]
  )
  x[, "X6"] = 0.6 * x[, "X3"] + 0.8 * e[, 5L]
  eta = sc$b0 + sc$b_A * z[, 1L] + sc$b_B * z[, 2L] + 0.3 * z[, 3L] + 0.3 * (x[, "X5"]^2 - 1)
  list(x = x, y = as.integer(runif(n) < plogis(eta)))
}

# Stratified K-fold assignment: events and non-events are shuffled separately and dealt to the folds in turn, so
# fold sizes and event counts differ by at most one.
stratified_folds = function(y, k = settings$folds) {
  events = which(y == 1L)
  non_events = which(y == 0L)
  dealt = c(events[sample.int(length(events))], non_events[sample.int(length(non_events))])
  fold = integer(length(y))
  fold[dealt] = rep_len(seq_len(k), length(y))
  fold
}

# ---- Learner, loss, and held-out PFI ----

fit_learner = function(x, y) {
  xgb.train(params = settings$xgb_params, data = xgb.DMatrix(x, label = y, nthread = 1L),
    nrounds = settings$nrounds, verbose = 0)
}
predict_learner = function(model, x) predict(model, xgb.DMatrix(x, nthread = 1L))
# Log loss as in mlr3autoiml: probabilities clipped to [1e-15, 1 - 1e-15].
row_log_loss = function(y, prob) {
  prob = pmin(pmax(prob, settings$clip), 1 - settings$clip)
  -(y * log(prob) + (1 - y) * log(1 - prob))
}
log_loss = function(y, prob) mean(row_log_loss(y, prob))

# Held-out PFI per fold: increase in held-out log loss after permuting one predictor in the test fold, for each of R
# permutations (raw values: K x p x R). The model without predictors predicts the training-fold prevalence.
# `permutation(k, j, r, m)` can supply the permutations (used to reproduce the seeds of mlr3autoiml in --mode=verify).
cross_validated_pfi = function(x, y, fold, permutation = NULL) {
  k_folds = max(fold)
  repetitions = settings$permutations
  raw = array(NA_real_, c(k_folds, p, repetitions), dimnames = list(NULL, colnames(x), NULL))
  loss = loss_baseline = numeric(k_folds)
  n_test = integer(k_folds)
  for (k in seq_len(k_folds)) {
    test = which(fold == k)
    m = length(test)
    model = fit_learner(x[-test, , drop = FALSE], y[-test])
    x_test = x[test, , drop = FALSE]
    y_test = y[test]
    loss[[k]] = log_loss(y_test, predict_learner(model, x_test))
    loss_baseline[[k]] = log_loss(y_test, rep(mean(y[-test]), m))
    copies = x_test[rep.int(seq_len(m), p * repetitions), , drop = FALSE]
    for (j in seq_len(p)) {
      for (r in seq_len(repetitions)) {
        perm = if (is.null(permutation)) sample.int(m) else permutation(k, j, r, m)
        copies[((j - 1L) * repetitions + r - 1L) * m + seq_len(m), j] = x_test[perm, j]
      }
    }
    block_loss = colMeans(matrix(row_log_loss(rep.int(y_test, p * repetitions), predict_learner(model, copies)), m))
    raw[k, , ] = t(matrix(block_loss, repetitions, p)) - loss[[k]]
    n_test[[k]] = m
  }
  list(P = apply(raw, c(1L, 2L), mean), raw = raw, loss = loss, loss_baseline = loss_baseline, n_test = n_test,
    n_train = length(y) - n_test, cor = cor(x))
}

# ---- Default criteria of mlr3autoiml 0.3.0 ----

# Each function mirrors the package function named in its comment (R/csdg_check.R, R/csdg_intervals.R).

# csdg_learner_pfi_interval(): corrected resampled t interval, variance (1/K + n_test/n_train) s^2, t with K - 1 df.
corrected_interval = function(values, ratio) {
  k = length(values)
  se = sqrt((1 / k + ratio) * var(values))
  q = qt(1 - (1 - settings$level) / 2, k - 1L)
  c(estimate = mean(values), lower = mean(values) - q * se, upper = mean(values) + q * se)
}

# .csdg_mc_average(): Monte Carlo threshold of a fold average (raw_b = NULL) or of a margin raw_a - factor * raw_b,
# from the permutation values (K x R matrices): t quantile with R - 1 df times sqrt(sum_k se_k^2) / K.
mc_threshold = function(raw_a, raw_b = NULL, factor = 1) {
  repetitions = ncol(raw_a)
  v = apply(raw_a, 1L, var) / repetitions
  if (!is.null(raw_b)) v = v + apply(factor * raw_b, 1L, var) / repetitions
  qt(1 - (1 - settings$level) / 2, repetitions - 1L) * sqrt(sum(v)) / nrow(raw_a)
}

# .csdg_margin_status() with interval_rule = "open_if_inconclusive".
margin_status = function(estimate, lower, upper, threshold, delta, strict) {
  point = if (strict) estimate > 0 else estimate >= 0
  beyond = abs(estimate) > threshold
  if (!point && beyond && upper <= -delta) return("contradicted")
  if (point && beyond && lower >= delta) return("supported")
  "open"
}

# .csdg_floor_status(): minimum importance.
floor_status = function(estimate, tau, threshold, upper, has_scale) {
  if (!has_scale) return("open")
  if (abs(estimate - tau) <= threshold) return("open")
  if (estimate >= tau) return("supported")
  if (upper < tau) "contradicted" else "open"
}

worst_status = function(statuses) {
  if (any(statuses == "contradicted")) "contradicted" else if (any(statuses == "open")) "open" else "supported"
}

# csdg_adjudicate_claim(): not met if a required property is contradicted, unresolved if one is open, else met.
assessment_of = function(statuses) {
  switch(worst_status(statuses), contradicted = "not_met", open = "unresolved", supported = "met")
}

# csdg_check() for a claim of claim_relies_mainly() (kind "relies_mainly", predictors, factor) or claim_order() (kind
# "order", first, second) on one learner: content (.csdg_content), minimum importance (.csdg_minimum_importance),
# stability (.csdg_stability with .csdg_fold_ranking and .csdg_classify_failing), and the dependence screen
# (.csdg_dependence_screen). G0a is supported (the scope is generated from the analysis), G0b is judged supported (the
# variables are recorded as simulated), and the procedure property is supported (no grouped or conditional PFI).
# `assessment` is for the wording "Under marginal permutation" (marginal_only = TRUE), for which a named predictor
# with an R^2 of at least max_r2 on the other predictors is context; `assessment_default` is for the default wording,
# for which it is an unresolved threat that leaves G2 open unless G2 is contradicted. For numeric predictors without
# missing values, the R^2 of the package's least-squares regression on all other predictors is 1 - 1 / [R^-1]_jj,
# with R the correlation matrix of the predictors.
check_claim = function(cv, claim) {
  P = cv$P
  avg = colMeans(P)
  raw = cv$raw
  k_folds = nrow(P)
  repetitions = dim(raw)[[3L]]
  ratio = mean(cv$n_test) / mean(cv$n_train)
  improvement = mean(cv$loss_baseline - cv$loss)
  tau = settings$min_importance * improvement
  delta = settings$min_importance * max(improvement, 0)
  need = as.integer(ceiling(settings$stability * k_folds - 1e-8))
  stopifnot(need < k_folds) # with every fold required, the package uses a Holm step-down (not needed here)
  q_mc = qt(1 - (1 - settings$level) / 2, repetitions - 1L)
  relies = identical(claim$kind, "relies_mainly")
  if (relies) {
    S = claim$predictors
    f = claim$factor
    others = setdiff(colnames(P), S)
    largest = others[order(-avg[others], seq_along(others))][[1L]]
    # Every comparison of a named predictor with an unnamed one, each with its own interval and Monte Carlo threshold.
    pairs = CJ(j = S, l = others, sorted = FALSE)
    margins = rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
      j = pairs$j[[i]]
      l = pairs$l[[i]]
      ci = corrected_interval(P[, j] - f * P[, l], ratio)
      threshold = mc_threshold(raw[, j, ], raw[, l, ], f)
      data.table(versus = l, estimate = ci[["estimate"]], lower = ci[["lower"]], upper = ci[["upper"]],
        status = margin_status(ci[["estimate"]], ci[["lower"]], ci[["upper"]], threshold, delta, strict = FALSE))
    }))
    # Diagnostic: the content status if only the unnamed predictor with the largest mean PFI were compared (the
    # shortcut that version 0.3.0 corrects).
    content_largest = worst_status(margins[versus == largest]$status)
    floor_named = S
  } else {
    a = claim$first
    b = claim$second
    ci = corrected_interval(P[, a] - P[, b], ratio)
    threshold = mc_threshold(raw[, a, ], raw[, b, ], 1)
    margins = data.table(versus = b, estimate = ci[["estimate"]], lower = ci[["lower"]], upper = ci[["upper"]],
      status = margin_status(ci[["estimate"]], ci[["lower"]], ci[["upper"]], threshold, delta, strict = TRUE))
    content_largest = worst_status(margins$status)
    floor_named = a
  }
  content = worst_status(margins$status)
  has_scale = improvement > 0
  floor_statuses = vapply(floor_named, function(j) {
    ci = corrected_interval(P[, j], ratio)
    floor_status(avg[[j]], tau, mc_threshold(raw[, j, ]), ci[["upper"]], has_scale)
  }, character(1L))
  minimum = if (has_scale) worst_status(floor_statuses) else "open"
  fold_rows = lapply(seq_len(k_folds), function(k) {
    Pk = P[k, ]
    if (relies) {
      top = names(Pk)[order(-Pk, seq_along(Pk))][seq_along(S)]
      j = S[which.min(Pk[S])]
      l = others[which.max(Pk[others])]
      holds = setequal(top, S) && min(Pk[S]) > max(Pk[others]) && min(Pk[S]) >= f * max(Pk[others])
      f_eff = if (Pk[[l]] >= 0) f else 1
      d = Pk[[j]] - f_eff * Pk[[l]]
      se = sqrt(var(raw[k, j, ]) / repetitions + var(f_eff * raw[k, l, ]) / repetitions)
    } else {
      holds = Pk[[a]] > Pk[[b]]
      d = Pk[[a]] - Pk[[b]]
      se = sqrt(var(raw[k, a, ]) / repetitions + var(raw[k, b, ]) / repetitions)
    }
    c(holds = holds, d = d, se = se)
  })
  folds = as.data.table(do.call(rbind, fold_rows))
  folds[, holds := as.logical(holds)]
  # A failing fold fails only beyond Monte Carlo error and by at least the smallest relevant difference.
  folds[, class := fifelse(holds, "holds", fifelse(abs(d) > q_mc * se & abs(d) >= delta, "fails", "undecided"))]
  n_holds = sum(folds$class == "holds")
  n_undecided = sum(folds$class == "undecided")
  stability = if (n_holds >= need) "supported" else if (n_holds + n_undecided < need) "contradicted" else "open"
  named = if (relies) S else c(a, b)
  flagged = if (is.null(cv$cor)) NA else {
    r2_on_others = 1 - 1 / diag(solve(cv$cor))
    any(r2_on_others[named] >= settings$max_r2 - 1e-12)
  }
  g2 = worst_status(c(content, minimum, "supported"))
  g2_default = if (isTRUE(flagged) && g2 != "contradicted") "open" else g2
  list(content = content, minimum = minimum, stability = stability, content_largest = content_largest,
    assessment = assessment_of(c(g2, stability)), assessment_default = assessment_of(c(g2_default, stability)),
    assessment_largest = assessment_of(c(worst_status(c(content_largest, minimum)), stability)),
    flagged = flagged, folds_holding = n_holds, folds_undecided = n_undecided,
    binding_estimate = min(margins$estimate), binding_lower = min(margins$lower),
    binding_versus = margins$versus[[which.min(margins$lower)]], improvement = improvement)
}

# ---- True learner-level values ----

truth_seed = function(s, i_n, m) settings$seed + 5000000L + 1000000L * s + 100000L * i_n + m

# One fit on an independent training sample of the fold training size, evaluated on an independent test sample.
truth_fit = function(s, i_n, m) {
  set.seed(truth_seed(s, i_n, m))
  sc = scenario_list[[s]]
  train = simulate_data(round(0.9 * settings$n[[i_n]]), sc)
  model = fit_learner(train$x, train$y)
  test = simulate_data(settings$truth_test_n, sc)
  base = log_loss(test$y, predict_learner(model, test$x))
  featureless = log_loss(test$y, rep(mean(train$y), length(test$y)))
  pfi = vapply(seq_len(p), function(j) {
    mean(vapply(seq_len(settings$truth_permutations), function(r) {
      x_perm = test$x
      x_perm[, j] = x_perm[sample.int(nrow(x_perm)), j]
      log_loss(test$y, predict_learner(model, x_perm)) - base
    }, numeric(1L)))
  }, numeric(1L))
  c(setNames(pfi, predictors), improvement = featureless - base, loss = base, loss_featureless = featureless,
    prevalence_test = mean(test$y))
}

# Expected PFI and improvement over the training samples. X1 and X2 are exchangeable when r1 = r2 (S2), so their
# true PFI is equal; it is estimated by their average (column true_pfi), the raw estimates are kept.
summarize_truth = function(fits) {
  rbindlist(lapply(seq_len(nrow(fits)), function(i) {
    key = fits[i]
    values = key$values[[1L]]
    sc = scenario_list[[key$s]]
    pfi_raw = colMeans(values[, predictors, drop = FALSE])
    se_raw = apply(values[, predictors, drop = FALSE], 2L, sd) / sqrt(nrow(values))
    pfi = pfi_raw
    se = se_raw
    symmetric = isTRUE(all.equal(sc$r1, sc$r2))
    if (symmetric) {
      pair = rowMeans(values[, c("X1", "X2")])
      pfi[c("X1", "X2")] = mean(pair)
      se[c("X1", "X2")] = sd(pair) / sqrt(length(pair))
    }
    data.table(scenario = sc$scenario, n = settings$n[[key$i_n]], n_train = round(0.9 * settings$n[[key$i_n]]),
      predictor = predictors, true_pfi = pfi, mc_se = se, raw_estimate = pfi_raw, raw_mc_se = se_raw,
      symmetrized = symmetric & predictors %in% c("X1", "X2"),
      improvement = mean(values[, "improvement"]), improvement_mc_se = sd(values[, "improvement"]) / sqrt(nrow(values)),
      loss = mean(values[, "loss"]), loss_featureless = mean(values[, "loss_featureless"]),
      prevalence = mean(values[, "prevalence_test"]), fits = nrow(values))
  }))
}

# Truth of a claim with the true values of its scenario and n. R: each named predictor has at least twice the true
# PFI of every other predictor and at least 1% of the true improvement. O: true if PFI(top1) - PFI(top2) is at least
# the smallest relevant difference (1% of the true improvement), false if it is at most 0, boundary otherwise.
claim_truth = function(truth_values, claim, top1, top2) {
  pfi = setNames(truth_values$true_pfi, truth_values$predictor)
  delta = settings$min_importance * max(truth_values$improvement[[1L]], 0)
  if (claim == "R") {
    named = c(top1, top2)
    others = setdiff(predictors, named)
    margin = min(pfi[named]) / max(pfi[others])
    holds = min(pfi[named]) >= settings$factor * max(pfi[others]) && min(pfi[named]) >= delta
    list(truth = if (holds) "true" else "false", true_margin = margin)
  } else {
    # With an improvement of at most 0, the smallest relevant difference is 0 (as in csdg_check()).
    difference = pfi[[top1]] - pfi[[top2]]
    list(truth = if (difference <= 0) "false" else if (difference >= delta) "true" else "boundary",
      true_margin = if (delta > 0) difference / delta else NA_real_)
  }
}

# ---- One replication ----

replication_seed = function(s, i_n, replication) settings$seed + 1000000L * s + 100000L * i_n + replication

# Both samples and their folds are drawn first, so that --mode=verify can reproduce them with other permutations.
draw_samples = function(s, i_n, replication) {
  set.seed(replication_seed(s, i_n, replication))
  sc = scenario_list[[s]]
  n = settings$n[[i_n]]
  explore = simulate_data(n, sc)
  explore$fold = stratified_folds(explore$y)
  confirm = simulate_data(n, sc)
  confirm$fold = stratified_folds(confirm$y)
  list(explore = explore, confirm = confirm)
}

state_claims = function(cv) {
  avg = colMeans(cv$P)
  ranking = names(avg)[order(-avg, seq_along(avg))]
  list(
    ranking = ranking, avg = avg,
    R = list(kind = "relies_mainly", predictors = ranking[1:2], factor = settings$factor),
    O = list(kind = "order", first = ranking[[1L]], second = ranking[[2L]])
  )
}

# Standard practice in new data: the point estimates satisfy the same claim (R: each named predictor has at least
# twice the fold-averaged PFI of every other predictor; O: PFI(top1) > PFI(top2)).
point_claim_holds = function(avg, claim) {
  if (identical(claim$kind, "relies_mainly")) {
    others = setdiff(names(avg), claim$predictors)
    min(avg[claim$predictors]) >= claim$factor * max(avg[others])
  } else {
    avg[[claim$first]] > avg[[claim$second]]
  }
}

run_replication = function(s, i_n, replication, samples = draw_samples(s, i_n, replication)) {
  cv_explore = cross_validated_pfi(samples$explore$x, samples$explore$y, samples$explore$fold)
  cv_confirm = cross_validated_pfi(samples$confirm$x, samples$confirm$y, samples$confirm$fold)
  stated = state_claims(cv_explore)
  avg = stated$avg
  avg_new = colMeans(cv_confirm$P)
  improvement = mean(cv_explore$loss_baseline - cv_explore$loss)
  third = stated$ranking[[3L]]
  rbindlist(lapply(c("R", "O"), function(claim_type) {
    claim = stated[[claim_type]]
    explore = check_claim(cv_explore, claim)
    confirm = check_claim(cv_confirm, claim)
    margin = if (claim_type == "R") {
      if (avg[[third]] > 0) avg[[stated$ranking[[2L]]]] / avg[[third]] else Inf
    } else {
      if (improvement > 0) (avg[[stated$ranking[[1L]]]] - avg[[stated$ranking[[2L]]]]) /
        (settings$min_importance * improvement) else NA_real_
    }
    data.table(
      scenario = scenario_list[[s]]$scenario, n = settings$n[[i_n]], replication = replication, claim = claim_type,
      top1 = stated$ranking[[1L]], top2 = stated$ranking[[2L]], third = third,
      pfi_top1 = avg[[stated$ranking[[1L]]]], pfi_top2 = avg[[stated$ranking[[2L]]]], pfi_third = avg[[third]],
      improvement = improvement, margin = margin,
      standard_reported = if (claim_type == "R") avg[[stated$ranking[[2L]]]] >= settings$factor * avg[[third]] else
        TRUE,
      standard_holds_new = point_claim_holds(avg_new, claim),
      explore_content = explore$content, explore_minimum = explore$minimum, explore_stability = explore$stability,
      explore_assessment = explore$assessment, explore_assessment_default = explore$assessment_default,
      explore_flagged = explore$flagged, explore_folds_holding = explore$folds_holding,
      explore_binding_lower = explore$binding_lower, explore_binding_versus = explore$binding_versus,
      explore_assessment_largest = explore$assessment_largest,
      confirm_content = confirm$content, confirm_minimum = confirm$minimum, confirm_stability = confirm$stability,
      confirm_assessment = confirm$assessment, confirm_assessment_default = confirm$assessment_default,
      confirm_assessment_largest = confirm$assessment_largest,
      confirm_flagged = confirm$flagged, confirm_folds_holding = confirm$folds_holding,
      confirm_improvement = confirm$improvement
    )
  }))
}

# ---- Aggregates ----

# Proportion with its Monte Carlo standard error sqrt(p (1 - p) / m) and the Wilson 95% interval (used in the
# figure; it stays informative for proportions near 0 or 1 and small denominators).
proportion = function(x) {
  x = x[!is.na(x)]
  m = length(x)
  k = sum(x)
  if (!m) {
    return(data.table(numerator = 0L, denominator = 0L, estimate = NA_real_, mc_se = NA_real_, lower = NA_real_,
      upper = NA_real_))
  }
  est = k / m
  z = qnorm(0.975)
  center = (est + z^2 / (2 * m)) / (1 + z^2 / m)
  half = z * sqrt(est * (1 - est) / m + z^2 / (4 * m^2)) / (1 + z^2 / m)
  data.table(numerator = as.integer(k), denominator = m, estimate = est, mc_se = sqrt(est * (1 - est) / m),
    lower = max(0, center - half), upper = min(1, center + half))
}

# Every proportion reported for one cell (scenario or pooled, n, claim). Conditional shares are computed among the
# replications that meet the condition; their denominators are reported.
measures = function(d) {
  met = d$explore_assessment == "met"
  both = met & d$confirm_assessment == "met"
  met_default = d$explore_assessment_default == "met"
  is_true = d$truth == "true"
  is_false = d$truth == "false"
  # Standard practice in both samples: reported in the exploratory sample and satisfied by the point estimates of the
  # confirmation sample (a comparator for CSDG's test in new data).
  standard_both = d$standard_reported & d$standard_holds_new
  # Components of the default rule, each alone in the same workflow (exploratory; in both samples): the content check
  # (corrected interval with the smallest relevant difference and the Monte Carlo rule), content and minimum
  # importance (G2), and selection stability (G5). All defaults together give "met".
  components = list(
    content_only = function(stage) d[[paste0(stage, "_content")]] == "supported",
    g2_only = function(stage) d[[paste0(stage, "_content")]] == "supported" & d[[paste0(stage, "_minimum")]] ==
      "supported",
    stability_only = function(stage) d[[paste0(stage, "_stability")]] == "supported"
  )
  component_rows = unlist(lapply(names(components), function(name) {
    explore = components[[name]]("explore")
    both_stages = explore & components[[name]]("confirm")
    setNames(list(proportion(explore[is_false]), proportion(explore[is_true]), proportion(both_stages[is_false]),
      proportion(both_stages[is_true])), paste0(name, c("_among_false", "_power", "_both_among_false",
      "_both_power")))
  }), recursive = FALSE)
  rows = list(
    claim_true = proportion(is_true),
    claim_false = proportion(is_false),
    claim_boundary = proportion(d$truth == "boundary"),
    standard_reported = proportion(d$standard_reported),
    standard_false_among_reported = proportion(is_false[d$standard_reported]),
    standard_boundary_among_reported = proportion(d$truth[d$standard_reported] == "boundary"),
    standard_power = proportion(d$standard_reported[is_true]),
    standard_reported_and_false = proportion(d$standard_reported & is_false),
    standard_holds_new_among_reported = proportion(d$standard_holds_new[d$standard_reported]),
    standard_reported_among_false = proportion(d$standard_reported[is_false]),
    standard_both_reported = proportion(standard_both),
    standard_both_false_among_both = proportion(is_false[standard_both]),
    standard_both_power = proportion(standard_both[is_true]),
    standard_both_among_false = proportion(standard_both[is_false]),
    csdg_met = proportion(met),
    csdg_not_met = proportion(d$explore_assessment == "not_met"),
    csdg_unresolved = proportion(d$explore_assessment == "unresolved"),
    csdg_false_among_met = proportion(is_false[met]),
    csdg_boundary_among_met = proportion(d$truth[met] == "boundary"),
    csdg_power = proportion(met[is_true]),
    csdg_met_and_false = proportion(met & is_false),
    csdg_met_among_false = proportion(met[is_false]),
    confirm_met_among_met = proportion(d$confirm_assessment[met] == "met"),
    confirm_not_met_among_met = proportion(d$confirm_assessment[met] == "not_met"),
    confirm_unresolved_among_met = proportion(d$confirm_assessment[met] == "unresolved"),
    both_met = proportion(both),
    both_false_among_both_met = proportion(is_false[both]),
    both_boundary_among_both_met = proportion(d$truth[both] == "boundary"),
    both_power = proportion(both[is_true]),
    both_met_and_false = proportion(both & is_false),
    both_met_among_false = proportion(both[is_false]),
    confirm_met_among_true_met = proportion(d$confirm_assessment[met & is_true] == "met"),
    confirm_met_among_false_met = proportion(d$confirm_assessment[met & is_false] == "met"),
    improvement_not_positive = proportion(d$improvement <= 0),
    default_wording_dependence_flagged = proportion(d$explore_flagged),
    default_wording_csdg_met = proportion(met_default),
    default_wording_both_met = proportion(met_default & d$confirm_assessment_default == "met"),
    # Diagnostic: assessments that differ when only the unnamed predictor with the largest mean PFI is compared.
    explore_changed_by_all_comparisons = proportion(d$explore_assessment != d$explore_assessment_largest),
    confirm_changed_by_all_comparisons = proportion(d$confirm_assessment != d$confirm_assessment_largest)
  )
  rbindlist(c(rows, component_rows), idcol = "measure")
}

# Bins of the exploratory margin. R: PFI(top2) / PFI(third) (at least 1 by construction; Inf if PFI(third) <= 0).
# O: (PFI(top1) - PFI(top2)) / smallest relevant difference, undefined ("no scale") if the improvement is at most 0.
margin_bins = list(
  R = c(1, 1.5, 2, 3, 4, 5, Inf),
  O = c(0, 3, 10, 30, 100, 300, Inf)
)
bin_label = function(breaks) sprintf("[%s, %s)", as.character(head(breaks, -1L)), as.character(tail(breaks, -1L)))
no_scale_label = "no scale (improvement <= 0)"

# ---- Main run ----

run_simulation = function() {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  started = Sys.time()
  load_avg = tryCatch(system("sysctl -n vm.loadavg", intern = TRUE), error = function(e) NA_character_)

  # True values: settings$truth_fits fits per scenario and n.
  truth_jobs = CJ(s = seq_len(nrow(scenarios)), i_n = seq_along(settings$n), m = seq_len(settings$truth_fits))
  truth_values = mclapply(seq_len(nrow(truth_jobs)), function(i) {
    truth_fit(truth_jobs$s[[i]], truth_jobs$i_n[[i]], truth_jobs$m[[i]])
  }, mc.cores = workers, mc.preschedule = TRUE)
  stopifnot(!any(vapply(truth_values, inherits, logical(1L), what = "try-error")))
  truth_jobs[, values := truth_values]
  truth_fits = truth_jobs[, .(values = list(do.call(rbind, values))), by = .(s, i_n)]
  truth = summarize_truth(truth_fits)
  truth_minutes = as.numeric(difftime(Sys.time(), started, units = "mins"))
  message(sprintf("True values from %d fits in %.1f minutes.", nrow(truth_jobs), truth_minutes))

  truth_summary = truth[, {
    ordered = order(-true_pfi)
    pfi = true_pfi[ordered]
    names(pfi) = predictor[ordered]
    se = mc_se[ordered]
    .(top1 = names(pfi)[[1L]], top2 = names(pfi)[[2L]], third = names(pfi)[[3L]], pfi_top1 = pfi[[1L]],
      pfi_top2 = pfi[[2L]], pfi_third = pfi[[3L]], mc_se_top1 = se[[1L]], mc_se_top2 = se[[2L]],
      mc_se_third = se[[3L]], ratio_top2_third = pfi[[2L]] / pfi[[3L]], ratio_top1_top2 = pfi[[1L]] / pfi[[2L]],
      improvement = improvement[[1L]], improvement_mc_se = improvement_mc_se[[1L]],
      min_difference = settings$min_importance * max(improvement[[1L]], 0), loss = loss[[1L]],
      loss_featureless = loss_featureless[[1L]], prevalence = prevalence[[1L]])
  }, by = .(scenario, n, n_train)]
  truth_summary = merge(scenarios[, .(scenario, label, r1, r2, b_A, b_B, b0)], truth_summary, by = "scenario")

  # Replications, in batches so that an interrupted run can resume from the cache.
  jobs = CJ(s = seq_len(nrow(scenarios)), i_n = seq_along(settings$n), replication = seq_len(settings$replications))
  jobs[, key := sprintf("%d_%d_%d", s, i_n, replication)]
  done = list()
  cached_minutes = 0
  if (!is.null(cache_file) && file.exists(cache_file)) {
    cached = readRDS(cache_file)
    # data.table objects carry a session-specific pointer, so the scenarios are compared as data frames.
    if (!identical(cached$settings, settings) ||
      !identical(as.data.frame(cached$scenarios), as.data.frame(scenarios))) {
      stop("The cache was written with other settings; remove it or use another --cache file.")
    }
    done = cached$done
    cached_minutes = cached$replication_minutes %||% NA_real_
    message(sprintf("Resuming: %d of %d replications in the cache.", length(done), nrow(jobs)))
  }
  from_cache = length(done)
  todo = jobs[!key %in% names(done)]
  batch_size = 10L * workers
  replication_started = Sys.time()
  # Wall-clock minutes spent on replications, accumulated over interrupted and resumed runs.
  minutes_so_far = function() cached_minutes + as.numeric(difftime(Sys.time(), replication_started, units = "mins"))
  for (start in seq.int(1L, max(1L, nrow(todo)), by = batch_size)) {
    if (!nrow(todo)) break
    batch = todo[start:min(nrow(todo), start + batch_size - 1L)]
    out = mclapply(seq_len(nrow(batch)), function(i) {
      tryCatch(run_replication(batch$s[[i]], batch$i_n[[i]], batch$replication[[i]]),
        error = function(e) structure(conditionMessage(e), class = "replication_error"))
    }, mc.cores = workers, mc.preschedule = TRUE)
    failed = vapply(out, function(o) inherits(o, c("replication_error", "try-error")) || is.null(o), logical(1L))
    if (any(failed)) stop("Replication failed: ", paste(unique(unlist(out[failed])), collapse = "; "))
    done[batch$key] = out
    if (!is.null(cache_file)) {
      saveRDS(list(settings = settings, scenarios = scenarios, done = done, replication_minutes = minutes_so_far(),
        workers = workers), cache_file)
    }
    message(sprintf("%s: %d of %d replications (%.1f minutes).", format(Sys.time(), "%H:%M:%S"), length(done),
      nrow(jobs), minutes_so_far()))
  }
  replication_minutes = if (nrow(todo)) minutes_so_far() else cached_minutes
  results = rbindlist(done[jobs$key])
  stopifnot(nrow(results) == 2L * nrow(jobs))

  # Truth of each stated claim.
  results[, c("truth", "true_margin") := {
    tv = truth[scenario == .BY$scenario & n == .BY$n]
    out = lapply(seq_len(.N), function(i) claim_truth(tv, claim[[i]], top1[[i]], top2[[i]]))
    list(vapply(out, `[[`, character(1L), "truth"), vapply(out, `[[`, numeric(1L), "true_margin"))
  }, by = .(scenario, n)]

  # Claim R: every comparison against the comparison with the largest other mean only. Per condition, the analyses
  # whose decisive comparison (smallest lower limit) is not the one with the largest other mean, the assessments that
  # differ between the two rules, and the replications concerned.
  comparisons = results[claim == "R", .(analyses = .N,
    decisive_not_largest_other = sum(explore_binding_versus != third),
    explore_changed = sum(explore_assessment != explore_assessment_largest),
    confirm_changed = sum(confirm_assessment != confirm_assessment_largest),
    changed_replications = paste(sprintf("%d (exploratory %s, largest other only %s; new data %s, largest other only %s)",
      replication, explore_assessment, explore_assessment_largest, confirm_assessment,
      confirm_assessment_largest)[explore_assessment != explore_assessment_largest |
      confirm_assessment != confirm_assessment_largest], collapse = "; ")), by = .(scenario, n)]

  # Operating characteristics by scenario, n, and claim, and pooled over the scenarios.
  characteristics = rbind(
    results[, measures(.SD), by = .(scenario, n, claim)],
    results[, measures(.SD), by = .(n, claim)][, scenario := "pooled"]
  )
  characteristics[, claim := factor(claim, levels = c("R", "O"))]
  setcolorder(characteristics, c("scenario", "n", "claim", "measure"))
  setorder(characteristics, claim, scenario, n)
  characteristics[, claim := as.character(claim)]

  # Components of the default rule in the same workflow: shares of false and of true claims declared, in the
  # exploratory sample and in both samples, for point estimates, each component alone, and all defaults.
  component_measures = list(
    point_estimates = c("standard_reported_among_false", "standard_power", "standard_both_among_false",
      "standard_both_power"),
    content = c("content_only_among_false", "content_only_power", "content_only_both_among_false",
      "content_only_both_power"),
    content_and_minimum = c("g2_only_among_false", "g2_only_power", "g2_only_both_among_false",
      "g2_only_both_power"),
    stability = c("stability_only_among_false", "stability_only_power", "stability_only_both_among_false",
      "stability_only_both_power"),
    all_defaults = c("csdg_met_among_false", "csdg_power", "both_met_among_false", "both_power")
  )
  quantities = c("false_declared", "true_declared", "false_declared_both", "true_declared_both")
  components = rbindlist(lapply(names(component_measures), function(rule) {
    m = component_measures[[rule]]
    characteristics[measure %in% m, .(claim, scenario, n, rule = rule, quantity = quantities[match(measure, m)],
      numerator, denominator, estimate)]
  }))
  components = dcast(components, claim + scenario + n + rule ~ factor(quantity, levels = quantities),
    value.var = c("estimate", "numerator", "denominator"))
  components[, `:=`(claim = factor(claim, levels = c("R", "O")), rule = factor(rule,
    levels = names(component_measures)))]
  setorder(components, claim, scenario, n, rule)
  components[, `:=`(claim = as.character(claim), rule = as.character(rule))]

  # Key numbers in wide form (estimate and Monte Carlo standard error).
  key_measures = c("claim_true", "claim_boundary", "standard_reported", "standard_false_among_reported",
    "csdg_met", "csdg_not_met", "csdg_unresolved", "csdg_false_among_met", "csdg_power", "confirm_met_among_met",
    "both_met", "both_false_among_both_met", "both_power", "standard_reported_and_false", "csdg_met_and_false",
    "both_met_and_false", "csdg_met_among_false", "standard_reported_among_false", "standard_power",
    "standard_both_false_among_both", "standard_both_power", "standard_both_among_false", "both_met_among_false")
  summary_table = dcast(characteristics[measure %in% key_measures],
    claim + scenario + n ~ factor(measure, levels = key_measures), value.var = c("estimate", "mc_se", "denominator"))
  summary_table[, claim := factor(claim, levels = c("R", "O"))]
  setorder(summary_table, claim, scenario, n)

  # Status of each property in each stage.
  status_long = melt(results, id.vars = c("scenario", "n", "claim"),
    measure.vars = c("explore_content", "explore_minimum", "explore_stability", "explore_assessment",
      "confirm_content", "confirm_minimum", "confirm_stability", "confirm_assessment"),
    variable.name = "property", value.name = "status")
  status_long[, `:=`(stage = sub("_.*$", "", property), property = sub("^[a-z]+_", "", property))]
  property_status = status_long[, .N, by = .(scenario, n, claim, stage, property, status)]
  property_status[, share := N / sum(N), by = .(scenario, n, claim, stage, property)]
  property_status = dcast(property_status, scenario + n + claim + stage + property ~ status, value.var = "share",
    fill = 0)
  property_columns = c("supported", "open", "contradicted")
  assessment_columns = c("met", "unresolved", "not_met")
  for (column in setdiff(c(property_columns, assessment_columns), names(property_status))) {
    property_status[, (column) := 0]
  }
  # Property statuses for content, minimum importance, and stability; assessments for the claim (NA otherwise).
  property_status[property == "assessment", (property_columns) := NA_real_]
  property_status[property != "assessment", (assessment_columns) := NA_real_]
  setcolorder(property_status, c("scenario", "n", "claim", "stage", "property", property_columns,
    assessment_columns))
  property_status[, `:=`(claim = factor(claim, levels = c("R", "O")), stage = factor(stage,
    levels = c("explore", "confirm")), property = factor(property, levels = c("content", "minimum", "stability",
    "assessment")))]
  setorder(property_status, claim, scenario, n, stage, property)
  property_status[, `:=`(claim = as.character(claim), stage = as.character(stage),
    property = as.character(property))]

  # Met in new data as a function of the exploratory margin: R, PFI(top2) / PFI(third); O, the difference
  # PFI(top1) - PFI(top2) divided by the smallest relevant difference (1% of the improvement).
  results[, margin_bin := NA_character_]
  for (claim_type in c("R", "O")) {
    breaks = margin_bins[[claim_type]]
    rows = results$claim == claim_type & !is.na(results$margin)
    results[rows, margin_bin := as.character(cut(pmin(margin, 1e12), breaks, labels = bin_label(breaks),
      right = FALSE))]
  }
  results[claim == "O" & is.na(margin), margin_bin := no_scale_label]
  stopifnot(!anyNA(results$margin_bin))
  margin_cells = function(d) {
    met = d$explore_assessment == "met"
    met_new = d$confirm_assessment == "met"
    all_claims = proportion(met_new)
    among_met = proportion(met_new[met])
    data.table(claims = nrow(d), claim_true = mean(d$truth == "true"), standard_reported = mean(d$standard_reported),
      csdg_met = mean(met), met_new = all_claims$estimate, met_new_mc_se = all_claims$mc_se, met_count = sum(met),
      met_new_among_met = among_met$estimate, met_new_among_met_mc_se = among_met$mc_se)
  }
  by_margin = rbind(
    results[, margin_cells(.SD), by = .(scenario, n, claim, margin_bin)],
    results[, margin_cells(.SD), by = .(n, claim, margin_bin)][, scenario := "pooled"],
    use.names = TRUE
  )
  by_margin[, bin_order := match(margin_bin, c(bin_label(margin_bins$R), no_scale_label, bin_label(margin_bins$O)))]
  by_margin[, claim := factor(claim, levels = c("R", "O"))]
  setorder(by_margin, claim, scenario, n, bin_order)
  by_margin[, `:=`(bin_order = NULL, claim = as.character(claim))]

  truth_pairs = results[, .N, by = .(scenario, n, claim, top1, top2, truth, true_margin)]
  truth_pairs[, share := N / sum(N), by = .(scenario, n, claim)]
  setorder(truth_pairs, claim, scenario, n, -N)

  session_minutes = as.numeric(difftime(Sys.time(), started, units = "mins"))
  runtime = data.table(workers = workers, replications_per_cell = settings$replications,
    replications_total = nrow(jobs), replications_from_cache = from_cache, truth_fits_total = nrow(truth_jobs),
    truth_minutes = truth_minutes, replication_minutes = replication_minutes,
    total_minutes = truth_minutes + replication_minutes, this_session_minutes = session_minutes,
    load_average_at_start = load_avg, started = format(started, "%Y-%m-%d %H:%M:%S %Z"))

  fwrite(characteristics, file.path(output_dir, "workflow_operating_characteristics.csv"))
  fwrite(summary_table, file.path(output_dir, "workflow_summary.csv"))
  fwrite(components, file.path(output_dir, "workflow_components.csv"))
  fwrite(comparisons, file.path(output_dir, "workflow_comparisons.csv"))
  fwrite(property_status, file.path(output_dir, "workflow_property_status.csv"))
  fwrite(by_margin, file.path(output_dir, "workflow_replication_by_margin.csv"))
  fwrite(truth, file.path(output_dir, "workflow_truth.csv"))
  fwrite(truth_summary, file.path(output_dir, "workflow_truth_summary.csv"))
  fwrite(truth_pairs, file.path(output_dir, "workflow_claim_pairs.csv"))
  fwrite(runtime, file.path(output_dir, "workflow_runtime.csv"))
  make_figure(characteristics)
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
  message(sprintf("Finished in %.1f minutes; results written to %s", session_minutes, output_dir))
  invisible(results)
}

# ---- Figure ----

# Rates conditional on the truth of the stated claim: (a) the share of false claims that each procedure declares
# (reported or met), (b) the share of true claims that it declares (power). Rates based on fewer than `min_claims`
# claims are not drawn; workflow_figure_data.csv lists every cell with its counts and whether it is drawn.
make_figure = function(characteristics, min_claims = 10L) {
  procedures = c(standard_reported_among_false = "Point estimates", standard_both_among_false = "Point estimates, both samples",
    csdg_met_among_false = "CSDG, exploratory", both_met_among_false = "CSDG, both samples",
    standard_power = "Point estimates", standard_both_power = "Point estimates, both samples",
    csdg_power = "CSDG, exploratory", both_power = "CSDG, both samples")
  levels_procedure = unique(unname(procedures))
  scenario_labels = c(S1 = "S1 (clear)", S2 = "S2 (ambiguous)")
  panel_levels = sprintf("%s\n%s", rep(c("Relies-mainly claim", "Order claim"), each = 2L), scenario_labels)
  rows = characteristics[scenario != "pooled" & measure %in% names(procedures)]
  rows[, `:=`(
    panel_letter = fifelse(measure %in% c("standard_power", "standard_both_power", "csdg_power", "both_power"), "b", "a"),
    procedure = factor(procedures[measure], levels = levels_procedure),
    facet = factor(sprintf("%s\n%s", fifelse(claim == "R", "Relies-mainly claim", "Order claim"), scenario_labels[scenario]),
      levels = panel_levels),
    n_label = factor(format(n, big.mark = ","), levels = format(settings$n, big.mark = ",")),
    drawn = denominator >= min_claims
  )]
  figure_data = rows[, .(panel = panel_letter, claim, scenario, n, measure, procedure = as.character(procedure),
    numerator, denominator, estimate, lower, upper, drawn)]
  # Facets without any drawn rate carry a short explanation.
  notes = rows[, .(drawn = any(drawn), any_claims = any(denominator > 0)), by = .(panel_letter, facet)][drawn == FALSE]
  notes[, label := fifelse(any_claims, sprintf("Fewer than %d\nclaims per n", min_claims),
    fifelse(panel_letter == "a", "No false claims", "No true claims"))]
  notes[, `:=`(n_label = factor(format(settings$n[[2L]], big.mark = ","), levels = format(settings$n, big.mark = ",")),
    estimate = 0.5)]
  dodge = position_dodge(width = 0.65)
  theme_figure = theme_minimal(base_size = 11, base_family = "Arial") +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      axis.text = element_text(color = "black", size = 9), strip.text = element_text(color = "black", size = 9.5),
      legend.position = "bottom", legend.text = element_text(size = 9.5), panel.spacing = unit(0.8, "lines"),
      plot.title = element_text(size = 11, face = "bold"), plot.margin = margin(4, 6, 4, 4))
  draw_panel = function(letter, title, ylab) {
    d = rows[panel_letter == letter & drawn == TRUE]
    ggplot(d, aes(x = n_label, y = estimate, shape = procedure, group = procedure)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0, linewidth = 0.4) +
      geom_point(position = dodge, size = 2, fill = "white", stroke = 0.6) +
      geom_text(data = notes[panel_letter == letter], aes(x = n_label, y = estimate, label = label),
        inherit.aes = FALSE, family = "Arial", size = 2.9, lineheight = 0.95, color = "gray25") +
      scale_shape_manual(values = c(21, 24, 17, 15), drop = FALSE) +
      guides(shape = guide_legend(nrow = 2L, byrow = TRUE)) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c("0", ".25", ".50", ".75", "1")) +
      facet_wrap(~facet, nrow = 1L, drop = FALSE) +
      labs(title = title, x = expression(italic(n)), y = ylab, shape = NULL) +
      theme_figure
  }
  panel_a = draw_panel("a", "(a) False claims declared", "Share of false claims")
  panel_b = draw_panel("b", "(b) True claims declared", "Share of true claims")
  figure = patchwork::wrap_plots(panel_a, panel_b, ncol = 1L, heights = c(1, 1), guides = "collect") &
    theme(legend.position = "bottom")
  ggsave(file.path(output_dir, "workflow_simulation.pdf"), figure, width = 6.5, height = 6.2, device = cairo_pdf,
    bg = "white")
  ggsave(file.path(output_dir, "workflow_simulation.png"), figure, width = 6.5, height = 6.2, dpi = 300, bg = "white")
  fwrite(figure_data, file.path(output_dir, "workflow_figure_data.csv"))
  invisible(figure)
}

# ---- Verification against mlr3autoiml 0.3.0 ----

# (1) End to end: for one replication per scenario and n (the first), the same samples and folds are analyzed with
# csdg_fit(), csdg_importance(), claim_relies_mainly(), claim_order(), csdg_check(), csdg_assess(), and
# csdg_confirm(); the functions of this script are applied to the package's fold PFI, permutation values, and fold
# losses, and every status is compared. The PFI of this script with the package's permutation seeds is compared with
# the package's PFI. (2) Rules: random fold PFI and permutation values that reach every branch are passed to the
# package's internal functions (.csdg_content(), .csdg_minimum_importance(), .csdg_stability()) and to this script.
verify_against_package = function() {
  suppressPackageStartupMessages({
    library(mlr3)
    library(mlr3learners)
    library(mlr3autoiml)
  })
  stopifnot(as.character(packageVersion("mlr3autoiml")) == "0.3.0")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  pkg = asNamespace("mlr3autoiml")
  learner = lrn("classif.xgboost", predict_type = "prob", nrounds = settings$nrounds, max_depth = 3L, eta = 0.1,
    subsample = 1, colsample_bytree = 1, nthread = 1L)
  package_importance = function(sample, id, seed) {
    dat = as.data.frame(sample$x)
    dat$y = factor(ifelse(sample$y == 1L, "event", "none"), levels = c("event", "none"))
    task = as_task_classif(dat, target = "y", positive = "event", id = id)
    stopifnot(setequal(task$feature_names, predictors)) # mlr3 orders the features alphabetically (X1, X10, X2, ...)
    k_folds = max(sample$fold)
    resampling = rsmp("custom")
    resampling$instantiate(task, train_sets = lapply(seq_len(k_folds), function(k) which(sample$fold != k)),
      test_sets = lapply(seq_len(k_folds), function(k) which(sample$fold == k)))
    fits = csdg_fit(task, list(xgboost = learner), resampling = resampling, seed = 1L)
    csdg_importance(fits, repetitions = settings$permutations, seed = seed)
  }
  # The script's input from a CSDGImportance result: fold PFI, permutation values, fold losses, and fold sizes.
  cv_from_package = function(imp) {
    P = pkg$.csdg_fold_matrix(imp, "xgboost", "marginal")[, predictors]
    raw = simplify2array(lapply(predictors, function(j) pkg$.csdg_raw(imp, "xgboost", j)))
    raw = aperm(raw, c(1L, 3L, 2L))
    dimnames(raw) = list(NULL, predictors, NULL)
    perf = imp$performance[imp$performance$learner == "xgboost"][order(iteration)]
    dat = imp$fits$task$data(cols = predictors)
    list(P = P, raw = raw, loss = perf$loss, loss_baseline = perf$loss_baseline, n_test = perf$n_test,
      n_train = perf$n_train, cor = cor(as.matrix(dat)))
  }
  # csdg_fold_pfi() seeds permutation r of feature group g in fold k with seed + k * 1e5 + g * 1000 + r, where g
  # indexes the task's (alphabetical) feature order. The PFI is reproduced with the columns in that order, because
  # xgboost breaks exact ties between split gains (which occur in small training sets) by column position.
  package_seed_permutation = function(seed) {
    function(k, j, r, m) {
      set.seed(as.integer(seed) + k * 100000L + j * 1000L + r)
      sample(seq_len(m))
    }
  }
  # The dependence screen of version 0.3.0 always reports its row: an unresolved threat if a named predictor is
  # flagged and the claim has the default wording, context otherwise; the flags are in `details$dependence`.
  status_rows = function(check, prefix) {
    table = check$table
    get_status = function(name) {
      hit = table$status[table$check == name & table$role != "context"]
      if (length(hit)) hit[[1L]] else NA_character_
    }
    flagged = any(check$details$dependence$flagged)
    role = table$role[table$check == "dependence"]
    stopifnot(length(role) == 1L, identical(role[[1L]], if (flagged && prefix == "default") "unresolved_threat" else
      "context"))
    data.table(property = c("content", "minimum", "stability", "dependence_flag"),
      package = c(get_status("content"), get_status("minimum_importance"), get_status("stability"),
        if (flagged) "flagged" else "not_flagged"))
  }
  # Measurement and data (G0b) stay open in version 0.3.0 until the researcher judges them.
  g0b = csdg_judge("G0b", supported(), note = "Predictors and outcome recorded as simulated.")
  end_to_end = list()
  pfi_agreement = list()
  for (s in seq_len(nrow(scenarios))) {
    for (i_n in seq_along(settings$n)) {
      samples = draw_samples(s, i_n, 1L)
      seeds = c(explore = 1000L * s + 10L * i_n + 1L, confirm = 1000L * s + 10L * i_n + 2L)
      imp_explore = package_importance(samples$explore, sprintf("explore_%d_%d", s, i_n), seeds[["explore"]])
      imp_confirm = package_importance(samples$confirm, sprintf("confirm_%d_%d", s, i_n), seeds[["confirm"]])
      cv_explore = cv_from_package(imp_explore)
      cv_confirm = cv_from_package(imp_confirm)
      for (stage in c("explore", "confirm")) {
        smp = samples[[stage]]
        features = (if (stage == "explore") imp_explore else imp_confirm)$fits$features
        own = cross_validated_pfi(smp$x[, features], smp$y, smp$fold,
          permutation = package_seed_permutation(seeds[[stage]]))
        reference = if (stage == "explore") cv_explore else cv_confirm
        pfi_agreement[[length(pfi_agreement) + 1L]] = data.table(scenario = scenarios$scenario[[s]],
          n = settings$n[[i_n]], stage = stage, max_abs_difference_pfi = max(abs(own$P[, predictors] - reference$P)),
          max_abs_difference_raw = max(abs(own$raw[, predictors, ] - reference$raw)),
          max_abs_difference_loss = max(abs(own$loss - reference$loss)),
          max_abs_difference_baseline = max(abs(own$loss_baseline - reference$loss_baseline)))
      }
      stated = state_claims(cv_explore)
      for (wording in c("marginal_only", "default")) {
        marginal_only = wording == "marginal_only"
        claims = list(
          R = suppressWarnings(claim_relies_mainly(imp_explore, k = 2L, factor = settings$factor,
            marginal_only = marginal_only)),
          O = suppressWarnings(claim_order(imp_explore, stated$ranking[1:2], marginal_only = marginal_only))
        )
        for (claim_type in c("R", "O")) {
          claim = claims[[claim_type]]
          named = c(claim$predictors, claim$first, claim$second)
          stopifnot(setequal(named, stated$ranking[1:2]))
          if (claim_type == "O") stopifnot(identical(claim$first, stated$ranking[[1L]]))
          chk = suppressWarnings(csdg_check(claim, imp_explore, judgments = g0b))
          explore_assessment = suppressWarnings(csdg_assess(chk))$assessment
          confirmation = suppressWarnings(csdg_confirm(chk, imp_confirm))
          for (stage in c("explore", "confirm")) {
            check = if (stage == "explore") chk else confirmation$check
            package_assessment = if (stage == "explore") explore_assessment else confirmation$assessment
            mine = check_claim(if (stage == "explore") cv_explore else cv_confirm, stated[[claim_type]])
            rows = status_rows(check, wording)
            rows = rbind(rows, data.table(property = "assessment", package = package_assessment))
            rows[, script := c(mine$content, mine$minimum, mine$stability,
              if (mine$flagged) "flagged" else "not_flagged",
              if (marginal_only) mine$assessment else mine$assessment_default)]
            end_to_end[[length(end_to_end) + 1L]] = data.table(scenario = scenarios$scenario[[s]],
              n = settings$n[[i_n]], replication = 1L, wording = wording, claim = claim_type, stage = stage,
              named = paste(named, collapse = ", "), rows)
          }
        }
      }
      message(sprintf("Verified %s, n = %d.", scenarios$scenario[[s]], settings$n[[i_n]]))
    }
  }
  end_to_end = rbindlist(end_to_end)
  end_to_end[, agree := package == script]
  pfi_agreement = rbindlist(pfi_agreement)

  # Rules on random inputs that reach every branch: margins near zero, within Monte Carlo error, and clearly
  # positive or negative; improvements that are negative, small, or large; pairs in and against the order of the
  # averages (as in new data); and an unnamed predictor whose mean PFI is below that of another unnamed predictor but
  # whose fold PFI varies strongly, so that the binding comparison is not the one with the largest other mean.
  rule_checks = rbindlist(lapply(seq_len(if (quick) 400L else 4000L), function(i) {
    set.seed(settings$seed + 9000000L + i)
    k_folds = settings$folds
    repetitions = settings$permutations
    scale = sample(c(0.002, 0.01, 0.05), 1L)
    means = sort(rexp(p) * scale, decreasing = TRUE)
    pattern = sample(c("dominance", "near_boundary", "flat", "reversed", "volatile_competitor"), 1L)
    if (pattern == "dominance") means[3:p] = means[3:p] / 4
    if (pattern == "near_boundary") means[[2L]] = 2 * means[[3L]] * runif(1L, 0.9, 1.1)
    if (pattern == "flat") means[1:3] = mean(means[1:3])
    if (pattern == "reversed") means[1:2] = means[2:1]
    if (pattern == "volatile_competitor") means[3:4] = means[[2L]] / 2 * c(runif(1L, 0.5, 0.9), runif(1L, 0.3, 0.5))
    fold_sd = scale * sample(c(0.02, 0.2, 1), 1L)
    perm_sd = scale * sample(c(0.01, 0.2, 2), 1L)
    mu = matrix(means, k_folds, p, byrow = TRUE) + matrix(rnorm(k_folds * p, sd = fold_sd), k_folds)
    if (pattern == "volatile_competitor") mu[, 4L] = mu[, 4L] + rnorm(k_folds, sd = 2 * means[[4L]])
    raw = array(rep(mu, repetitions), c(k_folds, p, repetitions)) +
      array(rnorm(k_folds * p * repetitions, sd = perm_sd), c(k_folds, p, repetitions))
    dimnames(raw) = list(NULL, predictors, NULL)
    P = apply(raw, c(1L, 2L), mean)
    rownames(P) = seq_len(k_folds)
    improvement = sample(c(-5, 1, 10, 50, 100, 200, 400), 1L) * scale * runif(1L, 0.5, 1.5)
    avg = colMeans(P)
    ranking = names(avg)[order(-avg, seq_along(avg))]
    pair = if (runif(1L) < 0.7) ranking[1:2] else sample(predictors, 2L)
    claims = list(R = list(kind = "relies_mainly", predictors = pair, factor = settings$factor),
      O = list(kind = "order", first = pair[[1L]], second = pair[[2L]]))
    n_test = rep(100L, k_folds)
    # mc = TRUE: PFI with several permutations per fold, for which the Monte Carlo rule applies (version 0.3.0).
    view = list(learner = "xgboost", label = "XGBoost", mc = TRUE, P = P, avg = avg, raw = setNames(lapply(predictors,
      function(j) matrix(raw[, j, ], k_folds)), predictors), raw_conditional = list(), I = improvement,
      tau = settings$min_importance * improvement, delta = settings$min_importance * max(improvement, 0),
      K = k_folds, R = repetitions, n_train = 9L * n_test, n_test = n_test, level = settings$level)
    criteria = list(interval_rule = "open_if_inconclusive", need = as.integer(ceiling(settings$stability * k_folds -
      1e-8)), level = settings$level)
    cv = list(P = P, raw = raw, loss = rep(1, k_folds), loss_baseline = rep(1 + improvement, k_folds),
      n_test = n_test, n_train = 9L * n_test, cor = NULL)
    rbindlist(lapply(c("R", "O"), function(claim_type) {
      claim = claims[[claim_type]]
      mine = check_claim(cv, claim)
      package = c(pkg$.csdg_content(claim, view, criteria)$status,
        pkg$.csdg_minimum_importance(claim, view, criteria)$status, pkg$.csdg_stability(claim, view, criteria)$status)
      data.table(case = i, pattern = pattern, claim = claim_type, property = c("content", "minimum", "stability"),
        package = package, script = c(mine$content, mine$minimum, mine$stability),
        differs_from_largest_only = c(mine$content != mine$content_largest, FALSE, FALSE))
    }))
  }))
  rule_checks[, agree := package == script]
  rule_summary = rule_checks[, .(cases = .N, agree = sum(agree), differs_from_largest_only =
    sum(differs_from_largest_only)), by = .(claim, property, package_status = package)]
  setorder(rule_summary, claim, property, package_status)

  fwrite(end_to_end, file.path(output_dir, "workflow_verification_end_to_end.csv"))
  fwrite(pfi_agreement, file.path(output_dir, "workflow_verification_pfi.csv"))
  fwrite(rule_summary, file.path(output_dir, "workflow_verification_rules.csv"))
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo_verify.txt"))
  message(sprintf("End to end: %d of %d statuses agree; PFI max abs difference %.2e.", sum(end_to_end$agree),
    nrow(end_to_end), max(pfi_agreement$max_abs_difference_pfi)))
  message(sprintf(paste("Rules on random inputs: %d of %d statuses agree; %d content statuses differ from those",
    "of the comparison with the largest other predictor only."), sum(rule_checks$agree), nrow(rule_checks),
    sum(rule_checks$differs_from_largest_only)))
  invisible(list(end_to_end = end_to_end, pfi = pfi_agreement, rules = rule_summary))
}

# ---- Timing ----

time_replications = function(count = 5L) {
  timing = rbindlist(lapply(seq_len(nrow(scenarios)), function(s) rbindlist(lapply(seq_along(settings$n),
    function(i_n) {
      seconds = vapply(seq_len(count), function(r) system.time(run_replication(s, i_n, r))[["elapsed"]],
        numeric(1L))
      truth_seconds = system.time(truth_fit(s, i_n, 1L))[["elapsed"]]
      data.table(scenario = scenarios$scenario[[s]], n = settings$n[[i_n]], seconds_per_replication = mean(seconds),
        seconds_per_truth_fit = truth_seconds)
    }))))
  total_hours = (sum(timing$seconds_per_replication) * 500 + sum(timing$seconds_per_truth_fit) * 100) / 3600
  print(timing)
  message(sprintf("Single process: %.2f hours for 500 replications per cell and 100 truth fits per cell;",
    total_hours), sprintf(" %.2f hours with 6 workers (ideal).", total_hours / 6))
  invisible(timing)
}

if (mode == "run") run_simulation()
if (mode == "figure") make_figure(fread(file.path(output_dir, "workflow_operating_characteristics.csv")))
if (mode == "verify") verify_against_package()
if (mode == "timing") time_replications()
