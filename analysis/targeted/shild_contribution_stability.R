# Compare contributions on one common, weighted reference neighborhood per case.
# Encoded terms are summed within their original input variable; no small-term cutoff is used.

targeted_contributions = function(design, coefficients, term_assign, feature_names, weights) {
  stopifnot(identical(colnames(design), names(coefficients)), length(term_assign) == ncol(design),
    length(weights) == nrow(design), all(weights > 0))
  weights = weights / sum(weights)
  center = colSums(design * weights)
  terms = sweep(sweep(design, 2L, center), 2L, coefficients, "*")
  grouped = vapply(seq_along(feature_names), function(index) {
    rowSums(terms[, term_assign == index, drop = FALSE])
  }, numeric(nrow(design)))
  colnames(grouped) = feature_names
  stopifnot(max(abs(rowSums(grouped) -
    (as.numeric(design %*% coefficients) - sum(center * coefficients)))) < 1e-8)
  grouped
}

targeted_contribution_case = function(oof, case_map, paired_results) {
  reference = targeted_case_design(oof, case_map, 1000L,
    402609100L + as.integer(sub("case_", "", case_map$case_label)))
  design = reference$design[-1L, , drop = FALSE]
  weights = reference$weights[-1L]
  weights = weights / sum(weights)
  summaries = list()
  for (scale in c("response", "link")) {
    contributions = lapply(paired_results, function(result) {
      stopifnot(identical(result$design_names, colnames(design)))
      targeted_contributions(design, result$coefficients[[scale]], result$term_assign,
        result$feature_names, weights)
    })
    magnitudes = vapply(contributions, function(value) sqrt(colSums(value^2 * weights)),
      numeric(ncol(contributions[[1L]])))
    pairs = combn(seq_along(contributions), 2L)
    differences = vapply(seq_len(ncol(pairs)), function(index) {
      sqrt(colSums((contributions[[pairs[1L, index]]] - contributions[[pairs[2L, index]]])^2 * weights))
    }, numeric(ncol(contributions[[1L]])))
    ranks = apply(magnitudes, 2L, function(value) rank(-value, ties.method = "average"))
    mean_magnitude = rowMeans(magnitudes)
    mean_difference = rowMeans(differences)
    summaries[[scale]] = data.table(feature = colnames(contributions[[1L]]), surrogate_scale = scale,
      mean_contribution_rms = mean_magnitude, mean_pairwise_contribution_rms_difference = mean_difference,
      relative_contribution_difference = ifelse(mean_magnitude > 0, mean_difference / mean_magnitude, NA_real_),
      magnitude_weighted_total_difference = sum(mean_difference) / sum(mean_magnitude),
      mean_magnitude_rank = rowMeans(ranks), magnitude_rank_sd = apply(ranks, 1L, sd),
      n_replicates = length(contributions), n_reference = nrow(design),
      reference_seed = 402609100L + as.integer(sub("case_", "", case_map$case_label)))
  }
  rbindlist(summaries)
}
