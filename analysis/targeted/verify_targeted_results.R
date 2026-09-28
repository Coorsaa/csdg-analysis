# Independent aggregate reconciliation from private pointwise caches; no model or surrogate is fitted.

verify_targeted_results = function(private_cache, project_root = getwd()) {
  suppressPackageStartupMessages(library(data.table))
  paths = list.files(private_cache, pattern = "^paired_case_.*rds$", full.names = TRUE)
  stopifnot(length(paths) == 100L)
  differences = numeric()
  paired_rows = list()
  for (path in paths) {
    cache = readRDS(path)
    stopifnot(length(cache$pointwise) == 5L, length(cache$coefficients) == 5L)
    metrics = cache$result$metrics
    case_results = list()
    for (replicate_id in 1:5) {
      points = cache$pointwise[[replicate_id]]
      stopifnot(nrow(points) == 500L, uniqueN(points$neighbor_id) == 500L,
        identical(sort(unique(points$evaluation_fold)), 1:5), all(points$weight > 0))
      weight = points$weight / sum(points$weight)
      probability = points$probability
      center = sum(weight * probability)
      errors = list()
      for (scale in c("response", "link")) {
        prediction = points[[paste0(scale, "_prediction")]]
        squared_error = sum(weight * (probability - prediction)^2)
        recalculated = c(weighted_rmse = sqrt(squared_error),
          weighted_r2 = 1 - squared_error / sum(weight * (probability - center)^2),
          weighted_mae = sum(weight * abs(probability - prediction)),
          out_of_probability_range_fraction = mean(prediction < 0 | prediction > 1))
        recorded = metrics[replicate == replicate_id & surrogate_scale == scale]
        differences = c(differences, abs(recalculated - unlist(recorded[, names(recalculated), with = FALSE])))
        errors[[scale]] = recalculated
      }
      case_results[[replicate_id]] = errors$link - errors$response
    }
    case_matrix = do.call(rbind, case_results)
    paired_rows[[length(paired_rows) + 1L]] = apply(case_matrix, 2L, median)
  }
  paired_matrix = do.call(rbind, paired_rows)
  public = fread(file.path(project_root,
    "analysis/targeted/results/shild/shild_common_scale_paired_differences.csv"))
  aggregate_errors = vapply(colnames(paired_matrix), function(metric_name) {
    values = paired_matrix[, metric_name]
    actual = c(median(values), quantile(values, c(.25, .75), type = 8, names = FALSE))
    recorded = unlist(public[metric == metric_name, .(median, q25, q75)])
    max(abs(actual - recorded))
  }, numeric(1L))
  stopifnot(max(differences) < 1e-10, max(aggregate_errors) < 1e-10)
  output = file.path(project_root, "analysis/targeted/qa")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  report = list(status = "passed", date = "2026-09-10", private_case_caches = 100L,
    paired_replicates = 500L, common_scale_metric_checks = length(differences),
    maximum_pointwise_reconciliation_error = max(differences),
    maximum_paired_aggregate_reconciliation_error = max(aggregate_errors),
    independent_formulas = "Base weighted SSE, weighted variance, MAE, and unclipped range checks",
    evaluation_rows_per_replicate = 500L, folds_per_replicate = 5L,
    aggregation = "Within-case five-seed paired differences; then across-case medians and type-8 quartiles",
    models_or_surrogates_fitted = 0L, private_records_exported = 0L)
  jsonlite::write_json(report, file.path(output, "POINTWISE_RECONCILIATION.json"),
    pretty = TRUE, auto_unbox = TRUE, digits = 16)
  message("Passed 4000 independent pointwise-metric checks and 12 paired-summary checks.")
  invisible(report)
}

if (sys.nframe() == 0L) {
  arguments = commandArgs(trailingOnly = TRUE)
  stopifnot(length(arguments) == 1L)
  verify_targeted_results(arguments[1L])
}
