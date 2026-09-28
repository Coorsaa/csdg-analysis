# Reproducibility identities for focused analysis runners.

.analysis_sha256 = function(path) {
  if (!file.exists(path) || file.info(path)$isdir) {
    stop("Cannot hash a missing or non-file source: ", path, call. = FALSE)
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

.analysis_declared_runner_sha256 = function(runner_path) {
  declared = Sys.getenv("CSDG_COMPUTATION_RUNNER_SHA256", unset = "")
  value = if (nzchar(declared)) tolower(declared) else .analysis_sha256(runner_path)
  if (!grepl("^[0-9a-f]{64}$", value)) {
    stop("CSDG_COMPUTATION_RUNNER_SHA256 must be a lowercase SHA-256 digest.", call. = FALSE)
  }
  value
}

.analysis_package_source_identity = function(package_source) {
  package_source = normalizePath(package_source, winslash = "/", mustWork = TRUE)
  description_path = file.path(package_source, "DESCRIPTION")
  if (!file.exists(description_path)) {
    stop("Package source identity requires a DESCRIPTION file.", call. = FALSE)
  }
  roots = c("R", "inst")
  files = c(description_path, file.path(package_source, "NAMESPACE"))
  for (root in roots) {
    directory = file.path(package_source, root)
    if (dir.exists(directory)) {
      files = c(files, list.files(directory, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE))
    }
  }
  files = unique(files[file.exists(files) & file.info(files)$isdir %in% FALSE])
  prefix = paste0(package_source, "/")
  relative = ifelse(startsWith(files, prefix), substring(files, nchar(prefix) + 1L), basename(files))
  index = order(relative)
  files = files[index]
  relative = relative[index]
  records = paste(
    relative,
    format(as.numeric(file.info(files)$size), scientific = FALSE, trim = TRUE),
    vapply(files, .analysis_sha256, character(1L)),
    sep = "\t"
  )
  description = read.dcf(description_path, fields = "Version")
  list(
    package_version = unname(description[[1L]]),
    package_source_tree_sha256 = digest::digest(
      paste(enc2utf8(records), collapse = "\n"),
      algo = "sha256",
      serialize = FALSE
    ),
    package_source_scope = "DESCRIPTION, NAMESPACE, R/**, and inst/** regular files"
  )
}

.analysis_computation_source_sha256 = function(runner_sha256, package_source_tree_sha256 = NULL) {
  records = c(paste0("runner_sha256=", runner_sha256))
  if (!is.null(package_source_tree_sha256)) {
    records = c(records, paste0("package_source_tree_sha256=", package_source_tree_sha256))
  }
  digest::digest(paste(records, collapse = "\n"), algo = "sha256", serialize = FALSE)
}
