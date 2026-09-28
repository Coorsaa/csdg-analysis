# Publication-run and upload-bundle contract ---------------------------------
#
# This file is deliberately standalone so that SHILD, PISA, and a coordinating
# run_all.R can source it before loading the package. Full private run outputs
# stay untouched. Only create_sanitized_upload_bundle() creates an uploadable
# derivative, and that derivative is verified before it is returned.

.run_contract_version = "1.1.1"

.run_contract_stop = function(...) {
  stop(..., call. = FALSE)
}

.run_contract_scalar_string = function(x, name, allow_empty = FALSE) {
  ok = is.character(x) && length(x) == 1L && !is.na(x)
  if (ok && !allow_empty) {
    ok = nzchar(x)
  }
  if (!ok) {
    .run_contract_stop("`", name, "` must be one non-missing character value.")
  }
  invisible(TRUE)
}

.run_contract_timestamp = function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

.run_contract_safe_name = function(x) {
  x = gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  x = gsub("_+", "_", x)
  x = sub("^[._-]+", "", x)
  x = sub("[._-]+$", "", x)
  if (!nzchar(x)) "run" else x
}

.run_contract_assert_contract = function(contract) {
  if (!inherits(contract, "CSDGRunContract") || !is.list(contract)) {
    .run_contract_stop("`contract` must be returned by create_run_contract().")
  }
  if (!dir.exists(contract$private_dir)) {
    .run_contract_stop("The private run directory no longer exists: ", contract$private_dir)
  }
  invisible(TRUE)
}

.run_contract_relative_path = function(path, root) {
  root = paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, root)) {
    .run_contract_stop("Path is outside the declared root: ", path)
  }
  substring(path, nchar(root) + 1L)
}

.run_contract_validate_relative = function(path, name = "path") {
  .run_contract_scalar_string(path, name)
  normalized = gsub("\\\\", "/", path)
  if (startsWith(normalized, "/") || grepl("^[A-Za-z]:/", normalized) ||
      any(strsplit(normalized, "/", fixed = TRUE)[[1L]] == "..")) {
    .run_contract_stop("`", name, "` must be a path relative to the run directory: ", path)
  }
  normalized
}

.run_contract_write_csv = function(x, path, append = FALSE, col_names = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    x,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = col_names,
    quote = TRUE,
    qmethod = "double",
    na = "",
    append = append,
    fileEncoding = "UTF-8"
  )
  invisible(path)
}

.run_contract_key_values = function(values, path) {
  if (is.null(names(values)) || any(!nzchar(names(values)))) {
    .run_contract_stop("Metadata values must be a named list.")
  }
  stringify = function(value) {
    if (is.null(value)) {
      return(NA_character_)
    }
    if (length(value) > 1L) {
      return(paste(as.character(value), collapse = " | "))
    }
    as.character(value)
  }
  table = data.frame(
    key = names(values),
    value = vapply(values, stringify, character(1L)),
    stringsAsFactors = FALSE
  )
  .run_contract_write_csv(table, path)
}

#' Create a collision-safe private run directory and its standard subdirectories.
#'
#' The function never reuses or cleans an existing run directory. When the
#' requested identifier already exists, `-001`, `-002`, and so on are appended.
#' The returned `run_salt` is intentionally not written to disk or upload output.
create_run_contract = function(
    study,
    output_root = "paper_artifacts",
    run_id = NULL,
    mode = c("publication", "smoke_test"),
    seed,
    expected_artifacts = character()) {
  .run_contract_scalar_string(study, "study")
  .run_contract_scalar_string(output_root, "output_root")
  mode = match.arg(mode)
  if (length(seed) != 1L || is.na(seed) || !is.finite(seed) || seed < 0 || seed != as.integer(seed)) {
    .run_contract_stop("`seed` must be one non-negative integer.")
  }
  if (!is.character(expected_artifacts)) {
    .run_contract_stop("`expected_artifacts` must be a character vector of relative paths or glob patterns.")
  }

  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  output_root = normalizePath(output_root, winslash = "/", mustWork = TRUE)
  if (is.null(run_id) || !nzchar(run_id)) {
    run_id = paste0(
      format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
      "-p", Sys.getpid()
    )
  }
  run_id = .run_contract_safe_name(run_id)
  study = .run_contract_safe_name(study)
  base_name = paste0(run_id, "_", study)

  private_dir = NULL
  for (index in 0:9999) {
    suffix = if (index == 0L) "" else sprintf("-%03d", index)
    candidate = file.path(output_root, paste0(base_name, suffix))
    if (dir.create(candidate, recursive = FALSE, showWarnings = FALSE)) {
      private_dir = normalizePath(candidate, winslash = "/", mustWork = TRUE)
      break
    }
  }
  if (is.null(private_dir)) {
    .run_contract_stop("Could not allocate a unique run directory below ", output_root, ".")
  }

  dirs = list(
    root = private_dir,
    figures = file.path(private_dir, "figures"),
    tables = file.path(private_dir, "tables"),
    audit_bundle = file.path(private_dir, "audit_bundle"),
    objects = file.path(private_dir, "objects"),
    logs = file.path(private_dir, "logs"),
    checkpoints = file.path(private_dir, "checkpoints"),
    provenance = file.path(private_dir, "provenance")
  )
  invisible(lapply(unname(dirs[-1L]), dir.create, recursive = TRUE, showWarnings = FALSE))

  if (!requireNamespace("digest", quietly = TRUE)) {
    .run_contract_stop("Package `digest` is required to create the run-specific upload salt.")
  }
  salt_material = paste(
    private_dir,
    .run_contract_timestamp(),
    Sys.getpid(),
    stats::runif(1L),
    sep = "::"
  )
  run_salt = digest::digest(salt_material, algo = "sha256", serialize = FALSE)

  contract = structure(
    list(
      contract_version = .run_contract_version,
      study = study,
      run_id = basename(private_dir),
      requested_run_id = run_id,
      mode = mode,
      seed = as.integer(seed),
      created_at_utc = .run_contract_timestamp(),
      private_dir = private_dir,
      dirs = dirs,
      expected_artifacts = expected_artifacts,
      run_salt = run_salt
    ),
    class = c("CSDGRunContract", "list")
  )

  .run_contract_key_values(
    list(
      contract_version = contract$contract_version,
      study = contract$study,
      run_id = contract$run_id,
      mode = contract$mode,
      seed = contract$seed,
      created_at_utc = contract$created_at_utc,
      private_run_directory = contract$private_dir
    ),
    file.path(contract$dirs$provenance, "run_metadata.csv")
  )
  contract
}

.run_contract_condition_row = function(type, condition) {
  call = conditionCall(condition)
  data.frame(
    timestamp_utc = .run_contract_timestamp(),
    type = type,
    message = conditionMessage(condition),
    call = if (is.null(call)) NA_character_ else paste(deparse(call), collapse = " "),
    stringsAsFactors = FALSE
  )
}

.run_contract_append_condition = function(contract, type, condition) {
  path = file.path(contract$dirs$logs, "conditions.csv")
  row = .run_contract_condition_row(type, condition)
  .run_contract_write_csv(
    row,
    path,
    append = file.exists(path),
    col_names = !file.exists(path)
  )
  line = sprintf(
    "[%s] [%s] %s\n",
    row$timestamp_utc[[1L]],
    type,
    gsub("[\r\n]+", " ", row$message[[1L]])
  )
  cat(line, file = file.path(contract$dirs$logs, "console.log"), append = TRUE)
  invisible(row)
}

.run_contract_assert_logging = function(logging) {
  if (!inherits(logging, "CSDGRunLogging") || !is.environment(logging)) {
    .run_contract_stop("`logging` must be returned by start_run_logging().")
  }
  invisible(TRUE)
}

.run_contract_set_global_handlers = function(handlers) {
  globalCallingHandlers(NULL)
  if (length(handlers)) {
    globalCallingHandlers(handlers)
  }
  invisible(TRUE)
}

.run_contract_handle_condition = function(logging, type, condition) {
  if (!isTRUE(logging$active)) {
    return(invisible(NULL))
  }
  if (identical(type, "error") && !isTRUE(logging$failure_recorded) &&
      !is.null(logging$failure_checkpoint)) {
    logging$failure_recorded = TRUE
    write_run_checkpoint(
      logging$contract,
      logging$failure_checkpoint,
      "failed",
      list(error = conditionMessage(condition))
    )
  }
  .run_contract_append_condition(logging$contract, type, condition)
  flush(logging$connection)
  previous = logging$previous_handlers[[type]]
  if (is.function(previous)) {
    previous(condition)
  }
  invisible(NULL)
}

#' Start teeing top-level output and recording unhandled conditions.
#'
#' The logger remains active across separately evaluated top-level expressions.
#' Unhandled errors are recorded and retain their normal terminating behavior.
start_run_logging = function(contract, failure_checkpoint = NULL) {
  .run_contract_assert_contract(contract)
  if (!is.null(failure_checkpoint)) {
    .run_contract_scalar_string(failure_checkpoint, "failure_checkpoint")
  }

  previous_handlers = globalCallingHandlers()
  if (any(vapply(previous_handlers, inherits, logical(1L), what = "CSDGRunLoggingHandler"))) {
    .run_contract_stop("A CSDG run logger is already active in this R process.")
  }

  logging = new.env(parent = emptyenv())
  class(logging) = c("CSDGRunLogging", "environment")
  logging$contract = contract
  logging$failure_checkpoint = failure_checkpoint
  logging$failure_recorded = FALSE
  logging$previous_handlers = previous_handlers
  logging$output_sinks = sink.number(type = "output")
  logging$connection = file(
    file.path(contract$dirs$logs, "console.log"),
    open = "a",
    encoding = "UTF-8"
  )
  close_connection = TRUE
  on.exit({
    if (close_connection && isOpen(logging$connection)) {
      close(logging$connection)
    }
  }, add = TRUE)
  logging$active = TRUE

  handler_types = c("message", "warning", "error")
  logging$handlers = setNames(lapply(handler_types, function(type) {
    handler = function(condition) {
      .run_contract_handle_condition(logging, type, condition)
    }
    class(handler) = c("CSDGRunLoggingHandler", "function")
    handler
  }), handler_types)
  retained_handlers = previous_handlers[!names(previous_handlers) %in% handler_types]

  .run_contract_set_global_handlers(c(logging$handlers, retained_handlers))
  sink_result = tryCatch({
    sink(logging$connection, type = "output", split = TRUE)
    list(ok = TRUE, error = NULL)
  }, error = function(error) {
    list(ok = FALSE, error = error)
  })
  if (!sink_result$ok) {
    .run_contract_set_global_handlers(previous_handlers)
    logging$active = FALSE
    stop(sink_result$error)
  }
  close_connection = FALSE
  logging
}

#' Stop a top-level run logger and restore the prior process state.
stop_run_logging = function(logging) {
  .run_contract_assert_logging(logging)
  if (!isTRUE(logging$active)) {
    return(invisible(FALSE))
  }

  current_sinks = sink.number(type = "output")
  missing_logging_sink = current_sinks < logging$output_sinks + 1L
  .run_contract_set_global_handlers(logging$previous_handlers)
  if (!missing_logging_sink) {
    while (sink.number(type = "output") > logging$output_sinks) {
      sink(type = "output")
    }
  }
  connection_open = tryCatch(isOpen(logging$connection), error = function(error) FALSE)
  if (connection_open) {
    flush(logging$connection)
    close(logging$connection)
  }
  logging$active = FALSE
  if (missing_logging_sink) {
    .run_contract_stop("The run-logging output sink was closed outside stop_run_logging().")
  }
  invisible(TRUE)
}

.run_contract_file_records = function(files, role) {
  if (!length(files)) {
    return(data.frame(
      role = character(), path = character(), bytes = numeric(), md5 = character(), sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(names(files))) {
    names(files) = rep(role, length(files))
  } else {
    names(files)[!nzchar(names(files))] = role
  }
  missing = files[!file.exists(files)]
  if (length(missing)) {
    .run_contract_stop("Provenance input does not exist: ", paste(missing, collapse = ", "))
  }
  paths = normalizePath(files, winslash = "/", mustWork = TRUE)
  data.frame(
    role = names(files),
    path = paths,
    bytes = as.numeric(file.info(paths)$size),
    md5 = unname(tools::md5sum(paths)),
    sha256 = .run_contract_sha256(paths),
    stringsAsFactors = FALSE
  )
}

.run_contract_sha256 = function(paths) {
  if (!length(paths)) {
    return(character())
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    .run_contract_stop("Package `digest` is required to calculate SHA-256 checksums.")
  }
  vapply(
    paths,
    function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE),
    character(1L)
  )
}

.run_contract_git_value = function(arguments, directory) {
  output = tryCatch(
    suppressWarnings(system2(
      "git",
      c("-C", shQuote(directory), arguments),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(error) character()
  )
  status = attr(output, "status")
  if ((length(status) && !identical(status, 0L)) || !length(output)) {
    return(NA_character_)
  }
  paste(output, collapse = "\n")
}

#' Record mode, seed, source, package, input, runtime, and optional metadata.
write_run_provenance = function(
    contract,
    source_files = character(),
    package_name = "mlr3autoiml",
    input_files = character(),
    extra = list()) {
  .run_contract_assert_contract(contract)
  .run_contract_scalar_string(package_name, "package_name")
  if (!is.list(extra)) {
    .run_contract_stop("`extra` must be a named list.")
  }

  source_records = .run_contract_file_records(source_files, "analysis source")
  input_records = .run_contract_file_records(input_files, "analysis input")
  .run_contract_write_csv(
    source_records,
    file.path(contract$dirs$provenance, "source_files.csv")
  )
  .run_contract_write_csv(
    input_records,
    file.path(contract$dirs$provenance, "input_files.csv")
  )

  package_version = tryCatch(
    as.character(utils::packageVersion(package_name)),
    error = function(error) NA_character_
  )
  package_library = tryCatch(
    dirname(find.package(package_name)),
    error = function(error) NA_character_
  )
  .run_contract_write_csv(
    data.frame(
      package = package_name,
      version = package_version,
      library = package_library,
      stringsAsFactors = FALSE
    ),
    file.path(contract$dirs$provenance, "package.csv")
  )

  source_root = if (length(source_files)) {
    dirname(normalizePath(source_files[[1L]], winslash = "/", mustWork = TRUE))
  } else {
    normalizePath(".", winslash = "/", mustWork = TRUE)
  }
  while (!identical(source_root, dirname(source_root)) &&
         !dir.exists(file.path(source_root, ".git"))) {
    source_root = dirname(source_root)
  }
  git_commit = if (dir.exists(file.path(source_root, ".git"))) {
    .run_contract_git_value(c("rev-parse", "HEAD"), source_root)
  } else {
    NA_character_
  }
  git_status = if (dir.exists(file.path(source_root, ".git"))) {
    .run_contract_git_value(c("status", "--short"), source_root)
  } else {
    NA_character_
  }

  metadata = c(
    list(
      contract_version = contract$contract_version,
      study = contract$study,
      run_id = contract$run_id,
      mode = contract$mode,
      seed = contract$seed,
      created_at_utc = contract$created_at_utc,
      provenance_written_at_utc = .run_contract_timestamp(),
      R_version = R.version.string,
      platform = R.version$platform,
      working_directory = normalizePath(".", winslash = "/", mustWork = TRUE),
      source_root = source_root,
      git_commit = git_commit,
      git_status = git_status,
      package_name = package_name,
      package_version = package_version
    ),
    extra
  )
  .run_contract_key_values(
    metadata,
    file.path(contract$dirs$provenance, "run_metadata.csv")
  )
  invisible(metadata)
}

.run_contract_list_files = function(root) {
  root = normalizePath(root, winslash = "/", mustWork = TRUE)
  files = list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  files[file.info(files)$isdir %in% FALSE]
}

#' Write a checksum manifest for every file recursively below a directory.
write_recursive_manifest = function(
    root,
    manifest_path = file.path(root, "MANIFEST.csv"),
    exclude = basename(manifest_path)) {
  .run_contract_scalar_string(root, "root")
  if (!dir.exists(root)) {
    .run_contract_stop("Manifest root does not exist: ", root)
  }
  root = normalizePath(root, winslash = "/", mustWork = TRUE)
  manifest_path = normalizePath(manifest_path, winslash = "/", mustWork = FALSE)
  root_prefix = paste0(root, "/")
  if (!startsWith(manifest_path, root_prefix)) {
    .run_contract_stop("`manifest_path` must be inside `root`.")
  }
  files = .run_contract_list_files(root)
  relative = vapply(files, .run_contract_relative_path, character(1L), root = root)
  exclude = gsub("\\\\", "/", exclude)
  keep = !relative %in% exclude
  files = files[keep]
  relative = relative[keep]
  order = order(relative)
  files = files[order]
  relative = relative[order]
  manifest = data.frame(
    path = relative,
    bytes = as.numeric(file.info(files)$size),
    md5 = unname(tools::md5sum(files)),
    sha256 = .run_contract_sha256(files),
    stringsAsFactors = FALSE
  )
  .run_contract_write_csv(manifest, manifest_path)
  invisible(manifest)
}

#' Verify a recursive manifest and fail on missing, extra, resized, or changed files.
verify_recursive_manifest = function(
    root,
    manifest_path = file.path(root, "MANIFEST.csv")) {
  if (!dir.exists(root) || !file.exists(manifest_path)) {
    .run_contract_stop("Manifest root or manifest file is missing.")
  }
  root = normalizePath(root, winslash = "/", mustWork = TRUE)
  manifest_path = normalizePath(manifest_path, winslash = "/", mustWork = TRUE)
  manifest = utils::read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  required = c("path", "bytes", "md5", "sha256")
  if (!all(required %in% names(manifest)) || anyDuplicated(manifest$path)) {
    .run_contract_stop("Manifest is malformed: ", manifest_path)
  }
  valid_md5 = grepl("^[0-9a-f]{32}$", manifest$md5)
  valid_sha256 = grepl("^[0-9a-f]{64}$", manifest$sha256)
  if (anyNA(valid_md5) || anyNA(valid_sha256) || !all(valid_md5) || !all(valid_sha256)) {
    .run_contract_stop("Manifest contains a missing or malformed checksum: ", manifest_path)
  }
  manifest_relative = .run_contract_relative_path(manifest_path, root)
  files = .run_contract_list_files(root)
  relative = vapply(files, .run_contract_relative_path, character(1L), root = root)
  keep = relative != manifest_relative
  files = files[keep]
  relative = relative[keep]
  if (!setequal(relative, manifest$path)) {
    missing = setdiff(manifest$path, relative)
    extra = setdiff(relative, manifest$path)
    .run_contract_stop(
      "Manifest file set differs. Missing: ", paste(missing, collapse = ", "),
      "; extra: ", paste(extra, collapse = ", "), "."
    )
  }
  index = match(manifest$path, relative)
  bytes = as.numeric(file.info(files[index])$size)
  md5 = unname(tools::md5sum(files[index]))
  sha256 = .run_contract_sha256(files[index])
  bad = manifest$bytes != bytes | manifest$md5 != md5 | manifest$sha256 != sha256
  if (any(bad)) {
    .run_contract_stop("Manifest checksum mismatch: ", paste(manifest$path[bad], collapse = ", "))
  }
  invisible(TRUE)
}

#' Require every expected relative file, directory, or glob match to be present.
validate_expected_artifacts = function(root, expected, allow_empty = FALSE) {
  if (!dir.exists(root)) {
    .run_contract_stop("Artifact root does not exist: ", root)
  }
  if (!is.character(expected)) {
    .run_contract_stop("`expected` must be a character vector.")
  }
  root = normalizePath(root, winslash = "/", mustWork = TRUE)
  records = list()
  for (index in seq_along(expected)) {
    item = .run_contract_validate_relative(expected[[index]], "expected artifact")
    has_glob = grepl("[*?[]", item)
    matches = if (has_glob) {
      Sys.glob(file.path(root, item), dirmark = FALSE)
    } else {
      candidate = file.path(root, item)
      if (file.exists(candidate) || dir.exists(candidate)) candidate else character()
    }
    if (!length(matches)) {
      .run_contract_stop("Missing expected artifact: ", item)
    }
    for (match in matches) {
      resolved = normalizePath(match, winslash = "/", mustWork = TRUE)
      if (!startsWith(resolved, paste0(root, "/")) && !identical(resolved, root)) {
        .run_contract_stop("Expected artifact resolves outside the run directory: ", item)
      }
      if (dir.exists(resolved)) {
        contained = .run_contract_list_files(resolved)
        if (!allow_empty && !length(contained)) {
          .run_contract_stop("Expected artifact directory is empty: ", item)
        }
        bytes = sum(as.numeric(file.info(contained)$size), na.rm = TRUE)
      } else {
        bytes = as.numeric(file.info(resolved)$size)
        if (!allow_empty && (!is.finite(bytes) || bytes <= 0)) {
          .run_contract_stop("Expected artifact file is empty: ", item)
        }
      }
      records[[length(records) + 1L]] = data.frame(
        expected = item,
        matched = .run_contract_relative_path(resolved, root),
        bytes = bytes,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(records)) {
    return(data.frame(expected = character(), matched = character(), bytes = numeric()))
  }
  do.call(rbind, records)
}

#' Append a durable run checkpoint and write a per-checkpoint metadata file.
write_run_checkpoint = function(
    contract,
    checkpoint,
    status = c("started", "completed", "failed"),
    details = list()) {
  .run_contract_assert_contract(contract)
  .run_contract_scalar_string(checkpoint, "checkpoint")
  status = match.arg(status)
  if (!is.list(details)) {
    .run_contract_stop("`details` must be a named list.")
  }
  stringify = function(x) paste(as.character(x), collapse = " | ")
  detail_text = if (length(details)) {
    if (is.null(names(details))) names(details) = rep("detail", length(details))
    paste(paste0(names(details), "=", vapply(details, stringify, character(1L))), collapse = "; ")
  } else {
    ""
  }
  row = data.frame(
    timestamp_utc = .run_contract_timestamp(),
    checkpoint = checkpoint,
    status = status,
    details = detail_text,
    stringsAsFactors = FALSE
  )
  ledger = file.path(contract$dirs$checkpoints, "checkpoints.csv")
  .run_contract_write_csv(
    row,
    ledger,
    append = file.exists(ledger),
    col_names = !file.exists(ledger)
  )
  stamp = gsub("[^0-9]", "", row$timestamp_utc[[1L]])
  individual = file.path(
    contract$dirs$checkpoints,
    paste0(stamp, "_", .run_contract_safe_name(checkpoint), "_", status, ".csv")
  )
  .run_contract_write_csv(row, individual)
  invisible(row)
}

#' Read the checkpoint ledger from a run contract or private run directory.
read_run_checkpoints = function(contract) {
  path = if (inherits(contract, "CSDGRunContract")) {
    file.path(contract$dirs$checkpoints, "checkpoints.csv")
  } else {
    .run_contract_scalar_string(contract, "contract")
    file.path(contract, "checkpoints", "checkpoints.csv")
  }
  if (!file.exists(path)) {
    return(data.frame(
      timestamp_utc = character(), checkpoint = character(), status = character(), details = character(),
      stringsAsFactors = FALSE
    ))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

.run_contract_allowed_extensions = c(
  "", "csv", "tsv", "txt", "log", "md", "json", "yaml", "yml", "toml", "ini", "cfg", "tex", "html",
  "xml", "r", "lock", "sha256", "md5", "pdf", "png", "jpg", "jpeg", "svg"
)

.run_contract_text_extensions = c(
  "", "csv", "tsv", "txt", "log", "md", "json", "yaml", "yml", "toml", "ini", "cfg", "tex", "html",
  "xml", "r", "lock", "sha256", "md5", "svg"
)

.run_contract_risky_extensions = c(
  "rds", "rda", "rdata", "qs", "fst", "feather", "parquet", "sav", "dta", "sas7bdat", "pkl", "pickle",
  "joblib", "model", "bin", "sqlite", "sqlite3", "db", "zip", "tar", "gz", "bz2", "xz", "7z", "xlsx", "xls"
)

.run_contract_risky_directories = c(
  "objects", "models", "model", "tasks", "task", "backends", "backend", "raw", "raw_data", "predictions",
  "prediction", "oof_predictions"
)

.run_contract_identifier_columns = c(
  "row_id", "row_ids", "case_id", "participant_id", "respondent_id", "student_id", "subject_id", "person_id",
  "CNTSTUID", "CNTSCHID", "school_id", "cluster"
)

.run_contract_prediction_columns = c(
  "prediction", "predictions", "predicted", "response", "truth", "probability", "prob", "risk", "fitted",
  "residual"
)

.run_contract_truth_columns = c(
  "truth", "outcome", "observed", "label", "response"
)

.run_contract_fitted_columns = c(
  "prediction", "predictions", "predicted", "probability", "prob", "risk", "fitted", "residual"
)

.run_contract_shild_columns = c(
  "life_satisfaction", "gender", "age", "A2", "D2", "D8", "M8", "audit_gender", "audit_age_group"
)

.run_contract_is_text_file = function(path) {
  size = as.numeric(file.info(path)$size)
  if (!is.finite(size) || size == 0) {
    return(TRUE)
  }
  connection = file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes = readBin(connection, what = "raw", n = min(size, 8192L))
  !any(bytes == as.raw(0L))
}

.run_contract_local_path_patterns = function() {
  terminator = "[^\\s,\\\"'<>\\[\\](){};]+"
  path_boundary = "(?=$|[\\s,\\\"'<>\\[\\](){};])"
  common_posix_roots = paste(c(
    "Applications", "Library", "System", "Users", "bin", "data", "etc", "home", "media", "mnt", "opt",
    "private", "sbin", "srv", "tmp", "usr", "var", "Volumes", "workspace", "workspaces"
  ), collapse = "|")
  c(
    paste0("(?i)file://(?:[A-Za-z0-9._$-]+)?/", terminator),
    paste0("(?i)(?<![A-Za-z0-9\\\\])\\\\\\\\", terminator, "\\\\", terminator),
    paste0("(?i)(?<![A-Za-z0-9])\\b[A-Z]:[\\\\/]", terminator),
    paste0(
      "(?i)(?<![A-Za-z0-9:/])/(?:", common_posix_roots, ")(?:/", terminator, ")*", path_boundary
    ),
    paste0("(?<![A-Za-z0-9:/])/(?:", terminator, "/)+", terminator)
  )
}

.run_contract_has_local_path = function(text) {
  patterns = .run_contract_local_path_patterns()
  any(vapply(patterns, grepl, logical(length(text)), x = text, perl = TRUE))
}

.run_contract_redact_paths = function(text) {
  patterns = .run_contract_local_path_patterns()
  for (pattern in patterns) {
    text = gsub(pattern, "<REDACTED_LOCAL_PATH>", text, perl = TRUE)
  }
  text
}

.run_contract_header = function(path, extension) {
  if (!extension %in% c("csv", "tsv")) {
    return(character())
  }
  line = readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(line)) {
    return(character())
  }
  separator = if (extension == "tsv") "\t" else ","
  parsed = tryCatch(
    scan(text = line, what = character(), sep = separator, quote = "\"", quiet = TRUE),
    error = function(error) character()
  )
  sub("^\\ufeff", "", parsed)
}

.run_contract_raw_signature = function(header, text = character(), study = "") {
  header = trimws(header)
  shild_header = length(intersect(header, .run_contract_shild_columns)) >= 4L ||
    ("life_satisfaction" %in% header && length(intersect(header, c("A2", "D2", "D8", "M8"))) >= 2L)
  pisa_header = sum(grepl("^PV(?:[1-9]|10)READ$", header)) >= 3L ||
    all(c("CNTSTUID", "CNTSCHID", "PV1READ") %in% header)
  if (shild_header || pisa_header) {
    return(TRUE)
  }
  if (!length(text)) {
    return(FALSE)
  }
  collapsed = paste(text, collapse = "\n")
  shild_keys = sum(vapply(
    .run_contract_shild_columns,
    function(column) grepl(paste0("\\\"", column, "\\\"\\s*:"), collapsed, perl = TRUE),
    logical(1L)
  ))
  pv_keys = sum(vapply(
    paste0("PV", 1:10, "READ"),
    function(column) grepl(paste0("\\\"", column, "\\\"\\s*:"), collapsed, perl = TRUE),
    logical(1L)
  ))
  shild_keys >= 4L || pv_keys >= 3L
}

.run_contract_sensitive_object_signature = function(text) {
  if (!length(text)) {
    return(FALSE)
  }
  collapsed = paste(text, collapse = "\n")
  has_key = function(key) {
    grepl(paste0("\\\"", key, "\\\"\\s*:"), collapsed, perl = TRUE, ignore.case = TRUE)
  }
  backend_object = has_key("backend")
  fitted_object = has_key("task") && any(vapply(
    c("model", "models", "learner", "state", "data"),
    has_key,
    logical(1L)
  ))
  truth_predictions = any(vapply(
    .run_contract_truth_columns,
    has_key,
    logical(1L)
  )) && any(vapply(
    .run_contract_fitted_columns,
    has_key,
    logical(1L)
  ))
  row_predictions = any(vapply(c("row_id", "row_ids", "case_id"), has_key, logical(1L))) &&
    any(vapply(c("prediction", "predictions", "probability", "truth", "response"), has_key, logical(1L)))
  split_membership = any(vapply(
    c("train_sets", "test_sets", "row_map", "assignments"),
    has_key,
    logical(1L)
  ))
  backend_object || fitted_object || truth_predictions || row_predictions || split_membership
}

.run_contract_disguised_risky_name = function(path) {
  risky = paste(.run_contract_risky_extensions, collapse = "|")
  grepl(paste0("\\.(?:", risky, ")(?:\\.|$)"), basename(path), ignore.case = TRUE, perl = TRUE)
}

.run_contract_binary_text_runs = function(bytes, minimum_length = 8L) {
  values = as.integer(bytes)
  printable = values >= 32L & values <= 126L
  runs = rle(printable)
  ends = cumsum(runs$lengths)
  starts = ends - runs$lengths + 1L
  keep = which(runs$values & runs$lengths >= minimum_length)
  if (!length(keep)) {
    return(character())
  }
  vapply(
    keep,
    function(index) rawToChar(bytes[starts[[index]]:ends[[index]]]),
    character(1L)
  )
}

.run_contract_binary_has_local_path = function(bytes) {
  posix_roots = c(
    "Applications", "Library", "System", "Users", "bin", "data", "etc", "home", "media", "mnt", "opt",
    "private", "sbin", "srv", "tmp", "usr", "var", "Volumes", "workspace", "workspaces"
  )
  prefixes = c(paste0("/", posix_roots, "/"), "file:///", "file://localhost/")
  fixed_match = any(vapply(prefixes, function(prefix) {
    length(grepRaw(prefix, bytes, fixed = TRUE)) > 0L
  }, logical(1L)))
  text = .run_contract_binary_text_runs(bytes)
  if (!length(text)) {
    return(fixed_match)
  }
  patterns = c(
    "(?i)(?<![A-Za-z0-9])\\b[A-Z]:[\\\\/][A-Za-z0-9._$~ +%-]{2,}(?:[\\\\/][A-Za-z0-9._$~ +%-]+)*",
    "(?i)(?<![A-Za-z0-9\\\\])\\\\\\\\[A-Za-z0-9._$-]{2,}\\\\[A-Za-z0-9._$~ +%-]{2,}",
    "(?i)file://[A-Za-z0-9._$-]+/[A-Za-z0-9._$~+%/-]{2,}"
  )
  fixed_match || any(vapply(patterns, grepl, logical(length(text)), x = text, perl = TRUE))
}

.run_contract_hash_id = function(value, column, run_salt) {
  if (is.na(value) || !nzchar(as.character(value))) {
    return(NA_character_)
  }
  material = paste(run_salt, column, as.character(value), sep = "::")
  paste0("id_", substr(digest::digest(material, algo = "sha256", serialize = FALSE), 1L, 20L))
}

.run_contract_pseudonymize_fold_table = function(source, destination, extension, run_salt) {
  separator = if (extension == "tsv") "\t" else ","
  table = utils::read.table(
    source,
    header = TRUE,
    sep = separator,
    quote = "\"",
    comment.char = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fill = TRUE
  )
  id_columns = intersect(names(table), .run_contract_identifier_columns)
  if (!length(id_columns)) {
    .run_contract_stop(
      "Fold table was marked for pseudonymization but contains no recognized identifier column: ",
      source
    )
  }
  retained_id_columns = character()
  for (column in id_columns) {
    duplicate_identifier = any(vapply(retained_id_columns, function(retained) {
      identical(as.character(table[[column]]), as.character(table[[retained]]))
    }, logical(1L)))
    if (duplicate_identifier) {
      table[[column]] = NULL
    } else {
      retained_id_columns = c(retained_id_columns, column)
    }
  }
  for (column in retained_id_columns) {
    table[[column]] = vapply(
      table[[column]],
      .run_contract_hash_id,
      character(1L),
      column = column,
      run_salt = run_salt
    )
  }
  sensitive_assignment_columns = intersect(
    names(table),
    c("stratum", "truth", "outcome", "label", "response", "target", "class")
  )
  if (length(sensitive_assignment_columns)) {
    table[sensitive_assignment_columns] = NULL
  }
  order_columns = c(retained_id_columns, setdiff(names(table), retained_id_columns))
  if (nrow(table) && length(order_columns)) {
    table = table[do.call(order, c(table[order_columns], list(na.last = TRUE))), , drop = FALSE]
    rownames(table) = NULL
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    table,
    file = destination,
    sep = separator,
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    qmethod = "double",
    na = "",
    fileEncoding = "UTF-8"
  )
  lines = readLines(destination, warn = FALSE, encoding = "UTF-8")
  writeLines(.run_contract_redact_paths(lines), destination, useBytes = TRUE)
  invisible(retained_id_columns)
}

.run_contract_row_prediction_signature = function(header, relative) {
  lower_header = tolower(header)
  prediction_columns = lower_header[
    lower_header %in% tolower(.run_contract_prediction_columns) | grepl("^prob[._]", lower_header)
  ]
  truth_columns = lower_header[lower_header %in% tolower(.run_contract_truth_columns)]
  fitted_columns = lower_header[
    lower_header %in% tolower(.run_contract_fitted_columns) | grepl("^prob[._]", lower_header)
  ]
  observation_level = grepl(
    "(?:^|[/_.-])(observation|observations|row|rows|case|cases|individual|oof|out.of.fold)(?:[/_.-]|$)",
    relative,
    ignore.case = TRUE,
    perl = TRUE
  )
  (length(truth_columns) && length(fitted_columns)) ||
    (observation_level && length(unique(prediction_columns)) >= 2L)
}

.run_contract_aggregate_calibration_signature = function(header, relative) {
  lower_header = tolower(header)
  identical(sort(lower_header), sort(c("bin", "n", "observed", "predicted"))) &&
    grepl("calibration.*curve|curve.*calibration", relative, ignore.case = TRUE, perl = TRUE)
}

.run_contract_verify_aggregate_calibration = function(path, extension, relative) {
  separator = if (extension == "tsv") "\t" else ","
  table = utils::read.table(
    path,
    header = TRUE,
    sep = separator,
    quote = "\"",
    comment.char = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fill = TRUE
  )
  valid = nrow(table) >= 1L && nrow(table) <= 100L &&
    all(is.finite(table$n) & table$n >= 1 & table$n == floor(table$n)) &&
    all(is.finite(table$observed)) && all(is.finite(table$predicted))
  if (!valid) {
    .run_contract_stop("Aggregate calibration curve failed structural validation: ", relative)
  }
  invisible(TRUE)
}

.run_contract_classify_upload_file = function(path, relative, study) {
  extension = tolower(tools::file_ext(path))
  components = strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]]
  lower_components = tolower(components)
  if (basename(relative) %in% c("MANIFEST.csv", "UPLOAD_MANIFEST.csv", "run_manifest.csv")) {
    return(list(action = "exclude", reason = "private manifest is replaced by an upload manifest"))
  }
  if (any(lower_components %in% .run_contract_risky_directories)) {
    return(list(action = "exclude", reason = "private object/raw/prediction directory"))
  }
  if (extension %in% .run_contract_risky_extensions) {
    return(list(action = "exclude", reason = paste0("risky extension .", extension)))
  }
  if (.run_contract_disguised_risky_name(path)) {
    return(list(action = "exclude", reason = "risky serialized extension is disguised by a second suffix"))
  }
  if (!extension %in% .run_contract_allowed_extensions) {
    return(list(action = "exclude", reason = paste0("extension is not allowlisted: .", extension)))
  }
  if (extension %in% .run_contract_text_extensions && !.run_contract_is_text_file(path)) {
    return(list(action = "exclude", reason = "binary content under a text extension"))
  }
  if (!extension %in% .run_contract_text_extensions) {
    return(list(action = "copy_binary", reason = "allowlisted figure"))
  }

  header = .run_contract_header(path, extension)
  lower_header = tolower(header)
  identifier_columns = header[lower_header %in% tolower(.run_contract_identifier_columns)]
  prediction_columns = header[lower_header %in% tolower(.run_contract_prediction_columns) |
    grepl("^prob[._]", lower_header)]
  fold_table = grepl("(?:^|[/_.-])(fold|split|assignment)(?:[/_.-]|$)", relative, ignore.case = TRUE, perl = TRUE)
  aggregate_calibration = .run_contract_aggregate_calibration_signature(header, relative)
  if (!aggregate_calibration && ((length(identifier_columns) && length(prediction_columns)) ||
      .run_contract_row_prediction_signature(header, relative))) {
    return(list(action = "exclude", reason = "row-level prediction table"))
  }
  if (.run_contract_raw_signature(header, study = study)) {
    return(list(action = "exclude", reason = "raw analysis-data column signature"))
  }
  if (length(identifier_columns) && fold_table) {
    return(list(action = "pseudonymize_fold", reason = "exact fold identifiers require pseudonymization"))
  }
  if (length(identifier_columns)) {
    return(list(action = "exclude", reason = "row-level identifier table outside a fold assignment"))
  }
  list(action = "copy_text", reason = "allowlisted aggregate/text artifact")
}

.run_contract_sanitization_report_row = function(relative, action, reason) {
  data.frame(
    source_path = relative,
    action = action,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

#' Create and verify a separate upload-safe derivative of a private run.
#'
#' The private directory is read-only to this function. Serialized objects,
#' models, task/backend/raw directories, row-level predictions, direct row
#' identifiers, and non-allowlisted file types are omitted. Fold identifiers
#' are replaced by deterministic run-specific hashes, duplicate identifiers and
#' row-level strata are removed, and rows are sorted by pseudonym. Text paths are redacted.
create_sanitized_upload_bundle = function(
    private_dir,
    upload_dir,
    study,
    run_salt,
    expected_artifacts = character(),
    overwrite = FALSE) {
  .run_contract_scalar_string(private_dir, "private_dir")
  .run_contract_scalar_string(upload_dir, "upload_dir")
  .run_contract_scalar_string(study, "study")
  .run_contract_scalar_string(run_salt, "run_salt")
  if (nchar(run_salt) < 16L) {
    .run_contract_stop("`run_salt` must contain at least 16 characters.")
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    .run_contract_stop("Package `digest` is required to pseudonymize fold identifiers.")
  }
  if (!dir.exists(private_dir)) {
    .run_contract_stop("Private run directory does not exist: ", private_dir)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    .run_contract_stop("`overwrite` must be TRUE or FALSE.")
  }
  private_dir = normalizePath(private_dir, winslash = "/", mustWork = TRUE)
  upload_parent = dirname(normalizePath(upload_dir, winslash = "/", mustWork = FALSE))
  dir.create(upload_parent, recursive = TRUE, showWarnings = FALSE)
  upload_parent = normalizePath(upload_parent, winslash = "/", mustWork = TRUE)
  upload_dir = file.path(upload_parent, basename(upload_dir))
  if (identical(upload_dir, private_dir) || startsWith(upload_dir, paste0(private_dir, "/")) ||
      startsWith(private_dir, paste0(upload_dir, "/"))) {
    .run_contract_stop("Upload and private run directories must be separate and non-nested.")
  }
  if ((file.exists(upload_dir) || dir.exists(upload_dir)) && !overwrite) {
    .run_contract_stop("Upload directory already exists: ", upload_dir)
  }
  if (length(expected_artifacts)) {
    validate_expected_artifacts(private_dir, expected_artifacts)
  }

  temporary = tempfile(
    pattern = paste0(".", .run_contract_safe_name(basename(upload_dir)), "-"),
    tmpdir = upload_parent
  )
  if (!dir.create(temporary, recursive = FALSE, showWarnings = FALSE)) {
    .run_contract_stop("Could not create temporary upload directory.")
  }
  complete = FALSE
  on.exit({
    if (!complete && dir.exists(temporary)) {
      unlink(temporary, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  files = .run_contract_list_files(private_dir)
  relative = vapply(files, .run_contract_relative_path, character(1L), root = private_dir)
  report = vector("list", length(files))
  for (index in seq_along(files)) {
    source = files[[index]]
    rel = relative[[index]]
    decision = .run_contract_classify_upload_file(source, rel, study)
    destination = file.path(temporary, rel)
    action = decision$action
    if (action == "copy_binary") {
      bytes = readBin(source, what = "raw", n = as.numeric(file.info(source)$size))
      if (.run_contract_binary_has_local_path(bytes)) {
        .run_contract_stop("A binary figure contains an absolute local path and cannot be safely redacted: ", rel)
      }
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(source, destination, overwrite = FALSE, copy.mode = FALSE, copy.date = FALSE)) {
        .run_contract_stop("Could not copy upload artifact: ", rel)
      }
    } else if (action == "copy_text") {
      lines = readLines(source, warn = FALSE, encoding = "UTF-8")
      if (.run_contract_raw_signature(character(), lines, study) ||
          .run_contract_sensitive_object_signature(lines)) {
        action = "exclude"
        decision$reason = "raw analysis-data or fitted-object key signature"
      } else {
        dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
        writeLines(.run_contract_redact_paths(lines), destination, useBytes = TRUE)
      }
    } else if (action == "pseudonymize_fold") {
      extension = tolower(tools::file_ext(source))
      .run_contract_pseudonymize_fold_table(source, destination, extension, run_salt)
    }
    report[[index]] = .run_contract_sanitization_report_row(rel, action, decision$reason)
  }
  report = do.call(rbind, report)
  .run_contract_write_csv(report, file.path(temporary, "SANITIZATION_REPORT.csv"))
  writeLines(
    c(
      "# Sanitized analysis upload bundle",
      "",
      paste0("Study: `", study, "`"),
      "",
      "This directory is a derived upload bundle. The full private run remains in the analyst environment.",
      "Serialized objects, raw/task/backend/model content, and row-level predictions are intentionally excluded.",
      paste(
        "When an allowlisted fold-assignment table contains direct identifiers, those identifiers are replaced",
        "by deterministic run-specific hashes, duplicate identifiers and row-level strata are removed, and rows",
        "are reordered by pseudonym. Other shareable aggregate or pseudonymous tables retain their analytic",
        "ordering and strata. Local absolute paths are redacted from all copied text files."
      ),
      "",
      paste(
        "Checksum, sanitization, and structural checks do not by themselves make results manuscript-ready.",
        "Consult OUTPUT_AUDIT.md: release eligibility separately requires publication mode, an affirmative",
        "publication-settings declaration, manuscript readiness, no logged warnings or errors, and a clean",
        "structural audit."
      )
    ),
    file.path(temporary, "UPLOAD_README.md"),
    useBytes = TRUE
  )

  if (length(expected_artifacts)) {
    validate_expected_artifacts(temporary, expected_artifacts)
  }
  manifest_path = file.path(temporary, "UPLOAD_MANIFEST.csv")
  write_recursive_manifest(temporary, manifest_path, exclude = "UPLOAD_MANIFEST.csv")
  verify_sanitized_upload_bundle(temporary, study, manifest_path)

  if (dir.exists(upload_dir) || file.exists(upload_dir)) {
    unlink(upload_dir, recursive = TRUE, force = TRUE)
  }
  if (!file.rename(temporary, upload_dir)) {
    .run_contract_stop("Could not atomically publish sanitized upload directory: ", upload_dir)
  }
  complete = TRUE
  upload_dir = normalizePath(upload_dir, winslash = "/", mustWork = TRUE)
  verify_sanitized_upload_bundle(
    upload_dir,
    study,
    file.path(upload_dir, "UPLOAD_MANIFEST.csv")
  )
  invisible(upload_dir)
}

.run_contract_verify_upload_table = function(path, relative, study) {
  extension = tolower(tools::file_ext(path))
  header = .run_contract_header(path, extension)
  lower_header = tolower(header)
  identifiers = header[lower_header %in% tolower(.run_contract_identifier_columns)]
  predictions = header[lower_header %in% tolower(.run_contract_prediction_columns) |
    grepl("^prob[._]", lower_header)]
  aggregate_calibration = .run_contract_aggregate_calibration_signature(header, relative)
  if (aggregate_calibration) {
    .run_contract_verify_aggregate_calibration(path, extension, relative)
  }
  if (!aggregate_calibration && ((length(identifiers) && length(predictions)) ||
      .run_contract_row_prediction_signature(header, relative))) {
    .run_contract_stop("Sanitized bundle contains a row-level prediction table: ", relative)
  }
  if (.run_contract_raw_signature(header, study = study)) {
    .run_contract_stop("Sanitized bundle contains a raw analysis-data column signature: ", relative)
  }
  if (!length(identifiers)) {
    return(invisible(TRUE))
  }
  fold_table = grepl("(?:^|[/_.-])(fold|split|assignment)(?:[/_.-]|$)", relative, ignore.case = TRUE, perl = TRUE)
  if (!fold_table) {
    .run_contract_stop("Sanitized bundle contains direct row identifiers outside a fold table: ", relative)
  }
  separator = if (extension == "tsv") "\t" else ","
  table = utils::read.table(
    path,
    header = TRUE,
    sep = separator,
    quote = "\"",
    comment.char = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fill = TRUE
  )
  for (column in identifiers) {
    values = as.character(table[[column]])
    values = values[!is.na(values) & nzchar(values)]
    if (length(values) && any(!grepl("^id_[0-9a-f]{20}$", values))) {
      .run_contract_stop("Fold identifiers were not pseudonymized in ", relative, ": ", column)
    }
  }
  invisible(TRUE)
}

#' Verify that an upload bundle contains only sanitized, allowlisted artifacts.
verify_sanitized_upload_bundle = function(
    upload_dir,
    study,
    manifest_path = file.path(upload_dir, "UPLOAD_MANIFEST.csv")) {
  .run_contract_scalar_string(upload_dir, "upload_dir")
  .run_contract_scalar_string(study, "study")
  if (!dir.exists(upload_dir)) {
    .run_contract_stop("Upload directory does not exist: ", upload_dir)
  }
  upload_dir = normalizePath(upload_dir, winslash = "/", mustWork = TRUE)
  files = .run_contract_list_files(upload_dir)
  relative = vapply(files, .run_contract_relative_path, character(1L), root = upload_dir)
  symlinks = nzchar(Sys.readlink(files))
  if (any(symlinks)) {
    .run_contract_stop("Sanitized upload bundle contains symbolic links: ", paste(relative[symlinks], collapse = ", "))
  }
  for (index in seq_along(files)) {
    path = files[[index]]
    rel = relative[[index]]
    extension = tolower(tools::file_ext(path))
    components = tolower(strsplit(gsub("\\\\", "/", rel), "/", fixed = TRUE)[[1L]])
    if (extension %in% .run_contract_risky_extensions ||
        !extension %in% .run_contract_allowed_extensions ||
        .run_contract_disguised_risky_name(path)) {
      .run_contract_stop("Sanitized upload bundle contains a risky file type: ", rel)
    }
    if (any(components %in% .run_contract_risky_directories)) {
      .run_contract_stop("Sanitized upload bundle contains a risky directory: ", rel)
    }
    if (extension %in% .run_contract_text_extensions) {
      if (!.run_contract_is_text_file(path)) {
        .run_contract_stop("Sanitized upload bundle contains binary content under a text extension: ", rel)
      }
      lines = readLines(path, warn = FALSE, encoding = "UTF-8")
      if (.run_contract_has_local_path(lines)) {
        .run_contract_stop("Sanitized upload bundle contains an absolute local path: ", rel)
      }
      if (.run_contract_raw_signature(character(), lines, study)) {
        .run_contract_stop("Sanitized upload bundle contains raw record-like content: ", rel)
      }
      if (.run_contract_sensitive_object_signature(lines)) {
        .run_contract_stop("Sanitized upload bundle contains fitted-object or row-prediction content: ", rel)
      }
      if (extension %in% c("csv", "tsv")) {
        .run_contract_verify_upload_table(path, rel, study)
      }
    } else {
      bytes = readBin(path, what = "raw", n = as.numeric(file.info(path)$size))
      if (.run_contract_binary_has_local_path(bytes)) {
        .run_contract_stop("Sanitized binary artifact contains an absolute local path: ", rel)
      }
    }
  }
  verify_recursive_manifest(upload_dir, manifest_path)
  invisible(TRUE)
}
