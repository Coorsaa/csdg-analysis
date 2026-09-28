#!/usr/bin/env Rscript
# Structural publication-output audit ---------------------------------------
#
# This script inspects a completed run without changing its analytical outputs.
# It writes only the four audit reports documented in .publication_audit_report_paths.

.publication_audit_version = "1.3.0"

.publication_audit_report_paths = c(
  "provenance/output_audit.csv",
  "provenance/output_audit_summary.csv",
  "provenance/figure_inventory.csv",
  "OUTPUT_AUDIT.md"
)

.publication_audit_risky_extensions = c(
  "rds", "rda", "rdata", "qs", "fst", "feather", "parquet", "sav", "dta", "sas7bdat", "pkl",
  "pickle", "joblib", "model", "bin", "sqlite", "sqlite3", "db", "zip", "tar", "gz", "bz2", "xz",
  "7z", "xlsx", "xls"
)

.publication_audit_risky_directories = c(
  "objects", "models", "model", "tasks", "task", "backends", "backend", "raw", "raw_data", "predictions",
  "prediction", "oof_predictions"
)

.publication_audit_identifier_columns = c(
  "row_id", "row_ids", "case_id", "participant_id", "respondent_id", "student_id", "subject_id", "person_id",
  "cntstuid", "cntschid", "school_id", "cluster"
)

.publication_audit_figure_latex_metadata_path = "provenance/figure_latex_metadata.csv"

.publication_audit_figure_latex_metadata_columns = c(
  "study", "figure_id", "figure_group_id", "is_grouped_figure", "latex_group_id",
  "panel_id", "panel_order", "panel_heading", "pdf_path", "png_path", "title", "subtitle", "caption",
  "group_title", "group_subtitle", "group_caption", "source_tables", "generator_script", "generator_function",
  "width_in", "height_in", "role", "estimand_or_scope", "uncertainty_semantics", "privacy_scope",
  "latex_assembly_note"
)

.publication_audit_empty = function() {
  data.frame(
    check_id = character(),
    category = character(),
    path = character(),
    status = character(),
    severity = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

.publication_audit_row = function(check_id, category, path, status, severity, message) {
  data.frame(
    check_id = as.character(check_id),
    category = as.character(category),
    path = as.character(path),
    status = as.character(status),
    severity = as.character(severity),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

.publication_audit_relative = function(path, root) {
  root = paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, root)) {
    stop("An audited path is outside the declared run directory: ", path, call. = FALSE)
  }
  substring(path, nchar(root) + 1L)
}

.publication_audit_files = function(root) {
  paths = list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  paths = paths[file.info(paths)$isdir %in% FALSE]
  relative = vapply(paths, .publication_audit_relative, character(1L), root = root)
  keep = !relative %in% .publication_audit_report_paths
  paths[keep][order(relative[keep])]
}

.publication_audit_read_csv = function(path) {
  old_options = options(warn = 2L)
  on.exit(options(old_options), add = TRUE)
  tryCatch(
    list(
      value = utils::read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        comment.char = "",
        fill = FALSE
      ),
      error = NULL
    ),
    error = function(condition) list(value = NULL, error = conditionMessage(condition))
  )
}

.publication_audit_read_json = function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(value = NULL, error = "Package `jsonlite` is unavailable."))
  }
  tryCatch(
    list(value = jsonlite::fromJSON(path, simplifyVector = FALSE), error = NULL),
    error = function(condition) list(value = NULL, error = conditionMessage(condition))
  )
}

.publication_audit_boolean = function(value) {
  text = tolower(trimws(as.character(value)))
  result = rep(NA, length(text))
  result[text %in% c("true", "t", "1", "yes")] = TRUE
  result[text %in% c("false", "f", "0", "no")] = FALSE
  result
}

.publication_audit_state = function(value) {
  if (!length(value)) {
    return("UNKNOWN")
  }
  if (anyNA(value)) {
    return("INVALID")
  }
  if (length(unique(value)) > 1L) {
    return("CONFLICT")
  }
  if (isTRUE(value[[1L]])) "TRUE" else "FALSE"
}

.publication_audit_mode = function(value) {
  value = tolower(trimws(as.character(value)))
  value[grepl("quick|smoke|nonpublication", value)] = "smoke_test"
  value[value %in% c("publication", "full_publication")] = "publication"
  value[!nzchar(value)] = NA_character_
  value
}

.publication_audit_release_context = function(files, root, structural_status) {
  relative = vapply(files, .publication_audit_relative, character(1L), root = root)
  mode_values = character()
  readiness_values = logical()
  publication_setting_values = logical()
  declared_blockers = character()
  metadata_status = "MISSING"
  conditions_status = "MISSING"
  warning_count = 0L
  error_count = 0L

  metadata_index = which(relative == "provenance/run_metadata.csv")
  if (length(metadata_index) == 1L) {
    parsed = .publication_audit_read_csv(files[[metadata_index]])
    if (is.null(parsed$error) && all(c("key", "value") %in% names(parsed$value))) {
      keys = tolower(trimws(as.character(parsed$value$key)))
      mode_values = c(mode_values, parsed$value$value[keys == "mode"])
      metadata_status = "PARSED"
    } else {
      metadata_status = "INVALID"
    }
  } else if (length(metadata_index) > 1L) {
    metadata_status = "CONFLICT"
  }

  mode_indices = which(grepl("(^|/)[^/]*run_mode\\.csv$", relative, perl = TRUE))
  for (index in mode_indices) {
    parsed = .publication_audit_read_csv(files[[index]])
    if (!is.null(parsed$error)) {
      next
    }
    columns = tolower(names(parsed$value))
    mode_column = match("mode", columns)
    if (!is.na(mode_column)) {
      mode_values = c(mode_values, parsed$value[[mode_column]])
    }
    readiness_column = match(c("manuscript_ready", "manuscript_eligible"), columns, nomatch = 0L)
    readiness_column = readiness_column[readiness_column > 0L]
    if (length(readiness_column)) {
      readiness_values = c(
        readiness_values,
        unlist(lapply(readiness_column, function(column) .publication_audit_boolean(parsed$value[[column]])))
      )
    }
    publication_column = match("publication_settings", columns)
    if (!is.na(publication_column)) {
      publication_setting_values = c(
        publication_setting_values,
        .publication_audit_boolean(parsed$value[[publication_column]])
      )
    }
    blocker_column = match("eligibility_blocker", columns)
    if (!is.na(blocker_column)) {
      blockers = trimws(as.character(parsed$value[[blocker_column]]))
      declared_blockers = c(declared_blockers, blockers[!is.na(blockers) & nzchar(blockers)])
    }
  }

  normalized_modes = .publication_audit_mode(mode_values)
  modes = unique(normalized_modes)
  run_mode = if (!length(normalized_modes)) {
    "UNKNOWN"
  } else if (anyNA(normalized_modes)) {
    "INVALID"
  } else if (length(modes) > 1L) {
    "CONFLICT"
  } else {
    toupper(modes[[1L]])
  }
  manuscript_ready = .publication_audit_state(readiness_values)
  publication_settings = .publication_audit_state(publication_setting_values)

  condition_indices = which(relative == "logs/conditions.csv")
  if (length(condition_indices) == 1L) {
    parsed = .publication_audit_read_csv(files[[condition_indices]])
    if (is.null(parsed$error) && "type" %in% tolower(names(parsed$value))) {
      type_column = match("type", tolower(names(parsed$value)))
      types = tolower(trimws(as.character(parsed$value[[type_column]])))
      known_types = c("message", "warning", "error")
      if (anyNA(types) || any(!types %in% known_types)) {
        conditions_status = "INVALID"
      } else {
        warning_count = sum(types == "warning")
        error_count = sum(types == "error")
        conditions_status = "PARSED"
      }
    } else {
      conditions_status = "INVALID"
    }
  } else if (length(condition_indices) > 1L) {
    conditions_status = "CONFLICT"
  }

  blockers = character()
  if (!identical(structural_status, "PASS")) {
    blockers = c(blockers, paste0("structural audit status is ", structural_status))
  }
  if (!identical(run_mode, "PUBLICATION")) {
    blockers = c(blockers, paste0("run mode is ", run_mode))
  }
  if (!identical(publication_settings, "TRUE")) {
    blockers = c(blockers, paste0("publication settings are ", publication_settings))
  }
  if (!identical(manuscript_ready, "TRUE")) {
    blockers = c(blockers, paste0("manuscript readiness is ", manuscript_ready))
  }
  if (!identical(metadata_status, "PARSED")) {
    blockers = c(blockers, paste0("run metadata status is ", metadata_status))
  }
  if (!identical(conditions_status, "PARSED")) {
    blockers = c(blockers, paste0("conditions log status is ", conditions_status))
  }
  if (warning_count > 0L) {
    blockers = c(blockers, paste0(warning_count, " warning(s) recorded in logs/conditions.csv"))
  }
  if (error_count > 0L) {
    blockers = c(blockers, paste0(error_count, " error(s) recorded in logs/conditions.csv"))
  }
  blockers = unique(c(blockers, declared_blockers))

  list(
    release_eligibility = if (length(blockers)) "NOT_ELIGIBLE" else "ELIGIBLE",
    run_mode = run_mode,
    publication_settings = publication_settings,
    manuscript_ready = manuscript_ready,
    metadata_status = metadata_status,
    conditions_status = conditions_status,
    logged_warnings = warning_count,
    logged_errors = error_count,
    eligibility_blockers = if (length(blockers)) paste(blockers, collapse = " | ") else "none"
  )
}

.publication_audit_interval_pairs = function(columns) {
  lower = tolower(columns)
  replacement_pair = function(pattern, substitute) {
    selected = grepl(pattern, lower, perl = TRUE)
    if (!any(selected)) {
      return(data.frame(low = character(), high_key = character(), stringsAsFactors = FALSE))
    }
    data.frame(
      low = columns[selected],
      high_key = sub(pattern, substitute, lower[selected], perl = TRUE),
      stringsAsFactors = FALSE
    )
  }
  definitions = list(
    c("(^|_)ci_low$", "\\1ci_high"),
    c("(^|_)lower$", "\\1upper"),
    c("(^|_)low$", "\\1high"),
    c("(^|_)lwr$", "\\1upr"),
    c("(^|_)left$", "\\1right"),
    c("(^|_)bottom$", "\\1top"),
    c("^q05(?=_|$)", "q95"),
    c("^q10(?=_|$)", "q90"),
    c("^q025(?=_|$)", "q975"),
    c("^q25(?=_|$)", "q75"),
    c("^minimum(?=_|$)", "maximum"),
    c("^min(?=_|$)", "max")
  )
  candidates = do.call(
    rbind,
    lapply(definitions, function(definition) replacement_pair(definition[[1L]], definition[[2L]]))
  )
  high_index = match(candidates$high_key, lower)
  keep = !is.na(high_index)
  if (!any(keep)) {
    return(data.frame(low = character(), high = character(), stringsAsFactors = FALSE))
  }
  unique(data.frame(low = candidates$low[keep], high = columns[high_index[keep]], stringsAsFactors = FALSE))
}

.publication_audit_numeric = function(value) {
  if (is.numeric(value) || is.integer(value)) {
    return(as.numeric(value))
  }
  text = trimws(as.character(value))
  text[text == ""] = NA_character_
  numeric = suppressWarnings(as.numeric(text))
  valid = is.na(text) | !is.na(numeric)
  if (!all(valid)) NULL else numeric
}

.publication_audit_intervals = function(table, relative) {
  pairs = .publication_audit_interval_pairs(names(table))
  checks = .publication_audit_empty()
  if (!nrow(pairs)) {
    return(list(checks = checks, n_pairs = 0L))
  }
  for (index in seq_len(nrow(pairs))) {
    low_name = pairs$low[[index]]
    high_name = pairs$high[[index]]
    low = .publication_audit_numeric(table[[low_name]])
    high = .publication_audit_numeric(table[[high_name]])
    check_path = paste0(relative, "::", low_name, "<=", high_name)
    if (is.null(low) || is.null(high)) {
      checks = rbind(
        checks,
        .publication_audit_row(
          "interval_order", "tabular", check_path, "not_checked", "warning",
          "Interval-like columns are not consistently numeric."
        )
      )
      next
    }
    incomplete = xor(is.na(low), is.na(high))
    reversed = !is.na(low) & !is.na(high) & low > high
    if (any(incomplete) || any(reversed)) {
      details = c(
        if (any(reversed)) paste0(sum(reversed), " reversed row(s)") else character(),
        if (any(incomplete)) paste0(sum(incomplete), " one-sided missing row(s)") else character()
      )
      checks = rbind(
        checks,
        .publication_audit_row(
          "interval_order", "tabular", check_path, "fail", "error",
          paste(details, collapse = "; ")
        )
      )
    } else {
      compared = sum(!is.na(low) & !is.na(high))
      checks = rbind(
        checks,
        .publication_audit_row(
          "interval_order", "tabular", check_path, "pass", "info",
          paste0("Ordering is valid for ", compared, " complete interval row(s).")
        )
      )
    }
  }
  list(checks = checks, n_pairs = nrow(pairs))
}

.publication_audit_identifier_check = function(table, relative) {
  columns = names(table)[tolower(names(table)) %in% .publication_audit_identifier_columns]
  if (!length(columns)) {
    return(.publication_audit_empty())
  }
  invalid = vapply(columns, function(column) {
    values = unique(trimws(as.character(table[[column]])))
    values = values[!is.na(values) & nzchar(values)]
    length(values) && !all(grepl("^id_[0-9a-f]{20}$", values))
  }, logical(1L))
  if (any(invalid)) {
    return(.publication_audit_row(
      "direct_identifier", "privacy", relative, "fail", "error",
      paste0("Direct or nonstandard identifiers remain in: ", paste(columns[invalid], collapse = ", "), ".")
    ))
  }
  .publication_audit_row(
    "direct_identifier", "privacy", relative, "pass", "info",
    paste0("Identifier columns use run-specific pseudonyms: ", paste(columns, collapse = ", "), ".")
  )
}

.publication_audit_table_privacy = function(table, relative) {
  checks = .publication_audit_identifier_check(table, relative)
  header = names(table)
  lower = tolower(header)
  shild_columns = tolower(c(
    "life_satisfaction", "gender", "age", "A2", "D2", "D8", "M8", "audit_gender", "audit_age_group"
  ))
  shild_raw = length(intersect(lower, shild_columns)) >= 4L ||
    ("life_satisfaction" %in% lower && length(intersect(lower, c("a2", "d2", "d8", "m8"))) >= 2L)
  pisa_raw = sum(grepl("^pv(?:[1-9]|10)read$", lower)) >= 3L ||
    all(c("cntstuid", "cntschid", "pv1read") %in% lower)
  if (shild_raw || pisa_raw) {
    checks = rbind(
      checks,
      .publication_audit_row(
        "raw_analysis_table", "privacy", relative, "fail", "error",
        "The column signature is consistent with row-level raw analysis data."
      )
    )
  }

  truth_columns = c("truth", "outcome", "observed", "label", "response")
  fitted_columns = c("prediction", "predictions", "predicted", "probability", "prob", "risk", "fitted", "residual")
  prediction_columns = c(fitted_columns, "response", "truth")
  aggregate_calibration = identical(sort(lower), sort(c("bin", "n", "observed", "predicted"))) &&
    grepl("calibration.*curve|curve.*calibration", relative, ignore.case = TRUE, perl = TRUE)
  observation_level = grepl(
    "(?:^|[/_.-])(observation|observations|row|rows|case|cases|individual|oof|out.of.fold)(?:[/_.-]|$)",
    relative,
    ignore.case = TRUE,
    perl = TRUE
  )
  row_predictions = (length(intersect(lower, truth_columns)) && length(intersect(lower, fitted_columns))) ||
    (observation_level && length(intersect(lower, prediction_columns)) >= 2L)
  if (!aggregate_calibration && row_predictions) {
    checks = rbind(
      checks,
      .publication_audit_row(
        "row_level_predictions", "privacy", relative, "fail", "error",
        "The table contains a row-level truth/prediction signature."
      )
    )
  }
  checks
}

.publication_audit_json_privacy = function(path, relative) {
  text = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  has_key = function(key) {
    grepl(paste0('"', key, '"\\s*:'), text, ignore.case = TRUE, perl = TRUE)
  }
  fitted_keys = c("prediction", "predictions", "predicted", "probability", "prob", "risk", "fitted")
  truth_keys = c("truth", "outcome", "observed", "label", "response")
  sensitive = has_key("backend") ||
    (has_key("task") && any(vapply(c("model", "models", "learner", "state", "data"), has_key, logical(1L)))) ||
    (any(vapply(truth_keys, has_key, logical(1L))) && any(vapply(fitted_keys, has_key, logical(1L)))) ||
    (any(vapply(c("row_id", "row_ids", "case_id"), has_key, logical(1L))) &&
      any(vapply(c(fitted_keys, truth_keys), has_key, logical(1L)))) ||
    any(vapply(c("train_sets", "test_sets", "row_map", "assignments"), has_key, logical(1L)))
  if (!sensitive) {
    return(.publication_audit_empty())
  }
  .publication_audit_row(
    "sensitive_json_payload", "privacy", relative, "fail", "error",
    "The JSON key signature is consistent with a fitted object, row-level predictions, or split membership."
  )
}

.publication_audit_forbidden_paths = function(files, root) {
  relative = vapply(files, .publication_audit_relative, character(1L), root = root)
  checks = .publication_audit_empty()
  for (index in seq_along(files)) {
    path = relative[[index]]
    components = strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1L]]
    lower_components = tolower(components)
    extension = tolower(tools::file_ext(path))
    risky_pattern = paste(.publication_audit_risky_extensions, collapse = "|")
    disguised = grepl(
      paste0("\\.(?:", risky_pattern, ")(?:\\.|$)"),
      basename(path),
      ignore.case = TRUE,
      perl = TRUE
    )
    reasons = c(
      if (basename(path) == ".DS_Store") "macOS .DS_Store metadata" else character(),
      if (any(lower_components %in% .publication_audit_risky_directories)) {
        "private object/raw/prediction directory"
      } else {
        character()
      },
      if (extension %in% .publication_audit_risky_extensions) {
        paste0("risky extension .", extension)
      } else {
        character()
      },
      if (disguised) "disguised serialized or archive extension" else character()
    )
    if (length(reasons)) {
      checks = rbind(
        checks,
        .publication_audit_row(
          "forbidden_private_artifact", "privacy", path, "fail", "error", paste(unique(reasons), collapse = "; ")
        )
      )
    }
  }
  if (!nrow(checks)) {
    checks = .publication_audit_row(
      "forbidden_private_artifact", "privacy", ".", "pass", "info",
      paste0("No forbidden path, extension, or .DS_Store artifact was found across ", length(files), " file(s).")
    )
  }
  checks
}

.publication_audit_pdf_pages = function(path) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    result = tryCatch(pdftools::pdf_info(path)$pages, error = function(condition) condition)
    if (!inherits(result, "condition")) {
      return(list(value = as.integer(result), error = NULL, tool = "pdftools"))
    }
  }
  executable = Sys.which("pdfinfo")
  if (!nzchar(executable)) {
    return(list(value = NA_integer_, error = "Neither `pdftools` nor `pdfinfo` is available.", tool = NA_character_))
  }
  output = suppressWarnings(system2(executable, shQuote(path), stdout = TRUE, stderr = TRUE))
  status = attr(output, "status")
  if (!is.null(status) && status != 0L) {
    return(list(value = NA_integer_, error = paste(output, collapse = " "), tool = "pdfinfo"))
  }
  line = grep("^Pages:[[:space:]]+[0-9]+", output, value = TRUE)
  if (length(line) != 1L) {
    return(list(value = NA_integer_, error = "`pdfinfo` did not report a page count.", tool = "pdfinfo"))
  }
  pages = as.integer(sub("^Pages:[[:space:]]+", "", line))
  list(value = pages, error = NULL, tool = "pdfinfo")
}

.publication_audit_uint32 = function(bytes) {
  if (length(bytes) != 4L) {
    return(NA_real_)
  }
  sum(as.numeric(bytes) * 256^(3:0))
}

.publication_audit_png_info = function(path) {
  size = as.numeric(file.info(path)$size)
  if (!is.finite(size) || size < 33 || size > .Machine$integer.max) {
    return(list(error = "PNG size is invalid or unsupported."))
  }
  connection = file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes = readBin(connection, what = "raw", n = as.integer(size))
  signature = as.raw(c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L))
  if (length(bytes) != size || !identical(bytes[seq_len(8L)], signature)) {
    return(list(error = "File does not have a valid PNG signature."))
  }
  width = height = dpi_x = dpi_y = NA_real_
  position = 9L
  while (position + 11L <= length(bytes)) {
    chunk_length = .publication_audit_uint32(bytes[position:(position + 3L)])
    chunk_type = rawToChar(bytes[(position + 4L):(position + 7L)])
    data_start = position + 8L
    data_end = data_start + chunk_length - 1L
    next_position = data_end + 5L
    if (!is.finite(chunk_length) || data_end > length(bytes) - 4L || next_position <= position) {
      return(list(error = "PNG contains a malformed chunk boundary."))
    }
    if (chunk_type == "IHDR" && chunk_length == 13) {
      width = .publication_audit_uint32(bytes[data_start:(data_start + 3L)])
      height = .publication_audit_uint32(bytes[(data_start + 4L):(data_start + 7L)])
    }
    if (chunk_type == "pHYs" && chunk_length == 9) {
      pixels_per_meter_x = .publication_audit_uint32(bytes[data_start:(data_start + 3L)])
      pixels_per_meter_y = .publication_audit_uint32(bytes[(data_start + 4L):(data_start + 7L)])
      unit = as.integer(bytes[[data_start + 8L]])
      if (unit == 1L) {
        dpi_x = pixels_per_meter_x * 0.0254
        dpi_y = pixels_per_meter_y * 0.0254
      }
    }
    position = as.integer(next_position)
    if (chunk_type == "IEND") {
      break
    }
  }
  if (!is.finite(width) || !is.finite(height)) {
    return(list(error = "PNG does not contain a valid IHDR dimension chunk."))
  }
  list(
    width = width,
    height = height,
    dpi_x = dpi_x,
    dpi_y = dpi_y,
    error = NULL
  )
}

.publication_audit_png_visibility = function(path, maximum_sample_side = 500L) {
  if (!requireNamespace("png", quietly = TRUE)) {
    return(list(error = "The `png` package is required to inspect rendered figure visibility."))
  }
  image = tryCatch(png::readPNG(path), error = function(condition) condition)
  if (inherits(image, "condition")) {
    return(list(error = conditionMessage(image)))
  }
  dimensions = dim(image)
  if (length(dimensions) < 2L || any(dimensions[1:2] < 1L)) {
    return(list(error = "PNG pixel dimensions are invalid."))
  }
  row_index = unique(as.integer(round(seq(
    1,
    dimensions[[1L]],
    length.out = min(dimensions[[1L]], maximum_sample_side)
  ))))
  column_index = unique(as.integer(round(seq(1, dimensions[[2L]], length.out = min(
    dimensions[[2L]], maximum_sample_side
  )))))
  sampled = if (length(dimensions) == 2L) {
    image[row_index, column_index, drop = FALSE]
  } else {
    image[row_index, column_index, , drop = FALSE]
  }
  channels = if (length(dim(sampled)) == 2L) 1L else dim(sampled)[[3L]]
  if (!channels %in% 1:4) {
    return(list(error = paste0("Unsupported PNG channel count: ", channels, ".")))
  }
  if (channels == 1L) {
    red = green = blue = sampled
    alpha = matrix(1, nrow = nrow(sampled), ncol = ncol(sampled))
  } else if (channels == 2L) {
    red = green = blue = sampled[, , 1L]
    alpha = sampled[, , 2L]
  } else {
    red = sampled[, , 1L]
    green = sampled[, , 2L]
    blue = sampled[, , 3L]
    alpha = if (channels == 4L) sampled[, , 4L] else matrix(1, nrow = nrow(red), ncol = ncol(red))
  }
  composite_red = red * alpha + 1 - alpha
  composite_green = green * alpha + 1 - alpha
  composite_blue = blue * alpha + 1 - alpha
  distance_from_white = pmax(
    abs(composite_red - 1),
    abs(composite_green - 1),
    abs(composite_blue - 1)
  )
  list(
    alpha_mean = mean(alpha),
    nonwhite_fraction = mean(distance_from_white > 0.02),
    error = NULL
  )
}

.publication_audit_figures = function(files, root, min_png_pixels, min_png_dpi) {
  relative = vapply(files, .publication_audit_relative, character(1L), root = root)
  extensions = tolower(tools::file_ext(relative))
  figure_files = relative[extensions %in% c("pdf", "png")]
  stems = sort(unique(sub("\\.(?:pdf|png)$", "", figure_files, ignore.case = TRUE, perl = TRUE)))
  checks = .publication_audit_empty()
  inventory = data.frame(
    figure_id = character(),
    pdf_path = character(),
    png_path = character(),
    pdf_exists = logical(),
    png_exists = logical(),
    pdf_pages = integer(),
    png_width_px = integer(),
    png_height_px = integer(),
    png_dpi_x = numeric(),
    png_dpi_y = numeric(),
    png_alpha_mean = numeric(),
    png_nonwhite_fraction = numeric(),
    pair_status = character(),
    pdf_status = character(),
    png_status = character(),
    png_visibility_status = character(),
    overall_status = character(),
    stringsAsFactors = FALSE
  )
  if (!length(stems)) {
    checks = .publication_audit_row(
      "figure_presence", "figures", "figures", "fail", "error", "No PDF or PNG figure was found."
    )
    return(list(checks = checks, inventory = inventory))
  }

  for (stem in stems) {
    pdf_relative = paste0(stem, ".pdf")
    png_relative = paste0(stem, ".png")
    pdf_path = file.path(root, pdf_relative)
    png_path = file.path(root, png_relative)
    has_pdf = file.exists(pdf_path)
    has_png = file.exists(png_path)
    pair_status = if (has_pdf && has_png) "pass" else "fail"
    checks = rbind(
      checks,
      .publication_audit_row(
        "figure_pair", "figures", stem, pair_status, if (pair_status == "pass") "info" else "error",
        if (pair_status == "pass") {
          "Matching PDF and PNG files are present."
        } else {
          "The PDF/PNG figure pair is incomplete."
        }
      )
    )

    pages = NA_integer_
    pdf_status = "fail"
    if (has_pdf) {
      pdf = .publication_audit_pdf_pages(pdf_path)
      pages = pdf$value
      if (!is.null(pdf$error)) {
        pdf_status = "not_checked"
        checks = rbind(
          checks,
          .publication_audit_row("pdf_pages", "figures", pdf_relative, pdf_status, "warning", pdf$error)
        )
      } else {
        pdf_status = if (identical(pages, 1L)) "pass" else "fail"
        checks = rbind(
          checks,
          .publication_audit_row(
            "pdf_pages", "figures", pdf_relative, pdf_status, if (pdf_status == "pass") "info" else "error",
            paste0("PDF page count: ", pages, ".")
          )
        )
      }
    }

    width = height = dpi_x = dpi_y = alpha_mean = nonwhite_fraction = NA_real_
    png_status = "fail"
    png_visibility_status = "fail"
    if (has_png) {
      png = .publication_audit_png_info(png_path)
      if (!is.null(png$error)) {
        png_status = "not_checked"
        checks = rbind(
          checks,
          .publication_audit_row("png_specification", "figures", png_relative, png_status, "warning", png$error)
        )
      } else {
        width = png$width
        height = png$height
        dpi_x = png$dpi_x
        dpi_y = png$dpi_y
        dimension_ok = is.finite(width) && is.finite(height) &&
          width >= min_png_pixels && height >= min_png_pixels
        dpi_ok = is.finite(dpi_x) && is.finite(dpi_y) && dpi_x >= min_png_dpi && dpi_y >= min_png_dpi
        png_status = if (dimension_ok && dpi_ok) "pass" else "fail"
        checks = rbind(
          checks,
          .publication_audit_row(
            "png_specification", "figures", png_relative, png_status,
            if (png_status == "pass") "info" else "error",
            sprintf(
              "PNG is %.0f x %.0f px at %s x %s DPI; minimums are %.0f px per side and %.0f DPI.",
              width,
              height,
              if (is.finite(dpi_x)) sprintf("%.1f", dpi_x) else "missing",
              if (is.finite(dpi_y)) sprintf("%.1f", dpi_y) else "missing",
              min_png_pixels,
              min_png_dpi
            )
          )
        )
      }
      visibility = .publication_audit_png_visibility(png_path)
      if (!is.null(visibility$error)) {
        png_visibility_status = "not_checked"
        checks = rbind(
          checks,
          .publication_audit_row(
            "png_visibility", "figures", png_relative, png_visibility_status, "warning", visibility$error
          )
        )
      } else {
        alpha_mean = visibility$alpha_mean
        nonwhite_fraction = visibility$nonwhite_fraction
        visibility_ok = is.finite(alpha_mean) && alpha_mean >= 0.95 &&
          is.finite(nonwhite_fraction) && nonwhite_fraction >= 0.0005
        png_visibility_status = if (visibility_ok) "pass" else "fail"
        checks = rbind(
          checks,
          .publication_audit_row(
            "png_visibility", "figures", png_relative, png_visibility_status,
            if (png_visibility_status == "pass") "info" else "error",
            sprintf(
              "Mean alpha is %.4f and %.4f%% of sampled pixels differ visibly from white.",
              alpha_mean,
              100 * nonwhite_fraction
            )
          )
        )
      }
    }

    statuses = c(pair_status, pdf_status, png_status, png_visibility_status)
    overall = if (any(statuses == "fail")) {
      "fail"
    } else if (any(statuses == "not_checked")) {
      "not_checked"
    } else {
      "pass"
    }
    inventory = rbind(
      inventory,
      data.frame(
        figure_id = basename(stem),
        pdf_path = if (has_pdf) pdf_relative else NA_character_,
        png_path = if (has_png) png_relative else NA_character_,
        pdf_exists = has_pdf,
        png_exists = has_png,
        pdf_pages = pages,
        png_width_px = as.integer(width),
        png_height_px = as.integer(height),
        png_dpi_x = dpi_x,
        png_dpi_y = dpi_y,
        png_alpha_mean = alpha_mean,
        png_nonwhite_fraction = nonwhite_fraction,
        pair_status = pair_status,
        pdf_status = pdf_status,
        png_status = png_status,
        png_visibility_status = png_visibility_status,
        overall_status = overall,
        stringsAsFactors = FALSE
      )
    )
  }
  list(checks = checks, inventory = inventory)
}

.publication_audit_blank = function(value) {
  text = trimws(as.character(value))
  is.na(value) | is.na(text) | !nzchar(text) | tolower(text) %in% c("na", "n/a", "null")
}

.publication_audit_safe_figure_path = function(value, extension) {
  vapply(value, function(path) {
    if (.publication_audit_blank(path)) {
      return(FALSE)
    }
    path = as.character(path)
    if (grepl("\\\\", path) || startsWith(path, "/") || startsWith(path, "~") ||
        grepl("^[A-Za-z]:", path) || grepl("^[A-Za-z][A-Za-z0-9+.-]*:", path) ||
        grepl("//", path, fixed = TRUE) || !grepl(paste0("\\.", extension, "$"), path)) {
      return(FALSE)
    }
    components = strsplit(path, "/", fixed = TRUE)[[1L]]
    length(components) >= 2L && all(nzchar(components)) && !any(components %in% c(".", ".."))
  }, logical(1L))
}

.publication_audit_consistent_optional_text = function(value) {
  normalized = ifelse(.publication_audit_blank(value), "<BLANK>", trimws(as.character(value)))
  length(unique(normalized)) == 1L
}

.publication_audit_actual_figure_stems = function(files, root) {
  relative = vapply(files, .publication_audit_relative, character(1L), root = root)
  figure_files = relative[tolower(tools::file_ext(relative)) %in% c("pdf", "png")]
  sort(unique(sub("\\.(?:pdf|png)$", "", figure_files, ignore.case = TRUE, perl = TRUE)))
}

.publication_audit_figure_latex_metadata = function(files, root, inventory) {
  relative = vapply(files, .publication_audit_relative, character(1L), root = root)
  indices = which(relative == .publication_audit_figure_latex_metadata_path)
  checks = .publication_audit_empty()
  empty_result = function(status, rows = 0L, table = NULL) {
    list(checks = checks, status = status, n_rows = as.integer(rows), table = table)
  }
  if (!length(indices)) {
    checks = .publication_audit_row(
      "figure_latex_metadata_presence",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      "fail",
      "error",
      "The canonical LaTeX figure metadata file is required."
    )
    return(empty_result("FAIL"))
  }
  if (length(indices) != 1L) {
    checks = .publication_audit_row(
      "figure_latex_metadata_presence",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      "fail",
      "error",
      "More than one LaTeX figure metadata file resolved to the canonical relative path."
    )
    return(empty_result("FAIL"))
  }

  metadata_path = files[[indices]]
  checks = .publication_audit_row(
    "figure_latex_metadata_presence",
    "figure_metadata",
    .publication_audit_figure_latex_metadata_path,
    "pass",
    "info",
    "The canonical LaTeX figure metadata file is present."
  )
  parsed = .publication_audit_read_csv(metadata_path)
  if (!is.null(parsed$error)) {
    checks = rbind(
      checks,
      .publication_audit_row(
        "figure_latex_metadata_schema",
        "figure_metadata",
        .publication_audit_figure_latex_metadata_path,
        "fail",
        "error",
        paste0("The LaTeX figure metadata CSV could not be parsed: ", parsed$error)
      )
    )
    return(empty_result("FAIL"))
  }
  metadata = parsed$value
  missing_columns = setdiff(.publication_audit_figure_latex_metadata_columns, names(metadata))
  duplicated_columns = unique(names(metadata)[duplicated(names(metadata))])
  schema_ok = !length(missing_columns) && !length(duplicated_columns)
  schema_details = c(
    if (length(missing_columns)) paste0("missing columns: ", paste(missing_columns, collapse = ", ")),
    if (length(duplicated_columns)) paste0("duplicated columns: ", paste(duplicated_columns, collapse = ", "))
  )
  checks = rbind(
    checks,
    .publication_audit_row(
      "figure_latex_metadata_schema",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      if (schema_ok) "pass" else "fail",
      if (schema_ok) "info" else "error",
      if (schema_ok) {
        paste0("All ", length(.publication_audit_figure_latex_metadata_columns), " required columns are present.")
      } else {
        paste(schema_details, collapse = "; ")
      }
    )
  )
  if (!schema_ok) {
    return(empty_result("FAIL", nrow(metadata), metadata))
  }

  required_text = c(
    "study", "figure_id", "figure_group_id", "latex_group_id", "pdf_path", "png_path", "source_tables",
    "generator_script", "generator_function", "role", "estimand_or_scope", "uncertainty_semantics",
    "privacy_scope", "latex_assembly_note"
  )
  blank_counts = vapply(required_text, function(column) {
    sum(.publication_audit_blank(metadata[[column]]))
  }, integer(1L))
  widths = .publication_audit_numeric(metadata$width_in)
  heights = .publication_audit_numeric(metadata$height_in)
  dimensions_ok = !is.null(widths) && !is.null(heights) &&
    all(is.finite(widths) & widths > 0) && all(is.finite(heights) & heights > 0)
  required_ok = nrow(metadata) > 0L && !any(blank_counts) && dimensions_ok
  required_details = c(
    if (!nrow(metadata)) "the metadata table has no rows",
    if (any(blank_counts)) paste0(
      "blank required values: ",
      paste(paste0(names(blank_counts)[blank_counts > 0L], "=", blank_counts[blank_counts > 0L]), collapse = ", ")
    ),
    if (!dimensions_ok) "width_in and height_in must be positive finite numbers"
  )
  checks = rbind(
    checks,
    .publication_audit_row(
      "figure_latex_metadata_required_values",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      if (required_ok) "pass" else "fail",
      if (required_ok) "info" else "error",
      if (required_ok) {
        paste0("All ", nrow(metadata), " row(s) contain the required values and valid dimensions.")
      } else {
        paste(required_details, collapse = "; ")
      }
    )
  )

  identifier_pattern = "^[A-Za-z0-9][A-Za-z0-9._-]*$"
  required_ids = c("figure_id", "figure_group_id", "latex_group_id")
  identifier_values_ok = all(vapply(required_ids, function(column) {
    values = as.character(metadata[[column]])
    all(!.publication_audit_blank(values) & grepl(identifier_pattern, values))
  }, logical(1L)))
  panel_ids = as.character(metadata$panel_id)
  panel_ids_blank = .publication_audit_blank(panel_ids)
  optional_ids_ok = all(panel_ids_blank | grepl(identifier_pattern, panel_ids))
  actual_ids_unique = !anyDuplicated(inventory$figure_id)
  identifiers_ok = identifier_values_ok && optional_ids_ok &&
    !anyDuplicated(metadata$figure_id) && actual_ids_unique
  checks = rbind(
    checks,
    .publication_audit_row(
      "figure_latex_metadata_identifiers",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      if (identifiers_ok) "pass" else "fail",
      if (identifiers_ok) "info" else "error",
      if (identifiers_ok) {
        "Figure, group, and optional panel IDs are unique, safe, and coherent."
      } else {
        "Figure, group, or panel identifiers are unsafe, duplicated, or incoherent."
      }
    )
  )

  pdf_safe = .publication_audit_safe_figure_path(metadata$pdf_path, "pdf")
  png_safe = .publication_audit_safe_figure_path(metadata$png_path, "png")
  pdf_exists = vapply(seq_len(nrow(metadata)), function(index) {
    pdf_safe[[index]] && file.exists(file.path(root, as.character(metadata$pdf_path[[index]])))
  }, logical(1L))
  png_exists = vapply(seq_len(nrow(metadata)), function(index) {
    png_safe[[index]] && file.exists(file.path(root, as.character(metadata$png_path[[index]])))
  }, logical(1L))
  paths_ok = all(pdf_safe) && all(png_safe) && all(pdf_exists) && all(png_exists) &&
    !anyDuplicated(metadata$pdf_path) && !anyDuplicated(metadata$png_path)
  checks = rbind(
    checks,
    .publication_audit_row(
      "figure_latex_metadata_paths",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      if (paths_ok) "pass" else "fail",
      if (paths_ok) "info" else "error",
      if (paths_ok) {
        "Every PDF and PNG path is unique, safe, relative, and resolves inside the run directory."
      } else {
        "PDF or PNG paths are unsafe, duplicated, absolute, traversal-bearing, or missing from the run directory."
      }
    )
  )

  pdf_stems = sub("\\.pdf$", "", as.character(metadata$pdf_path))
  png_stems = sub("\\.png$", "", as.character(metadata$png_path))
  path_ids = basename(pdf_stems)
  pairing_ok = isTRUE(all(pdf_stems == png_stems)) &&
    isTRUE(all(path_ids == as.character(metadata$figure_id)))
  checks = rbind(
    checks,
    .publication_audit_row(
      "figure_latex_metadata_pairing",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      if (pairing_ok) "pass" else "fail",
      if (pairing_ok) "info" else "error",
      if (pairing_ok) {
        "Each row names one identically stemmed PDF/PNG pair matching its actual figure ID."
      } else {
        "At least one PDF/PNG stem differs or does not match the row's actual figure ID."
      }
    )
  )

  actual_stems = .publication_audit_actual_figure_stems(files, root)
  coverage_ok = nrow(metadata) == length(actual_stems) && !anyDuplicated(pdf_stems) &&
    identical(sort(pdf_stems), actual_stems) && identical(sort(png_stems), actual_stems)
  missing_stems = setdiff(actual_stems, union(pdf_stems, png_stems))
  extra_stems = setdiff(union(pdf_stems, png_stems), actual_stems)
  coverage_details = c(
    if (length(missing_stems)) paste0("missing: ", paste(missing_stems, collapse = ", ")),
    if (length(extra_stems)) paste0("unexpected: ", paste(extra_stems, collapse = ", ")),
    if (nrow(metadata) != length(actual_stems)) paste0(
      "metadata rows=", nrow(metadata), ", actual stems=", length(actual_stems)
    )
  )
  checks = rbind(
    checks,
    .publication_audit_row(
      "figure_latex_metadata_coverage",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      if (coverage_ok) "pass" else "fail",
      if (coverage_ok) "info" else "error",
      if (coverage_ok) {
        paste0("Exactly one metadata row covers each of the ", length(actual_stems), " actual figure stem(s).")
      } else if (length(coverage_details)) {
        paste(coverage_details, collapse = "; ")
      } else {
        "Metadata paths do not provide exact one-row coverage of the actual figure stems."
      }
    )
  )

  multipanel = .publication_audit_boolean(metadata$is_grouped_figure)
  panel_order = .publication_audit_numeric(metadata$panel_order)
  panel_ok = nrow(metadata) > 0L && length(multipanel) == nrow(metadata) && !anyNA(multipanel) &&
    !is.null(panel_order) &&
    all(is.finite(panel_order) & panel_order > 0 & panel_order == floor(panel_order))
  panel_details = character()
  if (!panel_ok) {
    panel_details = c(panel_details, "multipanel flags or panel orders are missing or invalid")
  } else {
    group_indices = split(seq_len(nrow(metadata)), as.character(metadata$latex_group_id))
    for (group_name in names(group_indices)) {
      rows = group_indices[[group_name]]
      flags = unique(multipanel[rows])
      group_orders = panel_order[rows]
      group_panel_ids = as.character(metadata$panel_id[rows])
      group_headings = as.character(metadata$panel_heading[rows])
      group_ids = unique(as.character(metadata$figure_group_id[rows]))
      group_consistent = length(flags) == 1L && length(group_ids) == 1L &&
        .publication_audit_consistent_optional_text(metadata$group_title[rows]) &&
        .publication_audit_consistent_optional_text(metadata$group_subtitle[rows]) &&
        .publication_audit_consistent_optional_text(metadata$group_caption[rows])
      if (length(flags) == 1L && isTRUE(flags[[1L]])) {
        group_consistent = group_consistent && length(rows) >= 2L &&
          identical(sort(as.integer(group_orders)), seq_len(length(rows))) &&
          all(!.publication_audit_blank(group_panel_ids)) && !anyDuplicated(group_panel_ids) &&
          all(!.publication_audit_blank(group_headings)) &&
          all(!.publication_audit_blank(metadata$group_title[rows]))
      } else if (length(flags) == 1L) {
        panel_presence = !.publication_audit_blank(group_panel_ids)
        heading_presence = !.publication_audit_blank(group_headings)
        group_consistent = group_consistent && length(rows) == 1L && group_orders[[1L]] == 1 &&
          identical(panel_presence, heading_presence) &&
          !.publication_audit_blank(metadata$title[rows]) &&
          all(.publication_audit_blank(metadata$group_title[rows])) &&
          all(.publication_audit_blank(metadata$group_subtitle[rows])) &&
          all(.publication_audit_blank(metadata$group_caption[rows]))
      }
      if (!group_consistent) {
        panel_details = c(panel_details, paste0("incoherent group: ", group_name))
      }
    }
    figure_groups = split(as.character(metadata$latex_group_id), as.character(metadata$figure_group_id))
    if (any(vapply(figure_groups, function(value) length(unique(value)) != 1L, logical(1L)))) {
      panel_details = c(panel_details, "a figure group maps to multiple LaTeX groups")
    }
    panel_ok = panel_ok && !length(panel_details)
  }
  checks = rbind(
    checks,
    .publication_audit_row(
      "figure_latex_metadata_panels",
      "figure_metadata",
      .publication_audit_figure_latex_metadata_path,
      if (panel_ok) "pass" else "fail",
      if (panel_ok) "info" else "error",
      if (panel_ok) {
        "Standalone and grouped rows have coherent group membership, text placement, and panel order."
      } else {
        paste(unique(panel_details), collapse = "; ")
      }
    )
  )

  status = if (any(checks$status == "fail")) "FAIL" else "PASS"
  list(checks = checks, status = status, n_rows = as.integer(nrow(metadata)), table = metadata)
}

.publication_audit_write_csv = function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(value, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

.publication_audit_markdown = function(summary, checks, root_name, min_png_pixels, min_png_dpi) {
  failures = checks[checks$status == "fail", , drop = FALSE]
  warnings = checks[checks$status %in% c("warn", "not_checked"), , drop = FALSE]
  finding_lines = function(table) {
    if (!nrow(table)) {
      return("- None.")
    }
    paste0("- `", table$path, "` — ", table$message)
  }
  eligibility_notice = if (identical(summary$release_eligibility[[1L]], "ELIGIBLE")) {
    paste(
      "> **Automated release eligibility: ELIGIBLE.** The structural, run-mode, publication-settings, readiness,",
      "and logged-condition checks",
      "passed; this is not a substitute for scientific or manuscript review."
    )
  } else {
    paste(
      "> **DO NOT RELEASE FOR MANUSCRIPT USE: NOT_ELIGIBLE.** Resolve every eligibility blocker and rerun this audit",
      "before treating the directory as a manuscript result."
    )
  }
  c(
    "# Publication output audit",
    "",
    eligibility_notice,
    "",
    paste0("- Run: `", root_name, "`"),
    paste0("- Audit version: `", .publication_audit_version, "`"),
    paste0("- Structural status: **", summary$structural_status[[1L]], "**"),
    paste0("- Release eligibility: **", summary$release_eligibility[[1L]], "**"),
    paste0("- Run mode: **", summary$run_mode[[1L]], "**"),
    paste0("- Publication settings declared: **", summary$publication_settings[[1L]], "**"),
    paste0("- Manuscript-ready declaration: **", summary$manuscript_ready[[1L]], "**"),
    paste0(
      "- Logged analysis conditions: ", summary$substantive_warning_count[[1L]], " warning(s) and ",
      summary$logged_errors[[1L]], " error(s)"
    ),
    paste0("- Eligibility blockers: ", summary$eligibility_blockers[[1L]]),
    paste0(
      "- Checks: ", summary$passed[[1L]], " passed, ", summary$failures[[1L]], " failed, ",
      summary$warnings[[1L]], " warning(s), and ", summary$not_checked[[1L]], " not checked"
    ),
    paste0(
      "- Figure standard: paired one-page PDF and PNG; PNG at least ", min_png_pixels,
      " px per side and ", min_png_dpi, " DPI, with an opaque, visibly rendered panel"
    ),
    paste0(
      "- LaTeX figure metadata: **", summary$figure_latex_metadata_status[[1L]], "** (",
      summary$figure_latex_metadata_rows[[1L]], " row(s))"
    ),
    "",
    "## Coverage",
    "",
    paste0("The audit parsed ", summary$csv_files[[1L]], " CSV file(s) and ", summary$json_files[[1L]],
      " JSON file(s), checked ", summary$interval_pairs[[1L]], " interval-column pair(s), and inventoried ",
      summary$figures[[1L]], " figure stem(s)."),
    "",
    "## Failures",
    "",
    finding_lines(failures),
    "",
    "## Warnings and checks not performed",
    "",
    finding_lines(warnings),
    "",
    "## Scope",
    "",
    paste(
      "Structural status reports file integrity, parseability, privacy signatures, interval ordering, and figure",
      "specifications. Release eligibility is a separate automated gate requiring publication mode, affirmative",
      "publication-settings and manuscript-readiness declarations, no logged warnings or errors, and a clean",
      "structural audit. Every logged",
      "warning is conservatively treated as substantive until resolved. Neither status establishes statistical",
      "validity, semantic label correctness, or whether a figure supports the manuscript claim."
    ),
    ""
  )
}

#' Audit a publication run directory and write structural release reports.
#'
#' Analytical artifacts are read only. The function creates or replaces only
#' `provenance/output_audit.csv`, `provenance/output_audit_summary.csv`,
#' `provenance/figure_inventory.csv`, and `OUTPUT_AUDIT.md` below `run_dir`.
audit_publication_output = function(run_dir, min_png_pixels = 1500L, min_png_dpi = 299) {
  if (!is.character(run_dir) || length(run_dir) != 1L || is.na(run_dir) || !dir.exists(run_dir)) {
    stop("`run_dir` must name one existing directory.", call. = FALSE)
  }
  if (!is.numeric(min_png_pixels) || length(min_png_pixels) != 1L || is.na(min_png_pixels) ||
      min_png_pixels < 1 || min_png_pixels != as.integer(min_png_pixels)) {
    stop("`min_png_pixels` must be one positive integer.", call. = FALSE)
  }
  if (!is.numeric(min_png_dpi) || length(min_png_dpi) != 1L || is.na(min_png_dpi) ||
      !is.finite(min_png_dpi) || min_png_dpi <= 0) {
    stop("`min_png_dpi` must be one positive finite number.", call. = FALSE)
  }
  run_dir = normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  files = .publication_audit_files(run_dir)
  checks = .publication_audit_forbidden_paths(files, run_dir)
  csv_files = files[grepl("\\.csv(?:\\.gz)?$", files, ignore.case = TRUE, perl = TRUE)]
  json_files = files[grepl("\\.json$", files, ignore.case = TRUE)]
  interval_pairs = 0L

  for (path in csv_files) {
    relative = .publication_audit_relative(path, run_dir)
    parsed = .publication_audit_read_csv(path)
    if (!is.null(parsed$error)) {
      checks = rbind(
        checks,
        .publication_audit_row("csv_parse", "parseability", relative, "fail", "error", parsed$error)
      )
      next
    }
    checks = rbind(
      checks,
      .publication_audit_row(
        "csv_parse", "parseability", relative, "pass", "info",
        paste0("Parsed ", nrow(parsed$value), " row(s) and ", ncol(parsed$value), " column(s).")
      ),
      .publication_audit_table_privacy(parsed$value, relative)
    )
    intervals = .publication_audit_intervals(parsed$value, relative)
    interval_pairs = interval_pairs + intervals$n_pairs
    checks = rbind(checks, intervals$checks)
  }

  for (path in json_files) {
    relative = .publication_audit_relative(path, run_dir)
    parsed = .publication_audit_read_json(path)
    if (is.null(parsed$error)) {
      checks = rbind(
        checks,
        .publication_audit_row("json_parse", "parseability", relative, "pass", "info", "JSON parsed successfully."),
        .publication_audit_json_privacy(path, relative)
      )
    } else {
      checks = rbind(
        checks,
        .publication_audit_row("json_parse", "parseability", relative, "fail", "error", parsed$error)
      )
    }
  }

  figures = .publication_audit_figures(files, run_dir, as.integer(min_png_pixels), min_png_dpi)
  latex_metadata = .publication_audit_figure_latex_metadata(files, run_dir, figures$inventory)
  checks = rbind(checks, figures$checks, latex_metadata$checks)
  checks = checks[order(checks$category, checks$path, checks$check_id), , drop = FALSE]
  rownames(checks) = NULL
  structural_status = if (any(checks$status == "fail")) {
    "FAIL"
  } else if (any(checks$status == "warn" | (checks$status == "not_checked" & checks$severity != "info"))) {
    "PASS_WITH_WARNINGS"
  } else {
    "PASS"
  }
  release = .publication_audit_release_context(files, run_dir, structural_status)
  summary = data.frame(
    audit_version = .publication_audit_version,
    structural_status = structural_status,
    overall_status = structural_status,
    release_eligibility = release$release_eligibility,
    run_mode = release$run_mode,
    publication_settings = release$publication_settings,
    manuscript_ready = release$manuscript_ready,
    metadata_status = release$metadata_status,
    conditions_status = release$conditions_status,
    logged_warnings = release$logged_warnings,
    substantive_warning_count = release$logged_warnings,
    logged_errors = release$logged_errors,
    eligibility_blockers = release$eligibility_blockers,
    passed = sum(checks$status == "pass"),
    failures = sum(checks$status == "fail"),
    warnings = sum(checks$status == "warn"),
    not_checked = sum(checks$status == "not_checked"),
    source_files = length(files),
    csv_files = length(csv_files),
    json_files = length(json_files),
    interval_pairs = interval_pairs,
    figures = nrow(figures$inventory),
    figure_latex_metadata_status = latex_metadata$status,
    figure_latex_metadata_rows = latex_metadata$n_rows,
    min_png_pixels = as.integer(min_png_pixels),
    min_png_dpi = min_png_dpi,
    stringsAsFactors = FALSE
  )

  .publication_audit_write_csv(checks, file.path(run_dir, "provenance", "output_audit.csv"))
  .publication_audit_write_csv(summary, file.path(run_dir, "provenance", "output_audit_summary.csv"))
  .publication_audit_write_csv(figures$inventory, file.path(run_dir, "provenance", "figure_inventory.csv"))
  markdown = .publication_audit_markdown(
    summary,
    checks,
    basename(run_dir),
    as.integer(min_png_pixels),
    min_png_dpi
  )
  writeLines(markdown, file.path(run_dir, "OUTPUT_AUDIT.md"), useBytes = TRUE)

  invisible(list(
    summary = summary,
    checks = checks,
    figure_inventory = figures$inventory,
    figure_latex_metadata = latex_metadata$table
  ))
}

.write_upload_completion_checkpoints = function(run, analysis_checkpoint, finalization_checkpoint) {
  completion_details = list(
    upload_verified = TRUE,
    publication_transaction = "committed"
  )
  write_run_checkpoint(run, analysis_checkpoint, "completed", completion_details)
  write_run_checkpoint(run, finalization_checkpoint, "completed", completion_details)
  invisible(TRUE)
}

.write_staged_upload_completion = function(
    run,
    staging_dir,
    analysis_checkpoint,
    finalization_checkpoint) {
  # These commit markers are verified in staging and become visible only through the atomic publication rename.
  staged_run = run
  staged_run$private_dir = staging_dir
  staged_run$dirs$root = staging_dir
  staged_run$dirs$checkpoints = file.path(staging_dir, "checkpoints")
  .write_upload_completion_checkpoints(
    staged_run,
    analysis_checkpoint,
    finalization_checkpoint
  )
}

.finalize_study_output = function(
    run,
    upload_expected,
    package_source,
    project_root,
    analysis_checkpoint,
    finalization_checkpoint,
    label,
    require_release_eligibility = FALSE,
    before_publish = NULL) {
  upload_dir = paste0(run$private_dir, "_upload")
  if (dir.exists(upload_dir) || file.exists(upload_dir)) {
    stop("The final upload path already exists: ", upload_dir, call. = FALSE)
  }

  staging_parent = tempfile(
    pattern = paste0(".", run$study, "-release-"),
    tmpdir = dirname(upload_dir)
  )
  if (!dir.create(staging_parent, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create the upload staging directory.", call. = FALSE)
  }
  staging_dir = file.path(staging_parent, basename(upload_dir))
  published = FALSE

  tryCatch({
    write_run_checkpoint(run, finalization_checkpoint, "started")
    validate_expected_artifacts(run$private_dir, run$expected_artifacts)
    write_run_checkpoint(
      run,
      paste0(finalization_checkpoint, "_candidate"),
      "completed",
      list(private_artifacts_verified = TRUE, publication_pending = TRUE)
    )
    private_manifest = file.path(run$private_dir, "MANIFEST.csv")
    write_recursive_manifest(run$private_dir, private_manifest)
    verify_recursive_manifest(run$private_dir, private_manifest)

    create_sanitized_upload_bundle(
      private_dir = run$private_dir,
      upload_dir = staging_dir,
      study = run$study,
      run_salt = run$run_salt,
      expected_artifacts = upload_expected
    )
    verify_sanitized_upload_bundle(staging_dir, run$study)
    .write_staged_upload_completion(
      run,
      staging_dir,
      analysis_checkpoint,
      finalization_checkpoint
    )
    output_audit = audit_publication_output(staging_dir)
    validate_expected_artifacts(staging_dir, .publication_audit_report_paths)
    upload_manifest = file.path(staging_dir, "UPLOAD_MANIFEST.csv")
    write_recursive_manifest(staging_dir, upload_manifest, exclude = "UPLOAD_MANIFEST.csv")
    verify_sanitized_upload_bundle(staging_dir, run$study, upload_manifest)

    structural_status = output_audit$summary$structural_status[[1L]]
    if (!identical(structural_status, "PASS")) {
      findings = output_audit$checks[output_audit$checks$status != "pass", ]
      stop(
        "The sanitized ", label, " publication-output audit returned ", structural_status, ": ",
        paste(unique(findings$message), collapse = " | "),
        call. = FALSE
      )
    }
    if (isTRUE(require_release_eligibility)) {
      release_eligibility = output_audit$summary$release_eligibility[[1L]]
      if (!identical(release_eligibility, "ELIGIBLE")) {
        stop(
          "The sanitized ", label, " output is structurally clean but not release-eligible: ",
          output_audit$summary$eligibility_blockers[[1L]],
          call. = FALSE
        )
      }
    }
    .assert_package_source_snapshot(package_source, project_root, paste(label, "finalization"))
    if (!is.null(before_publish)) {
      if (!is.function(before_publish)) {
        stop("`before_publish` must be NULL or a function.", call. = FALSE)
      }
      before_publish()
    }

    if (!file.rename(staging_dir, upload_dir)) {
      stop("Could not atomically publish the verified upload.", call. = FALSE)
    }
    published = TRUE
  }, error = function(error) {
    if (!published && dir.exists(staging_parent)) {
      unlink(staging_parent, recursive = TRUE, force = TRUE)
    }
    try(
      write_run_checkpoint(
        run,
        finalization_checkpoint,
        "failed",
        list(error = conditionMessage(error))
      ),
      silent = TRUE
    )
    try(.run_contract_append_condition(run, "error", error), silent = TRUE)
    stop(error)
  })

  unlink(staging_parent, recursive = TRUE, force = TRUE)
  private_completion_error = tryCatch({
    .write_upload_completion_checkpoints(
      run,
      analysis_checkpoint,
      finalization_checkpoint
    )
    NULL
  }, error = function(error) error)
  if (inherits(private_completion_error, "condition")) {
    try(
      write_run_checkpoint(
        run,
        paste0(finalization_checkpoint, "_private_bookkeeping"),
        "failed",
        list(
          publication_transaction = "committed",
          error = conditionMessage(private_completion_error)
        )
      ),
      silent = TRUE
    )
  }
  private_manifest_error = tryCatch({
    write_recursive_manifest(run$private_dir, private_manifest)
    verify_recursive_manifest(run$private_dir, private_manifest)
    NULL
  }, error = function(error) error)
  post_commit_errors = Filter(
    Negate(is.null),
    list(checkpoints = private_completion_error, manifest = private_manifest_error)
  )
  if (length(post_commit_errors)) {
    details = paste(
      paste0(names(post_commit_errors), ": ", vapply(post_commit_errors, conditionMessage, character(1L))),
      collapse = " | "
    )
    message(
      "The upload transaction committed, but private post-commit bookkeeping was incomplete: ",
      details
    )
  }

  upload_dir
}

.publication_audit_main = function(arguments = commandArgs(trailingOnly = TRUE)) {
  if (length(arguments) != 1L) {
    stop("Usage: Rscript analysis/shared/output_audit.R RUN_DIRECTORY", call. = FALSE)
  }
  result = audit_publication_output(arguments[[1L]])
  print(result$summary, row.names = FALSE)
  if (identical(result$summary$structural_status[[1L]], "FAIL")) {
    quit(save = "no", status = 1L, runLast = FALSE)
  }
  invisible(result)
}

if (sys.nframe() == 0L) {
  .publication_audit_main()
}
