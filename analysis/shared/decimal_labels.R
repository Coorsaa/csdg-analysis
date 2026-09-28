# APA leading-zero formatting at the text-grob layer preserves panel geometry.
# Apply only to axes or annotations whose quantities are bounded by one.

.analysis_omit_decimal_zero = function(labels) {
  stopifnot(is.character(labels))
  vapply(labels, function(label) {
    if (is.na(label)) return(NA_character_)
    pattern = "(?<![[:alnum:]_.])[+\\-]?0\\.[0-9]+(?![[:alnum:]_.])"
    locations = gregexpr(pattern, label, perl = TRUE)
    tokens = regmatches(label, locations)[[1L]]
    if (!length(tokens)) return(label)
    ends = locations[[1L]] + attr(locations[[1L]], "match.length")
    percentages = substring(label, ends, ends) == "%"
    replacements = sub("^([+\\-]?)0\\.", "\\1.", tokens)
    replacements[percentages] = tokens[percentages]
    stopifnot(identical(as.numeric(tokens), as.numeric(replacements)))
    regmatches(label, locations) = list(replacements)
    label
  }, character(1L), USE.NAMES = FALSE)
}

.analysis_decimal_grob = function(plot, axes = character(), annotations = FALSE) {
  stopifnot(all(axes %in% c("x", "y")), is.logical(annotations), length(annotations) == 1L)
  original = if (inherits(plot, "gtable")) plot else ggplot2::ggplotGrob(plot)
  state = new.env(parent = emptyenv())
  state$changes = list()
  walk = function(grob, path) {
    if (inherits(grob, "text") && is.character(grob$label)) {
      before = grob$label
      grob$label = .analysis_omit_decimal_zero(before)
      if (!identical(before, grob$label)) {
        state$changes[[length(state$changes) + 1L]] = list(path = path, before = before, after = grob$label)
      }
    }
    if (length(grob$grobs)) {
      grob$grobs = lapply(seq_along(grob$grobs), function(i) walk(grob$grobs[[i]], paste0(path, "/g", i)))
    }
    if (length(grob$children)) {
      for (i in seq_along(grob$children)) {
        grob$children[[i]] = walk(grob$children[[i]], paste0(path, "/c", i))
      }
    }
    grob
  }
  selected = rep(FALSE, nrow(original$layout))
  if ("x" %in% axes) selected = selected | grepl("^axis-[bt]", original$layout$name)
  if ("y" %in% axes) selected = selected | grepl("^axis-[lr]", original$layout$name)
  if (annotations) selected = selected | grepl("^panel", original$layout$name)
  revised = original
  for (index in which(selected)) {
    revised$grobs[[index]] = walk(original$grobs[[index]], original$layout$name[[index]])
  }
  stopifnot(identical(original$widths, revised$widths), identical(original$heights, revised$heights))
  # Reset only changed text and require all other drawing instructions to be identical.
  without_text = function(grob) {
    if (inherits(grob, "text")) grob$label = character()
    if (length(grob$grobs)) grob$grobs = lapply(grob$grobs, without_text)
    if (length(grob$children)) {
      for (i in seq_along(grob$children)) grob$children[[i]] = without_text(grob$children[[i]])
    }
    grob
  }
  stopifnot(identical(without_text(original), without_text(revised)))
  list(original = original, revised = revised, changes = state$changes)
}
