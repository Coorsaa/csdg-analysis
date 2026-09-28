#!/usr/bin/env Rscript
# Run from the analysis repository root. Modes never silently escalate to predictive refitting.

targeted_main = function(arguments = commandArgs(trailingOnly = TRUE)) {
  option = function(key, default = NULL) {
    values = grep(paste0("^--", key, "="), arguments, value = TRUE)
    if (length(values) > 1L) stop("Duplicate option: ", key)
    if (length(values)) sub(paste0("^--", key, "="), "", values[[1L]]) else default
  }
  mode = option("mode")
  if (is.null(mode) || !mode %in% c("aggregate", "synthetic", "protected_extension")) {
    stop("Supply --mode=aggregate|synthetic|protected_extension explicitly.")
  }
  root = normalizePath(getwd())
  if (!file.exists("analysis/targeted/run.R")) stop("Run from the analysis repository root.")
  suppressPackageStartupMessages(library(data.table))
  library(mlr3autoiml)
  load_module = function(name) source(file.path(root, "analysis/targeted", paste0(name, ".R")))
  run_script = function(script, args = character()) {
    status = system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), shQuote(args)))
    if (status != 0L) stop("Subprocess failed: ", script)
  }
  if (mode == "aggregate") {
    for (module in c("pisa_synthesis", "render_targeted_figures")) {
      load_module(module)
    }
    pisa_content_synthesis(root, additional_aggregate = option("pisa-aggregate"))
    render_targeted_figures(root)
    run_script("analysis/render_retained_figures.R")
  }
  if (mode == "synthetic") {
    load_module("controlled_examples")
    run_controlled_examples(root)
  }
  if (mode == "protected_extension") {
    workspace = option("authorized-workspace")
    private_output = option("private-output")
    if (is.null(workspace) || is.null(private_output)) {
      stop("Private extensions require --authorized-workspace and --private-output; aggregate medians are insufficient.")
    }
    private_normalized = if (file.exists(private_output)) {
      normalizePath(private_output, mustWork = TRUE)
    } else {
      # Resolve the existing parent, including symlinks; a relative nonexistent path is not an absolute path.
      file.path(normalizePath(dirname(private_output), mustWork = TRUE), basename(private_output))
    }
    if (startsWith(paste0(private_normalized, "/"), paste0(root, "/"))) {
      stop("Private output must be outside the public source tree.")
    }
    for (module in c("local_helpers", "shild_contribution_stability", "shild_common_scale", "shild_convergence")) {
      load_module(module)
    }
    run_script("analysis/targeted/inventory_local_resources.R", workspace)
    run_shild_common_scale(workspace, private_normalized, root)
    run_shild_convergence(workspace, private_normalized, root)
  }
  invisible(mode)
}

if (sys.nframe() == 0L) targeted_main()
