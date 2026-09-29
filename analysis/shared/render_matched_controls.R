# Save the two PISA matched-control contrast panels (retained display, not in the current article) as PDF and PNG.

.save_loco_contrast_plots = function(plots, figure_dir) {
  expected = c("size_matched_exclusion", "training_size_contrast")
  if (!identical(names(plots), expected)) {
    stop("The PISA matched-control renderer did not return the expected two panels.", call. = FALSE)
  }
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  stems = file.path(
    figure_dir,
    c("pisa_pv1_loco_size_matched_exclusion_penalty", "pisa_pv1_loco_training_size_contrast")
  )
  paths = unlist(Map(function(plot, stem) {
    pdf_path = paste0(stem, ".pdf")
    png_path = paste0(stem, ".png")
    ggplot2::ggsave(
      pdf_path,
      plot = plot,
      width = 4.7,
      height = 8.7,
      units = "in",
      device = grDevices::cairo_pdf,
      bg = "white"
    )
    ggplot2::ggsave(
      png_path,
      plot = plot,
      width = 4.7,
      height = 8.7,
      units = "in",
      dpi = 320,
      bg = "white"
    )
    c(pdf_path, png_path)
  }, plots, stems), use.names = FALSE)
  normalizePath(paths, winslash = "/", mustWork = TRUE)
}
