register_demo_blocks <- function() { # nocov start
  register_blocks(
    c(
      "new_ard_summary_block",
      "new_ard_barplot_block",
      "new_attrition_plot_block"
    ),
    name = c(
      "ARD summary table",
      "Barplot from ARD",
      "Attrition plot"
    ),
    description = c(
      "Create a summary table from ARD data",
      "Visualize ARD data as barplot",
      "Create an attrition plot as flowchart"
    ),
    category = c(
      "table",
      "plot",
      "plot"
    ),
    package = pkg_name(),
    overwrite = TRUE
  )
}

.onLoad <- function(libname, pkgname) {

  register_demo_blocks()

  invisible(NULL)
} # nocov end
