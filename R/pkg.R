#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import blockr.core
NULL

pkg_name <- function(env = parent.frame()) {
  utils::packageName(env)
}
