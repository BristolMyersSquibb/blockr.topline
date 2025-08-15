#' @keywords internal
#' @noRd
new_flextable_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "flextable_block"), ctor, ...)
}

#' @export
block_output.flextable_block <- function(x, result, session) {
  renderUI(flextable::htmltools_value(result))
}

#' @export
block_ui.flextable_block <- function(id, x, ...) {
  tagList(
    uiOutput(NS(id, "result"))
  )
}
