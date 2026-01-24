#' @export
print.tabmult <- function(x, ...) {
  variable_label <- attr(x, "variable_label")
  by_label <- attr(x, "by_label")
  wide_table <- attr(x, "wide_table")

  if (!is.null(variable_label)) {
    cat(variable_label, "\n")
  }
  if (!is.null(by_label)) {
    cat("By:", by_label, "\n")
  }
  if (!is.null(wide_table)) {
    print(wide_table, row.names = FALSE)
  } else {
    print.data.frame(x, row.names = FALSE)
  }
  invisible(x)
}
