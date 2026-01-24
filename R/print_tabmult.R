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
    display <- wide_table
    if ("p_value" %in% names(display)) {
      display$p_value <- ifelse(
        is.na(display$p_value),
        "",
        formatC(display$p_value, digits = 3, format = "f")
      )
    }
    if ("percent" %in% names(display)) {
      display$percent <- ifelse(
        is.na(display$percent),
        "",
        formatC(display$percent, digits = 1, format = "f")
      )
    }
    table_out <- knitr::kable(display, format = "simple", align = "l")
    print(table_out)
  } else {
    print.data.frame(x, row.names = FALSE)
  }
  invisible(x)
}
