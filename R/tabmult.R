#' Tabulate Multiple-Choice (mchoice) Variables
#'
#' Create one-way or two-way tabulations for `mchoice` variables, including
#' counts, percentages, value labels, variable labels, and per-level p-values.
#'
#' @param formula A formula of the form `column ~ by` or `column ~ NULL`.
#' @param data A data frame containing the variables in the formula.
#' @param percent_by Character string specifying how to calculate percentages:
#'        "column" (default) or "row". Only used for two-way tables.
#' @param show.na Logical value indicating whether to include missing values.
#'
#' @return A data frame with counts, percentages, labels, and p-values.
#'
#' @examples
#' \dontrun{
#' # One-way
#' tabmult(symptoms ~ NULL, data = export_data)
#'
#' # Two-way
#' tabmult(symptoms ~ gender, data = export_data)
#' }
#'
#' @export
tabmult <- function(formula, data, percent_by = "column", show.na = TRUE) {
  if (!inherits(formula, "formula")) {
    formula <- stats::as.formula(formula)
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!percent_by %in% c("column", "row")) {
    warning("'percent_by' must be either 'column' or 'row'. Using default 'column'")
    percent_by <- "column"
  }

  formula_text <- deparse(formula)
  if (!grepl("~", formula_text)) {
    stop("Formula must be of the form 'column ~ by' or 'column ~ NULL'")
  }

  formula_parts <- strsplit(as.character(formula)[2], "\\|")
  column <- trimws(strsplit(formula_parts[[1]][1], "\\+")[[1]])
  by <- trimws(strsplit(formula_parts[[1]][2], "\\+")[[1]])

  if (is.na(by) || by == "NA" || by == "NULL") {
    by <- NULL
  }

  if (!column %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", column))
  }
  if (!is.null(by) && !(by %in% names(data))) {
    stop(sprintf("By variable '%s' not found in data", by))
  }

  if (!is.mchoice(data[[column]])) {
    stop(sprintf("Column '%s' must be of class 'mchoice'", column))
  }

  variable_label <- attr(data[[column]], "label")
  if (is.null(variable_label)) {
    variable_label <- column
  }

  levels_attr <- attr(data[[column]], "levels")
  labels_attr <- attr(data[[column]], "labels")

  if (is.null(levels_attr) || is.null(labels_attr)) {
    unique_values <- unique(unlist(strsplit(trimws(as.character(data[[column]])), " ")))
    unique_values <- unique_values[unique_values != ""]
    levels_attr <- unique_values
    labels_attr <- unique_values
  }

  level_table <- data.frame(
    level_value = as.character(levels_attr),
    level_label = as.character(labels_attr),
    stringsAsFactors = FALSE
  )

  if (show.na) {
    level_table <- rbind(
      level_table,
      data.frame(level_value = "Missing", level_label = "Missing", stringsAsFactors = FALSE)
    )
  }

  values <- as.character(data[[column]])
  split_values <- strsplit(trimws(values), " ")

  selected_indicator <- function(level_value) {
    if (level_value == "Missing") {
      return(is.na(values) | trimws(values) == "")
    }
    vapply(split_values, function(x) level_value %in% x, logical(1))
  }

  if (is.null(by)) {
    total_n <- sum(!(is.na(values) | trimws(values) == ""))
    if (show.na) {
      total_n <- length(values)
    }

    results <- lapply(level_table$level_value, function(level_value) {
      selected <- selected_indicator(level_value)
      count <- sum(selected, na.rm = TRUE)
      percent <- if (total_n > 0) round((count / total_n) * 100, 1) else NA_real_
      data.frame(
        variable = column,
        variable_label = variable_label,
        level_value = level_value,
        level_label = level_table$level_label[level_table$level_value == level_value],
        group = "Overall",
        count = count,
        percent = percent,
        p_value = NA_real_,
        stringsAsFactors = FALSE
      )
    })
    return(do.call(rbind, results))
  }

  by_values <- data[[by]]
  by_groups <- unique(as.character(by_values))
  by_groups <- by_groups[!is.na(by_groups) & by_groups != ""]
  by_groups <- sort(by_groups)
  if (show.na) {
    by_groups <- c(by_groups, "Missing")
  }

  group_indicator <- function(group) {
    if (group == "Missing") {
      return(is.na(by_values) | trimws(as.character(by_values)) == "")
    }
    as.character(by_values) == group
  }

  results <- list()
  for (level_value in level_table$level_value) {
    selected <- selected_indicator(level_value)
    group_counts <- sapply(by_groups, function(group) {
      in_group <- group_indicator(group)
      sum(selected & in_group, na.rm = TRUE)
    })

    group_totals <- sapply(by_groups, function(group) {
      in_group <- group_indicator(group)
      if (show.na) {
        return(sum(in_group, na.rm = TRUE))
      }
      sum(in_group & !(is.na(values) | trimws(values) == ""), na.rm = TRUE)
    })

    percent <- if (percent_by == "column") {
      ifelse(group_totals > 0, round((group_counts / group_totals) * 100, 1), NA_real_)
    } else {
      row_total <- sum(group_counts, na.rm = TRUE)
      ifelse(row_total > 0, round((group_counts / row_total) * 100, 1), NA_real_)
    }

    not_selected <- pmax(group_totals - group_counts, 0)
    test_table <- rbind(Selected = group_counts, NotSelected = not_selected)
    p_value <- NA_real_
    if (ncol(test_table) >= 2 && sum(test_table, na.rm = TRUE) > 0) {
      p_value <- tryCatch({
        expected <- suppressWarnings(stats::chisq.test(test_table)$expected)
        if (all(expected >= 5) || ncol(test_table) > 2) {
          stats::chisq.test(test_table)$p.value
        } else {
          stats::fisher.test(test_table)$p.value
        }
      }, error = function(e) NA_real_)
    }

    results[[level_value]] <- data.frame(
      variable = column,
      variable_label = variable_label,
      level_value = level_value,
      level_label = level_table$level_label[level_table$level_value == level_value],
      group = by_groups,
      count = as.integer(group_counts),
      percent = percent,
      p_value = p_value,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}
