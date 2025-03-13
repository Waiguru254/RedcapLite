#' Create formatted contingency tables with percentages
#'
#' @description
#' This function creates nicely formatted contingency tables with percentages for categorical data.
#' It supports both one-way and two-way tables, and can handle multiple choice columns.
#'
#' @param formula A formula of the form column ~ by or column ~ NULL, where column is the variable to analyze
#'        and by is an optional grouping variable.
#' @param data A data frame containing the variables in the formula.
#' @param percent_by Character string specifying how to calculate percentages: "column" (default) or "row".
#' @param title Optional title for the table. If NULL, uses the variable label or column name.
#' @param overall Character string specifying the label for the overall column.
#' @param show.na Logical value indicating whether to include NA values as a separate category.
#' @param plot Logical value indicating whether to generate a plot.
#'
#' @return A kable table object formatted with kableExtra styling.
#'
#' @examples
#' # One-way table
#' mtabyl(gender ~ NULL, data = survey_data)
#'
#' # Two-way table
#' mtabyl(education ~ gender, data = survey_data, percent_by = "row")
#'
#' # With a plot
#' mtabyl(satisfaction ~ department, data = employee_data, plot = TRUE)
#'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec column_spec pack_rows
#' @importFrom stats as.formula
#' @export
mtabyl <- function(formula, data, percent_by = "column", title = NULL, overall = "Overall",
                   show.na = TRUE, plot = FALSE) {

  # Validate inputs
  if (!inherits(formula, "formula")) {
    formula <- as.formula(formula)
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!percent_by %in% c("column", "row")) {
    warning("'percent_by' must be either 'column' or 'row'. Using default 'column'")
    percent_by <- "column"
  }

  # Parse the formula
  formula_text <- deparse(formula)
  if (!grepl("~", formula_text)) {
    stop("Formula must be of the form 'column ~ by' or 'column ~ NULL'")
  }

  formula_parts <- strsplit(as.character(formula)[2], "\\|")
  column <- trimws(strsplit(formula_parts[[1]][1], "\\+")[[1]])
  by <- trimws(strsplit(formula_parts[[1]][2], "\\+")[[1]])

  # Handle NULL or NA in by variable
  if (is.na(by) || by == 'NA' || by == "NULL") {
    by <- NULL
  }

  # Validate column and by variable
  if (!column %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", column))
  }

  if (!is.null(by) && !(by %in% names(data))) {
    stop(sprintf("By variable '%s' not found in data", by))
  }

  # Get variable metadata
  label_variable <- attr(data[[column]], "label")
  if (is.null(label_variable)) {
    label_variable <- column
  }

  if (is.null(title)) {
    title <- label_variable
  }

  # Handle different column types
  is_mchoice <- inherits(data[[column]], "mchoice") ||
    inherits(data[[column]], "as.mchoice") ||
    exists("is.mchoice") && is.mchoice(data[[column]])

  # Extract levels and labels
  if (is_mchoice) {
    # Multiple choice handling
    ordered_labels <- attr(data[[column]], "labels")
    ordered_levels <- attr(data[[column]], "levels")

    if (is.null(ordered_labels) || is.null(ordered_levels)) {
      # Extract unique values from the column
      unique_values <- unique(unlist(strsplit(trimws(as.character(data[[column]])), " ")))
      unique_values <- unique_values[unique_values != ""]  # Remove empty strings

      # Convert to numeric if possible
      numeric_levels <- suppressWarnings(as.numeric(unique_values))

      if (all(!is.na(numeric_levels))) {
        ordered_levels <- sort(numeric_levels)
        ordered_labels <- as.character(ordered_levels)
      } else {
        ordered_levels <- unique_values
        ordered_labels <- ordered_levels
      }
    } else {
      # Use existing labels and levels
      ordered_levels <- ordered_levels
      ordered_labels <- ordered_labels[order(ordered_levels)]
    }
  } else {
    # Single select handling
    if (is.factor(data[[column]])) {
      ordered_levels <- levels(data[[column]])
      ordered_labels <- ordered_levels
    } else {
      unique_values <- unique(data[[column]])
      ordered_labels <- unique_values[!is.na(unique_values) &
                                        !is.null(unique_values) &
                                        unique_values != ""]
      ordered_levels <- ordered_labels
    }
  }

  # Add missing category if requested
  if (show.na) {
    if (is_mchoice) {
      ordered_levels <- c(unique(ordered_levels), "Missing")
      ordered_labels <- c(unique(ordered_labels), "Missing")
    } else {
      if (is.factor(ordered_levels)) {
        ordered_levels <- factor(c(as.character(ordered_levels), "Missing"),
                                 levels = c(levels(ordered_levels), "Missing"))
      } else {
        ordered_levels <- c(ordered_levels, "Missing")
      }
      ordered_labels <- c(as.character(ordered_labels), "Missing")
    }
  }

  # Initialize table matrix
  if (!is.null(by)) {
    # Two-way table
    by_variable_lab <- attr(data[[by]], "label")
    if (is.null(by_variable_lab)) {
      by_variable_lab <- by
    }

    # Get unique categories from by column
    unique_categories <- unique(as.character(data[[by]]))
    unique_categories <- unique_categories[!is.na(unique_categories) & unique_categories != ""]
    unique_categories <- sort(unique_categories)

    # Add Missing category if requested
    if (show.na) {
      unique_categories <- c(unique_categories, "Missing")
    }

    # Create table matrix
    table_matrix <- matrix(0, nrow = length(ordered_levels), ncol = length(unique_categories),
                           dimnames = list(ordered_labels, unique_categories))

    # Create total matrix for percentage calculations
    table_total <- table_matrix
    if (percent_by == "column") {
      for (cat in unique_categories) {
        for (lab in ordered_labels) {
          table_total[lab, cat] <- sum(data[data[[by]] == cat, column] != "" &
                                         !is.na(data[data[[by]] == cat, column]), na.rm = TRUE)
        }
        if (show.na && "Missing" %in% unique_categories) {
          table_total[, "Missing"] <- sum(data[[by]] == "" | is.na(data[[by]]), na.rm = TRUE)
        }
      }
    }
  } else {
    # One-way table
    table_matrix <- matrix(0, nrow = length(ordered_levels), ncol = 1,
                           dimnames = list(ordered_labels, "Frequency"))
  }

  # Fill the table matrix
  for (i in seq_len(nrow(data))) {
    row_values <- data[[column]][i]

    if (!is.null(by)) {
      by_value <- data[[by]][i]

      # Handle missing values in by column
      category <- if (is.na(by_value) || trimws(as.character(by_value)) == "") {
        if (show.na) "Missing" else next
      } else {
        as.character(by_value)
      }

      # Skip if category is not in our matrix columns
      if (!(category %in% colnames(table_matrix))) next
    } else {
      category <- "Frequency"
    }

    # Handle missing values in main column
    if ((is.na(row_values) || trimws(as.character(row_values)) == "") && show.na) {
      table_matrix["Missing", category] <- table_matrix["Missing", category] + 1
    } else if (!is.na(row_values) && trimws(as.character(row_values)) != "") {
      if (is_mchoice) {
        # Multiple choice handling
        row_levels <- unlist(strsplit(trimws(as.character(row_values)), " "))
        row_levels <- row_levels[!is.na(row_levels)]

        # Ensure ordered_levels is a character vector
        ordered_levels_char <- as.character(ordered_levels)

        # Update counts for matches
        for (lvl in row_levels) {
          lvl_str <- as.character(lvl)
          if (lvl_str %in% ordered_levels_char) {
            row_label <- ordered_labels[which(ordered_levels_char == lvl_str)]
            table_matrix[row_label, category] <- table_matrix[row_label, category] + 1
          }
        }
      } else {
        # Single select handling
        row_value <- as.character(row_values)
        if (row_value %in% as.character(ordered_levels)) {
          row_label <- row_value
          table_matrix[row_label, category] <- table_matrix[row_label, category] + 1
        }
      }
    }
  }

  # Convert to data frame
  table_df <- as.data.frame.matrix(table_matrix)

  # Calculate total count
  if (show.na) {
    total_count <- length(data[[column]])
  } else {
    total_count <- sum(data[[column]] != "" & !is.na(data[[column]]), na.rm = TRUE)
  }

  # Calculate percentages
  if (is.null(by)) {
    # One-way table percentages
    table_percent <- (table_df[, 1] / total_count) * 100
    table_percent[is.na(table_percent) | is.infinite(table_percent)] <- 0
  } else {
    # Two-way table percentages
    if (percent_by == "row") {
      # Row percentages
      table_df[[overall]] <- rowSums(table_df, na.rm = TRUE)
      table_percent <- round((table_df / table_df[[overall]]) * 100, 1)
      table_percent[, overall] <- round((table_df[, overall] / total_count) * 100, 1)
    } else {
      # Column percentages
      table_df[[overall]] <- rowSums(table_df, na.rm = TRUE)
      table_percent <- round((table_df / table_total) * 100, 1)
      table_percent[, overall] <- round((table_df[, overall] / total_count) * 100, 1)
    }
  }

  # Fix NaN issues
  table_percent[is.na(table_percent)] <- 0

  # Format cells as "count (percentage)"
  if (is.null(by)) {
    table_df$`Frequency (%)` <- sprintf("%d (%.1f%%)", table_df$Frequency, table_percent)
    table_df <- table_df[, "Frequency (%)", drop = FALSE]
  } else {
    table_df <- as.data.frame(
      mapply(function(count, percent) sprintf("%d(%.1f%%)", count, percent), table_df, table_percent)
    )
  }

  # Add row names
  rownames(table_df) <- ordered_labels

  # Create plot if requested
  if (plot) {
    if (is.null(by)) {
      # One-way bar plot with percentages as labels
      plot_data <- data.frame(Label = rownames(table_df), Count = as.numeric(table_matrix), Percent = table_percent)
      p <- ggplot2::ggplot(plot_data, aes(x = as.factor(Label), y = Count, fill = Label)) +
        ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +
        ggplot2::geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5, size = 3, fontface = "bold") +
        ggplot2::labs(title = label_variable, x = NULL, y = "Frequency (%)") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      # Two-way faceted bar plot with distinct borders
      if(percent_by == 'row') {
        plot_data <- reshape2::melt(table_matrix)
        colnames(plot_data) <- c("Label", "Category", "Count")

        plot_data <- plot_data %>%
          dplyr::group_by(if(percent_by == 'row') {Label} else {Category}) %>%
          dplyr::mutate(Percent = round((Count / sum(Count, na.rm = TRUE)) * 100, 1)) |> dplyr::ungroup()
      } else {
        value_mat <- apply(table_percent, c(1,2), as.numeric)  # Convert to numeric
        plot_data <- reshape2::melt(value_mat, varnames = c("Label", "Category"), value.name = "Count")
        plot_data$Percent <- round(plot_data$Count, 1)
        colnames(plot_data) <- c("Label", "Category", "Count", "Percent")
      }
      p <- ggplot2::ggplot(plot_data, aes(x = as.factor(Label), y = Count, fill = Category)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::geom_text(aes(label = sprintf("%.1f%%", Percent)),
                  position = position_dodge(width = 0.9),
                  vjust = ifelse(plot_data$Percent > 50, 1.5, -0.2),  # Inside large bars, above small bars
                  color = ifelse(plot_data$Percent > 50, "white", "black"),  # White text inside dark bars
                  size = 4) +
        ggplot2::labs(title = label_variable, x = by_variable_lab, y = "Frequency") +
        ggplot2::facet_wrap(~ Category, scales = "free_x") +  # Prevent overflow, limit columns
        ggplot2::theme_minimal() +
        ggplot2::coord_cartesian(ylim = c(0, max(plot_data$Count) + 2)) +  # Ensure space above bars
        ggplot2::theme(
          axis.text.x = element_text(angle = 45, hjust = 1),  # Prevents overlapping x-axis labels
          strip.text = element_text(size = 10, face = "bold"),  # Keeps facet labels readable
          strip.background = element_rect(fill = "lightgray", color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          plot.title = element_text(hjust = 0.5, face = "bold")
        )



    }
    print(p)  # Display the plot
  }
  ### Addin an overall row
  if (!is.null(by)) {
    colnames(table_df) <- paste0(colnames(table_df), "<br>(N = ", c(table_total[1,], sum(table_total[1,])), ")")
    fancy_table <- knitr::kable(table_df, format = "html", escape = FALSE, align = "c",
                                caption = paste("<div style='text-align: center; font-weight: bold; color: black;'>",
                                                title, "</div>")) %>%
      kableExtra::kable_styling(full_width = FALSE, position = "center", font_size = 14) %>%
      kableExtra::row_spec(0, bold = TRUE, extra_css = "border-top: 3px solid black; border-bottom: 3px solid black;") %>%  # Title bold with thick borders
      kableExtra::row_spec(nrow(table_df), extra_css = "border-bottom: 3px solid black;") %>% # Last row with a bold lower border
      kableExtra::column_spec(1, italic = TRUE, width = "150px") %>%  # Italicize first column (column 0 in R)
      kableExtra::row_spec(0, hline_after = TRUE) %>%  # Ensure header row has a separating line
      kableExtra::row_spec(1:nrow(table_df), extra_css = "text-align: right;") %>% # Align row names to the right
      kableExtra::pack_rows(label_variable, start_row = 1, end_row = nrow(table_df))  # Group rows under a single label

  } else {
    colnames(table_df) <- paste0('Frequency (%)', "<br>(N =",total_count, ")", sep = '')
    # Display as a kable table
    fancy_table <- knitr::kable(table_df, format = "html", escape = FALSE, align = "c",
                                caption = paste("<div style='text-align: center; font-weight: bold; color: black;'>",label_variable, "</div>")) %>%
      kableExtra::kable_styling(full_width = FALSE, position = "center", font_size = 14) %>%
      kableExtra::row_spec(0, bold = TRUE, extra_css = "border-top: 3px solid black; border-bottom: 3px solid black;") %>%
      kableExtra::row_spec(nrow(table_df), extra_css = "border-bottom: 3px solid black;") %>%
      kableExtra::column_spec(1, italic = TRUE, width = "350px")
  }

  return(fancy_table)
}
