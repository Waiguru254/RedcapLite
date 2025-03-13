#' Generate a Frequency Table with Percentages and Optional Plotting
#'
#' This function creates a frequency table for a given column in a dataset,
#' optionally stratified by another variable (`by`). It supports processing of checkbox
#' and multiple-choice columns, handling them appropriately in the summary.
#' Additionally, it provides options for computing percentages and visualizing the results with a bar plot.
#'
#' @param formula A formula specifying the column to summarize and optional stratification variable.
#' @param data A data frame containing the variables to be analyzed.
#' @param percent_by Character string indicating how percentages are computed.
#'        Options: "column" (default) or "row".
#' @param title Character string the title of the table.
#' @param overall Character string specifying the name for the overall total column (default: "Overall").
#' @param show.na Logical. Whether to include missing values in the table (default: TRUE).
#' @param plot Logical. Whether to generate a bar plot of the results (default: FALSE).
#'
#' @return A data frame with counts and percentages, optionally with a plot.
#'
#' @import ggplot2 dplyr tidyr
#' @export
#'
#' @examples
#' # Assuming 'data' is your dataset with variables var1, var2, var3, and var_by
#' mtably(~ var1 + var2 + var3 | var_by, data = data,
#'                  percent_by = "column", show.na = TRUE, plot = FALSE)
#'
#' # For a single variable without stratification
#' mtably(~ var1, data = data, show.na = TRUE, plot = TRUE)
#'
mtably <- function(formula, data, percent_by = "column", title = NULL, overall = "Overall", show.na = TRUE, plot = FALSE) {

  # Parse the formula
  formula_parts <- strsplit(as.character(formula)[2], "\\|")
  column <- trimws(strsplit(formula_parts[[1]][1], "\\+")[[1]])
  by <- trimws(strsplit(formula_parts[[1]][2], "\\+")[[1]])
  ### if by column if NA OR NULL return it as NULL
  if (is.na(by) || by == 'NA') {
    by <- NULL
  }
  ### Check whether by is a valid column
   if ((!is.null(by)) && !(by %in% names(data))) {
     stop("by column not found in data")
   }

  # Check if the column exists
  if (!column %in% names(data)) stop("Column not found in data")

  # Extract unique values from the column if labels and levels are missing
  ordered_labels <- attr(data[[column]], "labels")
  ordered_levels <- attr(data[[column]], "levels")
  label_variable <- attr(data[[column]], "label")

  # Default to column name if label is missing or incorrectly set
  if (is.null(label_variable) || identical(label_variable, ordered_labels)) {
    label_variable <- column
  }
  if (is.null(title)) title <- label_variable  # Default to column name if title is missing
  if (is.null(ordered_labels) || is.null(ordered_levels)) {
    # Extract unique values from the column, treating both NA and "" as missing
    unique_values <- unique(unlist(strsplit(trimws(as.character(data[[column]])), " ")))
    unique_values <- unique_values[unique_values != ""]  # Remove empty strings

    # Convert to numeric if possible, but fallback to character
    numeric_levels <- suppressWarnings(as.numeric(unique_values))

    if (all(!is.na(numeric_levels))) {
      ordered_levels <- sort(numeric_levels)
      ordered_labels <- as.character(ordered_levels)
    } else {
      ordered_levels <- sort(unique_values)  # Keep as character if conversion fails
      ordered_labels <- ordered_levels
    }
  } else {
    # Ensure labels and levels are sorted correctly
    ordered_levels <- as.numeric(ordered_levels)  # Ensure numeric levels
    ordered_labels <- ordered_labels[order(ordered_levels)]  # Reorder labels
  }

  # Include "Missing" category if show.na = TRUE
  if (show.na) {
    ordered_levels <- c(ordered_levels, "Missing")
    ordered_labels <- c(ordered_labels, "Missing")
  }

  # If `by` is provided → Create a two-way contingency table
  if (!is.null(by)) {
    by_variable_lab <- attr(data[[by]], "label")  # Extract variable label for plot title
    ### Check the label of the by columns
    if (is.null(by_variable_lab)) {
      by_variable_lab <- by
    }
    # Get unique categories from by column
    unique_categories <- unique(as.character(data[[by]]))
    unique_categories <- unique_categories[!is.na(unique_categories) & unique_categories != ""]
    unique_categories <- sort(unique_categories)

    # Add Missing category to by column if show.na = TRUE
    if (show.na) {
      unique_categories <- c(unique_categories, "Missing")
    }

    table_matrix <- matrix(0, nrow = length(ordered_levels), ncol = length(unique_categories),
                           dimnames = list(ordered_labels, unique_categories))
  } else {
    # One-way table: Only one column ("Frequency")
    table_matrix <- matrix(0, nrow = length(ordered_levels), ncol = 1,
                           dimnames = list(ordered_labels, "Frequency"))
  }

  ### Creating a total matrix
  if (!is.null(by)) {
    table_total <- table_matrix
    for(cat in unique_categories) {
      for(lab in ordered_labels) {
        table_total[lab, cat] <- sum(data[data[[by]] == cat, column] != "" & !is.na(data[data[[by]] == cat, column]), na.rm = TRUE)
        if (show.na) {
          table_total[lab, "Missing"] <- sum(data[[by]] == "" | is.na(data[[by]]), na.rm = TRUE)
        }
      }
    }
  }
  # Process each row
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
      row_levels <- unlist(strsplit(trimws(as.character(row_values)), " "))  # Extract values
      row_levels <- suppressWarnings(as.numeric(row_levels))  # Convert if numeric
      row_levels <- row_levels[!is.na(row_levels)]  # Remove NAs

      # Ensure ordered_levels is a character vector before grepl()
      ordered_levels <- as.character(ordered_levels)

      # Update counts only for whole-word matches
      for (lvl in row_levels) {
        lvl_str <- as.character(lvl)
        if (lvl_str %in% ordered_levels) {  # Changed to exact match
          row_label <- ordered_labels[which(ordered_levels == lvl_str)]
          table_matrix[row_label, category] <- table_matrix[row_label, category] + 1
        }
      }
    }
  }

  ### Convert to data frame
  table_df <- as.data.frame.matrix(table_matrix)

  if (show.na) {
    total_count <- length(data[[column]])
  } else {
    total_count <- sum(data[[column]] != "" & !is.na(data[[column]]), na.rm = TRUE)
  }

  ### Compute percentages
  if (is.null(by)) {
    # # One-way table: Compute frequency percentages
    #total_count <- sum(data[[column]] != "" & !is.na(data[[column]]), na.rm = TRUE)
    # Compute percentage and replace NA or infinite values with 0
    table_percent <- (table_df[,1] / total_count) * 100
    table_percent[is.na(table_percent) | is.infinite(table_percent)] <- 0
  } else {
    # Two-way table: Compute percentages based on user selection
    if (percent_by == "row") {
      table_df[[overall]] <- rowSums(table_df, na.rm = TRUE)
      table_percent <- round((table_df / table_df[[overall]]) * 100, 1)
      table_percent[,overall] <- round((table_df[,overall] / total_count) * 100, 1)
    } else {  # Default: column-wise percentages
      table_df[[overall]] <-  rowSums(table_df, na.rm = TRUE) # rowSums(table_total, na.rm = TRUE)
      table_percent <- round((table_df / table_total) * 100, 1)
      table_percent[,overall] <- round((table_df[,overall] / total_count) * 100, 1)

    }
  }

  #### Fix NaN issues: Replace NaN with 0
  table_percent[is.na(table_percent)] <- 0

  # Format cells as "count (percentage)"
  if (is.null(by)) {
    table_df$`Frequency (%)` <- sprintf("%  d (%.1f%%)", table_df$Frequency, table_percent)
    table_df <- table_df[, "Frequency (%)", drop = FALSE]  # Keep only the formatted column
  } else {
    table_df <- as.data.frame(
      mapply(function(count, percent) sprintf("%d(%.1f%%)", count, percent), table_df, table_percent)
    )
  }

  ### Adding the row names
  rownames(table_df) <- ordered_labels
  print(table_df)
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
