#' Generate a Frequency Table with Percentages and Optional Plotting
#'
#' This function creates a frequency table for a given column in a dataset,
#' optionally stratified by another variable (`by`). It supports processing of checkbox
#' and multiple-choice columns, handling them appropriately in the summary.
#' Additionally, it provides options for computing percentages and visualizing the results with a bar plot.
#'
#' @param data A data frame containing the variables to be analyzed.
#' @param column A character string specifying the name of the column to summarize. Can include checkbox or multiple-choice columns.
#' @param by (Optional) A character string specifying a column for stratification.
#' @param percent_by Character string indicating how percentages are computed. 
#'        Options: "column" (default) or "row".
#' @param overall Character string specifying the name for the overall total column (default: "Overall").
#' @param show.na Logical. Whether to include missing values in the table (default: TRUE).
#' @param plot Logical. Whether to generate a bar plot of the results (default: FALSE).
#'
#' @return A data frame with counts and percentages, optionally with a plot.
#'
#' @import ggplot2 dplyr reshape2
#' @export

multabyl <- function(data, column, by = NULL, percent_by = "column", overall = "Overall", show.na = TRUE, plot = FALSE) {
  # Extract unique values from the column if labels and levels are missing
  ordered_labels <- attr(data[[column]], "labels")
  ordered_levels <- attr(data[[column]], "levels")
  label_variable <- attr(data[[column]], "label")  # Extract variable label for plot title


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
  
  # Compute row and column totals (only if `by` is provided)
  if (!is.null(by)) {
    table_df[[overall]] <- rowSums(table_df, na.rm = TRUE)
  }
  
  ### Compute percentages
  if (is.null(by)) {
    # One-way table: Compute frequency percentages

    total_count <- sum(table_df$Frequency, na.rm = TRUE)
    # Compute percentage and replace NA or infinite values with 0
    table_percent <- (table_df[,1] / total_count) * 100
    table_percent[is.na(table_percent) | is.infinite(table_percent)] <- 0

  } else {
    # Two-way table: Compute percentages based on user selection
    if (percent_by == "row") {
      table_percent <- round((table_df / table_df[[overall]]) * 100, 1)
    } else {  # Default: column-wise percentages
      col_totals <- colSums(table_df, na.rm = TRUE)
      col_totals_matrix <- matrix(rep(col_totals, each = nrow(table_df)), 
                                nrow = nrow(table_df), byrow = FALSE)
      
      ### Avoid division by zero
      col_totals_matrix[col_totals_matrix == 0] <- NA  
      table_percent <- round((table_df / col_totals_matrix) * 100, 1)
    }
  }
  
  #### Fix NaN issues: Replace NaN with 0
  table_percent[is.na(table_percent)] <- 0
  
  # Compute overall percentage safely (Only for two-way tables)
  if (!is.null(by)) {
    total_sum <- sum(table_df[[overall]], na.rm = TRUE)
    if (total_sum > 0) {
      table_percent[[overall]] <- round((table_df[[overall]] / total_sum) * 100, 1)
    } else {
      table_percent[[overall]] <- 0  # Avoid NaN in the overall column
    }
  }
  
  # Format cells as "count (percentage)"
  if (is.null(by)) {
    table_df$`Frequency (%)` <- sprintf("%d (%.1f%%)", table_df$Frequency, table_percent)
    table_df <- table_df[, "Frequency (%)", drop = FALSE]  # Keep only the formatted column
  } else {
    table_df <- as.data.frame(
      mapply(function(count, percent) sprintf("%d (%.1f%%)", count, percent), table_df, table_percent)
    )
  }
  
  ### Adding the row names
  rownames(table_df) <- ordered_labels
  
  # Create plot if requested
  if (plot) {
    if (is.null(by)) {
      # One-way bar plot with percentages as labels
      plot_data <- data.frame(Label = rownames(table_df), Count = as.numeric(table_matrix), Percent = table_percent)
      p <- ggplot(plot_data, aes(x = as.factor(Label), y = Count, fill = Label)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.1f%%", Percent)), vjust = -0.5, size = 3, fontface = "bold") +
        labs(title = label_variable, x = NULL, y = "Frequency (%)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      # Two-way faceted bar plot with distinct borders
      plot_data <- reshape2::melt(table_matrix)
      colnames(plot_data) <- c("Label", "Category", "Count")
      plot_data <- plot_data %>%
        dplyr::group_by(if(percent_by == 'row') {Label} else {Category}) %>%
        dplyr::mutate(Percent = round((Count / sum(Count, na.rm = TRUE)) * 100, 1)) |> dplyr::ungroup()
      
      p <- ggplot(plot_data, aes(x = as.factor(Label), y = Count, fill = Category)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = sprintf("%.1f%%", Percent)), 
                  position = position_dodge(width = 0.9), 
                  vjust = ifelse(plot_data$Percent > 50, 1.5, -0.2),  # Inside large bars, above small bars
                  color = ifelse(plot_data$Percent > 50, "white", "black"),  # White text inside dark bars
                  size = 4) +
        labs(title = label_variable, x = by_variable_lab, y = "Frequency") +
        facet_wrap(~ Category, scales = "free_x") +  # Prevent overflow, limit columns
        theme_minimal() +
        coord_cartesian(ylim = c(0, max(plot_data$Count) + 2)) +  # Ensure space above bars
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),  # Prevents overlapping x-axis labels
          strip.text = element_text(size = 10, face = "bold"),  # Keeps facet labels readable
          strip.background = element_rect(fill = "lightgray", color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          plot.title = element_text(hjust = 0.5, face = "bold")
        )
      

      
    }
    print(p)  # Display the plot
  }
  
  return(table_df)  
}
