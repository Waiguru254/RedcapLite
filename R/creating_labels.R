#
# data <-readcapdata(token = Sys.getenv('FACILITY_TRACKER'),url, preprocess_data = FALSE)
#
#  dict <- project_dictionary(Sys.getenv('FACILITY_TRACKER'),url)
#
#
#  # Single select columns
#  single_select <- dict |>
#    dplyr::filter(!is.na(select_choices_or_calculations)) |>
#    dplyr::filter(grepl('radio|dropdown|yesno', field_type)) |>
#    dplyr::select(field_name, select_choices_or_calculations)
#
#  extract_values_labels_dictionary <- function(text) {
#    # Split the string by the delimiter "|"
#    parts <- unlist(strsplit(text, "\\|"))
#    # Extract values and labels
#    values <- as.integer(sapply(parts, function(x) strsplit(x, ",")[[1]][1]))
#    labels <- sapply(parts, function(x) strsplit(x, ",")[[1]][2])
#    ### Remove names from the labels
#    names(labels) <- NULL
#    # Return as a list
#    list(values = values, labels = labels)
#  }
#
#  # Adding the value labels with error handling
#  data[unique(single_select$field_name)] <- lapply(unique(single_select$field_name), function(column) {
#    result <- tryCatch({
#      extract_values_labels_dictionary(text = single_select$select_choices_or_calculations[single_select$field_name == column])
#    }, error = function(e) {
#      print(paste("Error in column", names(column), ":", e))
#      return(column)
#    })
#    try({
#      factor(data[[column]], levels = result$values, labels = result$labels)
#    }, silent = TRUE)
#  })

#
#
#  checkbox_mapping <- dict %>%
#    dplyr::filter(field_type == "checkbox", !is.na(select_choices_or_calculations)) %>%
#    dplyr::mutate(
#      choices = strsplit(select_choices_or_calculations, " \\| ")
#    ) %>%
#    tidyr::unnest(choices) %>%
#    dplyr::mutate(
#      parts = strsplit(trimws(choices), ","),
#      value = sapply(parts, `[`, 1),
#      label = sapply(parts, `[`, 2),
#      column_name = paste0(field_name, "___", gsub("-", "_", value))
#    ) %>%
#    dplyr::select(column_name, label)
#
#
#  column_label_data <- dict|>
#    dplyr::select(field_name, field_label) |>
#    dplyr::filter(field_name != '' & !is.na(field_label)) |>
#    dplyr::mutate(
#      label = gsub("<[^>]+>", "",field_label)
#    ) |>
#    dplyr::rename(
#      column_name =  field_name
#    )
#
#
#  # Combine `column_label_data` with `checkbox_mapping`
#  if (exists("checkbox_mapping") && nrow(checkbox_mapping) > 0) {
#    column_label_data <- dplyr::bind_rows(column_label_data, checkbox_mapping)
#  }
#
#  # Iterate over unique column names from `column_label_data`
#  for (column in unique(column_label_data$column_name)) {
#    tryCatch({
#      # Check if the column exists in the data
#      if (column %in% colnames(data)) {
#        # Assign the label as an attribute to the column
#        attr(data[[column]], "label") <- column_label_data$label[column_label_data$column_name == column]
#      } else {
#        warning(paste("Column", column, "not found in data."))
#      }
#    }, error = function(e) {
#      warning(paste("Error assigning label to column", column, ":", e))
#    })
#  }
