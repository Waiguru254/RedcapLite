
value_label_mapping <- function(dictionary, form_name) {
  ### Filtering the single select (dropdown /radio /yesno etc)
  single_select<- dictionary |>
    dplyr::filter(!is.na(select_choices_or_calculations)) |>
    filter(grepl('radio|dropdown|yesno',field_type)) |> select(field_name,select_choices_or_calculations)

  process_field_choices <- function(field_names, choice_strings) {
    ### Initialize lists to store results
    all_levels <- list()
    all_labels <- list()
    ### Creating spaces and special characters that may break the quotes
    clean_column <- function(column) {
      column <- gsub("<[^>]+>", "", column)  ### Remove HTML tags
      column <- gsub("[\"']", " ", column)  ### Remove double and single quotation marks
      column <- trimws(column)  ### Remove leading/trailing whitespace
      return(column)
    }

    ### Process each field and its choices
    for(i in seq_along(field_names)) {
      ### Skip empty rows
      if(nchar(trimws(field_names[i])) == 0 || nchar(trimws(choice_strings[i])) == 0) {
        next
      }

      ### Split choices by pipe and process each one
      choices <- strsplit(choice_strings[i], "\\|")[[1]]
      levels <- c()
      labels <- c()

      for(choice in choices) {
        ### Clean up the choice string
        choice <- trimws(choice)
        parts <- strsplit(choice, ",")[[1]]

        if(length(parts) >= 2) {
          level <- trimws(parts[1])
          label <- trimws(paste(parts[-1], collapse = ",")) ### Rejoin any commas in the label
          levels <- c(levels, level)
          labels <- c(labels, label)
        }
      }

      ### Store results
      all_levels[[field_names[i]]] <- paste(levels, collapse = ", ")
      all_labels[[field_names[i]]] <- paste("'",clean_column(labels),"'", sep = '',collapse = ",")
    }

    ### Create data frame
    result_df <- data.frame(
      field_name = names(all_levels),
      levels = unlist(all_levels),
      labels = unlist(all_labels),
      stringsAsFactors = FALSE
    )

    return(result_df)
  }

  ### Organizing the single select column name and attched the associated value labels
  value_labels <- process_field_choices(single_select$field_name,single_select$select_choices_or_calculations) |>
    dplyr::mutate(
      single_select_labels = paste0('try(data$',field_name,'<- factor(data$',field_name,',levels = c(', levels, '), labels = c(',labels,')), silent =TRUE)', sep = '')
    )

  # Check if the directory exists, and create it if it does not
  if (!dir.exists(file.path("./Redcap Projects", form_name, "Dictionary"))) {dir.create(file.path("./Redcap Projects", form_name, "Dictionary"), recursive = TRUE) } else {FALSE }

  ### Creating the Path
  val_script <- paste("./Redcap Projects/",form_name,'/Dictionary/',form_name," Value Labels.R",sep = "")
  ### Creating the script in the working directory within the script sub-folder
  writeLines(unique(c(as.character(value_labels$single_select_labels))),val_script)

}

