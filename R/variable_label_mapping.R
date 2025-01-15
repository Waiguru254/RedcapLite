variable_label_mapping <- function(token, url, dictionary = NULL, form_name = NULL ) {
  ### Project details
  if (is.null(form_name)) {
      ### This include the project info, name and project id.
        project_details <- function(token, url = url) {
        formData <- list("token" = token,
                         content = 'project',
                         format = 'json',
                         returnFormat = 'json'
        )
        response <- httr::POST(url, body = formData, encode = "form")
        result <- httr::content(response)
        project <- paste( result$project_id,   
                          result$project_title, sep = '-')
        
        return(project)
        }
        
        
        ### this returns the project id and project name
        form_name <- paste('PID',project_details(token, url = url))
  }
  
    clean_column <- function(column) {
          column <- gsub("<[^>]+>", "", column)  # Remove HTML tags
          column <- gsub("\\bir\\b", "", column, ignore.case = TRUE)  # Remove "ir" (case-insensitive)
          column <- trimws(column)  # Remove leading/trailing whitespace
          return(column)
    }
    
    column_label_data <- dictionary|>
                  dplyr::select(field_name, field_label) |>
                  dplyr::filter(field_name != '' & !is.na(field_label)) |>
                  dplyr::mutate(
                    col_label = paste("try(attr(data$",field_name, ', "label") <- "',clean_column(field_label),'", silent =TRUE)',sep = '')
                  )
                  
    ### Check box label mapping 
    ### Function to parse a single choice string into a data frame
    parse_choices_to_df <- function(field_name, choices_string) {
      if (is.na(choices_string)) return(NULL)
      
      # Split choices into individual items
      choices <- strsplit(choices_string, " \\| ")[[1]]
      
      # Create a data frame for this field
      do.call(rbind, lapply(choices, function(x) {
        parts <- trimws(strsplit(x, ",")[[1]])
        data.frame(
          field_name = field_name,
          column_name = paste0(field_name, "___", gsub("-", "_", parts[1])),  # Replace hyphens with underscores here
          value = parts[1],  # Keep original value
          label = parts[2],
          stringsAsFactors = FALSE
        )
      }))
    }
    
    # Create the long format mapping
    checkbox_mapping <- dictionary %>%
      filter(field_type == "checkbox") %>%
      dplyr::rowwise() %>%
      dplyr::do({
        parse_choices_to_df(.$field_name, .$select_choices_or_calculations)
      }) %>%
      dplyr::ungroup() |>
      dplyr::mutate(
        col_label = paste("try(attr(data$",column_name, ', "label") <- "',clean_column(label),'", silent =TRUE)',sep = '')
      )
    
    # List of System columns
    extra_columns <- c("redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
    extra_columns_lab <- c("Redcap Event Name", "Redcap Repeat Instrument", "Redcap Repeat Instance")
    
    # Generate cleaned labels for the extra columns
    system_labels <- paste0(
      'try(attr(data$', extra_columns, ', "label") <- "', extra_columns_lab, '", silent = TRUE)'
    )
    
    ### if it does not exist it will be created.
    # Check if the directory exists, and create it if it does not
    if (!dir.exists(file.path("./Redcap Projects", form_name, "Dictionary"))) {dir.create(file.path("./Redcap Projects", form_name, "Dictionary"), recursive = TRUE) } else {FALSE }
    
    ### Creating the Path
    var_script <- paste("./Redcap Projects/",form_name,'/Dictionary/',form_name," Variable Labels.R",sep = "")
    ### Creating the script in the working directory within the script sub-folder
    writeLines(unique(c(system_labels, as.character(column_label_data$col_label), checkbox_mapping$col_label)),var_script)

}


