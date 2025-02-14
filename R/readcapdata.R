#' Retrieve and Process Data from REDCap
#'
#' A function to retrieve and process data from REDCap (Research Electronic Data Capture) via its API.
#' This function supports both raw data export and preprocessed data with value labels and column attributes.
#'
#' @param token A character string. The API token for REDCap project access.
#' @param url A character string. The REDCap API URL endpoint.
#' @param fields A vector or NULL. Specific fields to export. If NULL, all fields are exported.
#' @param events A vector or NULL. Specific events to export for longitudinal projects.
#' @param forms A vector or NULL. Specific forms to export.
#' @param drop_empty Logical. If TRUE, empty columns will be dropped. Defaults to `FALSE`.
#' @param preprocess_data Logical. If TRUE, processes data to include value labels and column attributes. Defaults to `FALSE`.
#' @param file_name A character string or NULL. If provided, saves the data to an RDS file. Include the extension .rds in the file_name
#'
#' @details
#' The function performs the following operations:
#' - Constructs API request parameters for REDCap data export.
#' - If `preprocess_data = TRUE`:
#'   - Downloads project dictionary.
#'   - Processes single-select fields.
#'   - Handles checkbox fields.
#'   - Adds value labels and column attributes.
#' - Supports both raw and processed data export formats.
#' - Can optionally save the exported data to a file.
#'
#' @return A data frame containing the requested REDCap data. If `preprocess_data = TRUE`,
#' the returned data frame includes:
#' - Factor levels for single-select fields.
#' - Processed checkbox columns.
#' - Column labels as attributes.
#' - Special handling for event names and repeat instruments.
#'
#'
#' @examples
#' \dontrun{
#' # Basic usage with raw data
#' data <- readcapdata(
#'     token = "YOUR_API_TOKEN",
#'     url = "YOUR_REDCAP_API_URL"
#' )
#'
#' # Export with preprocessing and save to file
#' processed_data <- readcapdata(
#'     token = "YOUR_API_TOKEN",
#'     url = "YOUR_REDCAP_API_URL",
#'     preprocess_data = TRUE,
#'     drop_empty = TRUE,
#'     file_name = "my_redcap_export"
#' )
#'
#' # Export specific fields
#' fields_data <- readcapdata(
#'     token = "YOUR_API_TOKEN",
#'     url = "YOUR_REDCAP_API_URL",
#'     fields = c("record_id", "age", "gender")
#' )
#' }
#'
#' @note
#' - The function requires appropriate REDCap API permissions.
#' - Large exports may take significant time to process.
#' - When `preprocess_data = TRUE`, additional API calls are made to fetch the data dictionary.
#' - The function handles both longitudinal and non-longitudinal projects.
#'
#'
#' @author
#' [Original author information should be added here]
#'
#' @references
#' REDCap (Research Electronic Data Capture)
#' @export

readcapdata <- function(token, url,fields = NULL, events = NULL, forms = NULL, drop_empty = FALSE, preprocess_data = FALSE, list_event_form = FALSE, file_name = NULL) {
   # Define the mchoice constructor function
mchoice <- function(values, levels, labels, label = NULL) {
  if (!is.numeric(values) && !is.character(values)) {
    stop("values should be either numeric or character")
  }
  if (length(levels) != length(labels)) {
    stop("levels and labels must have the same length")
  }
  
  # Create a structure with attributes
  obj <- structure(
    values,
    class = "mchoice",
    levels = levels,
    labels = labels,
    label = label
  )
  return(obj)
}

# Define conversion function
as.mchoice <- function(x, levels, labels, label = NULL) {
  if (!is.numeric(x) && !is.character(x)) {
    stop("x should be either numeric or character")
  }
  if (length(levels) != length(labels)) {
    stop("levels and labels must have the same length")
  }
  
  # Convert existing column to mchoice
  obj <- structure(
    x,
    class = "mchoice",
    levels = levels,
    labels = labels,
    label = label
  )
  return(obj)
}

# Define is.mchoice() function to check class
is.mchoice <- function(x) {
  inherits(x, "mchoice")
}
    # Displaying the Event and Form names for the project
    if (isTRUE(list_event_form)) {
      retrieve_redcap_events_forms(token = token, url = url)
    }

    ### Initialize formData list with the fixed parameters
    formData <- list(
      "token" = token,
      content = 'record',
      action = 'export',
      format = 'json',
      type = 'flat',
      csvDelimiter = '',
      rawOrLabel = 'raw',
      rawOrLabelHeaders = 'raw',
      exportCheckboxLabel = 'true',
      exportSurveyFields = 'true',
      exportDataAccessGroups = 'false',
      returnFormat = 'json'
    )

    # Add events, fields, and forms to formData if provided
    if (!is.null(events)) formData <- c(formData, setNames(as.list(events), paste0("events[", seq_along(events) - 1, "]")))
    if (!is.null(fields)) formData <- c(formData, setNames(as.list(fields), paste0("fields[", seq_along(fields) - 1, "]")))
    if (!is.null(forms)) formData <- c(formData, setNames(as.list(forms), paste0("forms[", seq_along(forms) - 1, "]")))

    #### When there is need to add the value label and column label to the data.
    ### Just want to download the data in raw format=
    if (isTRUE(preprocess_data)) {

      #### Check whether you need to drop empty columns
      if (isTRUE(drop_empty)) {
        data <- jsonlite::fromJSON(httr::content(httr::POST(url, body = formData, encode = "form"),'text')) |>
          janitor::remove_empty()|>
          ### Make all empty entries to be NA for consistency.
          ### Make is easy in data management to use is.na() without having to ==""
          dplyr::mutate(across(everything(), ~ ifelse(. == "", NA, .)))  |>
          dplyr::select(dplyr::starts_with('record_id'), dplyr::starts_with('redcap_'), dplyr::everything())
      } else {
        data <- jsonlite::fromJSON(httr::content(httr::POST(url, body = formData, encode = "form"),'text'))
      }
      ### Download Project Dictionary
      project_codebook <- project_dictionary(token = token ,url = url)
      ### Single select columns
      single_select <- project_codebook |>
        dplyr::filter(!is.na(select_choices_or_calculations)) |>
        dplyr::filter(grepl('radio|dropdown|yesno', field_type)) |>
        dplyr::select(field_name, select_choices_or_calculations) |>
        dplyr::filter(field_name %in% names(data))

      ### Processing the checkbox labels
      checkbox_mapping_1 <- project_codebook %>%
        dplyr::filter(field_type == "checkbox", !is.na(select_choices_or_calculations)) %>%
        dplyr::mutate(
          choices = strsplit(select_choices_or_calculations, "\\|")
        ) %>%
        tidyr::unnest(choices) %>%
        dplyr::mutate(
          parts = strsplit(trimws(choices), ","),
          value = sapply(parts, `[`, 1),
          label = sapply(parts, `[`, 2),
          column_name = paste0(field_name, "___", gsub("-", "_", value))
        )
       ### Check labels
       checkbox_mapping <- checkbox_mapping_1 %>%
        dplyr::select(column_name, label) |>
        dplyr::filter(column_name %in% names(data))
        
      ### Selecting value and column labels for the checbox columns 
        checkbox_val_labels <- checkbox_mapping |>
         dplyr::select(column_name,value, label) |> dplyr::rename(labels =label)
         print(checkbox_val_labels)
      ### Column label data
      column_label_data <- project_codebook |>
        dplyr::select(field_name, field_label) |>
        dplyr::filter(field_name != '' & !is.na(field_label)) |>
        dplyr::mutate(
          label = gsub("<[^>]+>", "",field_label)
        ) |>
        dplyr::rename(
          column_name =  field_name
        )
      ### Combine `column_label_data` with `checkbox_mapping`
      if (exists("checkbox_mapping") && nrow(checkbox_mapping) > 0) {
        column_label_data <- dplyr::bind_rows(column_label_data, checkbox_mapping)

        ### Organizing the single select column name and attched the associated value labels
        ### Cleaning and processing checkbox columns and ordering according to the codebooks in the server

        data <- data %>%
          # Process checkbox columns
          {
            df <- .
            if (exists("checkbox_mapping") && nrow(checkbox_mapping) > 0) {
              # Extract unique prefixes from column names
              checkbox_mapping <- checkbox_mapping %>%
                dplyr::mutate(prefix = sub("___.*", "", column_name))

              # Group and process each checkbox prefix
              checkbox_groups <- split(checkbox_mapping, checkbox_mapping$prefix)

              df <- purrr::reduce(checkbox_groups, function(df, group) {
                prefix <- unique(group$prefix)
                cols <- group$column_name

                # Combine checkbox values into a single column
                combined_col <- apply(df[, cols, drop = FALSE], 1, function(x) {
                  selected <- names(x)[x == 1]
                  if (length(selected) > 0) {
                    values <- sub(paste0(prefix, "___"), "", selected)
                    paste(sub("_", "-", values), collapse = " ")
                    ##paste0(" ", paste(sub("_", "-", values), collapse = " "), " ")
                  } else {
                    ""
                  }
                })

                # Add the combined column and update original columns
                df <- df %>%
                  mutate(
                    !!prefix := combined_col,
                    across(all_of(cols), ~ if_else(
                      .data[[prefix]] == "",
                      NA_real_,
                      suppressWarnings(as.numeric(.)),
                      missing = NA_real_
                    ))
                  )

                df
              }, .init = df)
            }
            df <- df %>%
              mutate(
                !!prefix := as.mchoice(
                  Color_Choice,
                  levels = checkbox_val_labels$value[checkbox_val_labels$column_name == prefix],
                  labels = checkbox_val_labels$labels[checkbox_val_labels$column_name == prefix]
                )
              )

            df
          } %>%
          # Dynamically select and reorder columns
          {
            df <- .
            selected_columns <- unique(unlist(lapply(project_codebook$field_name, function(prefix) {
              c(prefix, names(df)[startsWith(names(df), paste0(prefix, "___"))])
            })))

            df %>%
              dplyr::select(
                dplyr::starts_with("record_id"),
                dplyr::starts_with("redcap_"),
                dplyr::any_of(selected_columns)
              ) |>
              # Replace empty strings with NA for consistency
              dplyr::mutate(across(everything(), ~ ifelse(. == "", NA, .)))
          }
      }

      ### Adding the value labels with error handling
      if (nrow(single_select) > 0) {
        single_select_map <- single_select %>%
          dplyr::mutate(
            value_label_map = purrr::map(select_choices_or_calculations, ~ {
              parts <- strsplit(.x, "\\|")[[1]]
              list(
                values = as.integer(sub(",.*", "", parts)),
                labels = trimws(sub(".*,", "", parts))
              )
            })
          )

        data <- data %>%
          dplyr::mutate(across(single_select$field_name, ~ {
            map_data <- single_select_map$value_label_map[[match(cur_column(), single_select$field_name)]]
            factor(.x, levels = map_data$values, labels = map_data$labels)
          }))
      }

      ### Iterate over unique column names from `column_label_data`
      for (column in unique(column_label_data$column_name)) {
        tryCatch({
          # Check if the column exists in the data
          if (column %in% colnames(data)) {
            # Assign the label as an attribute to the column
            attr(data[[column]], "label") <- trimws(column_label_data$label[column_label_data$column_name == column])
          }
        }, error = function(e) {
          # warning(paste("Error assigning label to column", column, ":", e))
        })
      }
      ### Adding the extra columns to the data
      extra_columns <- c("redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
      extra_columns_lab <- c("Redcap Event Name", "Redcap Repeat Instrument", "Redcap Repeat Instance")

      for (i in seq_along(extra_columns)) {
        column <- extra_columns[i]
        label <- extra_columns_lab[i]

        if (column %in% colnames(data)) {
          # Assign the label as an attribute
          try(attr(data[[column]], "label") <- label, silent =TRUE)
        }
      }

    } else {
      response <- httr::POST(url, body = formData, encode = "form")
      result <- httr::content(response,'text')
      data <- jsonlite::fromJSON(result) |>
        # Replace empty strings with NA for consistency
        dplyr::mutate(across(everything(), ~ ifelse(. == "", NA, .)))
    }



    if(!is.null(file_name)) {
      saveRDS(data, file = file_name)
    }
    ### Exporting the data, what the function is returning
    return(data)
}
