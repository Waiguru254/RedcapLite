### Function to retrieve the events and forms
retrieve_redcap_events_forms <- function(token, url) {
  # Helper function to make POST request and retrieve data
  fetch_data <- function(content) {
    formData <- list(
      token = token,
      content = content,
      format = 'json',
      returnFormat = 'json'
    )
    response <- httr::POST(url, body = formData, encode = "form")
    result <- httr::content(response, 'text')
    data <- jsonlite::fromJSON(result)

    if (!is.null(data$error)) {
      stop("Error: ", data$error)
    }

    data
  }

  # Retrieve Events data
  event_data <- tryCatch({
    fetch_data("event") |>
      dplyr::select(event_name, unique_event_name) |>
      dplyr::rename(`Event Name` = event_name, `Unique Event Name` = unique_event_name)
  }, error = function(e) {
    message("Skipping event data retrieval: ", e$message)
    tibble(`Event Name` = character(0), `Unique Event Name` = character(0))
  })

  # Retrieve Forms data
  form_data <- fetch_data("instrument") |>
    dplyr::select(instrument_name, instrument_label) |>
    dplyr::rename(`Form Name` = instrument_name, `Form Label` = instrument_label)

  # Display Events table
  cat("\n**Events Table:**\n")
  print(kableExtra::kable(event_data, align = "l"))

  # Display Forms table
  cat("\n**Forms Table:**\n")
  print(kableExtra::kable(form_data, align = "l"))
}
