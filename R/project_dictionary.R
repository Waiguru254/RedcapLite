
project_dictionary <- function(token, url ) {
  ### The package inherently relies of readr package
  # Check and install readr if necessary
  if (!requireNamespace("readr", quietly = TRUE)) {
    install.packages("readr")
    library(readr)
  }
  formData <- list("token"=token,
                   content='metadata',
                   format='csv',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  ### Suppress the warning, deemed unnecessary
  result <- suppressMessages(httr::content(response)) |>
    dplyr::mutate(
      select_choices_or_calculations = ifelse((is.na(select_choices_or_calculations) & grepl('yesno',field_type)), '1, Yes|0, No',select_choices_or_calculations)
    )
  return(result)
}
