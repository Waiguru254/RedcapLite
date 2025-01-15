
field_type <- function(token, url, type = 'radio') {
  dict <- project_dictionary(token = token, url = url) |>
    dplyr::filter(field_type == type)
  return(unique(dict$field_name))
}
