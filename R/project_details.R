### Redcap Project details
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