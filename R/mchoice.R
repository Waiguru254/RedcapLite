#' Create a Multiple-Choice (mchoice) Object
#'
#' This function constructs an `mchoice` object, which is a structured data type with predefined levels and labels.
#'
#' @param values A vector of values representing selected choices.
#' @param levels A vector of unique possible values (must match the length of `labels`).
#' @param labels A vector of labels corresponding to `levels` (must have the same length as `levels`).
#' @param label An optional character string providing a description of the multiple-choice field.
#'
#' @return An object of class `"mchoice"` with associated attributes:
#' \itemize{
#'   \item \code{levels} - The possible values for the multiple-choice variable.
#'   \item \code{labels} - Descriptive labels for each level.
#'   \item \code{label} - A descriptive name for the field (optional).
#' }
#'
#' @details
#' The `mchoice` class is useful for representing categorical multiple-choice data where each selection corresponds to a predefined level.
#' 
#' @examples
#' # Create an mchoice object
#' m <- mchoice(
#'   values = c(1, 2, 1, 3),
#'   levels = c(1, 2, 3),
#'   labels = c("Yes", "No", "Maybe"),
#'   label = "Response Category"
#' )
#'
#' # Check structure
#' str(m)
#'
#' @export
mchoice <- function(values, levels, labels, label = NULL) {
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

#' Convert a Vector to an `mchoice` Object
#'
#' This function converts an existing vector into an `mchoice` object with predefined levels and labels.
#'
#' @param x A vector to be converted into an `mchoice` object.
#' @param levels A vector of unique possible values (must match the length of `labels`).
#' @param labels A vector of labels corresponding to `levels` (must have the same length as `levels`).
#' @param label An optional character string providing a description of the multiple-choice field.
#'
#' @return An object of class `"mchoice"` with associated attributes:
#' \itemize{
#'   \item \code{levels} - The possible values for the multiple-choice variable.
#'   \item \code{labels} - Descriptive labels for each level.
#'   \item \code{label} - A descriptive name for the field (optional).
#' }
#'
#' @details
#' The `as.mchoice()` function is useful when you need to convert an existing vector into an `mchoice` object.
#'
#' @examples
#' # Convert an existing vector to an mchoice object
#' responses <- c(1, 2, 1, 3)
#' m <- as.mchoice(
#'   x = responses,
#'   levels = c(1, 2, 3),
#'   labels = c("Yes", "No", "Maybe"),
#'   label = "Response Category"
#' )
#'
#' # Check structure
#' str(m)
#'
#' @export
as.mchoice <- function(x, levels, labels, label = NULL) {
  if (length(levels) != length(labels)) {
    stop("levels and labels must have the same length")
  }
  
  # Convert existing vector to mchoice
  obj <- structure(
    x,
    class = "mchoice",
    levels = levels,
    labels = labels,
    label = label
  )
  return(obj)
}

#' Check if an Object is of Class `mchoice`
#'
#' This function checks whether an object is of class `"mchoice"` and returns `TRUE` or `FALSE`.
#'
#' @param x An object to check.
#'
#' @return A logical value:
#' \itemize{
#'   \item `TRUE` if the object is of class `"mchoice"`.
#'   \item `FALSE` otherwise.
#' }
#'
#' @examples
#' # Create an mchoice object
#' m <- mchoice(
#'   values = c(1, 2, 1, 3),
#'   levels = c(1, 2, 3),
#'   labels = c("Yes", "No", "Maybe"),
#'   label = "Response Category"
#' )
#'
#' # Check if it is an mchoice object
#' is.mchoice(m)  # Returns TRUE
#'
#' # Check a regular vector
#' is.mchoice(c(1, 2, 3))  # Returns FALSE
#'
#' @export
is.mchoice <- function(x) {
  inherits(x, "mchoice")
}


  validate_params <- function(preprocess_data, compact_form, drop_empty) {
    if (!preprocess_data && (compact_form || drop_empty)) {
      message("Note: You can only have EITHER `compact_form` OR `drop_empty` set to TRUE if `preprocess_data = TRUE`.")
    }
  }
  
