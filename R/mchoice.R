   # Define the mchoice constructor function
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

  validate_params <- function(preprocess_data, compact_form, drop_empty) {
    if (!preprocess_data && (compact_form || drop_empty)) {
      message("Note: You can only have EITHER `compact_form` OR `drop_empty` set to TRUE if `preprocess_data = TRUE`.")
    }
  }
  
