#' Internal Function to Convert Column Types
#'
#' This function is designed for internal package use. It takes a column and converts it
#' to numeric if all elements are numbers, otherwise, it converts them to character.
#' This is particularly useful for ensuring consistent data types across data processing.
#'
#' @param x An object that needs to be checked and potentially converted.
#'
#' @return Returns a vector with its elements converted to either numeric or character type,
#'         based on the content of the vector.
#'
#' @noRd
convert_type <- function(x) {
  if (is.logical(x) | is.character(x) | is.list(x)) {
    return(x)
  } else if (is.numeric(x)) {
    as.numeric(x) ## Make everything a numeric (not int)
  } else if (is.call(x)) {
    return(deparse(x))
  } else {
    return(as.character(x))
  }
}



