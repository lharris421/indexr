#' Internal Function to Convert Column Types
#'
#' This function is designed for internal package use. It takes a column and converts it
#' to numeric if all elements are numbers, otherwise, it converts them to character.
#' This is particularly useful for ensuring consistent data types across data processing.
#'
#' @param column A vector (column) that needs to be checked and potentially converted.
#'
#' @return Returns a vector with its elements converted to either numeric or character type,
#'         based on the content of the vector.
#'
#' @noRd
convert_column <- function(column) {
  if (is.logical(column) | is.character(column)) {
    # If column is logical, return as is
    return(column)
  } else if (is.numeric(column)) {
    as.numeric(column)
  } else if (class(column) == "call") {
    # Convert function to a single character string
    return(deparse(column))
  } else {
    # Otherwise, convert to character
    return(as.character(column))
  }
}



