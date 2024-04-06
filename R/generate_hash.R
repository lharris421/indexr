#' Generate a Consistent Hash for an Argument List
#'
#' This function generates a hash value for a given list of arguments. It is designed to produce a consistent hash by optionally removing NA values, ordering arguments alphabetically, and handling timestamp inclusion.
#'
#' @param args_list A named list of arguments for which the hash will be generated. Each element in the list should correspond to a parameter.
#' @param hash_includes_timestamp Logical; if FALSE, any timestamp included in args_list will be removed before hash generation. If TRUE, the timestamp will be included in the hash calculation.
#' @param ignore_na Logical; if TRUE, any NA values in args_list will be removed before hash generation.
#' @param alphabetical_order Logical; if TRUE, the arguments in args_list will be sorted alphabetically by their names before hash generation.
#'
#' @return A character string representing the hash value of the provided argument list.
#' @export
#'
#' @examples
#' \dontrun{
#' args <- list(param1 = "value1", param2 = 100, param3 = NA)
#' hash_val <- generate_hash(args)
#' }
generate_hash <- function(args_list, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE, algo = "xxhash64") {

  # Apply data type conversion to each element of args_list
  args_list <- lapply(args_list, function(x) {
    if (is.logical(x) | is.numeric(x) | is.character(x)) {
      return(x)
    } else if (class(x) == "call") {
      return(deparse(x))  # Convert function to a single character string
    } else {
      return(as.character(x))
    }
  })

  # Remove NA values if ignore_na is TRUE
  if (ignore_na) {
    args_list <- Filter(function(x) !is.na(x), args_list)
  }

  # Order alphabetically if alphabetical_order is TRUE
  if (alphabetical_order) {
    args_list <- args_list[order(names(args_list))]
  }

  # Handle timestamp
  if (!hash_includes_timestamp) {
    args_list$timestamp <- NULL
  }

  return(digest::digest(args_list, algo = algo))
}



