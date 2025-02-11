#' Generate a Consistent Hash for an Argument List
#'
#' This function generates a hash value for a given list of arguments. It is designed to produce a consistent hash by optionally removing NA values, ordering arguments alphabetically, handling timestamp inclusion, etc.
#'
#' @param parameters_list A named list of arguments for which the hash will be generated. Each element in the list should correspond to a parameter.
#' @param hash_includes_timestamp Logical; if FALSE, any timestamp included in parameters_list will be removed before hash generation. If TRUE, the timestamp will be included in the hash calculation.
#' @param ignore_na Logical; if TRUE, any NA values in parameters_list will be removed before hash generation.
#' @param alphabetical_order Logical; if TRUE, the arguments in parameters_list will be sorted alphabetically by their names before hash generation.
#' @param algo The hash algorithm to use (See \code{?digest})
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#'
#' @return A character string representing the hash value of the provided argument list.
#' @export
#'
#' @examples
#' args <- list(param1 = "value1", param2 = 100, param3 = NA)
#' hash_val <- generate_hash(args)
generate_hash <- function(parameters_list, hash_includes_timestamp = FALSE, ignore_na = TRUE,
                          alphabetical_order = TRUE, algo = "xxhash64", ignore_script_name = FALSE) {

  ## Optionally remove 'script_name' if ignore_script_name is TRUE
  if (ignore_script_name && "script_name" %in% names(parameters_list)) {
    parameters_list$script_name <- NULL
  }

  ## Order alphabetically if alphabetical_order is TRUE
  if (alphabetical_order) {
    parameters_list <- sort_list_recursive(parameters_list)
  }

  ## Apply data type conversion to each element of parameters_list
  ## Ensures consistency when reading in and out of R
  parameters_list <- lapply(parameters_list, convert_type)

  ## Remove NA values if ignore_na is TRUE
  if (ignore_na) {
    parameters_list <- Filter(function(x) is.list(x) || !is.na(x), parameters_list)
  }

  ## Handle timestamp
  if (!hash_includes_timestamp) {
    parameters_list$timestamp <- NULL
  }

  ## Generate the hash
  res <- list(parameters_list = parameters_list, hash = digest::digest(parameters_list, algo = algo))

  return(res)
}

sort_list_recursive <- function(x) {
  if (is.list(x) & length(x) > 0) {
    # Sort the list based on names
    sorted_x <- x[order(names(x))]

    # Apply sorting recursively to any nested lists
    sorted_x <- lapply(sorted_x, sort_list_recursive)

    return(sorted_x)
  } else {
    # If it's not a list, return the element as is
    return(x)
  }
}
