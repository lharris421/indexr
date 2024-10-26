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
generate_hash <- function(args_list, hash_includes_timestamp = FALSE, ignore_na = TRUE,
                          alphabetical_order = TRUE, algo = "xxhash64", ignore_script_name = FALSE) {

  # Optionally remove 'script_name' if ignore_script_name is TRUE
  if (ignore_script_name && "script_name" %in% names(args_list)) {
    args_list$script_name <- NULL
  }

  # Check for 'function_name' and process through combine_arguments_with_defaults
  if (!is.null(args_list$function_name) && !is.na(args_list$function_name)) {
    args_list <- combine_arguments_with_defaults(args_list)
  }

  # Filter out NULL and empty list values
  args_list <- Filter(function(x) !is.null(x), args_list)
  args_list <- Filter(function(x) !(is.list(x) && length(x) == 0), args_list)

  # Order alphabetically if alphabetical_order is TRUE
  if (alphabetical_order) {
    args_list <- sort_list_recursive(args_list)
  }

  # Apply data type conversion to each element of args_list
  args_list <- lapply(args_list, convert_type)

  # Remove NA values if ignore_na is TRUE
  if (ignore_na) {
    args_list <- Filter(function(x) is.list(x) || !is.na(x), args_list)
  }

  # Handle timestamp
  if (!hash_includes_timestamp) {
    args_list$timestamp <- NULL
  }

  # Generate the hash
  res <- list(args_list = args_list, hash = digest::digest(args_list, algo = algo))

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
