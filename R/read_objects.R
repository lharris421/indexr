#' Read Objects Based on Parameter Grid
#'
#' Reads objects from files in a specified directory. The files are identified based on a hash generated from a grid of parameters. This function iterates over each combination of parameters in the grid, generates a hash for each, and attempts to load the corresponding object from the directory.
#'
#' @param folder A string specifying the directory where the objects are stored.
#' @param args A data frame or matrix where each row represents a different set of parameters to be hashed and checked for corresponding files.
#' @param hash_includes_timestamp Logical; if TRUE, includes timestamps in the hash generation process.
#' @param ignore_na Logical; if TRUE, NA values are ignored during hash generation.
#' @param alphabetical_order Logical; if TRUE, parameters are sorted alphabetically before hash generation.
#'
#' @return The function invisibly returns `NULL` and is primarily used for its side effect of loading objects into the global environment.
#' @export
#'
#' @examples
#' \dontrun{
#' folder_path <- "path/to/your/saved/objects"
#' args <- data.frame(param1 = c("a", "b"), param2 = c(1, 2))
#' read_objects(folder_path, args)
#' }
read_objects <- function(folder, args_list, hash_includes_timestamp = FALSE,
                         ignore_script_name = FALSE,
                         ignore_na = TRUE, alphabetical_order = TRUE,
                         algo = "xxhash64", print_hash = FALSE) {

  # Convert args to a list if it's a single row of a data frame or matrix
  if (is.data.frame(args_list) || is.matrix(args_list)) {
    if (nrow(args_list) != 1) {
      stop("args_list must be a single row of a data frame or matrix.")
    }
    args_list <- setNames(as.list(args_list[1, ]), names(args_list))
  } else if (!is.list(args_list)) {
    stop("args_list must be a list, data frame, or matrix.")
  }

  # Generate hash using generate_hash function
  res <- generate_hash(
    args_list, hash_includes_timestamp = hash_includes_timestamp,
    ignore_na = ignore_na, alphabetical_order = alphabetical_order,
    algo = algo, ignore_script_name = ignore_script_name
  )
  hash <- res$hash
  if (print_hash) print(hash)

  # Construct the file path based on save_method
  file_extension <- ".rds"
  file_path <- file.path(folder, paste0(hash, file_extension))

  if (file.exists(file_path)) {
      loaded_objects <- readRDS(file_path)
      if (length(loaded_objects) < 2) {
        stop("The loaded .rds file does not contain the expected number of objects.")
      }
      return(loaded_objects[[2]]) # Return the second item of the list
  } else {
    warning(paste0("File not found for hash: ", hash))
  }
}



