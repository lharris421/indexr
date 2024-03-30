#' Read Objects Based on Parameter Grid
#'
#' Reads objects from files in a specified directory. The files are identified based on a hash generated from a grid of parameters. This function iterates over each combination of parameters in the grid, generates a hash for each, and attempts to load the corresponding object from the directory.
#'
#' @param folder A string specifying the directory where the objects are stored.
#' @param params_grid A data frame or matrix where each row represents a different set of parameters to be hashed and checked for corresponding files.
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
#' params_grid <- data.frame(param1 = c("a", "b"), param2 = c(1, 2))
#' read_objects(folder_path, params_grid)
#' }
read_objects <- function(folder, params_grid, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE) {
  for (i in 1:nrow(params_grid)) {
    # Apply the data type conversion function to each column of the row
    converted_row <- lapply(params_grid[i, ], convert_column)

    # Convert the row to a named list
    args_list <- setNames(as.list(converted_row), names(params_grid))

    # Generate hash using generate_hash function
    hash <- generate_hash(args_list, hash_includes_timestamp = hash_includes_timestamp, ignore_na = ignore_na, alphabetical_order = alphabetical_order)

    # Construct the file path and check if it exists
    file_path <- file.path(folder, paste0(hash, ".rds"))
    if (file.exists(file_path)) {
      load(file_path, envir = globalenv())
    } else {
      warning(paste0("File not found for hash: ", hash))
    }
  }
}
