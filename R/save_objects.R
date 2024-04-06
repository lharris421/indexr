#' Save R Objects with Parameter Hashing
#'
#' Saves a list of R objects to a file in a specified directory, with filenames
#' based on a hash of their parameters. Optionally includes timestamps in the
#' parameter list before hashing and provides options for overwriting existing
#' files, ignoring NA values, and sorting parameters alphabetically.
#'
#' @param folder A string specifying the directory where the R objects will be saved.
#' @param args_list A named list of arguments based on which the hash for filename will be generated.
#' @param ... The data to be save.
#' @param hash_includes_timestamp Logical; if TRUE, the timestamp will be included in the hash calculation.
#' @param ignore_na Logical; if TRUE, NA values in `args_list` will be ignored for hash generation.
#' @param alphabetical_order Logical; if TRUE, the arguments in `args_list` will be sorted alphabetically before hash generation.
#' @param overwrite Logical; if TRUE, existing files with the same hash name will be overwritten.
#' @param include_timestamp Logical; if TRUE, a timestamp will be added to `args_list` before hash generation.
#'
#' @return Invisible NULL. This function is used for its side effect of saving files.
#' @export
#'
#' @examples
#' \dontrun{
#' args_list <- list(param1 = "value1", param2 = 100)
#' save_objects("path/to/save", args_list)
#' }
save_objects <- function(folder, args_list, ..., hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE, overwrite = FALSE, include_timestamp = TRUE,
                         algo = "xxhash64") {

  ## Add timestampe
  if (include_timestamp) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    args_list$timestamp <- timestamp
  }

  # Generate hash using generate_hash function
  hash <- generate_hash(args_list, hash_includes_timestamp = hash_includes_timestamp, ignore_na = ignore_na, alphabetical_order = alphabetical_order, algo = algo)

  # Construct the file path
  file_path <- file.path(folder, paste0(hash, ".rda"))

  # Check for existing file
  if (file.exists(file_path) && !overwrite) {
    warning("Existing file found. Set 'overwrite = TRUE' to overwrite.")
    return(invisible())
  }

  # Save args_list as the first object followed by the other objects
  save(args_list, ..., file = file_path)
}
