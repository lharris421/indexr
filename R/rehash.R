#' Rehash RDS Files in a Directory
#'
#' This function processes all RDS files in a specified directory, generating new hashes
#' for each file's `args_list` and renaming the files accordingly. It's useful when changing
#' the hash generation algorithm or parameters.
#'
#' @param folder A string specifying the directory containing the RDS files to be rehashed.
#' @param hash_includes_timestamp Logical; if TRUE, includes timestamps in the hash generation.
#' @param ignore_na Logical; if TRUE, NA values are ignored during hash generation.
#' @param alphabetical_order Logical; if TRUE, parameters are sorted alphabetically before hash generation.
#'
#' @return The function does not return a value but renames the RDS files in the specified directory based on new hashes.
#' @export
#'
#' @examples
#' directory_path <- "path/to/your/rds/files"
#' rehash(directory_path)
rehash <- function(folder, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE) {

  files <- list.files(folder, pattern = "\\.rds$", full.names = TRUE)

  for (file in files) {
    e <- new.env()
    load(file, envir = e)

    # Ensure args_list exists in the loaded environment
    if (!"args_list" %in% ls(envir = e)) {
      warning(paste0("args_list not found in RDS file: ", file))
      next
    }

    # Generate new hash using generate_hash function
    new_hash <- generate_hash(e$args_list, hash_includes_timestamp = hash_includes_timestamp, ignore_na = ignore_na, alphabetical_order = alphabetical_order)
    print(new_hash)
    # Construct new file path with the new hash
    new_file_path <- file.path(folder, paste0(new_hash, ".rds"))

    # Rename the file
    if (!file.exists(new_file_path)) {
      file.rename(file, new_file_path)
    }
  }
}
