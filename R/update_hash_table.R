#' Update Hash Table Based on New Parameters
#'
#' This function updates an existing hash table by re-hashing each set of parameters with potentially updated values. It loads RDS files based on their existing hashes, updates the parameters, generates new hashes, and saves the files with the new hashes. The old files are deleted if their hashes differ from the new ones.
#'
#' @param table_path A string specifying the path to the CSV file containing the existing hash table.
#' @param rds_folder A string specifying the directory containing the RDS files associated with the hash table.
#' @param hash_includes_timestamp Logical; if TRUE, timestamps are included in the hash generation.
#' @param ignore_na Logical; if TRUE, NA values are ignored during hash generation.
#' @param alphabetical_order Logical; if TRUE, parameters are sorted alphabetically before hash generation.
#'
#' @return The function does not return a value but saves updated RDS files and deletes old files as needed.
#' @export
#'
#' @examples
#' # Path to the existing hash table CSV
#' hash_table_path <- "path/to/hash_table.csv"
#' # Directory containing the associated RDS files
#' rds_directory <- "path/to/rds_files"
#' update_hash_table(hash_table_path, rds_directory)
update_hash_table <- function(table_path, rds_folder, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE) {
  updated_table <- read.csv(table_path, stringsAsFactors = FALSE)

  for (i in 1:nrow(updated_table)) {
    row <- updated_table[i, ]
    old_hash <- row$hash

    # Load the old RDS file
    old_file_path <- file.path(rds_folder, paste0(old_hash, ".rds"))
    e <- new.env()
    if (file.exists(old_file_path)) {
      load(old_file_path, envir = e)

      # Ensure args_list exists in the loaded environment
      if (!"args_list" %in% ls(envir = e)) {
        warning(paste0("args_list not found in RDS file: ", old_file_path))
        next
      }

      # Update args_list with values from the updated table row
      updated_args_list <- e$args_list
      for (col_name in names(updated_table)) {
        if (col_name != "hash" && !is.na(row[[col_name]])) {
          updated_args_list[[col_name]] <- row[[col_name]]
        }
      }

      # Calculate new hash for updated args_list using generate_hash
      if (updated_args_list$lambda == "across") {
        updated_args_list[["timestamp"]] <- NULL
      }
      new_hash <- generate_hash(updated_args_list, hash_includes_timestamp = hash_includes_timestamp, ignore_na = ignore_na, alphabetical_order = alphabetical_order)

      # Save the updated objects under new hash
      new_file_path <- file.path(rds_folder, paste0(new_hash, ".rds"))
      e$args_list <- updated_args_list
      save(list = ls(envir = e), file = new_file_path, envir = e)

      # Delete the old file if not overwritten and hashes differ
      if (old_hash != new_hash) {
        file.remove(old_file_path)
      }

    } else {
      warning(paste0("Old file not found for hash: ", old_hash))
    }
  }
}
