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
#' \dontrun{
#' # Path to the existing hash table CSV
#' hash_table_path <- "path/to/hash_table.csv"
#' # Directory containing the associated RDS files
#' rds_directory <- "path/to/rds_files"
#' update_hash_table(hash_table_path, rds_directory)
#' }
update_hash_table <- function(table_path, rds_folder, hash_includes_timestamp = FALSE,
                              ignore_na = TRUE, alphabetical_order = TRUE, algo = "xxhash64") {
  updated_table <- readr::read_csv(table_path)  # Read the updated CSV table

  for (i in 1:nrow(updated_table)) {
    row <- updated_table[i, ]
    old_hash <- row$hash

    # Load the old RDS file
    old_file_path <- file.path(rds_folder, glue("{old_hash}.rds"))
    e <- new.env()

    if (file.exists(old_file_path)) {
      e$res_list <- readRDS(old_file_path)
      e$args_list <- e$res_list$args_list

      # Ensure args_list exists in the loaded environment
      if (!"args_list" %in% ls(envir = e)) {
        warning(glue("args_list not found in RDS file: {old_file_path}"))
        next
      }

      # Update args_list with values from the updated table row
      updated_args_list <- e$args_list

      for (col_name in names(row)) {
        # Skip the hash column
        if (col_name == "hash") next

        # If the value in the CSV is not NA, update the args_list
        if (!is.na(row[[col_name]])) {
          # Retrieve the current value from args_list (if it exists)
          current_value <- get_nested_value_from_list(updated_args_list, col_name)
          print(current_value)

          # Convert the value from CSV using c_string_to_vector
          new_value <- c_string_to_vector(row[[col_name]])

          # Compare values before updating
          if (!identical(current_value, new_value)) {
            message(glue("Updating {col_name} in args_list: {current_value} -> {new_value}"))
            updated_args_list <- update_nested_list_from_csv(updated_args_list, col_name, new_value)
          }
        }
      }

      # Calculate new hash for updated args_list using generate_hash
      new_hash <- generate_hash(updated_args_list, hash_includes_timestamp = hash_includes_timestamp,
                                ignore_na = ignore_na, alphabetical_order = alphabetical_order, algo = algo)$hash

      new_file_path <- file.path(rds_folder, glue("{new_hash}.rds"))

      # Check if the new hash file already exists
      if (file.exists(new_file_path)) {
        message(glue("Hash {new_hash} already exists, skipping update for {old_hash}."))
      } else {
        # Save the updated objects under the new hash
        e$args_list <- updated_args_list
        e$res_list$args_list <- e$args_list
        saveRDS(e$res_list, file = new_file_path)
        message(glue("Updated hash {old_hash} to {new_hash}."))

        # Delete the old file if not overwritten and hashes differ
        if (old_hash != new_hash) {
          file.remove(old_file_path)
          message(glue("Deleted old hash {old_hash}."))
        }
      }

    } else {
      warning(paste0("Old file not found for hash: ", old_hash))
    }
  }
}

# Function to convert a c() string back into a vector or retain original value
c_string_to_vector <- function(str) {
  if (is.na(str)) {
    return(NA)
  }

  # Check if the string looks like a c() expression
  if (grepl("^c\\(", str)) {
    # Safely parse the string to convert it back to a vector
    tryCatch({
      eval(parse(text = str))
    }, error = function(e) {
      # In case of error, return the original string
      str
    })
  } else if (grepl("^\\d+$|^-?\\d*\\.\\d+$", str)) {
    # If it's a number, convert to numeric
    as.numeric(str)
  } else {
    # Otherwise, return the original string (e.g., for character data)
    str
  }
}

# Function to update the nested list based on a flattened column name from the CSV
update_nested_list_from_csv <- function(lst, col_name, value) {
  # Split the column name by "[[...]]" to get the nested elements
  split_names <- strsplit(col_name, "\\[\\[|\\]\\]")[[1]]
  split_names <- split_names[split_names != ""]  # Remove empty elements from splitting

  # Navigate through the list based on the split names and update the value
  current_list <- lst
  for (i in seq_along(split_names)) {
    name <- split_names[i]

    # If we reach the last name, we assign the value
    if (i == length(split_names)) {
      current_list[[name]] <- value
    } else {
      # If the sub-list doesn't exist, create it
      if (is.null(current_list[[name]])) {
        current_list[[name]] <- list()
      }
      current_list <- current_list[[name]]
    }
  }

  # Return the updated list
  return(lst)
}

# Function to retrieve a nested value from a list based on a flattened column name
get_nested_value_from_list <- function(lst, col_name) {
  split_names <- strsplit(col_name, "\\[\\[|\\]\\]")[[1]]
  split_names <- split_names[split_names != ""]  # Remove empty elements from splitting

  current_list <- lst
  for (name in split_names) {
    if (!is.null(current_list[[name]])) {
      current_list <- current_list[[name]]
    } else {
      return(NULL)
    }
  }

  return(current_list)
}
