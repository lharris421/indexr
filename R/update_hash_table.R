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

  # Read the updated CSV table without converting strings to factors
  updated_table <- readr::read_csv(table_path)

  # Loop over each row in the CSV
  for (i in 1:nrow(updated_table)) {
    row <- updated_table[i, ]
    old_hash <- row$hash

    # Load the old RDS file
    old_file_path <- file.path(rds_folder, paste0(old_hash, ".rds"))
    print(old_file_path)

    if (file.exists(old_file_path)) {
      res_list <- readRDS(old_file_path)

      if (!("args_list" %in% names(res_list))) {
        warning(glue("args_list not found in RDS file: {old_file_path}"))
        next
      }

      args_list <- res_list$args_list

      # Update args_list with values from the updated table row
      updated_args_list <- args_list

      for (col_name in names(row)) {
        # Skip the hash column
        if (col_name == "hash") next

        # If the value in the CSV is not NA, update the args_list
        # if (!is.na(row[[col_name]])) {
        # Retrieve the current value from args_list (if it exists)
        current_value <- get_nested_value_from_list(updated_args_list, col_name)

        # Convert the value from CSV using c_string_to_vector
        if (is.character(row[[col_name]])) {
          new_value <- c_string_to_vector(row[[col_name]])
        } else {
          new_value <- row[[col_name]]
        }

        # if ( is.null(current_value) ) next ## Not an argument used for that sim, need to only use NA in res lsits
        ## If they are both NA but different NA types, the NA type is not coerced
        if ((is.null(current_value) | all(is.na(current_value))) & all(is.na(new_value))) next

        # Coerce new_value to the type of current_value
        if (!all(is.na(new_value))) {
          new_value <- tryCatch({
            # If current_value is NULL or NA, keep new_value as is
            ## Do I want to coerce if it is a specific NA type??
            # if (is.null(current_value) | all(is.na(current_value))) {
            ## This now currently does that, this may help with consistency between sims
            if (is.null(current_value)) {
              new_value
            } else {
              # Coerce new_value to the class of current_value
              ## May need to rethink this
              as(new_value, class(current_value))
            }
          }, error = function(e) {
            # If coercion fails, keep new_value as is
            new_value
          })
        }

        # Compare values before updating
        if (!identical(current_value, new_value)) {
          message(glue("Updating {col_name} in args_list: {current_value} -> {new_value}"))
          updated_args_list <- update_nested_list_from_csv(updated_args_list, col_name, new_value)
        }
      }

      # Check if args_list has been updated
      if (!identical(args_list, updated_args_list)) {
        # Calculate new hash for updated args_list using generate_hash
        new_hash <- generate_hash(
          args_list = updated_args_list,
          hash_includes_timestamp = hash_includes_timestamp,
          ignore_na = ignore_na,
          alphabetical_order = alphabetical_order,
          algo = algo
        )$hash

        new_file_path <- file.path(rds_folder, paste0(new_hash, ".rds"))

        # Check if the new hash file already exists
        if (file.exists(new_file_path)) {
          message(glue("Hash {new_hash} already exists, skipping update for {old_hash}."))
        } else {
          # Update res_list with the updated args_list
          res_list$args_list <- updated_args_list

          # Save the updated objects under the new hash
          saveRDS(res_list, file = new_file_path)
          message(glue("Updated hash {old_hash} to {new_hash}."))

          # Delete the old file if hashes differ
          if (old_hash != new_hash) {
            file.remove(old_file_path)
            message(glue("Deleted old hash {old_hash}."))
          }
        }
      } else {
        message(glue("No changes detected for hash {old_hash}, skipping update."))
      }

    } else {
      warning(glue("Old file not found for hash: {old_hash}"))
    }
  }
}



# Function to convert a string back into the original R object
c_string_to_vector <- function(str) {

  if (is.na(str) || str == "") {
    return(NA)
  }

  # Remove leading and trailing whitespace
  str <- trimws(str)

  # Check if the string represents a logical value
  if (str %in% c("TRUE", "FALSE")) {
    return(as.logical(str))
  }

  # Check if the string represents a numeric value
  if (grepl("^-?\\d+\\.?\\d*$", str)) {
    return(as.numeric(str))
  }

  # Check if the string is a vector represented as c(...)
  if (grepl("^c\\(", str) && grepl("\\)$", str)) {
    # Extract the content inside c()
    content <- sub("^c\\((.*)\\)$", "\\1", str)
    # Split the content by commas
    elements <- strsplit(content, ",")[[1]]
    elements <- trimws(elements)

    # Determine the data type of the elements
    if (all(elements %in% c("TRUE", "FALSE"))) {
      return(as.logical(elements))
    } else if (all(grepl("^-?\\d+\\.?\\d*$", elements))) {
      return(as.numeric(elements))
    } else {
      elements <- sub('^"(.*)"$', '\\1', elements)
      return(elements)
    }
  }

  # Return the string as is for character data
  return(str)
}

# Function to update the nested list based on a flattened column name from the CSV
update_nested_list_from_csv <- function(lst, col_name, value) {
  split_names <- unlist(strsplit(col_name, "\\[\\[|\\]\\]"))
  split_names <- split_names[split_names != ""]  # Remove empty strings

  # Recursive function to update the nested list
  set_value <- function(current_list, names_vec, value) {
    name <- names_vec[1]
    if (length(names_vec) == 1) {
      current_list[[name]] <- value
    } else {
      if (is.null(current_list[[name]])) {
        current_list[[name]] <- list()
      }
      current_list[[name]] <- set_value(current_list[[name]], names_vec[-1], value)
    }
    return(current_list)
  }

  lst <- set_value(lst, split_names, value)
  return(lst)
}

# Function to retrieve a nested value from a list based on a flattened column name
get_nested_value_from_list <- function(lst, col_name) {
  split_names <- unlist(strsplit(col_name, "\\[\\[|\\]\\]"))
  split_names <- split_names[split_names != ""]  # Remove empty strings

  # Recursive function to get the nested value
  get_value <- function(current_list, names_vec) {
    name <- names_vec[1]
    if (is.null(current_list[[name]])) {
      return(NULL)
    } else if (length(names_vec) == 1) {
      return(current_list[[name]])
    } else {
      return(get_value(current_list[[name]], names_vec[-1]))
    }
  }

  value <- get_value(lst, split_names)
  return(value)
}

