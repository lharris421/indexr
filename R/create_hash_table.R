#' Create a Hash Table from RDS Files
#'
#' Scans a directory for RDS files, extracts arguments lists (`args_list`) from each file,
#' and combines them into a single data frame. Each row in the resulting data frame represents
#' the arguments used for one RDS file, identified by its hash. Optionally, the function can
#' filter the data frame based on specified criteria and save it to a file.
#'
#' @param folder A string specifying the directory containing the RDS files.
#' @param save_path An optional string specifying the path to save the resulting hash table as a CSV file.
#'                 If `NULL`, the hash table is not saved.
#' @param filter_list An optional list of filters to apply to the hash table.
#'                    Each element of the list should be named according to a column in the hash table
#'                    and contain the value to filter by.
#'
#' @return A data frame where each row corresponds to an `args_list` from an RDS file,
#'         with an additional column for the hash of each set of arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' directory_path <- "path/to/your/rds/files"
#' hash_table <- create_hash_table(directory_path)
#' # To save the hash table to a file
#' create_hash_table(directory_path, save_path = "path/to/save/hash_table.csv")
#' }
create_hash_table <- function(folder, save_path = NULL, filter_list = NULL) {

  check_is_directory(folder)
  check_missing_pairs(folder)

  if (!is.null(save_path)) save_path <- check_and_fix_extension(save_path, "csv")

  # List all files in the given directory based on the file pattern
  files <- list.files(folder, pattern = "_parameters\\.rds$", full.names = TRUE)

  # Initialize an empty list to store the args_lists along with their hashes
  all_args_lists <- list()

  for (file in files) {

    loaded_objects <- readRDS(file)
    args_list <- lapply(loaded_objects, convert_type)

    args_list$hash <- stringr::str_remove(basename(file), "_parameters\\.rds$")
    args_list <- convert_vectors_to_c_strings(args_list)

    # Flatten the nested list with descriptive column names
    flat_args_list <- flatten_nested_list(args_list)

    # Convert all elements to characters to avoid type conflict
    flat_args_list <- lapply(flat_args_list, as.character)

    all_args_lists[[basename(file)]] <- flat_args_list
  }

  # Combine all args_lists into a data frame using bind_rows
  args_df <- dplyr::bind_rows(lapply(all_args_lists, as.data.frame.list, optional = TRUE))

  # Apply filters if filter_list is provided
  if (!is.null(filter_list) && is.list(filter_list)) {
    for (col_name in names(filter_list)) {
      args_df <- args_df[!is.na(args_df[[col_name]]) & args_df[[col_name]] == filter_list[[col_name]], ]
    }
    args_df <- args_df[, !sapply(args_df, function(col) all(is.na(col)))]
  }

  # Save the table if a save_path is provided
  if (!is.null(save_path)) {
    readr::write_csv(args_df, file = save_path, quote = "needed")
  }

  return(args_df)
}

# Recursively flatten a nested list and create descriptive column names
flatten_nested_list <- function(lst, parent_name = NULL) {

  flat_list <- list()

  for (name in names(lst)) {

    item <- lst[[name]]

    if (is.null(parent_name)) {
      col_name <- name
    } else {
      col_name <- paste0(parent_name, "[[", name, "]]")
    }

    if (is.list(item) && !is.data.frame(item)) {
      # Recursively flatten the nested list
      flat_list <- c(flat_list, flatten_nested_list(item, col_name))
    } else {
      flat_list[[col_name]] <- item
    }
  }

  return(flat_list)
}

# Convert vectors to c() strings for CSV representation
convert_vectors_to_c_strings <- function(lst) {
  lapply(lst, function(element) {
    if (is.list(element)) {
      convert_vectors_to_c_strings(element)
    } else if (is.vector(element) && length(element) > 1) {
      # Convert vector to a c() string
      paste0("c(", paste(sapply(element, function(x) {
        if (is.character(x)) {
          x
        } else {
          as.character(x)
        }
      }), collapse = ", "), ")")
    } else {
      element
    }
  })
}



