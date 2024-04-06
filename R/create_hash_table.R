#' Create a Hash Table from RDS Files
#'
#' Scans a directory for RDS files, extracts arguments lists (`args_list`) from each file,
#' and combines them into a single data frame. Each row in the resulting data frame represents
#' the arguments used for one RDS file, identified by its hash. Optionally, the function can
#' filter the data frame based on specified criteria and save it to a file.
#'
#' @param path A string specifying the directory containing the RDS files.
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
create_hash_table <- function(path, save_path = NULL, filter_list = NULL) {
  # List all RDS files in the given directory
  files <- list.files(path, pattern = "\\.rda$", full.names = TRUE)

  # Initialize an empty list to store the args_lists along with their hashes
  all_args_lists <- list()

  # Read each file and extract args_list, converting all values to character
  for (file in files) {
    e <- new.env()
    load(file, envir = e)
    if ("args_list" %in% names(e)) {
      # Convert all elements of args_list to character
      char_args_list <- lapply(e$args_list, as.character)
      char_args_list$hash <- stringr::str_remove(basename(file), ".rda$")
      all_args_lists[[basename(file)]] <- char_args_list
    }
  }

  # Combine all args_lists into a data frame using bind_rows
  args_df <- bind_rows(lapply(all_args_lists, as.data.frame.list))

  # Apply filters if filter_list is provided
  if (!is.null(filter_list) && is.list(filter_list)) {
    for (col_name in names(filter_list)) {
      args_df <- args_df[args_df[[col_name]] == filter_list[[col_name]], ]
    }
  }

  # Save the table if a save_path is provided
  if (!is.null(save_path)) {
    write.csv(args_df, file = save_path, row.names = FALSE)
  }

  return(args_df)
}
