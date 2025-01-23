#' Start Tagging Files in a Directory
#'
#' Initializes the tagging process by creating an \code{indexer_tagging.txt} file in the specified directory.
#'
#' @param path A character string specifying the path to the directory where the tagging file will be created.
#' @return No return value. This function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' # Start tagging in the desired folder
#' start_tagging("/your/directory/path")
#' }
start_tagging <- function(path, tagging_file_name = "indexr_tagging.txt") {

  ## Checks
  check_is_directory(path)
  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  # Combine the provided path with the filename
  file_path <- file.path(path, tagging_file_name)

  # Create the file at the specified location if it doesn't exist
  if (!file.exists(file_path)) {
    file.create(file_path)
  }
}

#' Clean Up Untagged .rds Files in a Directory
#'
#' Removes any \code{.rds} files in the specified folder that are not listed in the \code{indexer_tagging.txt} tagging file.
#'
#' @param folder A character string specifying the path to the directory to clean up.
#' @return No return value. This function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' # Clean up untagged .rds files in the folder
#' cleanup("/your/directory/path")
#' }
cleanup <- function(folder, tagging_file_name = "indexr_tagging.txt", cutoff_date = NULL) {

  ## Checks
  check_is_directory(folder)
  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  # Path to the tagging file
  tagging_file <- file.path(folder, tagging_file_name)

  # Check if the tagging file exists
  if (!file.exists(tagging_file)) {
    stop("Tagging file 'indexer_tagging.txt' does not exist in the specified folder.")
  }

  # Read the tagging file as a data frame
  tagging_data <- read.table(tagging_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("hash", "timestamp"))

  # Check if the tagging file is empty
  if (nrow(tagging_data) == 0) {
    stop("The tagging file 'indexer_tagging.txt' is empty. No tagged files to compare for cleanup.")
  }

  # Convert timestamps to POSIXct objects
  tagging_data$timestamp <- as.POSIXct(tagging_data$timestamp, format = "%Y-%m-%d %H:%M:%S")

  # Get a list of all .rds files in the folder
  rds_files <- list.files(folder, pattern = "\\.rds$", full.names = TRUE)
  ## rds_parameter_files <- rds_files[stringr::str_detect(rds_files, "_parameters\\.rds")]
  rds_files <- rds_files[!stringr::str_detect(rds_files, "_parameters\\.rds")]

  # Extract the hashes (filenames without extension) from the .rds files
  rds_hashes <- basename(rds_files)
  rds_hashes <- sub("\\.rds$", "", rds_hashes)

  # Create a data frame of existing .rds files with their hashes
  rds_data <- data.frame(hash = rds_hashes, filepath = rds_files, stringsAsFactors = FALSE)

  # Identify untagged files (hashes not in tagging_data)
  untagged_files <- rds_data$filepath[!rds_data$hash %in% tagging_data$hash]

  # Initialize files to delete with untagged files
  files_to_delete <- untagged_files

  # If cutoff_date is supplied, identify tagged files not read since cutoff_date
  if (!is.null(cutoff_date)) {
    # Convert cutoff_date to POSIXct
    cutoff_datetime <- as.POSIXct(cutoff_date, format = "%Y-%m-%d %H:%M:%S")
    if (is.na(cutoff_datetime)) {
      stop("Invalid cutoff_date format. Please use 'YYYY-MM-DD HH:MM:SS'.")
    }

    # Identify hashes with last read timestamp before cutoff_date
    old_hashes <- tagging_data$hash[tagging_data$timestamp < cutoff_datetime]

    # Get filepaths of these old hashes
    old_files <- rds_data$filepath[rds_data$hash %in% old_hashes]

    # Add to files to delete
    files_to_delete <- unique(c(files_to_delete, old_files))
  }

  # Remove duplicates in case of overlap
  files_to_delete <- unique(files_to_delete)
  files_to_delete <- c(files_to_delete, str_replace(files_to_delete, "\\.rds", "_parameters\\.rds"))

  # Check if there are any files to delete
  if (length(files_to_delete) > 0) {
    message("The following .rds files will be removed:")
    print(files_to_delete)

    # Ask for user confirmation
    confirm <- utils::askYesNo("Do you want to proceed with deleting these files?")

    if (isTRUE(confirm)) {
      # Remove the files
      file.remove(files_to_delete)
      message("Specified .rds files have been deleted.")
    } else if (isFALSE(confirm)) {
      message("Deletion canceled by the user.")
    } else {
      message("No response detected. Deletion canceled.")
    }
  } else {
    message("No .rds files to remove.")
  }

  check_missing_pairs(folder)

}


#' Close Tagging Session by Deleting the Tagging File
#'
#' Deletes the \code{indexer_tagging.txt} tagging file from the specified folder, ending the tagging session.
#'
#' @param folder A character string specifying the path to the directory containing the tagging file.
#' @return No return value. This function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' # Close tagging by deleting the tagging file
#' close_tagging("/your/directory/path")
#' }
close_tagging <- function(folder, tagging_file_name = "indexr_tagging.txt") {

  ## Checks
  check_is_directory(path)
  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  # Path to the tagging file
  tagging_file <- file.path(folder, tagging_file_name)

  # Check if the tagging file exists
  if (file.exists(tagging_file)) {
    # Delete the tagging file
    file.remove(tagging_file)
    message(glue::glue("Tagging file '{tagging_file_name}' has been deleted."))
  } else {
    warning(glue::glue("Tagging file '{tagging_file_name}' does not exist in the specified folder."))
  }
}

#' Title
#'
#' @param hash_table
#' @param folder
#' @param mode
#' @param column
#'
#' @return
#' @export
#'
#' @examples
cleanup_from_hash_table <- function(folder, hash_table,
                                    mode = c("manual", "all"),
                                    column = NULL) {
  mode <- match.arg(mode)

  # Basic checks
  check_is_directory(folder)

  # In "manual" mode, we look for rows where 'column' == TRUE
  # In "all" mode, we remove every hash in hash_table
  if (mode == "manual") {
    if (is.null(column)) {
      stop("In 'manual' mode, you must specify the column name containing the deletion indicator.")
    }
    if (!column %in% names(hash_table)) {
      stop("Column '", column, "' not found in the hash table.")
    }
    # Identify rows for which the user has marked deletion
    del_rows <- which(isTRUE(hash_table[[column]]) | hash_table[[column]] == TRUE)
    if (length(del_rows) == 0) {
      message("No rows with '", column, "' == TRUE. Nothing to delete.")
      return(invisible(NULL))
    }
    # Subset to those hashes
    del_hashes <- hash_table$hash[del_rows]
  } else {
    # "all" mode: remove every hash in the hash_table
    del_hashes <- hash_table$hash
  }

  # Construct full paths for results and parameter files
  results_files  <- file.path(folder, paste0(del_hashes, ".rds"))
  params_files   <- file.path(folder, paste0(del_hashes, "_parameters.rds"))
  files_to_delete <- c(results_files, params_files)

  # Check which actually exist on disk
  files_to_delete <- files_to_delete[file.exists(files_to_delete)]

  if (length(files_to_delete) == 0) {
    message("No matching files found on disk to delete.")
    return(invisible(NULL))
  }

  message("The following files will be removed:")
  print(files_to_delete)

  # Ask for user confirmation
  confirm <- utils::askYesNo("Do you want to proceed with deleting these files?")
  if (isTRUE(confirm)) {
    file.remove(files_to_delete)
    message("Specified files have been deleted.")
  } else if (isFALSE(confirm)) {
    message("Deletion canceled by the user.")
  } else {
    message("No response detected. Deletion canceled.")
  }

  # Optionally, re-check pairs
  check_missing_pairs(folder)

  invisible(NULL)
}

