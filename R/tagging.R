#' Start Tagging File in a Directory
#'
#' Initializes the tagging process by creating an \code{txt} file in the specified directory. Tagging is mainly helpful for removing unused results.
#'
#' @param path A character string specifying the path to the directory where the tagging file will be created.
#' @param tagging_file_name A character string for a txt file the tagging information is to be saved under.
#' @return No return value. This function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
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
#' Removes any \code{.rds} files in the specified folder that are not listed in the tagging file.
#'
#' @param folder A character string specifying the path to the directory to clean up.
#' @param tagging_file_name A character string for a txt file the tagging information is saved under.
#' @param cutoff_date A character string in "%Y-%m-%d %H:%M:%S" format used to specify that any tagged files before the date should also be removed.
#' @return No return value. This function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' cleanup("/your/directory/path")
#' }
cleanup <- function(folder, tagging_file_name = "indexr_tagging.txt", cutoff_date = NULL) {

  ## Checks
  check_is_directory(folder)
  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  ## Get tagging data
  tagging_file <- file.path(folder, tagging_file_name)

  if (!file.exists(tagging_file)) {
    stop("Tagging file 'indexer_tagging.txt' does not exist in the specified folder.")
  }

  tagging_data <- read.table(tagging_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, col.names = c("hash", "timestamp"))

  if (nrow(tagging_data) == 0) {
    stop("The tagging file 'indexer_tagging.txt' is empty. No tagged files to compare for cleanup.")
  }

  ## Convert timestamps to POSIXct objects
  tagging_data$timestamp <- as.POSIXct(tagging_data$timestamp, format = "%Y-%m-%d %H:%M:%S")

  ## Get a list of all hashes from results files and put in df with file paths
  rds_files <- list.files(folder, pattern = "\\.rds$", full.names = TRUE)
  rds_files <- rds_files[!stringr::str_detect(rds_files, "_parameters\\.rds")]
  rds_hashes <- basename(rds_files)
  rds_hashes <- sub("\\.rds$", "", rds_hashes)
  rds_data <- data.frame(hash = rds_hashes, filepath = rds_files, stringsAsFactors = FALSE)

  ## Identify untagged files (hashes not in tagging_data)
  untagged_files <- rds_data$filepath[!rds_data$hash %in% tagging_data$hash]

  ## Initialize files to delete with untagged files
  files_to_delete <- untagged_files

  ## If cutoff_date is supplied, identify tagged files not read since cutoff_date
  if (!is.null(cutoff_date)) {
    # Convert cutoff_date to POSIXct
    cutoff_datetime <- as.POSIXct(cutoff_date, format = "%Y-%m-%d %H:%M:%S")
    if (is.na(cutoff_datetime)) {
      stop("Invalid cutoff_date format. Please use 'YYYY-MM-DD HH:MM:SS'.")
    }

    ## Identify hashes with last read timestamp before cutoff_date
    old_hashes <- tagging_data$hash[tagging_data$timestamp < cutoff_datetime]

    ## Add to files to delete
    old_files <- rds_data$filepath[rds_data$hash %in% old_hashes]
    files_to_delete <- unique(c(files_to_delete, old_files))

  }

  files_to_delete <- unique(files_to_delete) ## Shouldn't be needed but just a precaution
  ## Add all parameters files to be deleted too
  files_to_delete <- c(files_to_delete, str_replace(files_to_delete, "\\.rds", "_parameters\\.rds"))

  ## Delete files
  if (length(files_to_delete) > 0) {
    message("The following .rds files will be removed:")
    print(files_to_delete)

    ## Ask for user confirmation
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
#' Deletes the tagging file from the specified folder, ending the tagging session.
#'
#' @param folder A character string specifying the path to the directory containing the tagging file.
#' @param tagging_file_name A character string for a txt file the tagging information was saved under.
#' @return No return value. This function is called for its side effects.
#' @export
#' @examples
#' \dontrun{
#' close_tagging("/your/directory/path")
#' }
close_tagging <- function(folder, tagging_file_name = "indexr_tagging.txt") {

  ## Checks
  check_is_directory(path)
  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  ## Path to the tagging file
  tagging_file <- file.path(folder, tagging_file_name)

  ## Delete the tagging file
  if (file.exists(tagging_file)) {
    file.remove(tagging_file)
    message(glue::glue("Tagging file '{tagging_file_name}' has been deleted."))
  } else {
    warning(glue::glue("Tagging file '{tagging_file_name}' does not exist in the specified folder."))
  }

}
#' Remove Files Based on Hash Table
#'
#' Allows the user to leverage the \code{generate_hash} function to generate a table that is subsequently used to remove indicated results.
#'
#' @param folder A string specifying the directory containing the RDS files.
#' @param hash_table A \code{data.frame} from \code{create_hash_table}.
#' @param mode A character string. When \code{mode = "manual"} (default) the function expects that the user will add a column to a hash table that indicated which files to delete. When \code{mode = "all"}, any results in the hash table will be removed.
#' @param column A character string indicating the logical column in \code{hash_table} specifying which files to delete.
#'
#' @details
#' There are a few ways to use this. When \code{mode = "manual"} (default) the function expects that the user will add a column to a hash table that indicated which files to delete. When \code{mode = "all"}, any results in the hash table will be removed. This is generally only used when a \code{filter_list} is passed to \code{create_hash_table}.
#'
#' @return Nothing, this function is called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' Create a hash table
#' hash_table <- create_hash_table(folder = ".")
#'
#' Delete files based on hash table
#' cleanup_from_hash_table(folder = ".", hash_table = hash_table, mode = "all")
#' }
cleanup_from_hash_table <- function(folder, hash_table,
                                    mode = c("manual", "all"),
                                    column = NULL, verify = TRUE) {
  mode <- match.arg(mode)

  ## Basic checks
  check_is_directory(folder)

  ## In "manual" mode, look for rows where 'column' == TRUE
  if (mode == "manual") {
    if (is.null(column)) {
      stop("In 'manual' mode, you must specify the column name containing the deletion indicator.")
    }
    if (!column %in% names(hash_table)) {
      stop("Column '", column, "' not found in the hash table.")
    }
    ## Identify hashes for which the user has marked deletion
    del_rows <- which(isTRUE(hash_table[[column]]) | hash_table[[column]] == TRUE)
    if (length(del_rows) == 0) {
      message("No rows with '", column, "' == TRUE. Nothing to delete.")
      return(invisible(NULL))
    }
    del_hashes <- hash_table$hash[del_rows]
  } else {
    ## In "all" mode, remove every hash in hash_table
    del_hashes <- hash_table$hash
  }

  ## Construct full paths for results and parameter files
  results_files  <- file.path(folder, paste0(del_hashes, ".rds"))
  params_files   <- file.path(folder, paste0(del_hashes, "_parameters.rds"))
  files_to_delete <- c(results_files, params_files)
  files_to_delete <- files_to_delete[file.exists(files_to_delete)]

  if (length(files_to_delete) == 0) {
    message("No matching files found on disk to delete.")
    return(invisible(NULL))
  }

  message("The following files will be removed:")
  print(files_to_delete)

  ## Ask for user confirmation
  if (verify) {
    confirm <- utils::askYesNo("Do you want to proceed with deleting these files?")
  } else {
    confirm <- TRUE
  }

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

