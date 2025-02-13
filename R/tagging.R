#' Monitor result file usage and cleanup unused files
#'
#' Tagging is mainly helpful for removing unused results.
#' \code{start_tagging()} initializes the tagging process by creating a \code{txt} file in the results directory which will keep a record of which results are being read by \code{read_objects()}.
#' \code{cleanup()} removes any \code{.rds} files in the specified folder that are not listed in the tagging file.
#' \code{close_tagging()} deletes the tagging file, ending the tagging session.
#'
#' @param folder A character string specifying the path to the directory where the result files are saved and where the tagging file will be created.
#' @param tagging_file_name A character string for a txt file the tagging information is to be saved under.
#' @param cutoff_date A character string in "%Y-%m-%d %H:%M:%S" format used to specify that any tagged files before the date should also be removed.
#' @param request_confirmation Logical, if TRUE will request user input before proceeding to delete files.
#'
#' @return No return value. This function is called for its side effects.
#' @export
#' @examples
#' ## Setup
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#'
#' ## Save example objects
#' parameters_list1 <- list(example = "tagging1")
#' parameters_list2 <- list(example = "tagging2")
#' save_objects(folder = tmp_dir, results = 1, parameters_list = parameters_list1)
#' save_objects(folder = tmp_dir, results = 2, parameters_list = parameters_list2)
#'
#' ## See the files have been saved
#' list.files(tmp_dir)
#'
#' ## Start tagging
#' start_tagging(tmp_dir)
#'
#' ## Read back in one the first file, this causes this file to be tagged
#' res1 <- read_objects(folder = tmp_dir, parameters_list = parameters_list1)
#'
#' ## Remove untagged file without confirmation (that for parameters_list2)
#' cleanup(tmp_dir, request_confirmation = FALSE)
#'
#' ## See that one file was removed
#' list.files(tmp_dir)
#'
#' ## Close tagging (just removes tagging file)
#' close_tagging(tmp_dir)
#'
#' ## Cleanup
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @importFrom utils read.table
#' @rdname tagging_functions
start_tagging <- function(folder, tagging_file_name = "indexr_tagging.txt") {

  ## Checks
  check_is_directory(folder)
  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  # Combine the provided path with the filename
  file_path <- file.path(folder, tagging_file_name)

  # Create the file at the specified location if it doesn't exist
  if (!file.exists(file_path)) {
    file.create(file_path)
  }
}
#' @export
#' @rdname tagging_functions
cleanup <- function(folder, tagging_file_name = "indexr_tagging.txt", cutoff_date = NULL,
                    request_confirmation = TRUE) {

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
  files_to_delete <- c(files_to_delete, stringr::str_replace(files_to_delete, "\\.rds", "_parameters\\.rds"))

  ## Delete files
  if (length(files_to_delete) > 0) {

    message("The following .rds files will be removed:\n",
            paste(files_to_delete, collapse = "\n"))


    ## Ask for user confirmation
    if (request_confirmation) {
      confirm <- utils::askYesNo("Do you want to proceed with deleting these files?")
    } else {
      confirm <- TRUE
    }

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
#' @export
#' @rdname tagging_functions
close_tagging <- function(folder, tagging_file_name = "indexr_tagging.txt") {

  ## Checks
  check_is_directory(folder)
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
