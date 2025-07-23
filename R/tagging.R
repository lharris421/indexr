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
cleanup <- function(folder,
                    tagging_file_name = "indexr_tagging.txt",
                    cutoff_date = NULL,
                    request_confirmation = TRUE) {

  ## Basic checks
  check_is_directory(folder)
  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")
  tagging_file <- file.path(folder, tagging_file_name)
  if (!file.exists(tagging_file)) {
    stop(glue::glue(
      "Tagging file '{tagging_file_name}' does not exist in folder: {folder}"
    ))
  }

  ## Read tagging data
  tagging_data <- read.table(
    tagging_file,
    header = FALSE, sep = "\t", stringsAsFactors = FALSE,
    col.names = c("hash", "timestamp")
  )
  if (nrow(tagging_data) == 0) {
    stop(glue::glue(
      "Tagging file '{tagging_file_name}' is empty. No tagged files to compare."
    ))
  }
  tagging_data$timestamp <- as.POSIXct(
    tagging_data$timestamp,
    format = "%Y-%m-%d %H:%M:%S"
  )

  ## Detect backends
  param_files <- list.files(folder, pattern = "_parameters\\.rds$", full.names = TRUE)
  has_params  <- length(param_files) > 0
  yaml_file   <- file.path(folder, "indexr.yaml")
  has_yaml    <- file.exists(yaml_file)

  if (has_params && has_yaml) {
    stop("Both parameter RDS files and 'indexr.yaml' found; remove one before cleanup.")
  }
  if (!has_params && !has_yaml) {
    stop(glue::glue(
      "No parameter RDS files or 'indexr.yaml' found in folder: {folder}"
    ))
  }

  ## List result .rds files
  all_rds <- list.files(folder, pattern = "\\.rds$", full.names = TRUE)
  results_files <- if (has_params) {
    all_rds[!grepl("_parameters\\.rds$", all_rds)]
  } else {
    all_rds
  }

  ## Map to hashes
  file_hashes <- sub("\\.rds$", "", basename(results_files))
  df <- data.frame(hash = file_hashes, filepath = results_files, stringsAsFactors = FALSE)

  ## Identify untagged
  to_delete <- df$filepath[!df$hash %in% tagging_data$hash]

  ## Identify stale by cutoff
  if (!is.null(cutoff_date)) {
    cutoff_dt <- as.POSIXct(cutoff_date, format = "%Y-%m-%d %H:%M:%S")
    if (is.na(cutoff_dt)) {
      stop("Invalid cutoff_date format. Use 'YYYY-MM-DD HH:MM:SS'.")
    }
    old_hashes <- tagging_data$hash[tagging_data$timestamp < cutoff_dt]
    stale <- df$filepath[df$hash %in% old_hashes]
    to_delete <- unique(c(to_delete, stale))
  }
  to_delete <- unique(to_delete)

  ## Proceed with deletion
  if (length(to_delete) > 0) {
    message("The following .rds files will be removed:\n",
            paste(to_delete, collapse = "\n"))
    confirm <- if (request_confirmation) {
      utils::askYesNo("Do you want to proceed with deleting these files?")
    } else {
      TRUE
    }

    if (isTRUE(confirm)) {
      # Remove result files
      file.remove(to_delete)

      # In legacy mode, also remove parameter files
      if (has_params) {
        param_delete <- sub("\\.rds$", "_parameters.rds", to_delete)
        file.remove(param_delete)
      }

      # In YAML mode, also remove entries from indexr.yaml
      if (has_yaml) {
        if (!requireNamespace("yaml", quietly = TRUE)) {
          stop("The 'yaml' package is required for YAML cleanup. Please install it.")
        }
        removed_hashes <- sub("\\.rds$", "", basename(to_delete))
        index_list <- yaml::read_yaml(yaml_file)
        for (h in removed_hashes) {
          index_list[[h]] <- NULL
        }
        yaml::write_yaml(index_list, yaml_file)
      }

      message("Specified files have been deleted.")
    } else if (isFALSE(confirm)) {
      message("Deletion canceled by the user.")
    } else {
      message("No response detected. Deletion canceled.")
    }
  } else {
    message("No .rds files to remove.")
  }

  ## Final integrity check
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
