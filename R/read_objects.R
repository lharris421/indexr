#' Read Objects from Multiple Folders and Tag with Hash and Timestamp
#'
#' Reads R objects from specified folders based on a generated hash of the provided arguments. If the object is successfully read, the function appends the hash and the current timestamp to the \code{indexer_tagging.txt} file in the folder where the file was found.
#'
#' @param folders Character vector specifying the paths to directories containing the saved objects. The function will check each folder in order to find the file.
#' @param parameters_list A named list of arguments used to generate a unique hash for the file.
#' @param hash_includes_timestamp Logical. If \code{TRUE}, the timestamp is included in the hash generation.
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#' @param ignore_na Logical. If \code{TRUE}, \code{NA} values in \code{parameters_list} are ignored during hash generation.
#' @param alphabetical_order Logical. If \code{TRUE}, the names in \code{parameters_list} are sorted alphabetically before hash generation.
#' @param algo Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}.
#' @param print_hash Logical. If \code{TRUE}, prints the generated hash.
#' @return The second element of the loaded object list, typically the results. Returns \code{NULL} if the file is not found in any of the specified folders.
#' @export
#' @details
#' This function attempts to read an R object from files located in one of the specified folders. The file name is based on the hash of the provided arguments. If successful, it appends the hash and the timestamp when it was read to the \code{indexer_tagging.txt} file in the folder where the file was found.
#' @examples
#' \dontrun{
#' # Read objects from multiple folders and save the hash and timestamp if the tagging file exists
#' loaded_object <- read_objects(
#'   folders = c("/folder1/path", "/folder2/path"),
#'   parameters_list = list(arg1 = "value1", arg2 = "value2"),
#'   print_hash = TRUE
#' )
#' }
read_objects <- function(folders, parameters_list, hash_includes_timestamp = FALSE,
                         ignore_script_name = FALSE,
                         ignore_na = TRUE, alphabetical_order = TRUE,
                         algo = "xxhash64", print_hash = FALSE,
                         tagging_file_name = "indexr_tagging.txt",
                         silent = FALSE) {


  # Validate folders input
  if (missing(folders) || is.null(folders)) {
    stop("Please provide at least one folder path in 'folders'.")
  }
  if (!is.character(folders)) {
    stop("'folders' must be a character vector of folder paths.")
  }

  ## Checks
  sapply(folders, check_is_directory)

  if (!silent) {
    sapply(folders, check_missing_pairs)
  }

  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  # Convert args to a list if it's a single row of a data frame or matrix
  if (is.data.frame(parameters_list) || is.matrix(parameters_list)) {
    if (nrow(parameters_list) != 1) {
      stop("parameters_list must be a single row of a data frame or matrix.")
    }
    parameters_list <- setNames(as.list(parameters_list[1, ]), names(parameters_list))
  } else if (!is.list(parameters_list)) {
    stop("parameters_list must be a list, data frame, or matrix.")
  }

  # Generate hash
  res <- generate_hash(
    parameters_list, hash_includes_timestamp = hash_includes_timestamp,
    ignore_na = ignore_na, alphabetical_order = alphabetical_order,
    algo = algo, ignore_script_name = ignore_script_name
  )
  hash <- res$hash
  if (print_hash) print(hash)

  # Construct the file name
  file_extension <- ".rds"
  file_name <- paste0(hash, file_extension)

  # Initialize loaded_objects as NULL
  loaded_objects <- NULL

  # Loop over folders to find the file
  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    if (file.exists(file_path)) {

      loaded_objects <- readRDS(file_path)

      # Check if the tagging file exists in the same directory
      tagging_file <- file.path(folder, tagging_file_name)
      if (file.exists(tagging_file)) {
        # Read the current tagging data
        tagging_data <- read.table(tagging_file, header = FALSE, sep = "\t",
                                   stringsAsFactors = FALSE, col.names = c("hash", "timestamp"))
        # Get the current timestamp
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

        # Check if hash already exists
        existing_idx <- which(tagging_data$hash == hash)
        if (length(existing_idx) > 0) {
          # Update the timestamp for the existing entry
          tagging_data$timestamp[existing_idx] <- timestamp
        } else {
          # Append a new entry if it doesn't exist
          new_entry <- data.frame(hash = hash, timestamp = timestamp, stringsAsFactors = FALSE)
          tagging_data <- rbind(tagging_data, new_entry)
        }

        # Write updated tagging data back
        write.table(tagging_data, tagging_file, sep = "\t", row.names = FALSE,
                    col.names = FALSE, quote = FALSE)
      }

      return(loaded_objects)
    }
  }

  # If file not found in any folder
  warning(paste0("File not found for hash: ", hash))
  return(NULL)
}
