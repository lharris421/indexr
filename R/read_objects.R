#' Read Objects from Multiple Folders and Tag with Hash and Timestamp
#'
#' Reads R objects from specified folders based on a generated hash of the provided arguments. If the object is successfully read, the function appends the hash and the current timestamp to the \code{indexer_tagging.txt} file in the folder where the file was found.
#'
#' @param folders Character vector specifying the paths to directories containing the saved objects. The function will check each folder in order to find the file.
#' @param args_list A named list of arguments used to generate a unique hash for the file.
#' @param hash_includes_timestamp Logical. If \code{TRUE}, the timestamp is included in the hash generation.
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#' @param ignore_na Logical. If \code{TRUE}, \code{NA} values in \code{args_list} are ignored during hash generation.
#' @param alphabetical_order Logical. If \code{TRUE}, the names in \code{args_list} are sorted alphabetically before hash generation.
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
#'   args_list = list(arg1 = "value1", arg2 = "value2"),
#'   print_hash = TRUE
#' )
#' }
read_objects <- function(folders, args_list, hash_includes_timestamp = FALSE,
                         ignore_script_name = FALSE,
                         ignore_na = TRUE, alphabetical_order = TRUE,
                         algo = "xxhash64", print_hash = FALSE) {

  # Validate folders input
  if (missing(folders) || is.null(folders)) {
    stop("Please provide at least one folder path in 'folders'.")
  }
  if (!is.character(folders)) {
    stop("'folders' must be a character vector of folder paths.")
  }

  # Convert args to a list if it's a single row of a data frame or matrix
  if (is.data.frame(args_list) || is.matrix(args_list)) {
    if (nrow(args_list) != 1) {
      stop("args_list must be a single row of a data frame or matrix.")
    }
    args_list <- setNames(as.list(args_list[1, ]), names(args_list))
  } else if (!is.list(args_list)) {
    stop("args_list must be a list, data frame, or matrix.")
  }

  # Generate hash using generate_hash function
  res <- generate_hash(
    args_list, hash_includes_timestamp = hash_includes_timestamp,
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
      if (length(loaded_objects) < 2) {
        stop("The loaded .rds file does not contain the expected number of objects.")
      }

      # Check if the tagging file exists in the same directory
      tagging_file <- file.path(folder, "indexer_tagging.txt")
      if (file.exists(tagging_file)) {
        # Get the current timestamp
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        # Append the hash and timestamp to the tagging file
        cat(paste(hash, timestamp, sep = "\t"), file = tagging_file, append = TRUE, sep = "\n")
      }

      # Return the loaded object
      return(loaded_objects[[2]]) # Return the second item of the list
    }
  }

  # If file not found in any folder
  warning(paste0("File not found for hash: ", hash))
  return(NULL)
}
