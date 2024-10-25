#' Save Objects with Hashing and Incremental Saving Options
#'
#' Saves R objects to a specified folder with options for hashing, timestamp inclusion, and incremental saving.
#'
#' @param folder Character string specifying the path to the directory where the objects will be saved.
#' @param results The R object or list of objects to be saved.
#' @param args_list A named list of arguments used to generate a unique hash for the file. If \code{NULL}, the function attempts to extract it from \code{results}.
#' @param hash_includes_timestamp Logical. If \code{TRUE}, the timestamp is included in the hash generation.
#' @param ignore_na Logical. If \code{TRUE}, \code{NA} values in \code{args_list} are ignored during hash generation.
#' @param alphabetical_order Logical. If \code{TRUE}, the names in \code{args_list} are sorted alphabetically before hash generation.
#' @param overwrite Logical. If \code{TRUE}, existing files with the same hash will be overwritten.
#' @param include_timestamp Logical. If \code{TRUE}, a timestamp is added to \code{args_list}.
#' @param algo Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}.
#' @param get_script_name Logical. If \code{TRUE}, attempts to get the script name and add it to \code{args_list}.
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#' @param incremental Logical. If \code{TRUE}, results are saved incrementally in a subfolder named after the hash.
#' @param identifier Character string used to identify the incremental save files. If \code{NULL}, a random identifier is generated.
#' @return No return value. This function is called for its side effects.
#' @export
#' @details
#' This function saves R objects to disk with a filename based on a generated hash of the provided arguments. It supports incremental saving, where multiple results can be saved under the same hash in a subdirectory.
#' @examples
#' \dontrun{
#' # Define the folder to save the objects
#' folder_path <- "/your/directory/path"
#'
#' # Define the results to save
#' results <- list(data = mtcars, summary = summary(mtcars))
#'
#' # Define the arguments list used for hashing
#' args_list <- list(param1 = "value1", param2 = 42)
#'
#' # Save objects with default settings
#' save_objects(folder = folder_path, results = results, args_list = args_list)
#'
#' # Save objects with incremental saving
#' save_objects(folder = folder_path, results = results, args_list = args_list,
#'              incremental = TRUE, identifier = "experiment1")
#' }
save_objects <- function(folder, results, args_list = NULL,
                         hash_includes_timestamp = FALSE, ignore_na = TRUE,
                         alphabetical_order = TRUE, overwrite = FALSE,
                         include_timestamp = TRUE, algo = "xxhash64",
                         get_script_name = TRUE, ignore_script_name = FALSE,
                         incremental = FALSE, identifier = NULL) {

  # If args_list is not provided, attempt to construct it from the first object passed
  if (is.null(args_list) && !missing(results)) {
    if (class(results) == "list" && !is.null(results$call)) {
      # Extract call details and then combine arguments with defaults
      args_list <- extract_call_details(results)
    } else {
      stop("Either provide an args_list or a results list with a call.")
    }
  }

  # Try to get the script name and add to args_list if it doesn't already exist
  if (get_script_name) {
    if (!"script_name" %in% names(args_list)) {
      # Get the script name
      script_name <- tryCatch({
        cmd_args <- commandArgs(trailingOnly = FALSE)
        # Find '--file=' in command arguments (used by Rscript)
        script_flag <- grep("--file=", cmd_args)
        if (length(script_flag) > 0) {
          # Extract the script file name and remove the path and extension
          script_path <- sub("--file=", "", cmd_args[script_flag])
          script_name <- basename(script_path)
          tools::file_path_sans_ext(script_name)
        } else {
          # For interactive sessions, fallback to sys.calls
          script_call <- sys.calls()[[1]]
          # Extract script name from call
          script_name <- basename(deparse(script_call))
          tools::file_path_sans_ext(script_name)
        }
      }, error = function(e) {
        NA
      })

      print(script_name)

      if (!is.na(script_name)) {
        args_list$script_name <- script_name
      }
    }
  }

  ## Add timestamp if required
  if (include_timestamp) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    args_list$timestamp <- timestamp
  }

  # Generate hash using generate_hash function
  res <- generate_hash(
    args_list,
    hash_includes_timestamp = hash_includes_timestamp, ignore_na = ignore_na,
    alphabetical_order = alphabetical_order, algo = algo,
    ignore_script_name = ignore_script_name
  )
  hash <- res$hash
  args_list <- res$args_list

  if (incremental) {
    # If identifier is NULL, set it to a random value
    if (is.null(identifier)) {
      # Generate a random identifier
      identifier <- paste(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = '')
    }

    # Set up the temporary folder
    temp_folder <- file.path(folder, hash)

    # Create the directory if it doesn't exist
    dir.create(temp_folder, recursive = TRUE, showWarnings = FALSE)

    # Now, create a filename for this particular result
    # Since we might be writing multiple files, we need to avoid overwriting existing ones
    # Let's check how many files with this identifier already exist
    existing_files <- list.files(temp_folder, pattern = paste0("^", identifier, "_[0-9]+\\.rds$"))

    # Determine the next subscript
    if (length(existing_files) == 0) {
      subscript <- 1
    } else {
      # Extract subscripts from existing files
      subscripts <- as.numeric(sub("^.*_([0-9]+)\\.rds$", "\\1", existing_files))
      subscript <- max(subscripts, na.rm = TRUE) + 1
    }

    # Create the filename
    filename <- paste0(identifier, "_", subscript, ".rds")

    # Create the full file path
    file_path <- file.path(temp_folder, filename)

    # Save the results
    saveRDS(list("args_list" = args_list, "results" = results), file = file_path)
  } else {
    # Existing behavior
    # Construct the file path
    file_path <- file.path(folder, paste0(hash, ".rds"))

    # Check for existing file
    if (file.exists(file_path) && !overwrite) {
      warning("Existing file found. Set 'overwrite = TRUE' to overwrite.")
      return(invisible())
    }

    saveRDS(list("args_list" = args_list, "results" = results), file = file_path)
  }
}
