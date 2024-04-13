#' Save R Objects with Parameter Hashing
#'
#' Saves a list of R objects to a file in a specified directory, with filenames
#' based on a hash of their parameters. Optionally includes timestamps in the
#' parameter list before hashing and provides options for overwriting existing
#' files, ignoring NA values, and sorting parameters alphabetically.
#'
#' @param folder A string specifying the directory where the R objects will be saved.
#' @param args_list A named list of arguments based on which the hash for filename will be generated.
#' @param ... The data to be save.
#' @param hash_includes_timestamp Logical; if TRUE, the timestamp will be included in the hash calculation.
#' @param ignore_na Logical; if TRUE, NA values in `args_list` will be ignored for hash generation.
#' @param alphabetical_order Logical; if TRUE, the arguments in `args_list` will be sorted alphabetically before hash generation.
#' @param overwrite Logical; if TRUE, existing files with the same hash name will be overwritten.
#' @param include_timestamp Logical; if TRUE, a timestamp will be added to `args_list` before hash generation.
#'
#' @return Invisible NULL. This function is used for its side effect of saving files.
#' @export
#'
#' @examples
#' \dontrun{
#' args_list <- list(param1 = "value1", param2 = 100)
#' save_objects("path/to/save", args_list)
#' }
save_objects <- function(folder, ..., args_list = NULL, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE, overwrite = FALSE, include_timestamp = TRUE, algo = "xxhash64", save_method = "rds") {

  # Additional check for save_method 'rds'
  if (save_method == "rds") {
    objects <- list(...)
    if (length(objects) != 1) {
      stop("All objects must be of length 1 when using 'rds' save method. Combine objects into a single list or save as an .rda.")
    }
  }

  # If args_list is not provided, attempt to construct it from the first object passed
  if (is.null(args_list) && !missing(...)) {
    objects <- list(...)
    if (length(objects) == 1 && !is.null(objects[[1]]$call)) {
      # Extract call details and then combine arguments with defaults
      args_list <- extract_call_details(objects[[1]])
      args_list <- combine_arguments_with_defaults(args_list)
    } else {
      stop("Either provide an args_list or a single object with a call property.")
    }
  }

  ## Add timestamp if required
  if (include_timestamp) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    args_list$timestamp <- timestamp
  }

  # Generate hash using generate_hash function
  hash <- generate_hash(args_list, hash_includes_timestamp = hash_includes_timestamp, ignore_na = ignore_na, alphabetical_order = alphabetical_order, algo = algo)

  # Check for save_method and construct the file path accordingly
  file_extension <- ifelse(save_method == "rda", ".rda", ".rds")
  file_path <- file.path(folder, paste0(hash, file_extension))

  # Check for existing file
  if (file.exists(file_path) && !overwrite) {
    warning("Existing file found. Set 'overwrite = TRUE' to overwrite.")
    return(invisible())
  }

  # Save according to the specified method
  if (save_method == "rda") {
    # Combine all objects into a list with args_list being the first element
    call <- match.call(expand.dots = FALSE)
    ellipsis <- call[["..."]]
    object_names <- sapply(ellipsis, deparse)
    save(list = c("args_list", object_names), file = file_path, envir = globalenv())
  } else {
    # Save args_list as the first object followed by the other objects for .rds
    saveRDS(list("args_list" = args_list, ...), file = file_path)
  }
}
