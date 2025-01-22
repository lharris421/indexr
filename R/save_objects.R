#' Save Objects with Hashing and Incremental Saving Options
#'
#' Saves R objects to a specified folder with options for hashing, timestamp inclusion, and incremental saving.
#'
#' @param folder Character string specifying the path to the directory where the objects will be saved.
#' @param results The R object or list of objects to be saved.
#' @param parameters_list A named list of arguments used to generate a unique hash for the file. If \code{NULL}, the function attempts to extract it from \code{results}.
#' @param hash_includes_timestamp Logical. If \code{TRUE}, the timestamp is included in the hash generation.
#' @param ignore_na Logical. If \code{TRUE}, \code{NA} values in \code{parameters_list} are ignored during hash generation.
#' @param alphabetical_order Logical. If \code{TRUE}, the names in \code{parameters_list} are sorted alphabetically before hash generation.
#' @param overwrite Logical. If \code{TRUE}, existing files with the same hash will be overwritten.
#' @param include_timestamp Logical. If \code{TRUE}, a timestamp is added to \code{parameters_list}.
#' @param algo Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}.
#' @param get_script_name Logical. If \code{TRUE}, attempts to get the script name and add it to \code{parameters_list}.
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
#' parameters_list <- list(param1 = "value1", param2 = 42)
#'
#' # Save objects with default settings
#' save_objects(folder = folder_path, results = results, parameters_list = parameters_list)
#'
#' # Save objects with incremental saving
#' save_objects(folder = folder_path, results = results, parameters_list = parameters_list,
#'              incremental = TRUE, identifier = "experiment1")
#' }
save_objects <- function(folder, results, parameters_list = NULL,
                         hash_includes_timestamp = FALSE, ignore_na = TRUE,
                         alphabetical_order = TRUE, overwrite = FALSE,
                         include_timestamp = TRUE, algo = "xxhash64",
                         get_script_name = TRUE, ignore_script_name = FALSE,
                         incremental = FALSE, identifier = NULL) {

  # If parameters_list is not provided, we must stop or build from results
  if (is.null(parameters_list)) {
    stop("A parameters_list must be provided.")
  }

  # Potentially add a script name
  if (get_script_name) {
    if (!"script_name" %in% names(parameters_list)) {
      script_name <- tryCatch({
        cmd_args <- commandArgs(trailingOnly = FALSE)
        script_flag <- grep("--file=", cmd_args)
        if (length(script_flag) > 0) {
          script_path <- sub("--file=", "", cmd_args[script_flag])
          base_name <- basename(script_path)
          tools::file_path_sans_ext(base_name)
        } else {
          NULL
        }
      }, error = function(e) NA)

      if (!is.na(script_name) && !is.null(script_name)) {
        parameters_list$script_name <- script_name
      }
    }
  }

  # Add timestamp if required
  if (include_timestamp) {
    parameters_list$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }

  # Generate hash
  res <- generate_hash(
    parameters_list,
    hash_includes_timestamp = hash_includes_timestamp,
    ignore_na = ignore_na,
    alphabetical_order = alphabetical_order,
    algo = algo,
    ignore_script_name = ignore_script_name
  )

  # Unpack
  hash <- res$hash
  parameters_list <- res$parameters_list

  # 1) Check if we need to rename the hash if the file/folder already exists and overwrite = FALSE
  if (incremental) {
    # The folder we plan to write to
    temp_folder <- file.path(folder, hash)

    # If that folder already exists AND we are not overwriting, create a new "temporary" hash
    if (dir.exists(temp_folder) && !overwrite) {
      # One simple approach: add a timestamp or random suffix
      # e.g., hash_temp_ABC3
      tmp_suffix <- paste0("_temp_", paste0(sample(c(0:9, letters, LETTERS), 5, replace = TRUE), collapse = ""))
      hash <- paste0(hash, tmp_suffix)
      temp_folder <- file.path(folder, hash)
      warning(glue::glue("A folder for incremental results already exists with these paramters, results saved under a temporary hash: {hash}"))
    }

    # Now we can safely create the folder
    dir.create(temp_folder, recursive = TRUE, showWarnings = FALSE)

    # If no identifier is provided, generate one
    if (is.null(identifier)) {
      identifier <- paste0(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = "")
    }

    # Create a unique subscript
    existing_files <- list.files(
      temp_folder,
      pattern = paste0("^", identifier, "_[0-9]+\\.rds$")
    )
    if (length(existing_files) == 0) {
      subscript <- 1
    } else {
      subscripts <- as.numeric(sub("^.*_([0-9]+)\\.rds$", "\\1", existing_files))
      subscript <- max(subscripts, na.rm = TRUE) + 1
    }

    # Filenames for parameters and results
    parameters_filename <- paste0(identifier, "_", subscript, "_parameters.rds")
    results_filename    <- paste0(identifier, "_", subscript, ".rds")

    parameters_file_path <- file.path(temp_folder, parameters_filename)
    results_file_path    <- file.path(temp_folder, results_filename)

    # Save to separate files
    saveRDS(parameters_list, file = parameters_file_path)
    saveRDS(results, file = results_file_path)

  } else {
    # Non-incremental

    # Proposed file paths
    results_file_path    <- file.path(folder, paste0(hash, ".rds"))
    parameters_file_path <- file.path(folder, paste0(hash, "_parameters.rds"))

    # If the file already exists and we can't overwrite, append a suffix
    if ((file.exists(results_file_path) || file.exists(parameters_file_path)) && !overwrite) {

      # Append a suffix, e.g. _temp_<random>
      tmp_suffix <- paste0("_temp_", paste0(sample(c(0:9, letters, LETTERS), 5, replace = TRUE), collapse = ""))
      new_hash <- paste0(hash, tmp_suffix)

      # Warning
      warning(glue::glue("A file already exists with these paramters, results saved under a temporary hash: {new_hash}"))

      # Update the paths
      results_file_path    <- file.path(folder, paste0(new_hash, ".rds"))
      parameters_file_path <- file.path(folder, paste0(new_hash, "_parameters.rds"))
    }

    # Save parameters and results
    saveRDS(parameters_list, file = parameters_file_path)
    saveRDS(results, file = results_file_path)
  }

  invisible()
}
compress_incremental <- function(folder,
                                 parameters_list,
                                 hash_includes_timestamp = FALSE,
                                 ignore_na = TRUE,
                                 alphabetical_order = TRUE,
                                 algo = "xxhash64",
                                 get_script_name = TRUE,
                                 ignore_script_name = FALSE,
                                 remove_folder = TRUE) {
  # 1) Generate the hash from the parameters_list
  hash_res <- generate_hash(
    parameters_list,
    hash_includes_timestamp = hash_includes_timestamp,
    ignore_na = ignore_na,
    alphabetical_order = alphabetical_order,
    algo = algo,
    ignore_script_name = ignore_script_name
  )

  hash <- hash_res$hash

  # 2) Construct path for the incremental folder
  temp_folder <- file.path(folder, hash)

  # Quick check if the folder exists
  if (!dir.exists(temp_folder)) {
    stop("Incremental folder does not exist: ", temp_folder)
  }

  # 3) Identify all *.rds files
  all_rds_files <- list.files(
    temp_folder,
    pattern = "\\.rds$",
    full.names = TRUE
  )

  # Separate out parameter files vs result files
  parameter_files <- all_rds_files[grepl("_parameters\\.rds$", all_rds_files)]
  result_files    <- setdiff(all_rds_files, parameter_files)

  if (length(result_files) == 0) {
    warning("No result files found in incremental folder. Nothing to compress.")
    return(invisible(NULL))
  }

  # 4) Read all results
  results_list <- lapply(result_files, readRDS)

  # Check if all are data frames
  are_data_frames <- sapply(results_list, inherits, what = "data.frame")

  if (all(are_data_frames)) {
    # If all are data frames, rbind them
    combined_results <- do.call(rbind, results_list)
  } else {
    # Otherwise, store them as a list
    combined_results <- results_list
  }

  # Save combined results
  save_objects(folder, combined_results, parameters_list)

  # 6) Clean up old incremental files
  # Remove all the RDS files in the temp folder
  file.remove(all_rds_files)

  # Optionally remove the directory itself
  if (remove_folder) {
    # This will only remove the folder if it is empty after removing files
    # (On most systems, after removing all files, it should be empty.)
    unlink(temp_folder, recursive = TRUE, force = FALSE)
  }

  invisible(final_file_path)
}
