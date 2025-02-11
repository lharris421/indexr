#' Read Objects Based on Parameter List
#'
#' Reads R objects from specified folders based on a generated hash of the provided \code{parameters_list}.
#'
#' @param folders Character vector specifying the paths to directories containing the saved objects. The function will check each folder in order to find the file.
#' @param parameters_list A named list of arguments used to generate a unique hash for the file.
#' @param hash_includes_timestamp Logical. If \code{TRUE}, the timestamp is included in the hash generation.
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#' @param ignore_na Logical. If \code{TRUE}, \code{NA} values in \code{parameters_list} are ignored during hash generation.
#' @param alphabetical_order Logical. If \code{TRUE}, the names in \code{parameters_list} are sorted alphabetically before hash generation.
#' @param algo Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}.
#' @param print_hash Logical. If \code{TRUE}, prints the generated hash. This is helpful for debugging.
#' @param tagging_file_name Character string of a txt file that is being used for tagging results. See \code{?start_tagging}.
#' @param silent Logical. If \code{TRUE}, no check is done that pairs of results files (parameters and associated results) is done. This check is not necessary, but done by default to keep the user aware of a scenario that usually results from manual file manipulation.
#' @return The data stored in the file retrieved, typically the results. Returns \code{NULL} if the file is not found in any of the specified folders.
#' @export
#' @details
#' This function attempts to read an R object from files located in one of the specified folders. The file name is based on the hash of the provided arguments. If the object is successfully read and a tagging files exists and is specified, the function appends the hash and the current timestamp to the tagging file in the folder where the file was found.
#' @examples
#' ## Setup
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#'
#' ## Example using parameter list to run simulation and save results
#' parameters_list <- list(
#'   iterations = 1000,
#'   x_dist = "rnorm",
#'   x_dist_options = list(n = 10, mean = 1, sd = 2),
#'   error_dist = "rnorm",
#'   error_dist_options = list(n = 10, mean = 0, sd = 1),
#'   beta0 = 1,
#'   beta1 = 1
#' )
#'
#' betas <- numeric(parameters_list$iterations)
#' for (i in 1:parameters_list$iterations) {
#'   x <- do.call(parameters_list$x_dist, parameters_list$x_dist_options)
#'   err <- do.call(parameters_list$error_dist, parameters_list$error_dist_options)
#'   y <- parameters_list$beta0 + parameters_list$beta1*x + err
#'   betas[i] <- coef(lm(y ~ x))["x"]
#' }
#'
#' save_objects(folder = tmp_dir, results = betas, parameters_list = parameters_list)
#'
#' ## Read back in (consider clearing environment before running)
#' ## Re-setup
#' tmp_dir <- file.path(tempdir(), "example")
#'
#' parameters_list <- list(
#'   iterations = 1000,
#'   x_dist = "rnorm",
#'   x_dist_options = list(n = 10, mean = 1, sd = 2),
#'   error_dist = "rnorm",
#'   error_dist_options = list(n = 10, mean = 0, sd = 1),
#'   beta0 = 1,
#'   beta1 = 1
#' )
#'
#' betas <- read_objects(folder = tmp_dir, parameters_list = parameters_list)
#'
#' ## Cleanup
#' unlink(tmp_dir, recursive = TRUE)
#'
#' @importFrom utils read.table write.table
read_objects <- function(folders, parameters_list, hash_includes_timestamp = FALSE,
                         ignore_script_name = FALSE,
                         ignore_na = TRUE, alphabetical_order = TRUE,
                         algo = "xxhash64", print_hash = FALSE,
                         tagging_file_name = "indexr_tagging.txt",
                         silent = FALSE) {


  ## Validate folders input
  if (missing(folders) || is.null(folders)) {
    stop("Please provide at least one folder path in 'folders'.")
  }
  if (!is.character(folders)) {
    stop("'folders' must be a character vector of folder paths.")
  }
  sapply(folders, check_is_directory)

  ## Optional file check (good habit to catch problems early)
  if (!silent) {
    sapply(folders, check_missing_pairs)
  }

  tagging_file_name <- check_and_fix_extension(tagging_file_name, "txt")

  if (!is.list(parameters_list)) {
    stop("parameters_list must be a list, data frame, or matrix.")
  }

  ## Generate hash
  res <- generate_hash(
    parameters_list, hash_includes_timestamp = hash_includes_timestamp,
    ignore_na = ignore_na, alphabetical_order = alphabetical_order,
    algo = algo, ignore_script_name = ignore_script_name
  )
  hash <- res$hash
  if (print_hash) print(hash)

  ## Construct the file name
  file_extension <- ".rds"
  file_name <- paste0(hash, file_extension)

  ## Loop over folders to find the file
  loaded_objects <- NULL
  for (folder in folders) {
    file_path <- file.path(folder, file_name)
    if (file.exists(file_path)) {

      loaded_objects <- readRDS(file_path)

      ## Check if the tagging file exists in the same directory
      tagging_file <- file.path(folder, tagging_file_name)
      if (file.exists(tagging_file)) {
        ## Read the current tagging data
        tagging_data <- read.table(tagging_file, header = FALSE, sep = "\t",
                                   stringsAsFactors = FALSE, col.names = c("hash", "timestamp"))
        ## Get current timestamp
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

        ## Check if hash already exists in tagging
        existing_idx <- which(tagging_data$hash == hash)
        if (length(existing_idx) > 0) {
          ## Update the timestamp for the existing entry
          tagging_data$timestamp[existing_idx] <- timestamp
        } else {
          ## Append a new entry if it doesn't exist
          new_entry <- data.frame(hash = hash, timestamp = timestamp, stringsAsFactors = FALSE)
          tagging_data <- rbind(tagging_data, new_entry)
        }

        ## Write updated tagging data back
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
