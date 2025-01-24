#' Save Simulation Results with Names as Hashes from the Parameters that Generated Them
#'
#' Saves RDS files to a specified folder with a name that is a hash generated from a list of parameters used for the simulation. There are a number of options that control the behavior, however, the default functionality likely covers 99% of use cases.
#'
#' @param folder Character string specifying the path to the directory where the objects will be saved.
#' @param results The R object or list of objects to be saved.
#' @param parameters_list A named list of arguments used to generate a unique hash for the file.
#' @param ignore_na Logical. If \code{TRUE}, \code{NA} values in \code{parameters_list} are ignored during hash generation.
#' @param alphabetical_order Logical. If \code{TRUE}, the names in \code{parameters_list} are sorted alphabetically before hash generation.
#' @param overwrite Logical. If \code{TRUE}, existing files with the same hash will be overwritten. If \code{FALSE} and a conflict occurs, the results will be saved under a temporary hash.
#' @param include_timestamp Logical. If \code{TRUE}, a timestamp is added to \code{parameters_list}.
#' @param hash_includes_timestamp Logical. If \code{TRUE}, the timestamp is included in the hash generation.
#' @param algo Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}. See \code{?digest}
#' @param get_script_name Logical. If \code{TRUE}, attempts to get the script name and add it to \code{parameters_list}. Only works if script is run from command line, in an interactive session, this will always be \code{NULL}.
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#' @param incremental Logical. If \code{TRUE}, results are saved in a subfolder named after the hash and can be combined with \code{compress_incremental}. Note, if \code{TRUE}, no checks will be done for results that already exist, the user should check this in their script with `check_hash_existence`.
#' @param silent Logical. If \code{TRUE}, no check is done that pairs of results files (parameters and associated results) is done. This check is not necessary, but done by default to keep the user aware of a scenario that usually results from manual file manipulation.
#' @return No return value. This function is called for its side effects.
#' @export
#' @details
#' This function saves R objects to disk with a file name based on a generated hash of the provided arguments. It supports incremental saving, where multiple results can be saved under the same hash in a subdirectory and later collected. This can be helpful for a simulation that runs and saves results in parallel for the SAME set of simulation parameters.
#' @examples
#' \dontrun{
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
#' save_objects(folder = ".", results = betas, parameters_list = parameters_list)
#' }
save_objects <- function(folder, results, parameters_list = NULL,
                         ignore_na = TRUE,
                         alphabetical_order = TRUE, overwrite = FALSE,
                         include_timestamp = TRUE, hash_includes_timestamp = FALSE,
                         algo = "xxhash64",
                         get_script_name = TRUE, ignore_script_name = FALSE,
                         incremental = FALSE, silent = FALSE) {

  ## Checks
  check_is_directory(folder)

  if (is.null(parameters_list)) {
    stop("A parameters_list must be provided.")
  }

  ## Add script name (if run via command line)
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
          NA
        }
      }, error = function(e) NA)

      if (!is.na(script_name)) {
        parameters_list$script_name <- script_name
      }
    } else {
      massage("Using script name provided in parameters_list")
    }
  }

  ## Add timestamp
  if (include_timestamp) {
    parameters_list$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }

  ## Generate hash
  res <- generate_hash(
    parameters_list,
    hash_includes_timestamp = hash_includes_timestamp,
    ignore_na = ignore_na,
    alphabetical_order = alphabetical_order,
    algo = algo,
    ignore_script_name = ignore_script_name
  )

  ## Unpack
  hash <- res$hash
  parameters_list <- res$parameters_list

  ## Save logic
  if (incremental == TRUE) {

    temp_folder <- file.path(folder, hash)
    dir.create(temp_folder, recursive = TRUE, showWarnings = FALSE)

    ## Generate a unique subscript (random) to avoid collisions
    existing_files <- list.files(
      temp_folder,
      pattern = paste0("^", hash, "_[0-9A-Za-z]+\\.rds$")
    )

    repeat {
      ## temp 5-character numeric subscript
      subscript <- paste0(sample(c(0:9), 5, replace = TRUE), collapse = "")
      ## Check if a file with this subscript already exists
      match_pattern <- paste0("^", hash, "_", subscript, "\\.rds$")
      if (!any(grepl(match_pattern, existing_files))) {
        break
      }
    }

    ## Save results
    parameters_filename <- paste0(hash, "_", subscript, "_parameters.rds")
    results_filename    <- paste0(hash, "_", subscript, ".rds")

    parameters_file_path <- file.path(temp_folder, parameters_filename)
    results_file_path    <- file.path(temp_folder, results_filename)

    saveRDS(parameters_list, file = parameters_file_path)
    saveRDS(results, file = results_file_path)

  } else {

    ## Non-incremental
    results_file_path    <- file.path(folder, paste0(hash, ".rds"))
    parameters_file_path <- file.path(folder, paste0(hash, "_parameters.rds"))

    ## If the file already exists and overwrite==FALSE, append a suffix
    if ((file.exists(results_file_path) || file.exists(parameters_file_path)) && !overwrite) {

      tmp_suffix <- paste0("_temp_", paste0(sample(c(0:9, letters, LETTERS), 5, replace = TRUE), collapse = ""))
      new_hash <- paste0(hash, tmp_suffix)

      warning(glue::glue(
        "A file already exists with these parameters, results saved under a temporary hash: {new_hash}"
      ))

      ## Update the paths
      results_file_path    <- file.path(folder, paste0(new_hash, ".rds"))
      parameters_file_path <- file.path(folder, paste0(new_hash, "_parameters.rds"))

    }

    ## Save parameters and results
    saveRDS(parameters_list, file = parameters_file_path)
    saveRDS(results, file = results_file_path)

  }

  ## Check for missing parameter/result pairs in the folder
  if (!silent) check_missing_pairs(folder)

  invisible()
}
