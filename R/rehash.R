#' Rehash RDS Files in a Directory
#'
#' This function processes all RDS files in a specified directory, generating new hashes
#' for each file's `args_list` and renaming the files accordingly. It's useful when changing
#' the hash generation algorithm or parameters (if the parameters are manually changed for some reason).
#'
#' @param folder A string specifying the directory containing the RDS files to be rehashed.
#' @param hash_includes_timestamp Logical; if TRUE, includes timestamps in the hash generation.
#' @param ignore_na Logical; if TRUE, NA values are ignored during hash generation.
#' @param alphabetical_order Logical; if TRUE, parameters are sorted alphabetically before hash generation.
#' @param algo The (potentially new) hash algorithm to use (see \code{?digest})
#'
#' @return The function does not return a value but renames the RDS files in the specified directory based on new hashes.
#' @export
#'
#' @examples
#' \dontrun{
#' Running this example as is will save files in your current working directory
#' Test data
#' obj1 <- rnorm(1000)
#' obj2 <- data.frame(x = runif(100), y = "something", z = rep(c(TRUE, FALSE), 50))
#' obj3 <- list(obj1, obj2)
#'
#' params1 <- list(distribution = "normal", other_params = list(param1 = TRUE, param2 = 1, param3 = NA))
#' params2 <- list(distribution = "uniform", other_params = list(param1 = FALSE, param2 = 2, param3 = "1", param4 = 4))
#' params3 <- list(distribution = "composite", other_params = list(param1 = TRUE, param2 = 3, param3 = 1))
#'
#' # Save objects
#' save_objects(".", obj1, params1)
#' save_objects(".", obj2, params2)
#' save_objects(".", obj3, params3)
#'
#'
#' # Observe current file names before continuing
#' rehash(test_dir, algo = "xxhash32")
#'
#' expect_false(any(names_before %in% names_after))
#' expect_true(length(names_before) == length(names_after))
#' }
rehash <- function(folder,
                   hash_includes_timestamp = FALSE,
                   ignore_na = TRUE,
                   alphabetical_order = TRUE,
                   algo = "xxhash64") {

  ## Identify parameter files
  parameter_files <- list.files(folder, pattern = "_parameters\\.rds$", full.names = TRUE)

  if (length(parameter_files) == 0) {
    message("No parameter files found in folder: ", folder)
    return(invisible(NULL))
  }

  ## Process each parameter file
  for (param_path in parameter_files) {

    ## Extract the old hash from the filename
    old_file_name <- basename(param_path)
    old_hash <- sub("_parameters\\.rds$", "", old_file_name)

    ## Construct the corresponding results file
    old_results_path <- file.path(folder, paste0(old_hash, ".rds"))

    # Check that the results file actually exist
    if (!file.exists(old_results_path)) {
      warning(glue::glue(
        "Results file missing for hash '{old_hash}'. Skipping."
      ))
      next
    }

    ## Read ONLY the parameters
    parameters_list <- readRDS(param_path)

    ## Generate the new hash with the desired arguments
    new_hash <- generate_hash(
      parameters_list,
      hash_includes_timestamp = hash_includes_timestamp,
      ignore_na = ignore_na,
      alphabetical_order = alphabetical_order,
      algo = algo
    )$hash

    ## If the new hash is the same as the old hash, do nothing
    if (identical(old_hash, new_hash)) {
      message(glue::glue(
        "Hash unchanged for '{old_hash}' with new algo '{algo}'. Skipping rename."
      ))
      next
    }

    ## Construct the new file paths
    new_results_path    <- file.path(folder, paste0(new_hash, ".rds"))
    new_parameters_path <- file.path(folder, paste0(new_hash, "_parameters.rds"))

    ## Check for collisions
    if (file.exists(new_results_path) || file.exists(new_parameters_path)) {
      stop(glue::glue(
        "New hash '{new_hash}' already exists in '{folder}'. ",
        "Potential collision or identical parameter set."
      ))
    }

    ## Rename old files to new hash
    file.rename(from = old_results_path, to = new_results_path)
    file.rename(from = param_path,        to = new_parameters_path)

    message(glue::glue(
      "Rehashed '{old_hash}' -> '{new_hash}' (algo = {algo})."
    ))
  }

  invisible(NULL)
}

