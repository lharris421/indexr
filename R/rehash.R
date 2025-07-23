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
#' ## Setup
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#'
#' # Save example objects
#' obj1 <- rnorm(1000)
#' obj2 <- data.frame(
#'   x = runif(100),
#'   y = "something",
#'   z = rep(c(TRUE, FALSE), 50)
#' )
#' obj3 <- list(obj1, obj2)
#'
#' params1 <- list(
#'   distribution = "normal",
#'   other_params = list(param1 = TRUE, param2 = 1, param3 = NA)
#' )
#' params2 <- list(
#'   distribution = "uniform",
#'   other_params = list(param1 = FALSE, param2 = 2, param3 = "1", param4 = 4)
#' )
#' params3 <- list(
#'   distribution = "composite",
#'   other_params = list(param1 = TRUE, param2 = 3, param3 = 1)
#' )
#'
#' save_objects(tmp_dir, obj1, params1)
#' save_objects(tmp_dir, obj2, params2)
#' save_objects(tmp_dir, obj3, params3)
#'
#' ## See current file names
#' list.files(tmp_dir)
#'
#' ## Rehash with new algo
#' rehash(tmp_dir, algo = "xxhash32")
#'
#' ## Observe new file names
#' list.files(tmp_dir)
#'
#' ## Cleanup
#' unlink(tmp_dir, recursive = TRUE)
rehash <- function(folder,
                   hash_includes_timestamp = FALSE,
                   ignore_na = TRUE,
                   alphabetical_order = TRUE,
                   algo = "xxhash64") {
  ## Ensure folder exists
  check_is_directory(folder)
  check_missing_pairs(folder)

  ## Detect parameter files and YAML index
  parameter_files <- list.files(folder, pattern = "_parameters\\.rds$", full.names = TRUE)
  yaml_file      <- file.path(folder, "indexr.yaml")
  has_params     <- length(parameter_files) > 0
  has_yaml       <- file.exists(yaml_file)

  ## Error if both backends are present
  if (has_params && has_yaml) {
    stop("Both parameter RDS files and 'indexr.yaml' found; remove one before rehashing.")
  }

  ## PATH 1: Legacy parameter‐file mode
  if (has_params) {
    for (param_path in parameter_files) {
      old_hash    <- sub("_parameters\\.rds$", "", basename(param_path))
      old_results <- file.path(folder, paste0(old_hash, ".rds"))

      if (!file.exists(old_results)) {
        warning(glue::glue("Results file missing for hash '{old_hash}'. Skipping."))
        next
      }

      parameters_list <- readRDS(param_path)
      new_hash <- generate_hash(
        parameters_list,
        hash_includes_timestamp = hash_includes_timestamp,
        ignore_na              = ignore_na,
        alphabetical_order      = alphabetical_order,
        algo                    = algo
      )$hash

      if (identical(old_hash, new_hash)) {
        message(glue::glue("Hash unchanged for '{old_hash}' (algo={algo}). Skipping."))
        next
      }

      new_results_path    <- file.path(folder, paste0(new_hash, ".rds"))
      new_parameters_path <- file.path(folder, paste0(new_hash, "_parameters.rds"))

      if (file.exists(new_results_path) || file.exists(new_parameters_path)) {
        stop(glue::glue("Target files for new hash '{new_hash}' already exist. Collision."))
      }

      file.rename(old_results,    new_results_path)
      file.rename(param_path,     new_parameters_path)
      message(glue::glue("Rehashed '{old_hash}' -> '{new_hash}' (algo={algo})."))
    }

    ## PATH 2: YAML‐index mode
  } else if (has_yaml) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("The 'yaml' package is required for rehashing via YAML index. Please install it.")
    }
    index_list <- yaml::read_yaml(yaml_file)
    new_index  <- list()

    for (old_hash in names(index_list)) {
      params   <- index_list[[old_hash]]
      new_hash <- generate_hash(
        params,
        hash_includes_timestamp = hash_includes_timestamp,
        ignore_na              = ignore_na,
        alphabetical_order      = alphabetical_order,
        algo                    = algo
      )$hash

      if (identical(old_hash, new_hash)) {
        new_index[[old_hash]] <- params
        message(glue::glue("Hash unchanged for '{old_hash}'. Skipping."))
        next
      }

      old_file <- file.path(folder, paste0(old_hash, ".rds"))
      new_file <- file.path(folder, paste0(new_hash, ".rds"))

      if (file.exists(new_file) || new_hash %in% names(new_index)) {
        stop(glue::glue("Cannot rehash '{old_hash}' -> '{new_hash}': target exists."))
      }

      if (file.exists(old_file)) {
        file.rename(old_file, new_file)
      } else {
        warning(glue::glue("Result file for '{old_hash}' not found; updating YAML only."))
      }

      new_index[[new_hash]] <- params
      message(glue::glue("Rehashed '{old_hash}' -> '{new_hash}' via YAML (algo={algo})."))
    }

    yaml::write_yaml(new_index, yaml_file)

  } else {
    message("No parameter RDS files or 'indexr.yaml' found in folder: ", folder)
  }

  invisible()
}
