#' Rehash RDS Files in a Directory
#'
#' This function processes all RDS files in a specified directory, generating new hashes
#' for each file's `args_list` and renaming the files accordingly. It's useful when changing
#' the hash generation algorithm or parameters.
#'
#' @param folder A string specifying the directory containing the RDS files to be rehashed.
#' @param hash_includes_timestamp Logical; if TRUE, includes timestamps in the hash generation.
#' @param ignore_na Logical; if TRUE, NA values are ignored during hash generation.
#' @param alphabetical_order Logical; if TRUE, parameters are sorted alphabetically before hash generation.
#'
#' @return The function does not return a value but renames the RDS files in the specified directory based on new hashes.
#' @export
#'
#' @examples
#' \dontrun{
#' directory_path <- "path/to/your/rds/files"
#' rehash(directory_path)
#' }
rehash <- function(folder,
                   hash_includes_timestamp = FALSE,
                   ignore_na = TRUE,
                   alphabetical_order = TRUE,
                   algo = "xxhash64") {
  # 1) Identify parameter files in the new format: {hash}_parameters.rds
  parameter_files <- list.files(
    folder,
    pattern = "_parameters\\.rds$",
    full.names = TRUE
  )

  if (length(parameter_files) == 0) {
    message("No parameter files found in folder: ", folder)
    return(invisible(NULL))
  }

  # 2) Process each parameter file
  for (param_path in parameter_files) {

    # Extract the old hash from the filename
    # e.g., path/to/abc123_parameters.rds -> "abc123"
    old_file_name <- basename(param_path)
    old_hash <- sub("_parameters\\.rds$", "", old_file_name)

    # The corresponding results file
    old_results_path <- file.path(folder, paste0(old_hash, ".rds"))

    # Check that both files actually exist
    if (!file.exists(old_results_path)) {
      warning(glue::glue(
        "Results file missing for hash '{old_hash}'. Skipping."
      ))
      next
    }

    # 3) Read ONLY the parameters
    parameters_list <- readRDS(param_path)

    # 4) Generate the new hash with the desired arguments
    new_hash <- generate_hash(
      parameters_list,
      hash_includes_timestamp = hash_includes_timestamp,
      ignore_na = ignore_na,
      alphabetical_order = alphabetical_order,
      algo = algo
    )$hash

    # If the new hash is the same as the old hash, do nothing
    if (identical(old_hash, new_hash)) {
      message(glue::glue(
        "Hash unchanged for '{old_hash}' with new algo '{algo}'. Skipping rename."
      ))
      next
    }

    # 5) Construct the new file paths
    new_results_path    <- file.path(folder, paste0(new_hash, ".rds"))
    new_parameters_path <- file.path(folder, paste0(new_hash, "_parameters.rds"))

    # 6) Check for collisions
    if (file.exists(new_results_path) || file.exists(new_parameters_path)) {
      stop(glue::glue(
        "New hash '{new_hash}' already exists in '{folder}'. ",
        "Potential collision or identical parameter set."
      ))
    }

    # 7) Rename old files to new hash
    file.rename(from = old_results_path, to = new_results_path)
    file.rename(from = param_path,        to = new_parameters_path)

    message(glue::glue(
      "Rehashed '{old_hash}' -> '{new_hash}' (algo = {algo})."
    ))
  }

  invisible(NULL)
}

