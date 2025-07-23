#' Check for the Existence of Results Under a Set of Parameters
#'
#' This function checks for the existence of results saved under specified parameter list
#' in RDS files (saved with \code{indexr}) within a given folder.
#'
#' @param folder A string specifying the directory containing the RDS files.
#' @param parameters_list A list of parameters for which a corresponding hash named file is checked.
#' @param halt Logical; if TRUE, the function stops execution if an existing file is found. This may be useful as a check before running a simulation.
#' @param hash_includes_timestamp Logical; if TRUE, timestamps are included in the hash generation process.
#' @param ignore_na Logical; if TRUE, NA values are ignored during hash generation.
#' @param alphabetical_order Logical; if TRUE, parameters are sorted alphabetically before hash generation.
#' @param algo Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}. See \code{?digest}
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#'
#' @return A logical of whether or not a file exists, unless \code{halt = TRUE} and a file is found, then an error is thrown.
#' @export
#'
#' @examples
#' ## Setup
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#'
#' ## Save an object
#' parameters_list <- list(example = "check_hash_existence")
#' save_objects(folder = tmp_dir, results = 1, parameters_list = parameters_list)
#'
#' ## Check that an object under specified parameters is saved
#' check_hash_existence(folder = tmp_dir, parameters_list)
#'
#' ## Cleanup
#' unlink(tmp_dir, recursive = TRUE)
check_hash_existence <- function(folder,
                                 parameters_list,
                                 halt = FALSE,
                                 hash_includes_timestamp = FALSE,
                                 ignore_na = TRUE,
                                 alphabetical_order = TRUE,
                                 algo = "xxhash64",
                                 ignore_script_name = FALSE) {

  ## Check that matching pairs all exist
  check_missing_pairs(folder)

  ## Generate the hash
  hash_res <- generate_hash(
    parameters_list         = parameters_list,
    hash_includes_timestamp = hash_includes_timestamp,
    ignore_na              = ignore_na,
    alphabetical_order     = alphabetical_order,
    algo                   = algo,
    ignore_script_name     = ignore_script_name
  )
  this_hash <- hash_res$hash

  ## Check existence
  file_exists <- file.exists(file.path(folder, paste0(this_hash, ".rds")))

  if (file_exists && halt) {
    stop("A file with hash '", this_hash, "' already exists in '", folder, "'.")
  }

  return(file_exists)
}
