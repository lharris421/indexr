#' Check for the Existence of Parameter Combinations in RDS Files
#'
#' This function checks for the existence or absence of specified parameter combinations
#' in RDS files within a given folder. It generates hashes for each parameter combination
#' and compares them against a hash table created from the RDS files.
#'
#' @param folder A string specifying the directory containing the RDS files.
#' @param parameters_list A list of parameters for which combinations are checked.
#'                    Each element of the list should be a vector of values for one parameter.
#' @param check_for Character vector to specify whether to check for 'missing' or 'existing' combinations.
#'                  Default options are 'missing' and 'existing'.
#' @param halt Logical; if TRUE, the function stops execution and prints the combinations found when a specified condition ('missing' or 'existing') is met.
#' @param hash_includes_timestamp Logical; if TRUE, timestamps are included in the hash generation process.
#' @param ignore_na Logical; if TRUE, NA values are ignored during hash generation.
#' @param alphabetical_order Logical; if TRUE, parameters are sorted alphabetically before hash generation.
#'
#' @return A data frame with each row representing a parameter combination and an additional column indicating whether the combination is 'missing' or 'existing'.
#' @export
#'
#' @examples
#' \dontrun{
#' folder_path <- "path/to/your/rds/files"
#' parameters_list <- list(param1 = c("value1", "value2"), param2 = c(1, 2))
#' missing_combinations <- check_parameters_existence(folder_path, parameters_list, "missing")
#' }
check_hash_existence <- function(folder,
                                 parameters_list,
                                 halt = FALSE,
                                 hash_includes_timestamp = FALSE,
                                 ignore_na = TRUE,
                                 alphabetical_order = TRUE,
                                 algo = "xxhash64",
                                 ignore_script_name = FALSE) {
  # 1) Generate the hash
  hash_res <- generate_hash(
    parameters_list         = parameters_list,
    hash_includes_timestamp = hash_includes_timestamp,
    ignore_na              = ignore_na,
    alphabetical_order     = alphabetical_order,
    algo                   = algo,
    ignore_script_name     = ignore_script_name
  )
  this_hash <- hash_res$hash

  # 2) Construct the parameter file path
  param_file <- file.path(folder, paste0(this_hash, "_parameters.rds"))

  # 3) Check existence
  file_exists <- file.exists(param_file)

  if (file_exists && halt) {
    stop("A file with hash '", this_hash, "' already exists in '", folder, "'.")
  }

  return(file_exists)
}
