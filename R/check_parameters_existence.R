#' Check for the Existence of Parameter Combinations in RDS Files
#'
#' This function checks for the existence or absence of specified parameter combinations
#' in RDS files within a given folder. It generates hashes for each parameter combination
#' and compares them against a hash table created from the RDS files.
#'
#' @param folder A string specifying the directory containing the RDS files.
#' @param params_list A list of parameters for which combinations are checked.
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
#' params_list <- list(param1 = c("value1", "value2"), param2 = c(1, 2))
#' missing_combinations <- check_parameters_existence(folder_path, params_list, "missing")
#' }
check_parameters_existence <- function(folder, params_list, check_for = c("missing", "existing"), halt = FALSE, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE) {
  # Validate 'check_for' option
  check_for <- match.arg(check_for)

  # Create a hash table from the RDS files
  hash_table <- create_hash_table(folder)

  # Expand the parameters to all combinations (if they contain vectors)
  params_combinations <- expand.grid(params_list)

  # Convert params_combinations to a data frame for joining
  params_combinations <- as.data.frame(params_combinations, stringsAsFactors = FALSE)

  # Initialize a vector to store the status of each combination (missing or existing)
  params_status <- character(nrow(params_combinations))

  # Check each parameter combination by generating its hash and comparing with hash_table
  for (i in 1:nrow(params_combinations)) {
    # Convert the row to a named list
    args_list <- setNames(as.list(params_combinations[i, ]), names(params_combinations))

    # Generate hash for the args_list
    hash <- generate_hash(args_list, hash_includes_timestamp, ignore_na, alphabetical_order)

    # Check for the existence of the hash in the hash_table
    if (hash %in% hash_table$hash) {
      params_status[i] <- "existing"
    } else {
      params_status[i] <- "missing"
    }
  }

  # Create a result data frame
  result_df <- cbind(params_combinations, Status = params_status)

  # Filter based on 'check_for' option
  result <- switch(check_for,
                   missing = result_df[result_df$Status == "missing", ],
                   existing = result_df[result_df$Status == "existing", ])

  # Halt execution if required
  if (halt && nrow(result) > 0) {
    print(result)
    stop(paste0(check_for, " parameters found."))
  }

  return(result)
}
