#' Create a Table of the Parameters for Saved Results from RDS Files
#'
#' Reads in all the parameter files for a give folder, flattens nested lists, and then
#' combines the parameters into a data frame. Each row in the resulting data frame represents
#' the arguments used for one RDS file, identified by its hash. Optionally, the function can
#' filter the data frame based on specified criteria and save it to a file.
#'
#' Saving the hash table can be helpful for the manipulation of parameters (see \code{?update_hash_table})
#' or for removal of unwanted results (see \code{?cleanup_from_hash_table}).
#'
#' @param folder A string specifying the directory containing the RDS files.
#' @param save_path An optional string specifying the path to save the resulting hash table as a CSV file.
#'                 If `NULL`, the hash table is not saved.
#' @param filter_list An optional list of filters to apply to the hash table.
#'                    Each element of the list should be named according to a column in the hash table
#'                    and contain the value to filter for in that column.
#'
#' @return A data frame where each row corresponds to an `parameters_list` from an RDS file,
#'         with an additional column for the hash of each set of arguments.
#' @export
#'
#' @examples
#' ## Setup
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#'
#' ## Save objects
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
#' ## Create hash table
#' create_hash_table(tmp_dir, save_path = file.path(tmp_dir, "hash_table.csv"))
#'
#' ## View created hash table
#' read.csv(file.path(tmp_dir, "hash_table.csv"))
#'
#' ## Cleanup
#' unlink(tmp_dir, recursive = TRUE)
create_hash_table <- function(folder, save_path = NULL, filter_list = NULL) {

  ## Quality checks
  check_is_directory(folder)
  check_missing_pairs(folder)
  if (!is.null(save_path)) save_path <- check_and_fix_extension(save_path, "csv")

  # List all parameter files in the given directory based on the file pattern
  files <- list.files(folder, pattern = "_parameters\\.rds$", full.names = TRUE)

  # Initialize an empty list to store the parameters_lists along with their hashes
  all_parameters_lists <- list()

  for (file in files) {

    loaded_objects <- readRDS(file)

    ## Make sure types are consistent
    parameters_list <- lapply(loaded_objects, convert_type)

    parameters_list$hash <- stringr::str_remove(basename(file), "_parameters\\.rds$")

    ## Process vectors into a character sting like "c(1, 2, 3)"
    parameters_list <- convert_vectors_to_c_strings(parameters_list)

    # Flatten the nested list with descriptive column names
    flat_parameters_list <- flatten_nested_list(parameters_list)

    # Convert all elements to characters to avoid type conflict
    flat_parameters_list <- lapply(flat_parameters_list, as.character)

    all_parameters_lists[[basename(file)]] <- flat_parameters_list
  }

  # Combine all parameters_lists into a data frame using bind_rows
  args_df <- dplyr::bind_rows(lapply(all_parameters_lists, as.data.frame.list, optional = TRUE))

  # Apply filters if filter_list is provided
  if (!is.null(filter_list) && is.list(filter_list)) {
    for (col_name in names(filter_list)) {
      args_df <- args_df[!is.na(args_df[[col_name]]) & args_df[[col_name]] == filter_list[[col_name]], ]
    }
    args_df <- args_df[, !sapply(args_df, function(col) all(is.na(col)))]
  }

  # Save the table if a save_path is provided
  if (!is.null(save_path)) {
    readr::write_csv(args_df, file = save_path, quote = "needed")
  }

  return(args_df)
}

# Recursively flatten a nested list and create descriptive column names
flatten_nested_list <- function(lst, parent_name = NULL) {

  flat_list <- list()

  for (name in names(lst)) {

    item <- lst[[name]]

    if (is.null(parent_name)) {
      col_name <- name
    } else {
      col_name <- paste0(parent_name, "[[", name, "]]")
    }

    if (is.list(item) && !is.data.frame(item)) {
      # Recursively flatten the nested list
      flat_list <- c(flat_list, flatten_nested_list(item, col_name))
    } else {
      flat_list[[col_name]] <- item
    }
  }

  return(flat_list)
}

# Convert vectors to c() strings for CSV representation
convert_vectors_to_c_strings <- function(lst) {
  lapply(lst, function(element) {
    if (is.list(element)) {
      convert_vectors_to_c_strings(element)
    } else if (is.vector(element) && length(element) > 1) {
      # Convert vector to a c() string
      paste0("c(", paste(sapply(element, function(x) {
        if (is.character(x)) {
          x
        } else {
          as.character(x)
        }
      }), collapse = ", "), ")")
    } else {
      element
    }
  })
}



