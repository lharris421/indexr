#' Remove Files Based on Hash Table
#'
#' Allows the user to leverage the \code{generate_hash} function to generate a table that is subsequently used to remove indicated results.
#'
#' @param folder A string specifying the directory containing the RDS files.
#' @param hash_table A \code{data.frame} from \code{create_hash_table}.
#' @param mode A character string. When \code{mode = "manual"} (default) the function expects that the user will add a column to a hash table that indicated which files to delete. When \code{mode = "all"}, any results in the hash table will be removed.
#' @param column A character string indicating the logical column in \code{hash_table} specifying which files to delete.
#' @param request_confirmation Logical, if TRUE will request user input before proceeding to delete files.
#'
#' @details
#' There are a few ways to use this. When \code{mode = "manual"} (default) the function expects that the user will add a column to a hash table that indicated which files to delete. When \code{mode = "all"}, any results in the hash table will be removed. This is generally only used when a \code{filter_list} is passed to \code{create_hash_table}.
#'
#' @return Nothing, this function is called for its side effects.
#' @export
#'
#' @examples
#' ## Setup
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#'
#' ## Save example objects
#' parameters_list1 <- list(example = "tagging1")
#' parameters_list2 <- list(example = "tagging2")
#' save_objects(folder = tmp_dir, results = 1, parameters_list = parameters_list1)
#' save_objects(folder = tmp_dir, results = 2, parameters_list = parameters_list2)
#'
#' ## See the files saved
#' list.files(tmp_dir)
#'
#' ## Create hash table (flat file of result parameters)
#' hash_table <- create_hash_table(folder = tmp_dir)
#'
#' ## Delete "all" files based on hash table, without confirmation
#' cleanup_from_hash_table(
#'   folder = tmp_dir, hash_table = hash_table, mode = "all", request_confirmation = FALSE
#' )
#'
#' ## See the files have been deleted
#' list.files(tmp_dir)
#'
#' ## Cleanup
#' unlink(tmp_dir, recursive = TRUE)
#' @seealso [create_hash_table()]
cleanup_from_hash_table <- function(folder, hash_table,
                                    mode = c("manual", "all"),
                                    column = NULL, request_confirmation = TRUE) {
  mode <- match.arg(mode)

  ## Basic checks
  check_is_directory(folder)

  ## In "manual" mode, look for rows where 'column' == TRUE
  if (mode == "manual") {
    if (is.null(column)) {
      stop("In 'manual' mode, you must specify the column name containing the deletion indicator.")
    }
    if (!column %in% names(hash_table)) {
      stop("Column '", column, "' not found in the hash table.")
    }
    ## Identify hashes for which the user has marked deletion
    del_rows <- which(isTRUE(hash_table[[column]]) | hash_table[[column]] == TRUE)
    if (length(del_rows) == 0) {
      message("No rows with '", column, "' == TRUE. Nothing to delete.")
      return(invisible(NULL))
    }
    del_hashes <- hash_table$hash[del_rows]
  } else {
    ## In "all" mode, remove every hash in hash_table
    del_hashes <- hash_table$hash
  }

  ## Construct full paths for results and parameter files
  results_files  <- file.path(folder, paste0(del_hashes, ".rds"))
  params_files   <- file.path(folder, paste0(del_hashes, "_parameters.rds"))
  files_to_delete <- c(results_files, params_files)
  files_to_delete <- files_to_delete[file.exists(files_to_delete)]

  if (length(files_to_delete) == 0) {
    message("No matching files found on disk to delete.")
    return(invisible(NULL))
  }

  message("The following files will be removed:")
  print(files_to_delete)

  ## Ask for user confirmation
  if (request_confirmation) {
    confirm <- utils::askYesNo("Do you want to proceed with deleting these files?")
  } else {
    confirm <- TRUE
  }

  if (isTRUE(confirm)) {
    file.remove(files_to_delete)
    message("Specified files have been deleted.")
  } else if (isFALSE(confirm)) {
    message("Deletion canceled by the user.")
  } else {
    message("No response detected. Deletion canceled.")
  }

  # Optionally, re-check pairs
  check_missing_pairs(folder)

  invisible(NULL)
}

