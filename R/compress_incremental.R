#' Combine results saved by \code{save_objects} with \code{incremental=TRUE}
#'
#' This function is only intended to be used after \code{save_objects} with \code{incremental=TRUE}. In this case, \code{save_objects} with save results under temporary hashes in a folder with the hash corresponding the the parameters. \code{compress_incremental} then combines the results and saves them under the corresponding hash and deletes the old directory with the temporary results.
#'
#' If the individual results can be put into a \code{data.frame} they will be, otherwise they will be stored as a list.
#'
#' @param folder Character string specifying the path to the directory where the temporary folder was saved (should be the same as supplied to \code{save_objects}.
#' @param parameters_list The named list of arguments used with \code{save_objects}.
#' @param hash_includes_timestamp Logical. If \code{TRUE}, the timestamp is included in the hash generation.
#' @param ignore_na Logical. If \code{TRUE}, \code{NA} values in \code{parameters_list} are ignored during hash generation.
#' @param alphabetical_order Logical. If \code{TRUE}, the names in \code{parameters_list} are sorted alphabetically before hash generation.
#' @param algo Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}. See \code{?digest}
#' @param ignore_script_name Logical. If \code{TRUE}, the script name is ignored during hash generation.
#' @param remove_folder Logical. If \code{TRUE}, the folder and the temporary results files will be discarded after the combined results are saved.
#'
#' @return No return value. This function is called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' params <- list(a = "1", b = "2")
#'
#' for (i in 1:10) {
#'   save_objects(".", data.frame(idx = i, val = rnorm(1)), params, incremental = TRUE)
#' }
#'
#' compress_incremental(".", params)
#' res <- read_objects(".", params)
#' hist(res$val)
#' }
compress_incremental <- function(folder,
                                 parameters_list,
                                 hash_includes_timestamp = FALSE,
                                 ignore_na = TRUE,
                                 alphabetical_order = TRUE,
                                 algo = "xxhash64",
                                 ignore_script_name = FALSE,
                                 remove_folder = TRUE) {

  ## Checks
  check_is_directory(folder)

  ## Construct path to temp folder
  hash_res <- generate_hash(
    parameters_list,
    hash_includes_timestamp = hash_includes_timestamp,
    ignore_na = ignore_na,
    alphabetical_order = alphabetical_order,
    algo = algo,
    ignore_script_name = ignore_script_name
  )

  hash <- hash_res$hash
  temp_folder <- file.path(folder, hash)

  if (!dir.exists(temp_folder)) {
    stop("Incremental folder does not exist: ", temp_folder)
  }

  ## Identify all *.rds files
  all_rds_files <- list.files(
    temp_folder,
    pattern = "\\.rds$",
    full.names = TRUE
  )

  ## Separate out parameter files vs result files
  parameter_files <- all_rds_files[grepl("_parameters\\.rds$", all_rds_files)]
  result_files    <- setdiff(all_rds_files, parameter_files)

  if (length(result_files) == 0) {
    warning("No result files found in incremental folder. Nothing to compress.")
    return(invisible(NULL))
  }

  ## Read all results into a list
  results_list <- lapply(result_files, readRDS)

  ## Check if all are data frames
  are_data_frames <- sapply(results_list, inherits, what = "data.frame")

  if (all(are_data_frames)) {
    ## If all are data frames, rbind them
    combined_results <- do.call(rbind, results_list)
  } else {
    ## Otherwise, store them as a list
    combined_results <- results_list
  }

  ## Save combined results
  save_objects(folder, combined_results, parameters_list)

  ## Remove all the RDS files in the temp folder
  file.remove(all_rds_files)

  ## Optionally remove old files and directory
  if (remove_folder) {

    file.remove(all_rds_files)
    ## This will only remove the folder if it is empty after removing files
    ## (On most systems, after removing all files, it should be empty.)
    unlink(temp_folder, recursive = TRUE, force = FALSE)

  }

}
