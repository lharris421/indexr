#' Combine Results Saved by \code{save_objects} with \code{incremental=TRUE}
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
#' ## Save results incrementally
#' params <- list(a = "1", b = "2")
#'
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#' for (i in 1:10) {
#'   save_objects(tmp_dir, data.frame(idx = i, val = rnorm(1)), params, incremental = TRUE)
#' }
#'
#' ## See contents of tmp directory for incremental file
#' list.files(file.path(tmp_dir, generate_hash(params)))
#'
#' ## Compress results into a single file
#' compress_incremental(tmp_dir, params)
#' list.files(tmp_dir)
#'
#' ## Read in compressed file and view results
#' read_objects(tmp_dir, params)
#'
#' ## Cleanup
#' unlink(tmp_dir, recursive = TRUE)
#' @seealso [save_objects()]
compress_incremental <- function(
    folder,
    parameters_list,
    hash_includes_timestamp = FALSE,
    ignore_na = TRUE,
    alphabetical_order = TRUE,
    algo = "xxhash64",
    ignore_script_name = FALSE,
    remove_folder = TRUE
) {
  ## Ensure folder exists
  check_is_directory(folder)

  ## Construct hash and temp folder
  hash_res <- generate_hash(
    parameters_list,
    hash_includes_timestamp = hash_includes_timestamp,
    ignore_na              = ignore_na,
    alphabetical_order      = alphabetical_order,
    algo                    = algo,
    ignore_script_name      = ignore_script_name
  )
  hash        <- hash_res$hash
  temp_folder <- file.path(folder, hash)
  if (!dir.exists(temp_folder)) {
    stop("Incremental folder does not exist: ", temp_folder)
  }

  ## Gather .rds files
  all_rds         <- list.files(temp_folder, pattern = "\\.rds$", full.names = TRUE)
  parameter_files <- all_rds[grepl("_parameters\\.rds$", all_rds)]
  result_files    <- setdiff(all_rds, parameter_files)
  if (length(result_files) == 0) {
    warning("No result files found in incremental folder. Nothing to compress.")
    return(invisible(NULL))
  }

  ## If legacy parameters present, override parameters_list
  if (length(parameter_files) > 0) {
    parameters_list <- readRDS(parameter_files[[1]])
  }

  ## Combine results
  res_list <- lapply(result_files, readRDS)
  combined <- if (all(sapply(res_list, inherits, "data.frame"))) {
    do.call(rbind, res_list)
  } else {
    res_list
  }

  ## Determine backend
  yaml_file   <- file.path(folder, "indexr.yaml")
  has_yaml    <- file.exists(yaml_file)
  has_legacy  <- length(parameter_files) > 0
  if (has_yaml && has_legacy) {
    stop("Found both legacy `_parameters.rds` files and 'indexr.yaml'; remove one before compressing.")
  }

  ## Save via save_objects (yaml=FALSE for legacy, default TRUE otherwise)
  if (has_legacy) {
    save_objects(folder, combined, parameters_list, yaml = FALSE)
  } else {
    save_objects(folder, combined, parameters_list)
  }

  ## Cleanup originals if requested
  if (remove_folder) {
    file.remove(all_rds)
    unlink(temp_folder, recursive = TRUE)
  }

  invisible()
}
