save_objects <- function(folder, results, args_list = NULL,
                         hash_includes_timestamp = FALSE, ignore_na = TRUE,
                         alphabetical_order = TRUE, overwrite = FALSE,
                         include_timestamp = TRUE, algo = "xxhash64",
                         get_filename = TRUE) {

  # If args_list is not provided, attempt to construct it from the first object passed
  if (is.null(args_list) && !missing(results)) {
    if (class(results) == "list" && !is.null(results$call)) {
      # Extract call details and then combine arguments with defaults
      args_list <- extract_call_details(results)
      # args_list <- combine_arguments_with_defaults(args_list)
    } else {
      stop("Either provide an args_list or a results list with a call.")
    }
  } else if (!is.null(args_list) & get_filename) {
    args_list$filename <- NULL
  }

  ## Add timestamp if required
  if (include_timestamp) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    args_list$timestamp <- timestamp
  }

  # Generate hash using generate_hash function
  res <- generate_hash(
    args_list,
    hash_includes_timestamp = hash_includes_timestamp, ignore_na = ignore_na,
    alphabetical_order = alphabetical_order, algo = algo
  )
  hash <- res$hash
  args_list <- res$args_list

  # Check for save_method and construct the file path accordingly
  file_path <- file.path(folder, paste0(hash, ".rds"))

  # Check for existing file
  if (file.exists(file_path) && !overwrite) {
    warning("Existing file found. Set 'overwrite = TRUE' to overwrite.")
    return(invisible())
  }

  saveRDS(list("args_list" = args_list, "results" = results), file = file_path)

}
