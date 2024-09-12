save_objects <- function(folder, results, args_list = NULL,
                         hash_includes_timestamp = FALSE, ignore_na = TRUE,
                         alphabetical_order = TRUE, overwrite = FALSE,
                         include_timestamp = TRUE, algo = "xxhash64",
                         get_script_name = TRUE, ignore_script_name = FALSE) {

  # If args_list is not provided, attempt to construct it from the first object passed
  if (is.null(args_list) && !missing(results)) {
    if (class(results) == "list" && !is.null(results$call)) {
      # Extract call details and then combine arguments with defaults
      args_list <- extract_call_details(results)
    } else {
      stop("Either provide an args_list or a results list with a call.")
    }
  }

  # Try to get the script name and add to args_list if it doesn't already exist
  if (get_script_name) {
    if (!"script_name" %in% names(args_list)) {
      # Get the script name
      script_name <- tryCatch({
        cmd_args <- commandArgs(trailingOnly = FALSE)
        # Find '--file=' in command arguments (used by Rscript)
        script_flag <- grep("--file=", cmd_args)
        if (length(script_flag) > 0) {
          # Extract the script file name and remove the path and extension
          script_path <- sub("--file=", "", cmd_args[script_flag])
          script_name <- basename(script_path)
          tools::file_path_sans_ext(script_name)
        } else {
          # For interactive sessions, fallback to sys.calls
          script_call <- sys.calls()[[1]]
          # Extract script name from call
          script_name <- basename(deparse(script_call))
          tools::file_path_sans_ext(script_name)
        }
      }, error = function(e) {
        NA
      })

      if (!is.na(script_name)) {
        args_list$script_name <- script_name
      }
    }
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
    alphabetical_order = alphabetical_order, algo = algo,
    ignore_script_name = ignore_script_name
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
