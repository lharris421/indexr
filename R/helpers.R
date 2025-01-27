check_missing_pairs <- function(folder) {

  ## Collect all .rds files
  all_files <- list.files(folder, pattern = "\\.rds$", full.names = TRUE)

  ## Separate out files that match "<hash>_parameters.rds"
  param_files <- grep("_parameters\\.rds$", all_files, value = TRUE)

  ## Everything else is considered a "results" file
  results_files <- setdiff(all_files, param_files)

  ## Extract the "hash" portion:
  ##    - For param_files, remove the "_parameters.rds" part
  ##    - For results_files, remove the ".rds" part
  param_hashes   <- sub("_parameters\\.rds$", "", basename(param_files))
  results_hashes <- sub("\\.rds$", "", basename(results_files))

  ## Find all unique hashes
  unique_hashes <- unique(c(param_hashes, results_hashes))

  ## Containers to track missing pairs
  missing_params  <- character(0)
  missing_results <- character(0)

  ## For each hash, check if it has both a param file AND a results file
  for (h in unique_hashes) {
    has_param   <- h %in% param_hashes
    has_results <- h %in% results_hashes

    if (!has_param) {
      missing_params <- c(missing_params, h)
    }
    if (!has_results) {
      missing_results <- c(missing_results, h)
    }
  }

  ## If there are any mismatches, generate a single warning that lists them
  if (length(missing_params) > 0 || length(missing_results) > 0) {
    msg <- character(0)

    if (length(missing_params) > 0) {
      msg <- c(msg, paste0(
        "The following hashes have results but no parameters: ",
        paste(missing_params, collapse = ", ")
      ))
    }

    if (length(missing_results) > 0) {
      msg <- c(msg, paste0(
        "The following hashes have parameters but no results: ",
        paste(missing_results, collapse = ", ")
      ))
    }

    warning(paste(paste(msg, collapse = "\n"), "\nThis usually means that one of the corresponding files was manually deleted and not the other."))
  }
}
check_and_fix_extension <- function(filename, ext) {

  ## Normalize the extension the user provides, ensuring it starts with "."
  if (!grepl("^\\.", ext)) {
    ext <- paste0(".", ext)
  }

  ## Extract the current extension from the filename
  current_ext <- tools::file_ext(filename)         # e.g. "txt", "rds", or ""
  base_name   <- tools::file_path_sans_ext(filename) # Filename minus any extension

  ## Check extension of the filename provided by user
  if (nzchar(current_ext)) {
    # There is some extension present
    dot_current_ext <- paste0(".", current_ext)  # e.g. ".txt"

    if (dot_current_ext == ext) {
      # Matches the expected extension, do nothing
      return(filename)
    } else {
      # Different extension -> error out
      stop(
        "The file '", filename, "' has extension '", dot_current_ext,
        "' but '", ext, "' is required.\n",
        "Please provide a file name without an extension, or with the '", ext, "' extension."
      )
    }
  } else {
    # No extension present -> add the expected extension
    return(paste0(base_name, ext))
  }
}
check_is_directory <- function(path) {
  # Grab file info
  info <- file.info(path, extra_cols = FALSE)

  # If path does not exist (file.info returns NA in isdir)
  if (is.na(info$isdir)) {
    stop("The provided path does not exist or is not a directory: ", path)
  }

  # If everything is okay, return invisibly
  invisible(TRUE)
}
convert_type <- function(x) {
  if (is.logical(x) | is.character(x) | is.list(x)) {
    return(x)
  } else if (is.numeric(x)) {
    as.numeric(x) ## Make everything a numeric (not int)
  } else if (is.call(x)) {
    return(deparse(x))
  } else { ## Currently acts as a safety next, may not be needed
    return(as.character(x))
  }
}
