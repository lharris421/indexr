#' @importFrom methods as
check_missing_pairs <- function(folder) {
  ## Ensure folder exists
  check_is_directory(folder)

  ## If directory is empty, skip checks
  if (length(list.files(folder, all.files = FALSE, no.. = TRUE)) == 0) {
    message("Folder is empty; skipping missing-pairs check.")
    return(invisible())
  }

  ## Detect backends
  param_files <- list.files(folder, pattern = "_parameters\\.rds$", full.names = TRUE)
  yaml_file   <- file.path(folder, "indexr.yaml")
  has_params  <- length(param_files) > 0
  has_yaml    <- file.exists(yaml_file)

  ## Error if both or neither
  if (has_params && has_yaml) {
    warning("Both parameter RDS files and 'indexr.yaml' found; it is recommended to run update_from_legacy before proceeding.")
  }
  if (!has_params && !has_yaml) {
    message(paste0("No parameter RDS files or 'indexr.yaml' found in folder: ", folder))
  }

  if (has_yaml) {

    index_list     <- yaml::read_yaml(yaml_file)
    yaml_hashes    <- names(index_list)
    all_rds        <- list.files(folder, pattern = "\\.rds$", full.names = FALSE)
    result_files   <- all_rds[!grepl("_parameters\\.rds$", all_rds)]
    result_hashes  <- sub("\\.rds$", "", result_files)

    missing_in_yaml <- setdiff(result_hashes, yaml_hashes)
    missing_results <- setdiff(yaml_hashes, result_hashes)

    if (length(missing_in_yaml) || length(missing_results)) {
      msgs <- character()
      if (length(missing_in_yaml)) {
        msgs <- c(msgs,
                  paste0("Results without YAML entries: ",
                         paste(missing_in_yaml, collapse = ", ")))
      }
      if (length(missing_results)) {
        msgs <- c(msgs,
                  paste0("YAML entries without result files: ",
                         paste(missing_results, collapse = ", ")))
      }
      warning(paste(msgs, collapse = "\n"))
    }

  } else {
    ## Legacy parameter-file mode
    all_rds        <- list.files(folder, pattern = "\\.rds$", full.names = TRUE)
    result_files   <- all_rds[!grepl("_parameters\\.rds$", all_rds)]
    param_hashes   <- sub("_parameters\\.rds$", "", basename(param_files))
    result_hashes  <- sub("\\.rds$", "", basename(result_files))

    missing_params  <- setdiff(result_hashes, param_hashes)
    missing_results <- setdiff(param_hashes, result_hashes)

    if (length(missing_params) || length(missing_results)) {
      msgs <- character()
      if (length(missing_params)) {
        msgs <- c(msgs,
                  paste0("Results but no parameters: ",
                         paste(missing_params, collapse = ", ")))
      }
      if (length(missing_results)) {
        msgs <- c(msgs,
                  paste0("Parameters but no results: ",
                         paste(missing_results, collapse = ", ")))
      }
      warning(paste(msgs, collapse = "\n"))
    }
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
