#' Combined individual *_parameter.rds files (legacy) to a single indexr.yaml
#' parameters file. Prior to version 0.3.0, each result file was saved with a
#' corresponding *_parameters.rds file.
#'
#' For backward support, \code{save_objects} has been given an option \code{yaml}
#' which is \code{TRUE} by default. If switched to \code{FALSE}, the legacy behavior
#' will be produced. However, this option will go away in version 0.4.0.
#'
#' Functions which use the parameters will detect whether to use
#' *_parameters.rds files or the \code{indexr.yaml} file. If both are detected,
#' a warning/error is produced as appropriate. This can be addressed by running
#' \code{update_from_legacy}.
#'
#' In version 1.0.0, support for legacy will be dropped completely.
#'
#' @param folder the rds folder containing the *_parameter.rds files to convert
#' to a single indexr.yaml file.
#'
#' @returns The function does not return a value but compresses all \code{*_parameter.rds} files into \code{indexr.yaml}.
#' @export
#'
#' @examples
#' tmp_dir <- file.path(tempdir(), "example")
#' dir.create(tmp_dir)
#'
#' obj1 <- rnorm(1000)
#' obj2 <- data.frame(x = runif(100), y = "something", z = rep(c(TRUE, FALSE), 50))
#'
#' params1 <- list(distribution = "normal", other_params = list(param1 = TRUE, param2 = 1, param3 = NA))
#' params2 <- list(distribution = "uniform", other_params = list(param1 = FALSE, param2 = 2, param3 = "1", param4 = 4))
#'
#' # Save objects
#' save_objects(tmp_dir, obj1, params1, yaml = FALSE)
#' save_objects(tmp_dir, obj2, params2, yaml = FALSE)
#'
#' # Update
#' update_from_legacy(tmp_dir)
#'
#' # View file
#' yaml::read_yaml(file.path(tmp_dir, "indexr.yaml"))
update_from_legacy <- function(folder) {

  yaml_file <- "indexr.yaml"

  ## Ensure folder exists
  check_is_directory(folder)

  ## Locate all legacy parameter files
  param_files <- list.files(
    folder, pattern = "_parameters\\.rds$", full.names = TRUE
  )
  if (length(param_files) == 0) {
    stop("No parameter RDS files found in folder: ", folder)
  }

  ## Prepare YAML index path
  yaml_path <- file.path(folder, yaml_file)

  ## Load or initialize index list
  if (file.exists(yaml_path)) {
    index_list <- yaml::read_yaml(yaml_path)
  } else {
    index_list <- list()
  }

  ## Read each legacy file and add to the index (overwriting duplicates)
  hashes <- sub("_parameters\\.rds$", "", basename(param_files))
  for (i in seq_along(param_files)) {
    h <- hashes[i]
    params <- readRDS(param_files[i])
    index_list[[h]] <- params
  }

  ## Write updated YAML index
  yaml::write_yaml(index_list, yaml_path)

  ## Remove the legacy parameter files
  file.remove(param_files)

  message(
    "Added ", length(param_files),
    " entries to '", yaml_file,
    "' and removed the originals."
  )

  invisible(index_list)
}
