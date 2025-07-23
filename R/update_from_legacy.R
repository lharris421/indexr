#' Update individual parameter files (legacy) to a single indexr.yaml parameters
#' file.
#'
#' @param folder the rds folder
#'
#' @returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' update_from_legacy(path_to_rds)
#' }
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
