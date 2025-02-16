% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_objects.R
\name{save_objects}
\alias{save_objects}
\title{Save Simulation Results with Names as Hashes from the Parameters that Generated Them}
\usage{
save_objects(
  folder,
  results,
  parameters_list = NULL,
  ignore_na = TRUE,
  alphabetical_order = TRUE,
  overwrite = FALSE,
  include_timestamp = TRUE,
  hash_includes_timestamp = FALSE,
  algo = "xxhash64",
  get_script_name = TRUE,
  ignore_script_name = FALSE,
  incremental = FALSE,
  silent = FALSE
)
}
\arguments{
\item{folder}{Character string specifying the path to the directory where the objects will be saved.}

\item{results}{The R object or list of objects to be saved.}

\item{parameters_list}{A named list of arguments used to generate a unique hash for the file.}

\item{ignore_na}{Logical. If \code{TRUE}, \code{NA} values in \code{parameters_list} are ignored during hash generation.}

\item{alphabetical_order}{Logical. If \code{TRUE}, the names in \code{parameters_list} are sorted alphabetically before hash generation.}

\item{overwrite}{Logical. If \code{TRUE}, existing files with the same hash will be overwritten. If \code{FALSE} and a conflict occurs, the results will be saved under a temporary hash.}

\item{include_timestamp}{Logical. If \code{TRUE}, a timestamp is added to \code{parameters_list}.}

\item{hash_includes_timestamp}{Logical. If \code{TRUE}, the timestamp is included in the hash generation.}

\item{algo}{Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}. See \code{?digest}}

\item{get_script_name}{Logical. If \code{TRUE}, attempts to get the script name and add it to \code{parameters_list}. Only works if script is run from command line, in an interactive session, this will always be \code{NULL}.}

\item{ignore_script_name}{Logical. If \code{TRUE}, the script name is ignored during hash generation.}

\item{incremental}{Logical. If \code{TRUE}, results are saved in a subfolder named after the hash and can be combined with \code{compress_incremental}. Note, if \code{TRUE}, no checks will be done for results that already exist, the user should check this in their script with \code{check_hash_existence}.}

\item{silent}{Logical. If \code{TRUE}, no check is done that pairs of results files (parameters and associated results) is done. This check is not necessary, but done by default to keep the user aware of a scenario that usually results from manual file manipulation.}
}
\value{
No return value. This function is called for its side effects.
}
\description{
Saves RDS files to a specified folder with a name that is a hash generated from a list of parameters used for the simulation. There are a number of options that control the behavior, however, the default functionality likely covers 99\% of use cases.
}
\details{
This function saves R objects to disk with a file name based on a generated hash of the provided arguments. It supports incremental saving, where multiple results can be saved under the same hash in a subdirectory and later collected. This can be helpful for a simulation that runs and saves results in parallel for the SAME set of simulation parameters.
}
\examples{
## Setup
tmp_dir <- file.path(tempdir(), "example")
dir.create(tmp_dir)

## Example using parameter list to run simulation and save results
parameters_list <- list(
  iterations = 1000,
  x_dist = "rnorm",
  x_dist_options = list(n = 10, mean = 1, sd = 2),
  error_dist = "rnorm",
  error_dist_options = list(n = 10, mean = 0, sd = 1),
  beta0 = 1,
  beta1 = 1
)

betas <- numeric(parameters_list$iterations)
for (i in 1:parameters_list$iterations) {
  x <- do.call(parameters_list$x_dist, parameters_list$x_dist_options)
  err <- do.call(parameters_list$error_dist, parameters_list$error_dist_options)
  y <- parameters_list$beta0 + parameters_list$beta1*x + err
  betas[i] <- coef(lm(y ~ x))["x"]
}

save_objects(folder = tmp_dir, results = betas, parameters_list = parameters_list)

## Read back in (consider clearing environment before running)
## Re-setup
tmp_dir <- file.path(tempdir(), "example")

parameters_list <- list(
  iterations = 1000,
  x_dist = "rnorm",
  x_dist_options = list(n = 10, mean = 1, sd = 2),
  error_dist = "rnorm",
  error_dist_options = list(n = 10, mean = 0, sd = 1),
  beta0 = 1,
  beta1 = 1
)

betas <- read_objects(folder = tmp_dir, parameters_list = parameters_list)

## Cleanup
unlink(tmp_dir, recursive = TRUE)

}
\seealso{
\code{\link[=read_objects]{read_objects()}}
}
