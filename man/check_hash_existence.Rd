% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_hash_existence.R
\name{check_hash_existence}
\alias{check_hash_existence}
\title{Check for the Existence of Results Under a Set of Parameters}
\usage{
check_hash_existence(
  folder,
  parameters_list,
  halt = FALSE,
  hash_includes_timestamp = FALSE,
  ignore_na = TRUE,
  alphabetical_order = TRUE,
  algo = "xxhash64",
  ignore_script_name = FALSE
)
}
\arguments{
\item{folder}{A string specifying the directory containing the RDS files.}

\item{parameters_list}{A list of parameters for which a corresponding hash named file is checked.}

\item{halt}{Logical; if TRUE, the function stops execution if an existing file is found. This may be useful as a check before running a simulation.}

\item{hash_includes_timestamp}{Logical; if TRUE, timestamps are included in the hash generation process.}

\item{ignore_na}{Logical; if TRUE, NA values are ignored during hash generation.}

\item{alphabetical_order}{Logical; if TRUE, parameters are sorted alphabetically before hash generation.}

\item{algo}{Character string specifying the hashing algorithm to use. Default is \code{"xxhash64"}. See \code{?digest}}

\item{ignore_script_name}{Logical. If \code{TRUE}, the script name is ignored during hash generation.}
}
\value{
A logical of whether or not a file exists, unless \code{halt = TRUE} and a file is found, then an error is thrown.
}
\description{
This function checks for the existence of results saved under specified parameter list
in RDS files (saved with \code{indexr}) within a given folder.
}
\examples{
## Setup
tmp_dir <- file.path(tempdir(), "example")
dir.create(tmp_dir)

## Save an object
parameters_list <- list(example = "check_hash_existence")
save_objects(folder = tmp_dir, results = 1, parameters_list = parameters_list)

## Check that an object under specified parameters is saved
check_hash_existence(folder = tmp_dir, parameters_list)

## Cleanup
unlink(tmp_dir, recursive = TRUE)
}
