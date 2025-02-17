<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/indexr)](https://CRAN.R-project.org/package=indexr)
[![GitHub version](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/lharris421/indexr/master/.version.json)](https://github.com/lharris/indexr)
[![Codecov test coverage](https://codecov.io/gh/lharris421/indexr/graph/badge.svg)](https://app.codecov.io/gh/lharris421/indexr)
[![R-CMD-check](https://github.com/lharris421/indexr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lharris421/indexr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# indexr: Stop naming simulation results <img src="man/figures/indexr_hex_sticker.png" alt="A hex sticker for indexr: A red arrow going into the hex sticker, indicating saving results, which are depicted by yellow boxes, and a blue arrow going out of the hex sticker, indicating reading results." align="right" width="20%" />

`indexr` is an R package designed to automate the saving and reading of R objects resulting from simulations based on their parameter configurations. It provides tools for saving, reading, updating, and managing objects with hashed file names, enabling efficient and organized data handling. This package is especially useful for scenarios involving extensive parameter tuning, simulations, or any context where managing a large number of R objects is required.

`indexr` takes an opinionated perspective on how simulations should be set up and as such enforces certain behaviors, which will be explained more in a coming vignette.

## Core Features

- **Save and Read Objects**: Save (`save_objects`) and read (`read_objects`) R objects with file names generated from parameter hashes, avoiding cumbersome or uninformative file names.
- **Hash Table Creation**: Generate a table (`create_hash_table`) from your RDS files for easy reference of what simulations you have in a given folder.
- **File management:** Monitor which results you use with `start_tagging` and easily remove unused results with `cleanup`. Or remove specific simulations with the combination of `create_hash_table` and `cleanup_from_hash_table`.
- **Rehashing**: Update hashes when a parameter needs to be changed or added to define a simulation with `update_hash_table`.
- **Hash Existence Check**: Check for the existence of parameter combinations in your saved objects with `check_hash_existence` to avoid rerunning simulations you may have forgot about.

## Installation

```R
install.packages("indexr")
```

Install development version of `indexr` from GitHub:

```R
# install.packages("devtools")
devtools::install_github("lharris421/indexr")
```

## Usage

Here's a quick start guide to using `indexr`. Running this code will save and delete two files to R sessions temp directory.

```r
library(indexr)

# Example usage of save_objects

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

tmp_dir <- file.path(tempdir(), "example")
dir.create(tmp_dir)
save_objects(folder = tmp_dir, results = betas, parameters_list = parameters_list)

# Example usage of read_objects (consider clearing environment before running)

parameters_list <- list(
  iterations = 1000,
  x_dist = "rnorm",
  x_dist_options = list(n = 10, mean = 1, sd = 2),
  error_dist = "rnorm",
  error_dist_options = list(n = 10, mean = 0, sd = 1),
  beta0 = 1,
  beta1 = 1
)

tmp_dir <- file.path(tempdir(), "example")
betas <- read_objects(folder = tmp_dir, parameters_list = parameters_list) 
hist(betas)

# Create a hash table
hash_table <- create_hash_table(folder = tmp_dir)

# Delete files based on hash table
cleanup_from_hash_table(folder = tmp_dir, hash_table = hash_table, mode = "all")

# Remove the tmp folder
unlink(tmp_dir, recursive = TRUE)
```

For detailed usage, please refer to the package documentation.

## Contributing

Contributions to `indexr` are welcome! Whether it's feature requests, bug reports, or code contributions, your input is highly valued. Please feel free to submit issues and pull requests on the GitHub repository.

