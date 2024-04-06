# indexr: Efficient Parameter Hashing and Object Management in R <img src="man/figures/logo.webp" align="right" height="150" />

`indexr` is a cutting-edge R package designed to streamline the management of R objects based on parameter configurations. It provides robust tools for saving, reading, and updating objects with hashed file names, ensuring efficient and organized data handling. This package is especially useful for scenarios involving extensive parameter tuning, simulations, or any context where managing a large number of R objects is required.

## Features

- **Save and Read Objects**: Quickly save and read R objects with filenames generated from parameter hashes, ensuring easy tracking and retrieval.
- **Hash Table Creation**: Generate a comprehensive hash table from your RDS files for easy reference and filtering.
- **Intuitive Rehashing**: Effortlessly update hashes when changing hash algorithms or parameters with the `rehash` function.
- **Parameter Existence Check**: Verify the existence of parameter combinations in your saved objects with `check_parameters_existence`.

## Installation

Install `indexr` from GitHub (future CRAN release planned):

```R
# install.packages("devtools")
devtools::install_github("lharris421/indexr")
```

## Usage

Here's a quick start guide to using indexr:

```R
library(indexr)

# Example usage of save_objects and read_objects
args_list <- list(param1 = "value1", param2 = 100)
folder_path <- "path/to/your/rds/files"
save_objects(folder_path, args_list)
read_objects(folder_path, params_grid = data.frame(param1 = "value1", param2 = 100))

# Creating a hash table from RDS files
hash_table <- create_hash_table(folder_path)
```

For detailed usage, please refer to the package documentation.

## Contributing

Contributions to indexr are welcome! Whether it's feature requests, bug reports, or code contributions, your input is highly valued. Please feel free to submit issues and pull requests on the GitHub repository.

## License

This project is licensed under the MIT License.
