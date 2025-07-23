# Setup
test_dir <- file.path(tempdir(), "testing_grounds")
dir.create(test_dir)

# tests/testthat/test_parameterized_simulation.R
testthat::test_that("Simulation results are saved and read correctly", {
  # Set up parameters
  parameters_list <- list(
    iterations = 1000,
    x_dist = "rnorm",
    x_dist_options = list(n = 10, mean = 1, sd = 2),
    error_dist = "rnorm",
    error_dist_options = list(n = 10, mean = 0, sd = 1),
    beta0 = 1,
    beta1 = 1
  )

  # Run simulation
  betas <- numeric(parameters_list$iterations)
  for (i in 1:parameters_list$iterations) {
    x <- do.call(parameters_list$x_dist, parameters_list$x_dist_options)
    err <- do.call(parameters_list$error_dist, parameters_list$error_dist_options)
    y <- parameters_list$beta0 + parameters_list$beta1 * x + err
    betas[i] <- coef(lm(y ~ x))["x"]
  }

  save_objects(folder = test_dir, results = betas, parameters_list = parameters_list)

  # Check that results are saved correctly
  files <- list.files(test_dir)
  testthat::expect_true(length(files) == 3)
  testthat::expect_true(!any(grepl("_parameters\\.rds$", files)))
  testthat::expect_true(any(grepl("\\.rds$", files)))
})

testthat::test_that("Results can be read back and match original", {
  # Set up parameters
  parameters_list <- list(
    iterations = 1000,
    x_dist = "rnorm",
    x_dist_options = list(n = 10, mean = 1, sd = 2),
    error_dist = "rnorm",
    error_dist_options = list(n = 10, mean = 0, sd = 1),
    beta0 = 1,
    beta1 = 1
  )

  betas <- read_objects(folder = test_dir, parameters_list = parameters_list)

  # Validate betas
  testthat::expect_true(length(betas) == parameters_list$iterations)
  testthat::expect_true(is.numeric(betas))

})

testthat::test_that("Hash table creation and cleanup works correctly", {

  # Create hash table
  hash_table <- create_hash_table(folder = test_dir)
  testthat::expect_true(nrow(hash_table) == 1)

  # Cleanup files
  cleanup_from_hash_table(folder = test_dir, hash_table = hash_table, mode = "all", request_confirmation = FALSE)

  # Validate that directory is cleaned up
  files_after_cleanup <- list.files(test_dir)
  testthat::expect_true(length(files_after_cleanup) == 2)
})

unlink(test_dir, recursive = TRUE)
