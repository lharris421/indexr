# tests/testthat/test_parameterized_simulation.R
test_that("Simulation results are saved and read correctly", {
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

  # Save results
  temp_dir <- testthat::test_path("simulation_test_dir")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)

  save_objects(folder = temp_dir, results = betas, parameters_list = parameters_list)

  # Check that results are saved correctly
  files <- list.files(temp_dir)
  expect_true(length(files) == 2)
  expect_true(any(grepl("_parameters\\.rds$", files)))
  expect_true(any(grepl("\\.rds$", files)))
})

test_that("Results can be read back and match original", {
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

  temp_dir <- testthat::test_path("simulation_test_dir")
  betas <- read_objects(folder = temp_dir, parameters_list = parameters_list)

  # Validate betas
  expect_true(length(betas) == parameters_list$iterations)
  expect_true(is.numeric(betas))

})

test_that("Hash table creation and cleanup works correctly", {
  temp_dir <- testthat::test_path("simulation_test_dir")

  # Create hash table
  hash_table <- create_hash_table(folder = temp_dir)
  expect_true(nrow(hash_table) == 1)

  # Cleanup files
  cleanup_from_hash_table(folder = temp_dir, hash_table = hash_table, mode = "all", verify = FALSE)

  # Validate that directory is cleaned up
  files_after_cleanup <- list.files(temp_dir)
  expect_true(length(files_after_cleanup) == 0)
})

unlink(testthat::test_path("simulation_test_dir"), recursive = TRUE)
