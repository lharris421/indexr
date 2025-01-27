testthat::test_that("Manual cleanup from hash table", {
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
  temp_dir <- testthat::test_path("testing_grounds")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)

  save_objects(folder = temp_dir, results = betas, parameters_list = parameters_list)

  # Check that results are saved correctly
  files <- list.files(temp_dir)
  testthat::expect_true(length(files) == 2)
  testthat::expect_true(any(grepl("_parameters\\.rds$", files)))
  testthat::expect_true(any(grepl("\\.rds$", files)))

  # Create hash table
  hash_table <- create_hash_table(folder = temp_dir)
  testthat::expect_true(nrow(hash_table) == 1)

  # Expect error, name not given
  testthat::expect_error(cleanup_from_hash_table(folder = temp_dir, hash_table = hash_table, mode = "manual", request_confirmation = FALSE))

  # Expect error, column given but does not exist
  testthat::expect_error(cleanup_from_hash_table(folder = temp_dir, hash_table = hash_table, mode = "manual", request_confirmation = FALSE, column = "keep"))

  # Nothing to delete
  hash_table$delete <- FALSE
  testthat::expect_message(cleanup_from_hash_table(folder = temp_dir, hash_table = hash_table, mode = "manual", request_confirmation = FALSE, column = "delete"))

  # Proper usage
  hash_table$delete <- TRUE
  cleanup_from_hash_table(folder = temp_dir, hash_table = hash_table, mode = "manual", request_confirmation = FALSE, column = "delete")

  # Validate that directory is cleaned up
  files_after_cleanup <- list.files(temp_dir)
  testthat::expect_true(length(files_after_cleanup) == 0)

  # Run again, expect message that no matching files are found
  testthat::expect_message(cleanup_from_hash_table(folder = temp_dir, hash_table = hash_table, mode = "manual", request_confirmation = FALSE, column = "delete"))


})

unlink(testthat::test_path("testing_grounds"), recursive = TRUE)
