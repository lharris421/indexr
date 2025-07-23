# Setup
test_dir <- file.path(tempdir(), "testing_grounds")
dir.create(test_dir)

# tests/testthat/test_incremental_save.R
testthat::test_that("incremental saving and compression work correctly", {

  # Parameters
  params <- list(a = "1", b = "2")

  # Save objects incrementally
  for (i in 1:10) {
    save_objects(test_dir, data.frame(idx = i, val = rnorm(1)), params, incremental = TRUE, yaml = FALSE)
  }

  # Validate the number of saved incremental files
  tmp_dir <- file.path(test_dir, generate_hash(params)$hash)
  testthat::expect_equal(length(list.files(tmp_dir)), 20)

  # Compress incremental files
  compress_incremental(test_dir, params)

  # Check for hash
  testthat::expect_equal(check_hash_existence(test_dir, params), TRUE)
  testthat::expect_error(check_hash_existence(test_dir, params, halt = TRUE))

  # Validate the incremental folder is removed
  testthat::expect_error(check_is_directory(tmp_dir))

  # Validate only two files remain (results and parameters)
  testthat::expect_equal(length(list.files(test_dir)), 2)

  # Read and validate combined results
  res <- read_objects(test_dir, params)
  testthat::expect_true(is.data.frame(res))
  testthat::expect_true("val" %in% names(res))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("incremental saving and compression work correctly for lists", {

  # Setup
  unlink(test_dir, recursive = TRUE)
  dir.create(test_dir)

  # Parameters
  params <- list(a = "1", b = "2")

  # Save objects incrementally
  for (i in 1:10) {
    save_objects(test_dir, list(idx = i, val = rnorm(1)), params, incremental = TRUE, yaml = FALSE)
  }

  # Validate the number of saved incremental files
  tmp_dir <- file.path(test_dir, generate_hash(params)$hash)
  testthat::expect_equal(length(list.files(tmp_dir)), 20)

  # Compress incremental files
  compress_incremental(test_dir, params, remove_folder = FALSE) ## Dont remove

  # Validate the incremental folder still exists
  testthat::expect_true(indexr:::check_is_directory(tmp_dir))

  # Check for hash
  testthat::expect_equal(check_hash_existence(test_dir, params), TRUE)
  testthat::expect_error(check_hash_existence(test_dir, params, halt = TRUE))

  # Validate only two files remain (results and parameters)
  testthat::expect_equal(length(list.files(test_dir, pattern = "\\.rds")), 2)

  # Read and validate combined results
  res <- read_objects(test_dir, params)
  testthat::expect_true(is.list(res))

  ## Expect warning if try to compress folder that is now empty
  inc_folder <- file.path(test_dir, generate_hash(parameters_list = params)$hash)
  testthat::expect_true(all(file.remove(list.files(inc_folder, full.names = TRUE))))
  testthat::expect_warning(compress_incremental(test_dir, params))

  ## Remove folder and then expect error if try to compress with it not existing
  unlink(inc_folder, recursive = TRUE)
  testthat::expect_error(compress_incremental(test_dir, params))

})

# Clean up
unlink(test_dir, recursive = TRUE)
