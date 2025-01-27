# tests/testthat/test_incremental_save.R
test_that("incremental saving and compression work correctly", {
  # Setup
  test_dir <- testthat::test_path("incremental_save")
  unlink(test_dir, recursive = TRUE)
  dir.create(test_dir)

  # Parameters
  params <- list(a = "1", b = "2")

  # Save objects incrementally
  for (i in 1:10) {
    save_objects(test_dir, data.frame(idx = i, val = rnorm(1)), params, incremental = TRUE)
  }

  # Validate the number of saved incremental files
  tmp_dir <- file.path(test_dir, generate_hash(params)$hash)
  expect_equal(length(list.files(tmp_dir)), 20)

  # Compress incremental files
  compress_incremental(test_dir, params)

  # Check for hash
  expect_equal(check_hash_existence(test_dir, params), TRUE)
  expect_error(check_hash_existence(test_dir, params, halt = TRUE))

  # Validate the incremental folder is removed
  expect_error(check_is_directory(tmp_dir))

  # Validate only two files remain (results and parameters)
  expect_equal(length(list.files(test_dir)), 2)

  # Read and validate combined results
  res <- read_objects(test_dir, params)
  expect_true(is.data.frame(res))
  expect_true("val" %in% names(res))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("incremental saving and compression work correctly for lists", {
  # Setup
  test_dir <- testthat::test_path("incremental_save")
  unlink(test_dir, recursive = TRUE)
  dir.create(test_dir)

  # Parameters
  params <- list(a = "1", b = "2")

  # Save objects incrementally
  for (i in 1:10) {
    save_objects(test_dir, list(idx = i, val = rnorm(1)), params, incremental = TRUE)
  }

  # Validate the number of saved incremental files
  tmp_dir <- file.path(test_dir, generate_hash(params)$hash)
  expect_equal(length(list.files(tmp_dir)), 20)

  # Compress incremental files
  compress_incremental(test_dir, params, remove_folder = FALSE) ## Dont remove

  # Validate the incremental folder still exists
  expect_true(indexr:::check_is_directory(tmp_dir))

  # Check for hash
  expect_equal(check_hash_existence(test_dir, params), TRUE)
  expect_error(check_hash_existence(test_dir, params, halt = TRUE))

  # Validate only two files remain (results and parameters)
  expect_equal(length(list.files(test_dir, pattern = "\\.rds")), 2)

  # Read and validate combined results
  res <- read_objects(test_dir, params)
  expect_true(is.list(res))

  ## Expect warning if try to compress folder that is now empty
  inc_folder <- file.path(test_dir, generate_hash(parameters_list = params)$hash)
  testthat::expect_true(all(file.remove(list.files(inc_folder, full.names = TRUE))))
  testthat::expect_warning(compress_incremental(test_dir, params))

  ## Remove folder and then expect error if try to compress with it not existing
  unlink(inc_folder, recursive = TRUE)
  testthat::expect_error(compress_incremental(test_dir, params))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})
