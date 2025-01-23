# tests/testthat/test_incremental_save.R

# Load package
devtools::load_all()

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
