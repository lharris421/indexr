# Setup
test_dir <- file.path(tempdir(), "testing_grounds")
dir.create(test_dir)

testthat::test_that("general cleanup with tagging functionalty", {

  # Test data
  obj1 <- rnorm(1000)
  obj2 <- data.frame(x = runif(100), y = "something", z = rep(c(TRUE, FALSE), 50))
  obj3 <- list(obj1, obj2)

  params1 <- list(distribution = "normal", other_params = list(param1 = TRUE, param2 = 1, param3 = NA))
  params2 <- list(distribution = "uniform", other_params = list(param1 = FALSE, param2 = 2, param3 = "1", param4 = 4))
  params3 <- list(distribution = "composite", other_params = list(param1 = TRUE, param2 = 3, param3 = 1))

  # Save objects
  save_objects(test_dir, obj1, params1, yaml = FALSE)
  save_objects(test_dir, obj2, params2, yaml = FALSE)
  save_objects(test_dir, obj3, params3, yaml = FALSE)

  # Start tagging
  start_tagging(test_dir)

  # Check that file exists and is empty
  testthat::expect_true(file.exists(file.path(test_dir, "indexr_tagging.txt")))
  testthat::expect_true(file.info(file.path(test_dir, "indexr_tagging.txt"))$size == 0)

  ## Try to tag with empty file
  testthat::expect_error(cleanup(test_dir))

  # Read objects 1/3
  obj1_in <- read_objects(test_dir, params1)
  obj3_in <- read_objects(test_dir, params3)
  obj3_in <- read_objects(test_dir, params3) ## Test tagging file timestamp update

  ## Try to cleanup with incorrect date format
  testthat::expect_error(cleanup(test_dir, cutoff_date = "10-20-2000"))

  # Read in file, check there are two entries
  tagging_file <- readr::read_delim(file.path(test_dir, "indexr_tagging.txt"), delim = "\t", col_names = FALSE)
  testthat::expect_equal(nrow(tagging_file), 2)

  ## Delete unused file
  cleanup(test_dir, request_confirmation = FALSE, cutoff_date = "2000-10-01 12:12:12")
  testthat::expect_equal(length(list.files(test_dir, pattern = "\\.rds")), 4)

  # No files left to remove
  testthat::expect_message(cleanup(test_dir))

  ## Remove the tagging file
  close_tagging(test_dir)
  testthat::expect_false(file.exists(file.path(test_dir, "indexr_tagging.txt")))

  ## Try to cleanup without tagging file
  testthat::expect_error(cleanup(test_dir))

  ## Try to close file that no longer exists
  testthat::expect_warning(close_tagging(test_dir))

})


# Clean up after tests
unlink(test_dir, recursive = TRUE)
