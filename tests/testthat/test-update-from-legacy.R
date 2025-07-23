# Setup
test_dir <- file.path(tempdir(), "testing_grounds")
dir.create(test_dir)

# tests/testthat/test_save_and_read_objects.R
testthat::test_that("save_objects and read_objects work correctly", {

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

  # Check
  saved_files <- list.files(test_dir)
  testthat::expect_equal(length(saved_files), 4)

  # Update
  update_from_legacy(test_dir)

  # Expect error
  testthat::expect_error(update_from_legacy(test_dir))

  ## Run again and update existing file (and expect warning)
  testthat::expect_warning(save_objects(test_dir, obj3, params3, yaml = FALSE))

  # Update
  update_from_legacy(test_dir)

})

# Clean up after tests
unlink(test_dir, recursive = TRUE)
