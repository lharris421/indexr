# Setup
test_dir <- file.path(tempdir(), "testing_grounds")
dir.create(test_dir)

testthat::test_that("Check warnings, errors, messages for rehash", {

  ## Attempt to rehash empty dir
  testthat::expect_message(rehash(test_dir))

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

  # Rehash with no changes, expect message
  testthat::expect_message(rehash(test_dir))

  ## Remove results file, so that get warning if try to rehash
  current_files <- list.files(test_dir, pattern = "\\.rds", full.names = TRUE)
  current_files <- current_files[!stringr::str_detect(current_files, "parameters")]
  file.remove(current_files[1])
  testthat::expect_warning(rehash(test_dir))

  ## Expect collision error
  testthat::expect_warning(save_objects(test_dir, obj3, params3, yaml = FALSE))
  testthat::expect_warning(testthat::expect_error(rehash(test_dir)))

})

# Clean up after tests
unlink(test_dir, recursive = TRUE)
