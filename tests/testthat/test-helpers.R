# Setup
test_dir <- file.path(tempdir(), "testing_grounds")
dir.create(test_dir)

testthat::test_that("check missing pairs", {

  # Test data
  obj1 <- rnorm(1000)
  obj2 <- data.frame(x = runif(100), y = "something", z = rep(c(TRUE, FALSE), 50))
  obj3 <- list(obj1, obj2)

  params1 <- list(distribution = "normal", other_params = list(param1 = TRUE, param2 = 1, param3 = NA))
  params2 <- list(distribution = "uniform", other_params = list(param1 = FALSE, param2 = 2, param3 = "1", param4 = 4))
  params3 <- list(distribution = "composite", other_params = list(param1 = TRUE, param2 = 3, param3 = 1))

  # Save objects
  save_objects(test_dir, obj1, params1)
  save_objects(test_dir, obj2, params2)
  save_objects(test_dir, obj3, params3)

  curr_files <- list.files(test_dir, full.names = TRUE)
  file.remove(curr_files[c(1, 6)])

  testthat::expect_warning(indexr:::check_missing_pairs(test_dir))

})

testthat::test_that("check and fix extensions", {

  testthat::expect_error(indexr:::check_and_fix_extension("test.csv", "txt"))
  testthat::expect_equal(indexr:::check_and_fix_extension("test", "txt"), "test.txt")

})

testthat::test_that("directory check", {

  unlink(test_dir, recursive = TRUE)
  dir.create(test_dir)
  write.csv(data.frame("1" = 2, file.path(test_dir, "test.csv")))

  testthat::expect_error(indexr:::check_is_directory(file.path(test_dir, "test.csv")))

})

testthat::test_that("convert type", {

  x <- rnorm(100)
  y <- rnorm(100)
  lm_res <- lm(y ~ x)

  testthat::expect_type(indexr:::convert_type(lm_res$call), "character")
  testthat::expect_type(indexr:::convert_type(9 + 3i), "character")

})


unlink(test_dir, recursive = TRUE)
