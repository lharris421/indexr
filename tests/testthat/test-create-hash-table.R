testthat::test_that("test filter list for hash table", {

  # Setup
  test_dir <- testthat::test_path("testing_grounds")
  unlink(test_dir, recursive = TRUE)
  dir.create(test_dir)

  # Test data
  obj1 <- rnorm(1000)
  obj2 <- data.frame(x = runif(100), y = "something", z = rep(c(TRUE, FALSE), 50))

  params1 <- list(distribution = "normal", other_params = list(param1 = TRUE, param2 = 1, param3 = c(1, 2, 3)))
  params2 <- list(distribution = "normal", other_params = list(param1 = FALSE, param2 = 2, param3 = c("1", "2", "3")))

  # Save objects
  save_objects(test_dir, obj1, params1)
  save_objects(test_dir, obj2, params2)

  # Create hash table and validate
  tab <- create_hash_table(test_dir, filter_list = list(`other_params[[param1]]` = TRUE))
  testthat::expect_equal(nrow(tab), 1)

})

# Clean up after tests
unlink(testthat::test_path("testing_grounds"), recursive = TRUE)
