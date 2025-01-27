# tests/testthat/test_save_and_read_objects.R
testthat::test_that("save_objects and read_objects work correctly", {

  params1 <- list(distribution = "normal", other_params = list(param1 = TRUE, param2 = 1, param3 = NA))
  params2 <- list(distribution = "normal", other_params = list(param1 = TRUE, param2 = 1, param3 = 1))

  ## Error when no folder for rds file provided
  testthat::expect_error(read_objects(parameters_list = params1))

  ## Error when folder provided is not character vector
  testthat::expect_error(read_objects(folders = list("folder1", "folder2"), parameters_list = params1))

  ## Error when parameter list is not a list
  params1_df <- data.frame(distribution = "normal", `other_params[[param1]]` = TRUE, `other_params[[param2]]` = 1)
  testthat::expect_error(read_objects(folders = "folder1", parameters_list = params1_df))

  ## Expect warning (Return NULL)
  testthat::expect_warning({
   res <- read_objects(folders = ".", parameters_list = params2)
  })
  testthat::expect_equal(res, NULL)

})
