testthat::test_that("save object errors", {
  testthat::expect_error(save_objects(folder = ".", results = 1:10))
})
