# tests/testthat/test_save_and_read_objects.R
test_that("Test for expected errors when saving", {

  # Save objects
  expect_error(save_objects(test_dir, obj1))

})
