# Setup
test_dir <- file.path(tempdir(), "testing_grounds")
dir.create(test_dir)

testthat::test_that("check and fix extensions", {

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

  # Expect warnings if trying to overwrite
  testthat::expect_warning(save_objects(test_dir, obj1, params1, yaml = FALSE))

  # Create and update hash table with new parameters
  hash_table_file <- glue::glue("{test_dir}/test_hash_table.csv")
  create_hash_table(test_dir, save_path = hash_table_file)
  saved_hash_table <- readr::read_csv(hash_table_file)
  saved_hash_table$a_new_param <- c(NA, NA, "1", "1") ## These files will be the same
  readr::write_csv(saved_hash_table, hash_table_file)
  testthat::expect_message(update_from_hash_table(hash_table_file, test_dir))

  # Create and update hash table with new parameters
  curr_files <- list.files(test_dir, full.names = TRUE, pattern = "\\.rds$")
  create_hash_table(test_dir, save_path = hash_table_file)
  saved_hash_table <- readr::read_csv(hash_table_file)
  saved_hash_table$another_new_param <- c(1, 1, 1, 1)
  readr::write_csv(saved_hash_table, hash_table_file)

  file.remove(curr_files[c(1, 2, 3, 7)])
  testthat::expect_warning(update_from_hash_table(hash_table_file, test_dir))

})

testthat::test_that("indexr:::c_string_to_vector handles all cases correctly", {

  # 1. Check NA and empty string
  testthat::expect_true(is.na(indexr:::c_string_to_vector(NA)))
  testthat::expect_true(is.na(indexr:::c_string_to_vector("")))

  # 2. Check trimming whitespace and numeric conversion
  testthat::expect_equal(indexr:::c_string_to_vector("  42  "), 42)
  testthat::expect_equal(indexr:::c_string_to_vector("-3.14"), -3.14)

  # 3. Check logical conversion
  testthat::expect_true(indexr:::c_string_to_vector("TRUE"))
  testthat::expect_false(indexr:::c_string_to_vector("FALSE"))

  # 4. Check single string with no conversion
  #    (not numeric, not logical, does not start with c(...))
  testthat::expect_equal(indexr:::c_string_to_vector("hello"), "hello")

  # 5. Check vector of numeric with c(...)
  testthat::expect_equal(indexr:::c_string_to_vector("c(1, 2, 3)"), c(1, 2, 3))

  # 6. Check vector of logical with c(...)
  testthat::expect_equal(indexr:::c_string_to_vector("c(TRUE, FALSE, TRUE)"), c(TRUE, FALSE, TRUE))

  # 7. Check vector of mixed numeric formats with c(...)
  testthat::expect_equal(indexr:::c_string_to_vector("c(-1, 2.5, 3)"), c(-1, 2.5, 3))

  # 8. Check vector of character (unquoted) with c(...)
  testthat::expect_equal(indexr:::c_string_to_vector("c(abc, def)"), c("abc", "def"))

  # 9. Check vector of quoted character with c(...)
  testthat::expect_equal(indexr:::c_string_to_vector('c("hello", "world")'), c("hello", "world"))

  # 10. Check that malformed c(...) input falls back to plain string
  testthat::expect_equal(indexr:::c_string_to_vector("c(1,2,3"), "c(1,2,3")
  testthat::expect_equal(indexr:::c_string_to_vector("c1,2,3)"), "c1,2,3)")

})


unlink(test_dir, recursive = TRUE)
