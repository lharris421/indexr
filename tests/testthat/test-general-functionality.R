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
  save_objects(test_dir, obj1, params1)
  save_objects(test_dir, obj2, params2)
  save_objects(test_dir, obj3, params3)

  # Overwrite = TRUE (expect nothing)
  save_objects(test_dir, obj1, params1, overwrite = TRUE)

  # Expect 6 files
  saved_files <- list.files(test_dir)
  testthat::expect_equal(length(saved_files), 6)

  # Expect warnings if trying to overwrite
  testthat::expect_warning(save_objects(test_dir, obj1, params1))
  testthat::expect_equal(
    sum(stringr::str_detect(list.files(test_dir), glue::glue("{generate_hash(params1)$hash}_temp"))),
    2
  )

  # Read objects and validate
  obj1_in <- read_objects(test_dir, params1, print_hash = TRUE) ## Test print hash
  obj2_in <- read_objects(test_dir, params2)
  obj3_in <- read_objects(test_dir, params3)

  testthat::expect_identical(obj1_in, obj1)
  testthat::expect_identical(obj2_in, obj2)
  testthat::expect_identical(obj3_in, obj3)

  # Validate parameters
  params1_in <- readRDS(glue::glue("{test_dir}/{generate_hash(params1)$hash}_parameters.rds"))
  params2_in <- readRDS(glue::glue("{test_dir}/{generate_hash(params2)$hash}_parameters.rds"))
  params3_in <- readRDS(glue::glue("{test_dir}/{generate_hash(params3)$hash}_parameters.rds"))
  params1_in$timestamp <- NULL
  params2_in$timestamp <- NULL
  params3_in$timestamp <- NULL

  testthat::expect_identical(params1_in, params1)
  testthat::expect_identical(params2_in, params2)
  testthat::expect_identical(params3_in, params3)
})

testthat::test_that("update_from_hash_table works correctly", {

  hash_table_file <- glue::glue("{test_dir}/test_hash_table.csv")

  # Create hash table and validate
  create_hash_table(test_dir, save_path = hash_table_file)
  saved_hash_table <- readr::read_csv(hash_table_file)
  testthat::expect_equal(dim(saved_hash_table), c(4, 7))

  saved_files_before <- list.files(test_dir)

  # Update hash table and check consistency
  update_from_hash_table(hash_table_file, test_dir)
  saved_files_after <- list.files(test_dir)
  testthat::expect_identical(saved_files_before, saved_files_after)

  # Modify and validate updates
  saved_hash_table[,"other_params[[param4]]"] <- c("3", NA, NA, NA)
  readr::write_csv(saved_hash_table, hash_table_file)
  update_from_hash_table(hash_table_file, test_dir)

  changes <- glue::glue("{saved_hash_table$hash[c(1, 2)]}.rds")
  no_changes <- glue::glue("{saved_hash_table$hash[c(3, 4)]}.rds")

  saved_files <- list.files(test_dir, pattern = "\\.rds$")
  testthat::expect_true(all(!(changes %in% saved_files)))
  testthat::expect_true(all(no_changes %in% saved_files))
})

testthat::test_that("new parameters in hash table trigger updates", {

  hash_table_file <- glue::glue("{test_dir}/test_hash_table.csv")

  # Create and update hash table with new parameters
  create_hash_table(test_dir, save_path = hash_table_file)
  saved_hash_table <- readr::read_csv(hash_table_file)
  saved_hash_table$a_new_param <- c(NA, "1", "2", NA)
  readr::write_csv(saved_hash_table, hash_table_file)

  update_from_hash_table(hash_table_file, test_dir)

  changes <- glue::glue("{saved_hash_table$hash[c(2, 3)]}.rds")
  no_changes <- glue::glue("{saved_hash_table$hash[c(1, 4)]}.rds")

  saved_files <- list.files(test_dir, pattern = "\\.rds$")
  testthat::expect_true(all(!(changes %in% saved_files)))
  testthat::expect_true(all(no_changes %in% saved_files))
})

testthat::test_that("Test rehashing", {

  names_before <- list.files(test_dir, pattern = "rds")
  rehash(test_dir, algo = "xxhash32")
  names_after <- list.files(test_dir, pattern = "rds")

  testthat::expect_false(any(names_before %in% names_after))
  testthat::expect_true(length(names_before) == length(names_after))

})

# Clean up after tests
unlink(test_dir, recursive = TRUE)
