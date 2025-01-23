# tests/testthat/test_save_and_read_objects.R

# Load package
devtools::load_all()

test_that("save_objects and read_objects work correctly", {
  # Setup
  test_dir <- "./tests/testing_grounds"
  unlink(test_dir, recursive = TRUE)
  dir.create(test_dir)

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
  expect_equal(length(saved_files), 6)

  # Expect warnings if trying to overwrite
  expect_warning(save_objects(test_dir, obj1, params1))
  expect_equal(
    sum(stringr::str_detect(list.files(test_dir), glue::glue("{generate_hash(params1)$hash}_temp"))),
    2
  )

  # Read objects and validate
  obj1_in <- read_objects(test_dir, params1)
  obj2_in <- read_objects(test_dir, params2)
  obj3_in <- read_objects(test_dir, params3)

  expect_identical(obj1_in, obj1)
  expect_identical(obj2_in, obj2)
  expect_identical(obj3_in, obj3)

  # Validate parameters
  params1_in <- readRDS(glue::glue("{test_dir}/{generate_hash(params1)$hash}_parameters.rds"))
  params2_in <- readRDS(glue::glue("{test_dir}/{generate_hash(params2)$hash}_parameters.rds"))
  params3_in <- readRDS(glue::glue("{test_dir}/{generate_hash(params3)$hash}_parameters.rds"))

  expect_identical(params1_in, params1)
  expect_identical(params2_in, params2)
  expect_identical(params3_in, params3)
})

test_that("update_hash_table works correctly", {
  test_dir <- "./tests/testing_grounds"
  hash_table_file <- glue::glue("{test_dir}/test_hash_table.csv")

  # Create hash table and validate
  create_hash_table(test_dir, save_path = hash_table_file)
  saved_hash_table <- readr::read_csv(hash_table_file)
  expect_equal(dim(saved_hash_table), c(4, 6))

  saved_files_before <- list.files(test_dir)

  # Update hash table and check consistency
  update_hash_table(hash_table_file, test_dir)
  saved_files_after <- list.files(test_dir)
  expect_identical(saved_files_before, saved_files_after)

  # Modify and validate updates
  saved_hash_table[,"other_params[[param4]]"] <- c("3", NA, NA, NA)
  readr::write_csv(saved_hash_table, hash_table_file)
  update_hash_table(hash_table_file, test_dir)

  changes <- glue::glue("{saved_hash_table$hash[c(1, 2)]}.rds")
  no_changes <- glue::glue("{saved_hash_table$hash[c(3, 4)]}.rds")

  saved_files <- list.files(test_dir, pattern = "\\.rds$")
  expect_true(all(!(changes %in% saved_files)))
  expect_true(all(no_changes %in% saved_files))
})

test_that("new parameters in hash table trigger updates", {
  test_dir <- "./tests/testing_grounds"
  hash_table_file <- glue::glue("{test_dir}/test_hash_table.csv")

  # Create and update hash table with new parameters
  create_hash_table(test_dir, save_path = hash_table_file)
  saved_hash_table <- readr::read_csv(hash_table_file)
  saved_hash_table$a_new_param <- c(NA, "1", "2", NA)
  readr::write_csv(saved_hash_table, hash_table_file)

  update_hash_table(hash_table_file, test_dir)

  changes <- glue::glue("{saved_hash_table$hash[c(2, 3)]}.rds")
  no_changes <- glue::glue("{saved_hash_table$hash[c(1, 4)]}.rds")

  saved_files <- list.files(test_dir, pattern = "\\.rds$")
  expect_true(all(!(changes %in% saved_files)))
  expect_true(all(no_changes %in% saved_files))
})

# Clean up after tests
unlink("./tests/testing_grounds", recursive = TRUE)
