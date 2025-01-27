library(testthat)
library(indexr)
library(digest)

context("Tests for generate_hash function")

testthat::test_that("generate_hash produces consistent results", {
  args_list <- list(param1 = "value1", param2 = 100, param3 = NA)
  hash1 <- generate_hash(args_list, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE)
  hash2 <- generate_hash(args_list, hash_includes_timestamp = FALSE, ignore_na = TRUE, alphabetical_order = TRUE)

  testthat::expect_equal(hash1, hash2)
})

testthat::test_that("generate_hash handles NAs appropriately", {
  args_with_na <- list(param1 = "value1", param2 = NA)
  args_without_na <- list(param1 = "value1")

  hash_with_na <- generate_hash(args_with_na, ignore_na = TRUE)
  hash_without_na <- generate_hash(args_without_na, ignore_na = TRUE)

  testthat::expect_equal(hash_with_na, hash_without_na)
})

testthat::test_that("generate_hash respects alphabetical_order", {
  args_unordered <- list(b = 1, a = 2)
  args_ordered <- list(a = 2, b = 1)

  hash_unordered <- generate_hash(args_unordered, alphabetical_order = TRUE)
  hash_ordered <- generate_hash(args_ordered, alphabetical_order = TRUE)

  testthat::expect_equal(hash_unordered, hash_ordered)
})
