devtools::load_all()
test_dir <- "./tests/incremental_save"
unlink(test_dir, recursive = TRUE)
dir.create(test_dir)

params <- list(a = "1", b = "2")

for (i in 1:10) {
  save_objects(test_dir, data.frame(idx = i, val = rnorm(1)), params, incremental = TRUE)
}

tmp_dir <- file.path(test_dir, generate_hash(params)$hash)
testthat::expect_equal(length(list.files(tmp_dir)), 20)

compress_incremental(test_dir, params)
testthat::expect_error(check_is_directory(tmp_dir))
testthat::expect_equal(length(list.files(test_dir)), 2)
