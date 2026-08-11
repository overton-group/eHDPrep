test_that("nums_to_NA() warns when `nums_to_replace` is missing", {
  expect_warning(nums_to_NA(example_data), "`nums_to_replace` is missing and no values will be replaced with NA")
})

test_that("nums_to_NA() makes no changes when `nums_to_replace` is missing", {
  expect_warning(expect_equal(nums_to_NA(example_data), example_data))
})

test_that("nums_to_NA() support ranges in `nums_to_replace`", {
  expect_equal(sum(is.na(nums_to_NA(example_data, nums_to_replace = 1:3)$patient_id)) , 3)
})

test_that("encode_binary_cats() makes no changes when `values` is missing", {
  expect_warning(expect_equal(encode_binary_cats(example_data), example_data))
})

test_that("encode_ordinals() makes no changes when `values` is missing", {
  expect_warning(expect_equal(encode_ordinals(example_data), example_data))
})

test_that("encode_genotypes() makes no changes when `...` is missing", {
  expect_warning(expect_equal(encode_genotypes(example_data), example_data))
})

test_that("encode_cats() makes no changes when `...` is missing", {
  expect_warning(expect_equal(encode_cats(example_data), example_data))
})

test_that("encode_as_num_mat() throws error when columns can't be converted to numeric", {
  expect_error(encode_as_num_mat(example_data, patient_id))
})

#### coerce_numeric_vars() ####

test_that("coerce_numeric_vars() converts character variables to numeric", {
  df <- data.frame(x = c("1", "2.5", "3"), y = c("a", "b", "c"),
                   stringsAsFactors = FALSE)
  res <- coerce_numeric_vars(df, x)

  expect_type(res$x, "double")
  expect_equal(res$x, c(1, 2.5, 3))
  # unselected variables must be left alone
  expect_identical(res$y, df$y)
})

test_that("coerce_numeric_vars() makes no changes when `...` is missing", {
  df <- data.frame(x = c("1", "2"), stringsAsFactors = FALSE)
  expect_identical(coerce_numeric_vars(df), df)
})

test_that("coerce_numeric_vars() leaves already-numeric variables untouched", {
  # integer columns must not be silently promoted to double
  df <- data.frame(x = c(1L, 2L, NA))
  expect_identical(coerce_numeric_vars(df, x)$x, df$x)
})

test_that("coerce_numeric_vars() reads factor labels, not factor level indices", {
  df <- data.frame(x = factor(c("10", "20", "abc")))
  expect_message(res <- coerce_numeric_vars(df, x), "coerced to NA")
  # as.numeric() on a factor would return 1, 2, 3 (the level indices)
  expect_equal(res$x, c(10, 20, NA))
})

test_that("coerce_numeric_vars() coerces unparseable values to NA and reports them", {
  df <- data.frame(x = c("1", "2", "abc", "unknown"), stringsAsFactors = FALSE)
  expect_message(res <- coerce_numeric_vars(df, x),
                 "2 value\\(s\\) coerced to NA")
  expect_equal(res$x, c(1, 2, NA, NA))
})

test_that("coerce_numeric_vars() names the offending values in its message", {
  df <- data.frame(x = c("1", "abc"), stringsAsFactors = FALSE)
  expect_message(coerce_numeric_vars(df, x), '"abc"')
})

test_that("coerce_numeric_vars() does not count pre-existing NAs as coerced", {
  df <- data.frame(x = c("1", NA, "abc"), stringsAsFactors = FALSE)
  expect_message(res <- coerce_numeric_vars(df, x),
                 "1 value\\(s\\) coerced to NA")
  expect_equal(res$x, c(1, NA, NA))
})

test_that("coerce_numeric_vars() is silent when every value parses", {
  df <- data.frame(x = c("1", "2"), stringsAsFactors = FALSE)
  expect_silent(coerce_numeric_vars(df, x))
})

test_that("coerce_numeric_vars() caps the reported values at 10 uniques", {
  df <- data.frame(x = paste0("v", 1:12), stringsAsFactors = FALSE)
  expect_message(coerce_numeric_vars(df, x), "\\(\\+2 more\\)")
})

test_that("coerce_numeric_vars() counts occurrences, not unique values", {
  # "abc" appears three times: the count is 3, the preview lists it once
  df <- data.frame(x = c("abc", "abc", "abc", "1"), stringsAsFactors = FALSE)
  expect_message(coerce_numeric_vars(df, x), "3 value\\(s\\) coerced to NA")
})

test_that("coerce_numeric_vars() accepts multiple variables and tidyselect helpers", {
  df <- data.frame(a = c("1", "x"), b = c("2", "3"), z = c("4", "5"),
                   stringsAsFactors = FALSE)
  res <- suppressMessages(coerce_numeric_vars(df, dplyr::all_of(c("a", "b"))))

  expect_equal(res$a, c(1, NA))
  expect_equal(res$b, c(2, 3))
  expect_identical(res$z, df$z)
})

test_that("coerce_numeric_vars() preserves variable order and tibble class", {
  df <- tibble::tibble(a = c("1", "2"), b = c("x", "y"), z = c("3", "4"))
  res <- suppressMessages(coerce_numeric_vars(df, z))

  expect_named(res, c("a", "b", "z"))
  expect_s3_class(res, "tbl_df")
})