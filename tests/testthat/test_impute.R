test_that("impute_missing_values() imputes numeric columns with the median (auto)", {
  df <- data.frame(x = c(1, 2, 3, 4, NA), y = c("a", "a", "b", NA, NA),
                   stringsAsFactors = FALSE)
  res <- suppressMessages(impute_missing_values(df))
  expect_equal(sum(is.na(res)), 0)
  expect_equal(res$x[5], stats::median(df$x, na.rm = TRUE))
})

test_that("impute_missing_values() imputes non-numeric columns with the mode", {
  df <- data.frame(y = c("a", "a", "b", NA), stringsAsFactors = FALSE)
  res <- suppressMessages(impute_missing_values(df))
  expect_equal(res$y[4], "a")
})

test_that("impute_missing_values() respects column selection", {
  df <- data.frame(x = c(1, NA), y = c(1, NA))
  res <- suppressMessages(impute_missing_values(df, x))
  expect_false(anyNA(res$x))
  expect_true(anyNA(res$y))
})

test_that("impute_missing_values() supports mean and constant methods", {
  df <- data.frame(x = c(2, 4, NA))
  expect_equal(suppressMessages(impute_missing_values(df, method = "mean"))$x[3], 3)

  dfc <- data.frame(y = c("a", NA), stringsAsFactors = FALSE)
  res <- suppressMessages(impute_missing_values(dfc, method = "constant",
                                                constant = "Z"))
  expect_equal(res$y[2], "Z")
})

test_that("impute_missing_values() errors when constant is not supplied", {
  df <- data.frame(x = c(1, NA))
  expect_error(impute_missing_values(df, method = "constant"),
               "must be supplied")
})

test_that("impute_missing_values() falls back to mode for non-numeric mean/median", {
  df <- data.frame(y = c("a", "a", "b", NA), stringsAsFactors = FALSE)
  expect_message(res <- impute_missing_values(df, method = "median"),
                 "not numeric")
  expect_equal(res$y[4], "a")
})

test_that("impute_missing_values() preserves tibble class", {
  df <- tibble::tibble(x = c(1, NA))
  res <- suppressMessages(impute_missing_values(df))
  expect_s3_class(res, "tbl_df")
})

#### variables with no observed values ####

test_that("impute_missing_values() does not claim to impute an all-NA variable", {
  df <- data.frame(x = c(NA, NA))
  msgs <- capture_messages(res <- impute_missing_values(df))

  expect_true(all(is.na(res$x)))
  expect_true(any(grepl("no non-missing values", msgs)))
  # the misleading "n value(s) imputed" message must not be emitted
  expect_false(any(grepl("value\\(s\\) imputed", msgs)))
})

test_that("impute_missing_values() skips all-NA variables but imputes the rest", {
  df <- data.frame(x = c(1, 2, NA), y = c(NA, NA, NA))
  res <- suppressMessages(impute_missing_values(df))

  expect_false(anyNA(res$x))
  expect_true(all(is.na(res$y)))
})

test_that("impute_missing_values() still fills all-NA variables with a constant", {
  # `constant` does not depend on the observed values, so there is nothing to skip
  df <- data.frame(x = c(NA, NA))
  res <- suppressMessages(impute_missing_values(df, method = "constant",
                                                constant = 7))
  expect_equal(res$x, c(7, 7))
})

#### class preservation ####

test_that("impute_missing_values() keeps integer variables integer", {
  df <- data.frame(x = c(1L, 2L, NA))
  res <- suppressMessages(impute_missing_values(df))

  expect_type(res$x, "integer")
  # median(c(1L, 2L)) is 1.5, which an integer variable cannot hold
  expect_equal(res$x, c(1L, 2L, 2L))
})

test_that("impute_missing_values() reports rounding of an integer fill", {
  df <- data.frame(x = c(1L, 2L, NA))
  expect_message(impute_missing_values(df), "rounded to 2")
})

test_that("impute_missing_values() does not round an already-integral fill", {
  df <- data.frame(x = c(1L, 5L, 9L, NA))
  msgs <- capture_messages(res <- impute_missing_values(df))

  expect_type(res$x, "integer")
  expect_equal(res$x[4], 5L)
  expect_false(any(grepl("rounded", msgs)))
})

test_that("impute_missing_values() keeps double variables double", {
  df <- data.frame(x = c(1, 2, NA))
  res <- suppressMessages(impute_missing_values(df))
  expect_type(res$x, "double")
})

test_that("impute_missing_values() keeps factors as factors", {
  df <- data.frame(f = factor(c("a", "a", "b", NA)))
  res <- suppressMessages(impute_missing_values(df))

  expect_s3_class(res$f, "factor")
  expect_equal(as.character(res$f[4]), "a")
  expect_equal(levels(res$f), c("a", "b"))
})

test_that("impute_missing_values() adds an absent constant as a factor level", {
  df <- data.frame(f = factor(c("a", "b", NA)))
  res <- suppressMessages(impute_missing_values(df, method = "constant",
                                                constant = "Missing"))

  expect_s3_class(res$f, "factor")
  # without extending the levels, the assignment would drop back to NA
  expect_false(anyNA(res$f))
  expect_equal(as.character(res$f[3]), "Missing")
  expect_true("Missing" %in% levels(res$f))
})

test_that("impute_missing_values() keeps logical variables logical", {
  df <- data.frame(l = c(TRUE, TRUE, FALSE, NA))
  res <- suppressMessages(impute_missing_values(df))

  expect_type(res$l, "logical")
  expect_true(res$l[4])
})

test_that("impute_missing_values() preserves non-atomic classes such as Date", {
  d <- as.Date(c("2020-01-01", "2020-01-01", "2020-01-03", NA))
  res <- suppressMessages(impute_missing_values(data.frame(d = d)))

  expect_s3_class(res$d, "Date")
  expect_equal(res$d[4], as.Date("2020-01-01"))
})

#### incompatible `constant` ####

test_that("impute_missing_values() rejects a constant that would change the class", {
  df <- data.frame(x = c(1, NA))
  expect_error(suppressMessages(
    impute_missing_values(df, method = "constant", constant = "z")),
    "would change the variable")
})

test_that("impute_missing_values() names the variable in the class-change error", {
  df <- data.frame(ok = c(1, 2), bad = c(1, NA))
  expect_error(suppressMessages(
    impute_missing_values(df, method = "constant", constant = "z")),
    "bad", fixed = TRUE)
})

test_that("impute_missing_values() rejects a fractional constant for an integer variable", {
  df <- data.frame(x = c(1L, NA))
  expect_error(suppressMessages(
    impute_missing_values(df, method = "constant", constant = 2.7)),
    "whole number")
})

test_that("impute_missing_values() accepts a whole-number constant for an integer variable", {
  # `3` is a double literal in R, but is representable as an integer
  df <- data.frame(x = c(1L, NA))
  res <- suppressMessages(impute_missing_values(df, method = "constant",
                                                constant = 3))

  expect_type(res$x, "integer")
  expect_equal(res$x, c(1L, 3L))
})

test_that("impute_missing_values() rejects a multi-value constant", {
  df <- data.frame(x = c(1, NA))
  expect_error(suppressMessages(
    impute_missing_values(df, method = "constant", constant = c(1, 2))),
    "single value")
})

test_that("impute_missing_values() knn requires VIM", {
  skip_if_not_installed("VIM")
  df <- tibble::tibble(id = 1:6,
                       x = c(1, 2, 3, 4, 5, NA),
                       g = c("a", "a", "b", "b", "a", "b"))
  res <- suppressMessages(impute_missing_values(df, x, method = "knn",
                                                ignore = id, k = 2))
  expect_false(anyNA(res$x))
  expect_equal(names(res), names(df))
  expect_s3_class(res, "tbl_df")
})
