###Setup ####


data(example_data)

# create a consistency table
# below states: if a patient has a type of diabetes, they should have diabetes
ct <- tibble::tribble(
  ~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
  "diabetes_type", "diabetes", NA, "Type I", "Yes",
  "diabetes_type", "diabetes", NA, "Type II", "Yes"
)

### Tests ####

test_that("assess_completeness, and its elements, are correct type/class", {

  data_completeness <- assess_completeness(example_data, patient_id, plot = FALSE)

  expect_type(data_completeness, "list")
  expect_s3_class(data_completeness$variable_completeness, "data.frame")
  expect_s3_class(data_completeness$row_completeness, "data.frame")
  expect_s3_class(data_completeness$completeness_plot, "ggplot")
  expect_s3_class(data_completeness$completeness_heatmap, "pheatmap")
})

test_that("assess_quality works with `consis_tbl`", {

  dq_consis <- assess_quality(example_data, patient_id, ct)

  expect_s3_class(dq_consis$internal_inconsistency, "data.frame")
})

test_that("assess_quality works without `consis_tbl`", {

  dq <- assess_quality(example_data, patient_id)

  expect_vector(dq$internal_inconsistency, ptype = character(), size = 1)
})

#### apply_quality_ctrl(impute = ) ####

# Minimal dataset for exercising the imputation branch of apply_quality_ctrl().
#
# `size` is deliberately stored as character with a non-numeric missingness
# marker, and `grp` carries a marker that only strings_to_NA() recognises. This
# makes the pipeline ordering (coerce_numeric_vars -> strings_to_NA -> impute)
# observable in the result: if imputation ran before either step, "unknown"
# would still be present as a value and would skew the summary statistics.
#
# `size` values are chosen so median (2) and mean (3) differ, letting
# `impute_method` be verified rather than merely accepted.
imp_data <- tibble::tibble(
  pid  = 1:6,
  size = c("1", "2", "6", "unknown", "unknown", NA),
  grp  = c("a", "a", "b", "c", "unknown", NA)
)

imp_ct <- tibble::tribble(
  ~"var",  ~"datatype",
  "pid",   "id",
  "size",  "numeric",
  "grp",   "factor"
)

# `grp` is one-hot encoded by encode_cats(), so an unimputed NA surfaces as a
# `grp_NA` column. Its presence/absence is the cleanest signal of whether
# imputation ran.
has_col <- function(x, nm) nm %in% names(x)

test_that("apply_quality_ctrl() does not impute by default", {
  res <- suppressMessages(apply_quality_ctrl(imp_data, pid, imp_ct))

  expect_true(anyNA(res$size))
  expect_true(has_col(res, "grp_NA"))
})

test_that("apply_quality_ctrl() imputes all eligible variables when impute = TRUE", {
  res <- suppressMessages(apply_quality_ctrl(imp_data, pid, imp_ct,
                                             impute = TRUE))

  expect_false(anyNA(res$size))
  expect_false(has_col(res, "grp_NA"))
  expect_equal(sum(is.na(res)), 0)
})

test_that("apply_quality_ctrl() imputes after numeric coercion", {
  res <- suppressMessages(apply_quality_ctrl(imp_data, pid, imp_ct,
                                             impute = TRUE))

  # "unknown" -> NA via coerce_numeric_vars(), so the median is of c(1, 2, 6)
  expect_equal(res$size[match(c(4, 5, 6), res$pid)], rep(2, 3))
  # observed values are untouched
  expect_equal(res$size[match(c(1, 2, 3), res$pid)], c(1, 2, 6))
})

test_that("apply_quality_ctrl() imputes after missing values are encoded", {
  res <- suppressMessages(apply_quality_ctrl(imp_data, pid, imp_ct,
                                             impute = TRUE))

  # "unknown" must not survive as a category of its own ...
  expect_false(has_col(res, "grp_unknown"))
  # ... nor be counted when deriving the mode. grp is c(a,a,b,c,NA,NA) at the
  # point of imputation, so the mode is "a" and rows 5 and 6 both become "a".
  expect_equal(sum(res$grp_a), 4)
  expect_equal(sum(res$grp_b), 1)
  expect_equal(sum(res$grp_c), 1)
})

test_that("apply_quality_ctrl() forwards impute_method", {
  res <- suppressMessages(apply_quality_ctrl(imp_data, pid, imp_ct,
                                             impute = TRUE,
                                             impute_method = "mean"))

  # mean(c(1, 2, 6)) == 3, vs. median == 2
  expect_equal(res$size[match(c(4, 5, 6), res$pid)], rep(3, 3))
})

test_that("apply_quality_ctrl() falls back to the mode for non-numeric variables", {
  # `grp` cannot take a mean; the message from impute_missing_values() should
  # surface through apply_quality_ctrl() rather than being swallowed.
  # Several messages are emitted (coercion, per-variable imputation), so match
  # against all of them rather than relying on message order.
  msgs <- capture_messages(
    apply_quality_ctrl(imp_data, pid, imp_ct, impute = TRUE,
                       impute_method = "mean"))

  expect_true(any(grepl("not numeric", msgs)))
  expect_true(any(grepl("`grp`", msgs, fixed = TRUE)))
})

test_that("apply_quality_ctrl() rejects an unknown impute_method", {
  expect_error(suppressMessages(
    apply_quality_ctrl(imp_data, pid, imp_ct, impute = TRUE,
                       impute_method = "nonsense")))
})

test_that("apply_quality_ctrl() supports knn imputation", {
  skip_if_not_installed("VIM")
  res <- suppressMessages(apply_quality_ctrl(imp_data, pid, imp_ct,
                                             impute = TRUE,
                                             impute_method = "knn"))

  expect_equal(sum(is.na(res)), 0)
  expect_false(has_col(res, "grp_NA"))
  # the identifier is excluded from the distance calculation but preserved
  expect_equal(sort(res$pid), 1:6)
})

test_that("apply_quality_ctrl() cannot use the constant method", {
  # apply_quality_ctrl() exposes no `constant` argument to pass through
  expect_error(suppressMessages(
    apply_quality_ctrl(imp_data, pid, imp_ct, impute = TRUE,
                       impute_method = "constant")),
    "must be supplied")
})