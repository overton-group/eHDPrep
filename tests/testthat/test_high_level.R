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