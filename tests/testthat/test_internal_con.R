# creating test `data`: ---------------------------------------------------------------------------
beans <- tibble::tibble(red_beans = 1:15,
                        blue_beans = 1:15,
                        total_beans = 1:15*2,
                        red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))

# creating test `consis_tbl`s ----------------------------------------------------------------------

# logical tests only - should all be consistent
bean_rules_lgl_c <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                              "red_beans", "blue_beans", "==", NA, NA,
                              "red_beans", "blue_beans", "<=", NA, NA,
                              "red_beans", "blue_beans", ">=", NA, NA,
                              "red_beans", "total_beans", "<", NA, NA,
                              "total_beans", "blue_beans", ">", NA, NA,
                              "total_beans", "blue_beans", "!=", NA, NA)

# logical tests only - should all be inconsistent
bean_rules_lgl_in <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                               "red_beans", "blue_beans", ">", NA, NA,
                               "red_beans", "blue_beans", "!=", NA, NA)

# logical tests only - should be a mix of consistent and inconsistent
bean_rules_lgl_mix <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                                      "red_beans", "blue_beans", "==", NA, NA,
                                      "red_beans", "blue_beans", "!=", NA, NA)

# range tests

## consistent
bean_rules_range_c <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                                      "red_beans", "blue_beans", NA, "1-9", "few_beans")
## inconsistent
bean_rules_range_in <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                                       "red_beans", "blue_beans", NA, "1-9", "many_beans")

## mixture of consistent and inconsistent
bean_rules_range_mix <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                                        "red_beans", "blue_beans", NA, "1-9", "many_beans",
                                        "red_beans", "blue_beans", NA, "10-15", "many_beans")

  
bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                              "red_beans", "blue_beans", "==", "1-15", NA,
                              "red_beans", "total_beans", "<=",NA,"1-30",
                              "red_beans", "red_bean_summary", NA, "1-9", "few_beans",
                              "red_beans", "red_bean_summary", NA, "10-15", "many_beans")

# creating `consis_tbl` with bad rules
bean_rules_lgl_in <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                              "blue_beans", "red_beans", "==", NA, "1-15",
                              "total_beans","red_beans", "<=","1-30", NA,
                              "red_bean_summary", "red_beans", NA, "few_beans", "1-9",
                              "red_bean_summary", "red_beans", NA, "many_beans", "10-15")

# -------------------------------------------------------------------------------------------------------
test_that("validate_consistency_tbl() rejects wrong number of cols", {
  expect_error(validate_consistency_tbl(beans, dplyr::select(bean_rules,-5)),
               "✖ `consis_tbl` must have five columns. You have supplied a `consis_tbl` with 4 columns.")
  
  expect_error(validate_consistency_tbl(beans, dplyr::mutate(bean_rules, sixth_col = NA)),
               "✖ `consis_tbl` must have five columns. You have supplied a `consis_tbl` with 6 columns.")
})

test_that("identify_inconsistency() returns invisible dataset and desired message when no inconsistencies are found", {
  expect_message(expect_invisible(identify_inconsistency(beans, bean_rules_lgl_c)),"No inconsistencies were found.")
})

test_that("identify_inconsistency() returns tibble when inconsistency found", {
  expect_warning(expect_s3_class(identify_inconsistency(beans, bean_rules_lgl_in), "data.frame"))
  expect_warning(identify_inconsistency(beans, bean_rules_lgl_in),
                 "One or more inconsistencies were identified. They are shown in the returned tibble.")
})

test_that("identify_inconsistency() logical tests correctly finds no inconsistencies", {
  expect_message(identify_inconsistency(beans, bean_rules_lgl_c),
                 "No inconsistencies were found.")
})

test_that("identify_inconsistency() logical tests correctly finds inconsistencies", {
  expect_warning(identify_inconsistency(beans, bean_rules_lgl_in),
                 "One or more inconsistencies were identified. They are shown in the returned tibble.")
})

test_that("identify_inconsistency() logical tests correctly finds inconsistencies and returns only the inconsistencies", {
  expect_warning(identify_inconsistency(beans, bean_rules_lgl_mix),
                 "One or more inconsistencies were identified. They are shown in the returned tibble.")
  expect_equal(nrow(suppressWarnings(identify_inconsistency(beans, bean_rules_lgl_mix))), 15) 
})

test_that("identify_inconsistency() range/boundaries tests work as desired", {
  expect_message(identify_inconsistency(beans, bean_rules_lgl_c),
                 "No inconsistencies were found.")
  expect_warning(identify_inconsistency(beans, bean_rules_lgl_in),
                 "One or more inconsistencies were identified. They are shown in the returned tibble.")
})

test_that("identify_inconsistency() range/boundaries tests correctly find no inconsistency", {
  expect_message(identify_inconsistency(beans, bean_rules_range_c),
                 "No inconsistencies were found.")
})

# single-variable checks ---------------------------------------------------------------------------

# `data` with values that fall outside plausible boundaries
patients <- tibble::tibble(id = 1:6,
                           age = c(65, 72, 80, 45, 90, NA),
                           sex = c("Male", "Female", "Male", "Male", "Male", "Male"))

test_that("validate_consistency_tbl() accepts single-variable rules", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", NA, ">=", "70", NA,
                           "age", NA, NA, "0:120", NA,
                           "sex", NA, NA, "Male", NA)
  expect_message(validate_consistency_tbl(patients, rules), "Consistency table is valid.")
})

test_that("single-variable rules require a boundary in column 4", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", NA, ">=", NA, NA)
  expect_error(validate_consistency_tbl(patients, rules), "must have a value in col 4")
})

test_that("single-variable rules must not constrain a second variable", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", NA, NA, "0:120", "Male")
  expect_error(validate_consistency_tbl(patients, rules), "must not have a value in col 5")
})

test_that("a named variable is still required in column 2 when supplied", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", "70", ">=", NA, NA)
  expect_error(validate_consistency_tbl(patients, rules),
               "must be variable names in `data`")
})

test_that("single-variable comparison against a value finds inconsistencies", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", NA, ">=", "70", NA)
  expect_warning(identify_inconsistency(patients, rules, id_var = "id"),
                 "One or more inconsistencies were identified")
  res <- suppressWarnings(identify_inconsistency(patients, rules, id_var = "id"))
  # 65 and 45 are below 70; NA cannot be assessed and is not reported
  expect_equal(res$values_a, c("65", "45"))
  expect_equal(res$id, c(1L, 4L))
  expect_true(all(is.na(res$var_b)))
})

test_that("single-variable numeric range finds inconsistencies", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", NA, NA, "0:70", NA)
  res <- suppressWarnings(identify_inconsistency(patients, rules, id_var = "id"))
  expect_equal(res$values_a, c("72", "80", "90"))
})

test_that("single-variable permitted category finds inconsistencies", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "sex", NA, NA, "Male", NA)
  res <- suppressWarnings(identify_inconsistency(patients, rules, id_var = "id"))
  expect_equal(res$values_a, "Female")
  expect_equal(res$id, 2L)
})

test_that("single-variable rules report nothing when satisfied", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", NA, NA, "0:120", NA)
  expect_message(identify_inconsistency(patients, rules, id_var = "id"),
                 "No inconsistencies were found.")
})

test_that("missing values are not reported by single-variable rules", {
  rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "age", NA, ">", "0", NA)
  # every non-missing age exceeds 0, so only the NA row could be reported
  expect_message(identify_inconsistency(patients, rules, id_var = "id"),
                 "No inconsistencies were found.")
})

test_that("single-variable and two-variable rules can be mixed in one table", {
  mixed <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                           "red_beans", "blue_beans", "==", NA, NA,
                           "red_beans", NA, "<=", "10", NA)
  res <- suppressWarnings(identify_inconsistency(beans, mixed))
  # red_beans == blue_beans holds throughout; 11:15 exceed the limit of 10
  expect_equal(nrow(res), 5)
  expect_equal(res$values_a, as.character(11:15))
  expect_true(all(is.na(res$var_b)))
})
