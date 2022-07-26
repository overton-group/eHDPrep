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
