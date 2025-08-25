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