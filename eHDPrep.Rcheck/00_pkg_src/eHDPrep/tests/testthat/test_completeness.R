test_that("distant_neg_val() returns desired value on mtcars", {
  expect_equal(eHDPrep:::distant_neg_val(mtcars), -944)
})
