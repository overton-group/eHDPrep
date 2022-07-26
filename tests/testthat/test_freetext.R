sg <- skipgram_identify(x = example_data$free_text,
                  ids = example_data$patient_id,
                  max_interrupt_words = 2)

example_data_NAs <- example_data
example_data_NAs[1:5,"free_text"] <- NA

sg_frq <- skipgram_freq(sg, min_freq = 0.5)


test_that("skipgram_identify() accepts NA values in `x` (with warning)", {
  expect_warning(skipgram_identify(x = example_data_NAs$free_text,
                                 ids = example_data$patient_id,
                                 max_interrupt_words = 2),
                 "NA is replaced by empty string")
})

test_that("skipgram_identify() rejects numeric x", {
  expect_error(skipgram_identify(x = example_data$tumoursize,
                                 ids = example_data$patient_id,
                                 max_interrupt_words = 2))
})

test_that("skipgram_identify() returns desired object with example_data", {
  expect_s3_class(sg, "data.frame")
  expect_equal(nrow(sg), 1000)
  expect_equal(ncol(sg), 1317) 
})

test_that("skipgram_freq() returns desired object with example_data", {
  expect_s3_class(sg_frq, "data.frame")
  expect_equal(nrow(sg_frq), 40)
  expect_equal(ncol(sg_frq), 3)
})