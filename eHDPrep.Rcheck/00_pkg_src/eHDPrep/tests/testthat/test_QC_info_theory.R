tbl <- tibble(a = 1:10, b = rep(1,10), c = rep(c(1,NA),5))

data(example_data)
require(dplyr)
require(magrittr)
example_data %>%
  mutate(diabetes_merged = coalesce(diabetes_type, diabetes)) %>%
  select(starts_with("diabetes")) ->
  merged_data

example_data %>%
  strings_to_NA(diabetes, strings_to_replace = "missing") %>%
  mutate(diabetes_merged = coalesce(diabetes_type, diabetes)) %>%
  select(starts_with("diabetes")) ->
  merged_data2

example_data %>%
  strings_to_NA(diabetes, strings_to_replace = "missing") %>%
  mutate(diabetes = if_else(diabetes_type == "Type I" | diabetes_type == "Type II", "Yes", diabetes, missing = diabetes)) %>%
  mutate(diabetes_merged = coalesce(diabetes_type, diabetes)) %>%
  select(starts_with("diabetes")) ->
  merged_data3

test_that("zero_entropy_variables() returns names of variables with zero entropy (including those with NAs)", {
  expect_length(zero_entropy_variables(tbl), 2)
})

test_that("information_content_discrete() ignores NA values", {
  expect_equal(information_content_discrete(tbl$b), information_content_discrete(tbl$c))
})

# not sure if this meant to be equal.
# test_that("information_content_contin() ignores NA values", {
#   expect_equal(information_content_contin(tbl$b), information_content_contin(tbl$c))
# })

test_that("NAs lower MI in mi_content_discrete()", {
  expect_lt(mi_content_discrete(seq(1,5),c(1,NA,3,4,5)), mi_content_discrete(seq(1,5),c(1,2,3,4,5)))
})

test_that("Differences lower MI in mi_content_discrete", {
  expect_lt(mi_content_discrete(seq(1,5),c(1,3,3,4,5)), mi_content_discrete(seq(1,5),c(1,2,3,4,5)))
})

test_that("compare_info_content() gives MI == IC of inputs when all inputs same variable", {
  expect_setequal(round(as.numeric(compare_info_content(merged_data$diabetes,merged_data$diabetes,merged_data$diabetes)[,1, drop = T]),2), 1070.75)
})

test_that("compare_info_content() gives higher IC for merged variable than either input", {
  expect_gt(as.numeric(compare_info_content(merged_data$diabetes,merged_data$diabetes_type,merged_data$diabetes_merged)[,1, drop = T])[3],
            as.numeric(compare_info_content(merged_data$diabetes,merged_data$diabetes_type,merged_data$diabetes_merged)[,1, drop = T])[2])
  expect_gt(as.numeric(compare_info_content(merged_data$diabetes,merged_data$diabetes_type,merged_data$diabetes_merged)[,1, drop = T])[3],
            as.numeric(compare_info_content(merged_data$diabetes,merged_data$diabetes_type,merged_data$diabetes_merged)[,1, drop = T])[1]
  )
})

test_that("compare_info_content() gives higher MI for merged variable than input1 (due to internal inconsistency leading to info loss)", {
  expect_gt(as.numeric(compare_info_content(merged_data2$diabetes,merged_data2$diabetes_type,merged_data2$diabetes_merged)[,1, drop = T])[1],
            as.numeric(compare_info_content(merged_data2$diabetes,merged_data2$diabetes_type,merged_data2$diabetes_merged)[,1, drop = T])[4])
})

test_that("compare_info_content() gives equal MI for input variables with their IC (when NAs cleaned and no internal inconsistiencies)", {
  expect_equal(as.numeric(compare_info_content(merged_data3$diabetes,merged_data3$diabetes_type,merged_data3$diabetes_merged)[,1, drop = T])[1],
            as.numeric(compare_info_content(merged_data3$diabetes,merged_data3$diabetes_type,merged_data3$diabetes_merged)[,1, drop = T])[4])
})