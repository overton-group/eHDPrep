data(example_data)

write_class_tbl <- function(tbl) {
  path <- tempfile(fileext = ".csv")
  suppressMessages(readr::write_csv(tbl, path))
  path
}

test_that("import_var_classes() accepts the precoded ordinal datatypes", {
  # these are consumed by apply_quality_ctrl() and documented in the vignette,
  # so they must survive the round trip through a manually edited csv
  path <- write_class_tbl(tibble::tribble(
    ~var,        ~datatype,
    "t_stage",   "ordinal_tstage",
    "n_stage",   "ordinal_nstage"
  ))
  res <- suppressMessages(import_var_classes(path))
  expect_equal(res$datatype, c("ordinal_tstage", "ordinal_nstage"))
})

test_that("import_var_classes() accepts every permitted datatype", {
  types <- c("id", "numeric", "double", "integer", "character", "factor",
             "binary", "ordinal", "ordinal_tstage", "ordinal_nstage",
             "genotype", "freetext", "logical")
  path <- write_class_tbl(tibble::tibble(var = paste0("v", seq_along(types)),
                                        datatype = types))
  expect_equal(suppressMessages(import_var_classes(path))$datatype, types)
})

test_that("import_var_classes() still rejects unknown datatypes", {
  path <- write_class_tbl(tibble::tibble(var = "a", datatype = "nonsense"))
  expect_error(suppressMessages(import_var_classes(path)),
               "not a permitted datatype")
})

test_that("the documented class_tbl workflow runs end to end", {
  # mirrors the vignette: assume classes, amend externally, import, apply QC
  path <- write_class_tbl(tibble::tribble(
    ~var,             ~datatype,
    "patient_id",     "id",
    "tumoursize",     "numeric",
    "t_stage",        "ordinal_tstage",
    "n_stage",        "ordinal_nstage",
    "diabetes",       "factor",
    "diabetes_type",  "ordinal",
    "hypertension",   "binary",
    "rural_urban",    "binary",
    "marital_status", "factor",
    "SNP_a",          "genotype",
    "SNP_b",          "genotype",
    "free_text",      "freetext"
  ))
  class_tbl <- suppressMessages(import_var_classes(path))
  res <- suppressWarnings(suppressMessages(
    apply_quality_ctrl(example_data, patient_id, class_tbl,
                       bin_cats = c("No" = "Yes", "rural" = "urban"),
                       min_freq = 0.6)))

  # precoded ordinals become ordered factors
  expect_true(is.ordered(res$t_stage))
  expect_equal(levels(res$t_stage), c("T1", "T2", "T3a", "T3b", "T4"))

  # binary variables become two-level factors
  expect_equal(levels(res$hypertension), c("No", "Yes"))
  expect_equal(levels(res$rural_urban), c("rural", "urban"))

  # `ordinal` levels are not precoded, so the variable is left unmodified
  expect_true(is.character(res$diabetes_type))
  expect_setequal(unique(stats::na.omit(res$diabetes_type)),
                  c("Type I", "Type II"))
})

test_that("assume_var_classes() labels two-value variables as binary", {
  df <- data.frame(two = c("a", "b", "a"), three = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".csv")
  suppressMessages(assume_var_classes(df, path))
  res <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  expect_equal(res$datatype[res$var == "two"], "binary")
  expect_equal(res$datatype[res$var == "three"], "factor")
})

test_that("assume_var_classes() factor detection can be disabled", {
  df <- data.frame(three = c("x", "y", "z"), stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".csv")
  suppressMessages(assume_var_classes(df, path, factor_threshold = 0))
  res <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  expect_equal(res$datatype[res$var == "three"], "character")
})
