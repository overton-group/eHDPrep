dated <- tibble::tibble(
  id = 1:6,
  dob = as.Date(c("1950-01-01", "1960-06-15", "1970-12-31", NA,
                  "1980-02-29", "1990-07-04")),
  visit = as.POSIXct(c("2020-01-01 10:00", "2020-02-01 11:00",
                       "2020-03-01 12:00", "2020-04-01 13:00",
                       "2020-05-01 14:00", "2020-06-01 15:00"), tz = "UTC"),
  val = c(1, 2, 3, 4, 5, 6))

# detection and import ------------------------------------------------------------------------

test_that("assume_var_classes() handles date-time columns", {
  # class() returns c("POSIXct", "POSIXt") for a date-time, which previously
  # errored as map_chr() requires a result of length 1
  path <- tempfile(fileext = ".csv")
  expect_no_error(suppressMessages(assume_var_classes(dated, path)))
  res <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  expect_equal(res$datatype[res$var == "dob"], "date")
  expect_equal(res$datatype[res$var == "visit"], "date")
})

test_that("import_var_classes() accepts the date datatype", {
  path <- tempfile(fileext = ".csv")
  suppressMessages(readr::write_csv(
    tibble::tibble(var = "dob", datatype = "date"), path))
  expect_equal(suppressMessages(import_var_classes(path))$datatype, "date")
})

test_that("assume_var_classes() output round trips through import_var_classes()", {
  path <- tempfile(fileext = ".csv")
  suppressMessages(assume_var_classes(dated, path))
  expect_no_error(suppressMessages(import_var_classes(path)))
})

# coerce_dates --------------------------------------------------------------------------------

test_that("coerce_dates() parses ISO dates", {
  res <- suppressMessages(
    coerce_dates(tibble::tibble(x = c("2020-01-15", "2021-12-01")), x))
  expect_s3_class(res$x, "Date")
  expect_equal(as.character(res$x), c("2020-01-15", "2021-12-01"))
})

test_that("coerce_dates() parses day-first formats", {
  # strptime ignores trailing characters, so "15/01/2020" would otherwise be
  # read as the year 15 under "%Y/%m/%d"
  res <- suppressMessages(
    coerce_dates(tibble::tibble(x = c("15/01/2020", "01/12/2021")), x))
  expect_equal(as.character(res$x), c("2020-01-15", "2021-12-01"))

  res2 <- suppressMessages(coerce_dates(tibble::tibble(x = "15-01-2020"), x))
  expect_equal(as.character(res2$x), "2020-01-15")
})

test_that("coerce_dates() reports and NAs unparseable values", {
  expect_message(
    coerce_dates(tibble::tibble(x = c("2020-01-15", "not recorded")), x),
    "1 value\\(s\\) coerced to NA")
  res <- suppressMessages(
    coerce_dates(tibble::tibble(x = c("2020-01-15", "2020-02-30", "nope")), x))
  # 2020-02-30 is not a real date
  expect_equal(as.character(res$x), c("2020-01-15", NA, NA))
})

test_that("coerce_dates() leaves existing Date columns untouched", {
  d <- tibble::tibble(x = as.Date(c("2020-01-15", NA)))
  expect_equal(suppressMessages(coerce_dates(d, x)), d)
})

test_that("coerce_dates() converts date-times and reports the loss of time", {
  d <- tibble::tibble(x = as.POSIXct("2020-05-01 14:30", tz = "UTC"))
  expect_message(coerce_dates(d, x), "discarding the time of day")
  res <- suppressMessages(coerce_dates(d, x))
  expect_s3_class(res$x, "Date")
  expect_equal(as.character(res$x), "2020-05-01")
})

test_that("coerce_dates() returns data unchanged when no columns are selected", {
  expect_equal(coerce_dates(dated), dated)
})

# imputation ----------------------------------------------------------------------------------

test_that("dates are imputed with the median, not the mode", {
  d <- tibble::tibble(dob = as.Date(c("1950-01-01", "1960-01-01",
                                      "1970-01-01", "1980-01-01", NA)))
  res <- suppressMessages(impute_missing_values(d, dob))
  expect_s3_class(res$dob, "Date")
  expect_equal(as.character(res$dob[5]), "1965-01-01")
})

test_that("date imputation supports mean and mode", {
  d <- tibble::tibble(dob = as.Date(c("1950-01-01", "1950-01-01",
                                      "1970-01-01", NA)))
  expect_equal(
    as.character(suppressMessages(
      impute_missing_values(d, dob, method = "mode"))$dob[4]),
    "1950-01-01")
  expect_s3_class(
    suppressMessages(impute_missing_values(d, dob, method = "mean"))$dob, "Date")
})

test_that("date-time imputation preserves the class", {
  d <- tibble::tibble(t = as.POSIXct(c("2020-01-01 00:00", "2020-01-03 00:00", NA),
                                     tz = "UTC"))
  res <- suppressMessages(impute_missing_values(d, t))
  expect_s3_class(res$t, "POSIXct")
  expect_false(anyNA(res$t))
})

# high level pipeline -------------------------------------------------------------------------

test_that("apply_quality_ctrl() coerces and preserves date variables", {
  d <- tibble::tibble(
    patient_id = 1:6,
    diagnosis_date = c("2020-01-15", "2020-03-01", "not recorded",
                       "2020-06-30", "2021-02-01", "2021-08-08"),
    tumoursize = c(10, 20, 30, 40, 50, 60),
    sex = c("Male", "Female", "Male", "Female", "Male", "Female"))
  ct <- tibble::tribble(~var, ~datatype,
                        "patient_id", "id",
                        "diagnosis_date", "date",
                        "tumoursize", "numeric",
                        "sex", "binary")
  res <- suppressMessages(
    apply_quality_ctrl(d, patient_id, ct, bin_cats = c("Male" = "Female")))

  # dates are coerced from character but otherwise left unmodified
  expect_s3_class(res$diagnosis_date, "Date")
  expect_true(is.na(res$diagnosis_date[3]))
  expect_equal(as.character(res$diagnosis_date[1]), "2020-01-15")
})

test_that("apply_quality_ctrl() imputes date variables", {
  d <- tibble::tibble(
    patient_id = 1:4,
    dt = as.Date(c("2020-01-01", "2020-01-03", "2020-01-05", NA)),
    sex = c("Male", "Female", "Male", "Female"))
  ct <- tibble::tribble(~var, ~datatype,
                        "patient_id", "id", "dt", "date", "sex", "binary")
  res <- suppressMessages(
    apply_quality_ctrl(d, patient_id, ct, bin_cats = c("Male" = "Female"),
                       impute = TRUE))
  expect_false(anyNA(res$dt))
  expect_s3_class(res$dt, "Date")
})

test_that("apply_quality_ctrl() works when no categorical variables are present", {
  # encode_cats() previously errored when it selected zero columns
  d <- tibble::tibble(patient_id = 1:3, val = c(1, 2, 3))
  ct <- tibble::tribble(~var, ~datatype, "patient_id", "id", "val", "numeric")
  expect_no_error(suppressMessages(apply_quality_ctrl(d, patient_id, ct)))
})

# consistency ---------------------------------------------------------------------------------

test_that("consistency rules compare date variables", {
  d <- tibble::tibble(id = 1:3,
                      dob = as.Date(c("1950-01-01", "1960-01-01", "1970-01-01")),
                      dod = as.Date(c("2020-01-01", "2019-01-01", "1960-01-01")))
  ct <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                        "dob", "dod", "<=", NA, NA)
  res <- suppressWarnings(identify_inconsistency(d, ct, id_var = "id"))
  # row 3 records a death before the birth
  expect_equal(res$id, 3L)
})

test_that("single-variable rules constrain date variables", {
  d <- tibble::tibble(id = 1:3,
                      dt = as.Date(c("2019-06-01", "2020-06-01", "2021-06-01")))
  ct <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                        "dt", NA, ">=", "2020-01-01", NA)
  res <- suppressWarnings(identify_inconsistency(d, ct, id_var = "id"))
  expect_equal(res$id, 1L)
})
