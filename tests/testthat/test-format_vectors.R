# ---- Integration - Style-Agnostic -----------------------------------------------------------------

test_that("fround works", {
   expect_equal(fround(0.123456789), "0.1")
   expect_equal(fround(0.123456789, digits = 3), "0.123")
   expect_equal(fround(0.123456789, digits = 3, nsmall = 4), "0.1230")
})

test_that("fround_dtype works", {
   expect_equal(fround_dtype_lancet(0.123456789), "0·1%")
   expect_equal(fround_dtype_lancet(0.123456789, d_type = "pp"), "0·1 pp")
   expect_equal(fround_dtype_lancet(0.123456789, d_type = "count", digits = 3, nsmall = 4), "0·1230")
})

test_that("fmt_magnitude works", {
   #' fmt_magnitude(123456789) # "123.5 million"
   expect_equal(fmt_magnitude(123456789, 'count'), "123.5 million")
})

# ---- Integration Tests for Helper Functions -----------------------------------------------------
# Added 2025-12-03: Integration tests for helper functions from fround_count_rate refactoring

test_that("apply_sigfig_zero_padding pads correctly", {
  result <- apply_sigfig_zero_padding("1.2", sigfig = 3, decimal.mark = ".")
  expect_equal(result, "1.20")
})

test_that("format_decimal produces correct format", {
  result <- format_decimal(x_sc = 123.456, nsmall = 2, decimal.mark = ".", big.mark = ",")
  expect_match(result, "^123\\.46$")
})

test_that("format_int produces integer format", {
  result <- format_int(x_sc = 123.789, decimal.mark = ".", big.mark = ",")
  expect_match(result, "^124$")
})

test_that("fround_count_rate handles counts correctly", {
  result <- fround_count_rate(
    clu = c(12345, 10000, 15000),
    style_name = "nature",
    d_type = "count"
  )

  # Should return list structure
  expect_type(result, "list")
  expect_named(result, c("formatted", "df_mag_row"))

  # formatted should be chr[3]
  expect_length(result$formatted, 3)
  expect_type(result$formatted, "character")

  # df_mag_row should be data.frame[1,]
  expect_s3_class(result$df_mag_row, "data.frame")
  expect_equal(nrow(result$df_mag_row), 1)
  expect_named(result$df_mag_row, c("mag", "mag_label", "denom"))
})

test_that("fround_count_rate handles rates correctly", {
  result <- fround_count_rate(
    clu = c(0.0000123, 0.00001, 0.000015),
    style_name = "nature",
    d_type = "rate"
  )

  # Should return list structure
  expect_type(result, "list")
  expect_named(result, c("formatted", "df_mag_row"))

  # formatted should be chr[3]
  expect_length(result$formatted, 3)
  expect_type(result$formatted, "character")

  # df_mag_row should be data.frame[1,]
  expect_s3_class(result$df_mag_row, "data.frame")
  expect_equal(nrow(result$df_mag_row), 1)
  expect_named(result$df_mag_row, c("mag", "mag_label", "denom"))
})

# ---- Validation Function Tests -------------------------------------------------

test_that("assert_fround_return_schema validates correct structure", {

  # Valid structure should pass silently
  valid_result <- list(
    formatted = c("1.0", "0.8", "1.2"),
    df_mag_row = data.frame(
      mag = "",
      mag_label = "",
      denom = 1,
      stringsAsFactors = FALSE
    )
  )

  expect_silent(assert_fround_return_schema(valid_result, "test_function"))
})

test_that("assert_fround_return_schema catches invalid structures", {

  # Not a list
  expect_error(
    assert_fround_return_schema("invalid", "test"),
    "test must return a list"
  )

  # Missing required names
  incomplete_result <- list(formatted = c("1", "2", "3"))
  expect_error(
    assert_fround_return_schema(incomplete_result, "test"),
    "test must return list with names: formatted, df_mag_row"
  )

  # Wrong formatted type/length
  wrong_formatted <- list(
    formatted = c("1", "2"), # length 2 instead of 3
    df_mag_row = data.frame(mag = "", mag_label = "", denom = 1)
  )
  expect_error(
    assert_fround_return_schema(wrong_formatted, "test"),
    "test\\$formatted must be character vector of length 3"
  )

  # Wrong df_mag_row structure
  wrong_df <- list(
    formatted = c("1", "2", "3"),
    df_mag_row = data.frame(wrong_col = 1) # missing required columns
  )
  expect_error(
    assert_fround_return_schema(wrong_df, "test"),
    "test\\$df_mag_row must have columns: mag, mag_label, denom"
  )
})
