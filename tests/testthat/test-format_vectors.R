# ---- Integration - Style-Agnostic -----------------------------------------------------------------

test_that("fround works", {
   expect_equal(fround(0.123456789), "0.1")
   expect_equal(fround(0.123456789, digits = 3), "0.123")
   expect_equal(fround(0.123456789, digits = 3, nsmall = 4), "0.1230")
})

test_that("fround_metric works", {
   expect_equal(fround_metric_lancet(0.123456789), "0·1%")
   expect_equal(fround_metric_lancet(0.123456789, metric = "pp"), "0·1 pp")
   expect_equal(fround_metric_lancet(0.123456789, metric = "count", digits = 3, nsmall = 4), "0·1230")
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

# ---- add_epsilon Function Tests ------------------------------------------------

test_that("add_epsilon adds epsilon to positive values", {
  x <- c(1.0, 2.5, 10.0)
  epsilon <- 1e-10
  result <- add_epsilon(x, epsilon)
  expect_true(all(result > x))
  expect_equal(result, x + epsilon)
})

test_that("add_epsilon subtracts epsilon from negative values", {
  x <- c(-1.0, -2.5, -10.0)
  epsilon <- 1e-10
  result <- add_epsilon(x, epsilon)
  expect_true(all(result < x))
  expect_equal(result, x - epsilon)
})

test_that("add_epsilon handles mixed positive and negative values", {
  x <- c(-2.0, 1.0, -3.5, 4.2)
  epsilon <- 1e-10
  result <- add_epsilon(x, epsilon = epsilon)

  # Positive values should increase
  expect_equal(result[2], x[2] + epsilon)  # 1.0
  expect_equal(result[4], x[4] + epsilon)  # 4.2

  # Negative values should decrease (become more negative)
  expect_equal(result[1], x[1] - epsilon)  # -2.0
  expect_equal(result[3], x[3] - epsilon)  # -3.5
})

test_that("add_epsilon preserves zeros", {
  x <- c(0.0, 1.0, 0.0, -1.0)
  result <- add_epsilon(x, epsilon = 1e-10)

  # Zeros should remain unchanged
  expect_equal(result[1], 0.0)
  expect_equal(result[3], 0.0)

  # Non-zeros should be modified
  expect_equal(result[2], 1.0 + 1e-10)
  expect_equal(result[4], -1.0 - 1e-10)
})

test_that("add_epsilon warns when epsilon is too large", {
  # Test with very small values where epsilon would be inappropriate
  x <- c(1e-11, 2e-11)  # Very small values

  # Use large epsilon that violates the 3-order-of-magnitude rule
  expect_warning(
    result <- add_epsilon(x, epsilon = 1e-9),
    "epsilon.*must be 3 orders of magnitude smaller"
  )

  # Should set epsilon to 0 and return original values (except for sign handling)
  expect_equal(result[1], x[1])  # Positive values with epsilon=0 unchanged
  expect_equal(result[2], x[2])
})

test_that("add_epsilon works correctly with default epsilon", {
  x <- c(1.0, -1.0, 0.0)
  result <- add_epsilon(x)  # Uses default epsilon = 1e-12

  expect_equal(result[1], 1.0 + 1e-12)
  expect_equal(result[2], -1.0 - 1e-12)
  expect_equal(result[3], 0.0)
})

test_that("add_epsilon handles edge case with all zeros", {
  x <- c(0.0, 0.0, 0.0)
  result <- add_epsilon(x, epsilon = 1e-10)

  # All values should remain unchanged
  expect_equal(result, x)
})

test_that("add_epsilon validates inputs", {
  # Non-numeric x should error
  expect_error(add_epsilon("not numeric"))

  # Non-numeric epsilon should error
  expect_error(add_epsilon(c(1, 2), epsilon = "not numeric"))
})

test_that("add_epsilon magnitude calculation is correct", {
  # Test the magnitude calculation logic with known values
  x <- c(1000, 500)  # Values around 1000
  epsilon <- 1e-6    # Should be fine (3+ orders smaller than ~1000)

  # Should not warn and should apply epsilon
  expect_silent(result <- add_epsilon(x, epsilon = epsilon))
  expect_equal(result, x + epsilon)
})

test_that("add_epsilon works with rate-scale values", {
  # Test with rate-scale values (very small numbers)
  x <- c(1.1e-11, 0.9e-11, 1.2e-11)  # ~10 per 1 billion scale
  epsilon <- 1e-14  # Appropriate for this scale

  expect_silent(result <- add_epsilon(x, epsilon = epsilon))
  expect_equal(result, x + epsilon)
})

test_that("fround_count_rate handles counts correctly", {
  result <- fround_count_rate(
    clu = c(12345, 10000, 15000),
    style_name = "nature",
    metric = "count"
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
    metric = "rate"
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

# ---- Misc -----------------------------------------------------------------

test_that("Oxford comma formatting is correct for 1,2,3 and 4 element vectors",{
  expect_equal(format_oxford_comma(c("A")), "A")
  expect_equal(format_oxford_comma(c("A", "B")), "A and B")
  expect_equal(format_oxford_comma(c("A", "B", "C")), "A, B, and C")
  expect_equal(format_oxford_comma(c("A", "B", "C", "D")), "A, B, C, and D")
})
