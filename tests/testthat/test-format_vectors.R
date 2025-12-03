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

test_that("get_or_compute_df_mag_row returns valid df_mag in standalone mode", {
  result <- get_or_compute_df_mag_row(c(123456, 100000, 150000), "count", NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("mag", "mag_label", "denom") %in% names(result)))
})

test_that("handle_rounding_magnitude_crossover detects boundary crossing", {
  # Start with value near boundary
  df_mag_init <- set_magnitude(999999, d_type = "count", verbose = FALSE)
  # After rounding with sigfig=2, 999999 -> 1000000 (crosses to millions)
  init_df_mag_state(n = 1)
  result <- handle_rounding_magnitude_crossover(
    central_val = 999999,
    df_mag = df_mag_init,
    method = "sigfig",
    sigfig = 2,
    nsmall = 1,
    round_5_up = TRUE,
    d_type = "count",
    idx = 1,
    count_label_thousands = FALSE
  )
  flush_df_mag_state()
  # Should update to million magnitude
  expect_equal(result$mag, "m")
})

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
    idx = NULL,
    d_type = "count"
  )
  expect_length(result, 3)
  expect_type(result, "character")
})

test_that("fround_count_rate handles rates correctly", {
  result <- fround_count_rate(
    clu = c(0.0000123, 0.00001, 0.000015),
    style_name = "nature",
    idx = NULL,
    d_type = "rate"
  )
  expect_length(result, 3)
  expect_type(result, "character")
})


