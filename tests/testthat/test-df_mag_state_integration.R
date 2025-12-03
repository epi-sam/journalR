# Integration tests for df_mag state management
# Tests the full pipeline from format_journal_clu() through fround_count_rate() to state updates

# ---- Integration Tests -----------------------------------------------------------------

test_that("format_journal_clu properly manages state lifecycle", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # State should not be active before call
  expect_false(is_df_mag_active())

  # Test with count data that will exercise the state system
  result <- format_journal_clu(
    central = c(1e6, 2e9),
    lower = c(0.9e6, 1.8e9),
    upper = c(1.1e6, 2.2e9),
    d_type = "count",
    style_name = "nature"
  )

  # State should not be active after call (flushed via on.exit)
  expect_false(is_df_mag_active())

  # Results should be correctly formatted
  expect_length(result, 2)
  expect_type(result, "character")

  # Check that magnitude formatting was applied
  # First value should be in millions
  expect_match(result[1], "million")
  # Second value should be in billions
  expect_match(result[2], "billion")

  # Verify no side effects on package state
  expect_false(is_df_mag_active())
  expect_null(get_df_mag_state())
})

test_that("state survives errors and gets properly cleaned up", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Simulate an error during format_journal_clu processing
  # by providing invalid data that will trigger an error after state init
  expect_error({
    format_journal_clu(
      central = c(1, 2, 3),
      lower = c(2, 3, 4),    # Invalid: lower > central
      upper = c(3, 4, 5),
      d_type = "count",
      style_name = "nature"
    )
  })

  # Even after the error, state should be cleaned up due to on.exit()
  expect_false(is_df_mag_active())
  expect_null(get_df_mag_state())
})

test_that("magnitude edge case detection works in full pipeline", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Test data that will trigger magnitude boundary crossing during rounding
  # 999,999 should round to 1,000,000 and become "1.00 million" not "1,000,000"

  # First, verify the initial magnitude classification before rounding
  # 999,999 should initially be classified as thousands (no magnitude label)
  initial_mag <- set_magnitude(999999, d_type = "count", count_label_thousands = FALSE)
  expect_equal(initial_mag$mag, "")        # No magnitude initially
  expect_equal(initial_mag$mag_label, "") # No label initially
  expect_equal(initial_mag$denom, 1)      # No scaling initially

  result <- format_journal_clu(
    central = c(999999, 1234567),
    lower = c(950000, 1100000),
    upper = c(1050000, 1400000),
    d_type = "count",
    style_name = "nature"
  )

  expect_length(result, 2)

  # First value should cross boundary to millions
  expect_match(result[1], "million")
  expect_false(grepl("1,000,000", result[1]))  # Should not show raw number

  # Second value should also be in millions
  expect_match(result[2], "million")

  # State should be cleaned up
  expect_false(is_df_mag_active())
})

test_that("state isolation between different format_journal_clu calls", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # First call with millions
  result1 <- format_journal_clu(
    central = c(5e6),
    lower = c(4e6),
    upper = c(6e6),
    d_type = "count",
    style_name = "nature"
  )

  expect_false(is_df_mag_active())  # Should be clean after first call

  # Second call with billions - should not be affected by first call
  result2 <- format_journal_clu(
    central = c(5e9),
    lower = c(4e9),
    upper = c(6e9),
    d_type = "count",
    style_name = "nature"
  )

  expect_false(is_df_mag_active())  # Should be clean after second call

  # Results should have different magnitude labels
  expect_match(result1[1], "million")
  expect_match(result2[1], "billion")

  # No cross-contamination
  expect_false(grepl("billion", result1[1]))
  expect_false(grepl("million", result2[1]))
})

test_that("mixed magnitude data is handled correctly in single call", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Test data with different magnitudes in the same call
  # This exercises the state system with varying magnitude requirements
  result <- format_journal_clu(
    central    = c(1234, 1.5e6, 2.3e9, 456),
    lower      = c(1100, 1.3e6, 2.0e9, 400),
    upper      = c(1400, 1.7e6, 2.6e9, 500),
    d_type     = "count",
    style_name = "nature"
  )

  expect_length(result, 4)

  # First and fourth should be raw numbers (no magnitude)
  expect_false(grepl("million|billion|thousand", result[1]))
  expect_false(grepl("million|billion|thousand", result[4]))

  # Second should be millions
  expect_match(result[2], "million")

  # Third should be billions
  expect_match(result[3], "billion")

  # State should be cleaned up
  expect_false(is_df_mag_active())
})

test_that("backwards compatibility: fround_count works standalone", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Test that fround_count_rate can still be called directly (standalone mode)
  # without active state management
  result <- fround_count_rate(
    clu = c(1.5e6, 1.3e6, 1.7e6),
    style_name = "nature",
    idx = NULL,  # NULL idx should trigger standalone mode
    d_type = "count"
  )

  expect_length(result, 3)
  expect_type(result, "character")

  # Should format correctly in standalone mode
  expect_true(all(nchar(result) > 0))

  # State should remain inactive
  expect_false(is_df_mag_active())
})

test_that("thread safety guards are in place", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # This test verifies the data.table thread safety implementation exists
  # We can't easily test the actual thread behavior, but we can verify
  # the function works correctly and doesn't error with data.table loaded

  skip_if_not_installed("data.table")

  # Test with data.table loaded (which should trigger thread safety guards)
  result <- format_journal_clu(
    central = c(1e6, 2e6),
    lower = c(0.9e6, 1.8e6),
    upper = c(1.1e6, 2.2e6),
    d_type = "count",
    style_name = "nature"
  )

  expect_length(result, 2)
  expect_match(result[1], "million")
  expect_match(result[2], "million")

  # State should be cleaned up
  expect_false(is_df_mag_active())

  # If data.table is available, verify getDTthreads still works
  # (ensuring our thread management didn't break anything)
  if (requireNamespace("data.table", quietly = TRUE)) {
    threads <- data.table::getDTthreads()
    expect_type(threads, "integer")
    expect_gte(threads, 1)
  }
})
