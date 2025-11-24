# Tests for df_mag state management functions
# These functions manage state during vectorized count formatting in format_journal_clu()

test_that("init_df_mag_state creates correct structure", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Test initialization
  init_df_mag_state(5)
  expect_true(is_df_mag_active())

  state <- get_df_mag_state()
  expect_s3_class(state, "data.frame")
  expect_equal(nrow(state), 5)
  expect_equal(ncol(state), 3)
  expect_named(state, c("mag", "mag_label", "denom"))

  # All values should be NA initially
  expect_true(all(is.na(state$mag)))
  expect_true(all(is.na(state$mag_label)))
  expect_true(all(is.na(state$denom)))

  # Data types should match set_magnitude() output
  expect_type(state$mag, "character")
  expect_type(state$mag_label, "character")
  expect_type(state$denom, "double")

  # Clean up
  flush_df_mag_state()
})

test_that("init_df_mag_state validates input", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Test invalid inputs
  expect_error(init_df_mag_state(-1), "Element 1 is not >= 1")
  expect_error(init_df_mag_state(0), "Element 1 is not >= 1")
  expect_error(init_df_mag_state(1.5), "Must be of type 'integerish'")
  expect_error(init_df_mag_state(c(1, 2)), "Must have length 1")
  expect_error(init_df_mag_state("5"), "Must be of type 'integerish'")

  # Valid input should work
  expect_silent(init_df_mag_state(3))
  expect_true(is_df_mag_active())

  # Clean up
  flush_df_mag_state()
})

test_that("init_df_mag_state warns on nested calls", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # First call should be silent
  expect_silent(init_df_mag_state(2))

  # Second call should warn
  expect_warning(
    init_df_mag_state(3),
    "df_mag state already active.*Nested calls.*not supported.*Reinitializing"
  )

  # State should still be active with new size
  expect_true(is_df_mag_active())
  state <- get_df_mag_state()
  expect_equal(nrow(state), 3)

  # Clean up
  flush_df_mag_state()
})

test_that("set_df_mag_state stores complete df_mag", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Must initialize first
  expect_error(
    set_df_mag_state(data.frame(mag = "m", mag_label = "million ", denom = 1e6)),
    "Cannot set df_mag state: not initialized"
  )

  # Initialize state
  init_df_mag_state(2)

  # Create test df_mag
  test_df_mag <- data.frame(
    mag = c("m", ""),
    mag_label = c("million ", ""),
    denom = c(1e6, 1),
    stringsAsFactors = FALSE
  )

  # Store it
  expect_silent(set_df_mag_state(test_df_mag))

  # Retrieve and verify
  stored_state <- get_df_mag_state()
  expect_equal(stored_state, test_df_mag)

  # Clean up
  flush_df_mag_state()
})

test_that("set_df_mag_state validates input", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()
  init_df_mag_state(2)

  # Test invalid inputs
  expect_error(set_df_mag_state("not_a_dataframe"))
  expect_error(set_df_mag_state(list(a = 1, b = 2)))
  expect_error(set_df_mag_state(matrix(1:6, nrow = 2)))

  # Clean up
  flush_df_mag_state()
})

test_that("is_df_mag_active works correctly", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Should be FALSE initially
  expect_false(is_df_mag_active())

  # Should be TRUE after init
  init_df_mag_state(2)
  expect_true(is_df_mag_active())

  # Should be FALSE after flush
  flush_df_mag_state()
  expect_false(is_df_mag_active())
})

test_that("get_df_mag_state returns correct values", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Should return NULL when not active
  expect_null(get_df_mag_state())

  # Should return data.frame when active
  init_df_mag_state(3)
  state <- get_df_mag_state()
  expect_s3_class(state, "data.frame")
  expect_equal(nrow(state), 3)

  # Clean up
  flush_df_mag_state()
})

test_that("get_df_mag_row retrieves single row correctly", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Should error when not active
  expect_error(get_df_mag_row(1), "df_mag state not active")

  # Initialize and set test data
  init_df_mag_state(3)
  test_df_mag <- data.frame(
    mag = c("", "m", "b"),
    mag_label = c("", "million ", "billion "),
    denom = c(1, 1e6, 1e9),
    stringsAsFactors = FALSE
  )
  set_df_mag_state(test_df_mag)

  # Test valid retrieval
  row1 <- get_df_mag_row(1)
  expect_s3_class(row1, "data.frame")
  expect_equal(nrow(row1), 1)
  expect_equal(row1$mag, "")
  expect_equal(row1$mag_label, "")
  expect_equal(row1$denom, 1)

  row2 <- get_df_mag_row(2)
  expect_equal(row2$mag, "m")
  expect_equal(row2$mag_label, "million ")
  expect_equal(row2$denom, 1e6)

  row3 <- get_df_mag_row(3)
  expect_equal(row3$mag, "b")
  expect_equal(row3$mag_label, "billion ")
  expect_equal(row3$denom, 1e9)

  # Clean up
  flush_df_mag_state()
})

test_that("get_df_mag_row validates index bounds", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()
  init_df_mag_state(3)

  # Test invalid indices
  expect_error(get_df_mag_row(0), "Element 1 is not >= 1")
  expect_error(get_df_mag_row(4), "idx must be in range \\[1, 3\\]")
  expect_error(get_df_mag_row(-1), "Element 1 is not >= 1")
  expect_error(get_df_mag_row(1.5), "Must be of type 'integerish'")
  expect_error(get_df_mag_row(c(1, 2)), "Must have length 1")
  expect_error(get_df_mag_row("1"), "Must be of type 'integerish'")

  # Valid indices should work
  expect_silent(get_df_mag_row(1))
  expect_silent(get_df_mag_row(3))

  # Clean up
  flush_df_mag_state()
})

test_that("update_df_mag_state modifies correct index", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Should error when not active
  expect_error(
    update_df_mag_state(1, mag = "m"),
    "Cannot update df_mag state: state not initialized"
  )

  # Initialize state
  init_df_mag_state(3)

  # Update specific fields at index 2
  expect_silent(update_df_mag_state(2, mag = "m", mag_label = "million ", denom = 1e6))

  state <- get_df_mag_state()

  # Index 2 should be updated
  expect_equal(state$mag[2], "m")
  expect_equal(state$mag_label[2], "million ")
  expect_equal(state$denom[2], 1e6)

  # Other indices should be unchanged (NA)
  expect_true(is.na(state$mag[1]))
  expect_true(is.na(state$mag[3]))
  expect_true(is.na(state$mag_label[1]))
  expect_true(is.na(state$mag_label[3]))
  expect_true(is.na(state$denom[1]))
  expect_true(is.na(state$denom[3]))

  # Test partial updates
  update_df_mag_state(1, mag = "")
  state <- get_df_mag_state()
  expect_equal(state$mag[1], "")
  expect_true(is.na(state$mag_label[1]))  # Should remain NA
  expect_true(is.na(state$denom[1]))      # Should remain NA

  # Clean up
  flush_df_mag_state()
})

test_that("update_df_mag_state validates index and state", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()
  init_df_mag_state(2)

  # Test invalid indices
  expect_error(update_df_mag_state(0, mag = "m"), "Element 1 is not >= 1")
  expect_error(update_df_mag_state(3, mag = "m"), "idx must be integer in range \\[1, 2\\]")
  expect_error(update_df_mag_state(-1, mag = "m"), "Element 1 is not >= 1")
  expect_error(update_df_mag_state(1.5, mag = "m"), "Must be of type 'integerish'")
  expect_error(update_df_mag_state(c(1, 2), mag = "m"), "Must have length 1")
  expect_error(update_df_mag_state("1", mag = "m"), "Must be of type 'integerish'")

  # Valid calls with NULL values should work (no-op)
  expect_silent(update_df_mag_state(1))
  expect_silent(update_df_mag_state(1, mag = NULL, mag_label = NULL, denom = NULL))

  # Clean up
  flush_df_mag_state()
})

test_that("flush_df_mag_state clears state", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Should return NULL when no state
  result <- flush_df_mag_state()
  expect_null(result)

  # Initialize and set some data
  init_df_mag_state(3)
  test_df_mag <- data.frame(
    mag = c("", "m", "b"),
    mag_label = c("", "million ", "billion "),
    denom = c(1, 1e6, 1e9),
    stringsAsFactors = FALSE
  )
  set_df_mag_state(test_df_mag)

  expect_true(is_df_mag_active())

  # Flush should return the stored data
  result <- flush_df_mag_state()
  expect_equal(result, test_df_mag)

  # State should now be inactive
  expect_false(is_df_mag_active())
  expect_null(get_df_mag_state())

  # Second flush should return NULL
  result2 <- flush_df_mag_state()
  expect_null(result2)
})

test_that("state survives errors in processing", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Initialize state
  init_df_mag_state(3)
  expect_true(is_df_mag_active())

  # Simulate error during processing
  tryCatch(
    stop("simulated error"),
    error = function(e) NULL
  )

  # State should still be active (not auto-flushed by error)
  expect_true(is_df_mag_active())

  # Manual cleanup
  flush_df_mag_state()
  expect_false(is_df_mag_active())
})

test_that("env parameter works correctly in all functions", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # Create a test environment
  test_env <- new.env()

  # Test with custom environment
  init_df_mag_state(2, env = test_env)
  expect_true(is_df_mag_active(env = test_env))
  expect_false(is_df_mag_active())  # Package env should still be inactive

  # Set data in custom env
  test_df_mag <- data.frame(
    mag = c("m", ""),
    mag_label = c("million ", ""),
    denom = c(1e6, 1),
    stringsAsFactors = FALSE
  )
  set_df_mag_state(test_df_mag, env = test_env)

  # Retrieve from custom env
  result <- get_df_mag_state(env = test_env)
  expect_equal(result, test_df_mag)
  expect_null(get_df_mag_state())  # Package env should be NULL

  # Get row from custom env
  row1 <- get_df_mag_row(1, env = test_env)
  expect_equal(row1$mag, "m")

  # Update in custom env
  update_df_mag_state(2, mag = "b", env = test_env)
  result <- get_df_mag_state(env = test_env)
  expect_equal(result$mag[2], "b")

  # Flush custom env
  flushed <- flush_df_mag_state(env = test_env)
  expect_false(is_df_mag_active(env = test_env))
  expect_equal(nrow(flushed), 2)
})

test_that("state functions use get_dict_formats() by default", {

  # Clean slate
  if (is_df_mag_active()) flush_df_mag_state()

  # This is more of an integration test to ensure the functions
  # work with the package's default environment
  init_df_mag_state(2)
  expect_true(is_df_mag_active())

  # Verify we can work with the actual package environment
  test_df_mag <- set_magnitude(c(1e6, 1e9))
  set_df_mag_state(test_df_mag)

  retrieved <- get_df_mag_state()
  expect_equal(retrieved, test_df_mag)

  row1 <- get_df_mag_row(1)
  expect_equal(row1$mag, "m")
  expect_equal(row1$denom, 1e6)

  # Clean up
  flush_df_mag_state()
})
