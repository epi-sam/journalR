# Test Helper Functions Independently ----------------------------------------

test_that("set_magnitude_prop returns no scaling", {
   result <- set_magnitude_prop(c(0.5, 0.75, 0.25))
   expect_equal(result$mag, c("", "", ""))
   expect_equal(result$mag_label, c("", "", ""))
   expect_equal(result$denom, rep(0.01, 3))

   # Test that mag parameter is ignored for props
   result2 <- set_magnitude_prop(c(0.5), mag = "m")
   expect_equal(result2$mag, "")
   expect_equal(result2$denom, 0.01)
})

test_that("set_magnitude_prop validates input range", {
   expect_error(
      set_magnitude_prop(c(0.5, 1.5)),
      "Proportion values must be between -1 and \\+1"
   )
   expect_error(
      set_magnitude_prop(c(-1.5, 0.5)),
      "Proportion values must be between -1 and \\+1"
   )
})

test_that("set_magnitude_count detects billions/millions/thousands", {
   result <- set_magnitude_count(c(1e9, 1e6, 1e3))
   expect_equal(result$mag, c("b", "m", ""))
   expect_equal(result$mag_label, c("billion ", "million ", ""))
   expect_equal(result$denom, c(1e9, 1e6, 1))
})

test_that("set_magnitude_count validates positive values", {
   expect_error(
      set_magnitude_count(c(1e6, 0)),
      "Count values must be greater than 0"
   )
   expect_error(
      set_magnitude_count(c(-1e6)),
      "Count values must be greater than 0"
   )
})

test_that("set_magnitude_count respects label_thousands", {
   # Test warning
   expect_warning(
      result <- set_magnitude_count(1e3, label_thousands = TRUE),
      "'thousands' magnitude is not Lancet-valid"
   )
   expect_equal(result$mag, "t")
   expect_equal(result$mag_label, "thousand ")
   expect_equal(result$denom, 1e3)
})

test_that("set_magnitude_rate scales appropriately", {
   # Test per 100,000 scale
   result <- set_magnitude_rate(0.0000123)
   expect_equal(result$mag, "per1m")
   expect_equal(result$mag_label, " per 1 million")
   expect_equal(result$denom, 1e-6)

   # Verify scaling target is met
   scaled <- 0.0000123 / result$denom
   expect_true(scaled >= 0.1 && scaled <= 100)
})

test_that("set_magnitude_rate validates positive values", {
   expect_error(
      set_magnitude_rate(c(0.001, 0)),
      "Rate values must be greater than 0"
   )
   expect_error(
      set_magnitude_rate(c(-0.001)),
      "Rate values must be greater than 0"
   )
})

test_that("set_magnitude_rate warns for rates >= 1", {
   expect_warning(
      result <- set_magnitude_rate(1.5),
      "Rate value >= 1.*Consider using d_type='count'"
   )
   expect_equal(result$mag, "")
})

# Test Main Dispatcher Function ----------------------------------------------

test_that("set_magnitude routes to correct helper", {
   # Prop
   result_prop <- set_magnitude(0.5, d_type = "prop")
   expect_equal(result_prop$mag, "")
   expect_equal(result_prop$denom, 0.01)

   # Count
   result_count <- set_magnitude(1e6, d_type = "count")
   expect_equal(result_count$mag, "m")
   expect_equal(result_count$denom, 1e6)

   # Rate
   result_rate <- set_magnitude(0.0000123, d_type = "rate")
   expect_equal(result_rate$mag, "per1m")
   expect_equal(result_rate$denom, 1e-6)

   # Percentage points
   result_pp <- set_magnitude(0.05, d_type = "pp")
   expect_equal(result_pp$mag, "")
   expect_equal(result_pp$denom, 0.01)
})

test_that("set_magnitude requires d_type", {
   expect_error(
      set_magnitude(1e6),
      'argument "d_type" is missing'
   )
})

test_that("set_magnitude validates d_type", {
   expect_error(
      set_magnitude(1e6, d_type = "invalid"),
      "is not a valid choice for d_type"
   )
})

test_that("set_magnitude preserves other parameters", {
   # Test label_thousands is passed through
   expect_warning(
      result <- set_magnitude(1e3, d_type = "count", label_thousands = TRUE)
      , regexp = "'thousands' magnitude is not Lancet-valid"

   )
   expect_equal(result$mag, "t")

   # Test mag override is passed through
   result2 <- set_magnitude(1e6, d_type = "count", mag = "b")
   expect_equal(result2$mag, "b")
})

# Test Edge Cases and Integration ---------------------------------------------

test_that("set_magnitude errors for empty vectors", {
   expect_error(
      set_magnitude(numeric(), 'prop')
      , regexp = "Assertion on 'x' failed: Must have length >= 1, but has length 0."
   )
})

test_that("set_magnitude returns consistent structure", {
   # All helpers should return same column structure
   result_prop <- set_magnitude(0.5, d_type = "prop")
   result_count <- set_magnitude(1e6, d_type = "count")
   result_rate <- set_magnitude(0.0000123, d_type = "rate")

   expect_equal(names(result_prop), c("mag", "mag_label", "denom"))
   expect_equal(names(result_count), c("mag", "mag_label", "denom"))
   expect_equal(names(result_rate), c("mag", "mag_label", "denom"))

   expect_equal(typeof(result_prop$denom), "double")
   expect_equal(typeof(result_count$denom), "double")
   expect_equal(typeof(result_rate$denom), "double")
})
