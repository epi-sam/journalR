# API mag override ----------------------------------------------------------

test_that("API mag override works end-to-end: proportions", {
   df <- data.frame(
      id    = 1:3,
      mean  = c(055.8, 023.4, 078.9),
      lower = c(050.7, 020.1, 075.6),
      upper = c(060.7, 026.7, 082.1)
   )

   # no override
   expect_error(
      format_journal_df(df, metric = "prop")
      , regexp = "Proportion values must be between -1 and \\+1"
   )

   # with override
   result <- format_journal_df(df, metric = "prop", mag = 'as-is')
   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(nrow(result), 3)
   expect_equal(result$clu_fmt[1], "55.8% (50.7–60.7)")
   expect_equal(result$clu_fmt[2], "23.4% (20.1–26.7)")
   expect_equal(result$clu_fmt[3], "78.9% (75.6–82.1)")
})

test_that("API mag override works end-to-end: counts ", {
   df <- data.frame(
      id    = 1:3,
      mean  = c(55.8e7, 123.4e7, 5.67e10),
      lower = c(50.7e7, 110.2e7, 5.12e10),
      upper = c(60.7e7, 135.6e7, 6.23e10)
   )

   # no override
   result <- format_journal_df(df, metric = "count")
   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "558 million (507–607)")
   expect_equal(result$clu_fmt[2], "1.23 billion (1.10–1.36)")
   expect_equal(result$clu_fmt[3], "56.7 billion (51.2–62.3)")


   # with override
   result <- format_journal_df(df, metric = "count", mag = "b")
   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "0.558 billion (0.507–0.607)")
   expect_equal(result$clu_fmt[2], "1.23 billion (1.10–1.36)")
   expect_equal(result$clu_fmt[3], "56.7 billion (51.2–62.3)")
})

test_that("API mag override works end-to-end: rates ", {
   df <- data.frame(
      id    = 1:3,
      mean  = c(0.000123, 0.000456, 0.000789),
      lower = c(0.000100, 0.000400, 0.000700),
      upper = c(0.000150, 0.000500, 0.000900)
   )

   # no override
   result <- format_journal_df(df, metric = "rate", rate_unit = "deaths")
   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "12.3 deaths (10.0–15.0) per 100,000")
   expect_equal(result$clu_fmt[2], "45.6 deaths (40.0–50.0) per 100,000")
   expect_equal(result$clu_fmt[3], "78.9 deaths (70.0–90.0) per 100,000")

   # with override
   result <- format_journal_df(df, metric = "rate", rate_unit = "deaths", mag = "per1m")

   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "123 deaths (100–150) per 1 million")
   expect_equal(result$clu_fmt[2], "456 deaths (400–500) per 1 million")
   expect_equal(result$clu_fmt[3], "789 deaths (700–900) per 1 million")

})


# Test Helper Functions Independently ----------------------------------------

test_that("set_magnitude_prop scales correctly", {
   # default denom handling
   result <- set_magnitude_prop(c(0.5, 0.75, 0.25))
   expect_equal(result$mag, c("", "", ""))
   expect_equal(result$mag_label, c("", "", ""))
   expect_equal(result$denom, rep(0.01, 3))
   # user-override as-is handling
   result2 <- set_magnitude_prop(c(50, 75, 25), mag = "as-is")
   expect_equal(result2$mag, rep("",3))
   expect_equal(result2$denom, rep(1,3))
   expect_error(set_magnitude_prop(50, 'raw'), "Invalid mag for proportions")
   expect_error(set_magnitude_prop(50), "Proportion values must be between -1 and \\+1")
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
   expect_warning(
      set_magnitude_count(c(-1e6)),
      "Counts < 0 not yet supported"
   )
})

test_that("set_magnitude_count respects label_thousands", {
   # Test warning
   expect_warning(
      result <- set_magnitude_count(1e3, count_label_thousands = TRUE),
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
   expect_equal(result$mag_label, "per 1 million")
   expect_equal(result$denom, 1e-6)

   # Verify scaling target is met
   scaled <- 0.0000123 / result$denom
   expect_true(scaled >= 0.1 && scaled <= 100)
})

test_that("set_magnitude_rate validates positive values", {
   expect_error(
      set_magnitude_rate(c(-0.001)),
      "Rate values must be positive or 0"
   )
})

test_that("set_magnitude_rate warns for rates >= 1", {
   expect_warning(
      result <- set_magnitude_rate(1.5),
      "Rate value >= 1.*Consider using metric='count'"
   )
   expect_equal(result$mag, "")
})

# Test Main Dispatcher Function ----------------------------------------------

test_that("set_magnitude routes to correct helper", {
   # Prop
   result_prop <- set_magnitude(0.5, metric = "prop")
   expect_equal(result_prop$mag, "")
   expect_equal(result_prop$denom, 0.01)

   # Count
   result_count <- set_magnitude(1e6, metric = "count")
   expect_equal(result_count$mag, "m")
   expect_equal(result_count$denom, 1e6)

   # Rate
   result_rate <- set_magnitude(0.0000123, metric = "rate")
   expect_equal(result_rate$mag, "per1m")
   expect_equal(result_rate$denom, 1e-6)

   # Percentage points
   result_pp <- set_magnitude(0.05, metric = "pp")
   expect_equal(result_pp$mag, "")
   expect_equal(result_pp$denom, 0.01)
})

test_that("set_magnitude requires metric", {
   expect_error(
      set_magnitude(1e6),
      'argument "metric" is missing'
   )
})

test_that("set_magnitude validates metric", {
   expect_error(
      set_magnitude(1e6, metric = "invalid"),
      "is not a valid choice for metric"
   )
})

test_that("set_magnitude preserves other parameters", {
   # Test label_thousands is passed through
   expect_warning(
      result <- set_magnitude(1e3, metric = "count", count_label_thousands = TRUE)
      , regexp = "'thousands' magnitude is not Lancet-valid"

   )
   expect_equal(result$mag, "t")

   # Test mag override is passed through
   result2 <- set_magnitude(1e6, metric = "count", mag = "b")
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
   result_prop <- set_magnitude(0.5, metric = "prop")
   result_count <- set_magnitude(1e6, metric = "count")
   result_rate <- set_magnitude(0.0000123, metric = "rate")

   expect_equal(names(result_prop), c("mag", "mag_label", "denom"))
   expect_equal(names(result_count), c("mag", "mag_label", "denom"))
   expect_equal(names(result_rate), c("mag", "mag_label", "denom"))

   expect_equal(typeof(result_prop$denom), "double")
   expect_equal(typeof(result_count$denom), "double")
   expect_equal(typeof(result_rate$denom), "double")
})
