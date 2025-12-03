# Rate Support Tests
# Comprehensive tests for rate data type functionality

# ---- End-to-End Tests for All Data Types ----------------------------------------

test_that("end-to-end: proportions (backward compatibility)", {
   df <- data.frame(
      id = 1:3,
      mean = c(0.558, 0.234, 0.789),
      lower = c(0.507, 0.201, 0.756),
      upper = c(0.607, 0.267, 0.821)
   )

   result <- format_journal_df(df, d_type = "prop")

   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(nrow(result), 3)
   expect_equal(result$clu_fmt[1], "55.8% (50.7–60.7)")
   expect_equal(result$clu_fmt[2], "23.4% (20.1–26.7)")
   expect_equal(result$clu_fmt[3], "78.9% (75.6–82.1)")
})

test_that("end-to-end: percentage points (backward compatibility)", {
   df <- data.frame(
      id = 1:2,
      mean = c(0.123, -0.045),
      lower = c(0.089, -0.067),
      upper = c(0.156, -0.023)
   )

   result <- format_journal_df(df, d_type = "pp")

   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "12.3 pp (8.9–15.6)")
   expect_equal(result$clu_fmt[2], "-4.5 pp (2.3–6.7)")
})

test_that("end-to-end: counts (backward compatibility)", {
   df <- data.frame(
      id    = 1:3,
      mean  = c(55.8e6, 123.4e6, 5.67e9),
      lower = c(50.7e6, 110.2e6, 5.12e9),
      upper = c(60.7e6, 135.6e6, 6.23e9)
   )

   result <- format_journal_df(df, d_type = "count")

   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "55.8 million (50.7–60.7)")
   expect_equal(result$clu_fmt[2], "123 million (110–136)")
   expect_equal(result$clu_fmt[3], "5.67 billion (5.12–6.23)")
   expect_false(grepl("deaths|cases", result$clu_fmt[1])) # No rate units
})

test_that("end-to-end: rates", {
   df <- data.frame(
      id    = 1:3,
      mean  = c(0.0000123, 0.0000456, 0.0000789),
      lower = c(0.0000098, 0.0000401, 0.0000654),
      upper = c(0.0000152, 0.0000512, 0.0000923)
   )

   result <- format_journal_df(df, d_type = "rate", rate_unit = "deaths")

   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "12.3 deaths (9.80–15.2) per 1 million")
   expect_equal(result$clu_fmt[2], "45.6 deaths (40.1–51.2) per 1 million")
   expect_equal(result$clu_fmt[3], "78.9 deaths (65.4–92.3) per 1 million")
   expect_false(grepl("%|pp", result$clu_fmt[1])) # No percentage signs
})

test_that("end-to-end: rates with different magnitudes", {
   df <- data.frame(
      id    = 1:3,
      mean  = c(0.0000123, 0.00000001, 0.000123),  # per 100k, per 100m, per 1k
      lower = c(0.0000098, 0.000000008, 0.000098),
      upper = c(0.0000152, 0.000000012, 0.000152)
   )

   result <- format_journal_df(df, d_type = "rate", rate_unit = "cases")

   expect_equal(result$clu_fmt[1], "12.3 cases (9.80–15.2) per 1 million")
   expect_equal(result$clu_fmt[2], "11.0 cases (9.00–13.0) per 1 billion")
   expect_equal(result$clu_fmt[3], "12.3 cases (9.80–15.2) per 100,000")

   # All should have "cases" unit
   expect_true(all(grepl("cases", result$clu_fmt)))
})

# ---- Rate Unit Validation Tests ----------------------------------------------

test_that("rate_unit required for rates", {
   expect_error(
      format_journal_clu(
         central = 0.0000123,
         lower   = 0.0000098,
         upper   = 0.0000152,
         d_type  = "rate"
      ),
      "rate_unit is required"
   )

   expect_error(
      format_journal_df(
         data.frame(mean = 0.0000123, lower = 0.0000098, upper = 0.0000152),
         d_type = "rate"
      ),
      "rate_unit is required"
   )
})

test_that("rate_unit ignored for non-rates", {
   # Should not error for props
   result_prop <- format_journal_clu(
      central   = 0.5,
      lower     = 0.4,
      upper     = 0.6,
      d_type    = "prop",
      rate_unit = "ignored"
   )
   expect_false(grepl("ignored", result_prop))

   # Should not error for counts
   result_count <- format_journal_clu(
      central   = 1000000,
      lower     = 900000,
      upper     = 1100000,
      d_type    = "count",
      rate_unit = "ignored"
   )
   expect_false(grepl("ignored", result_count))
})

test_that("rate_unit validates as string", {
   expect_error(
      format_journal_clu(
         central   = 0.0000123,
         lower     = 0.0000098,
         upper     = 0.0000152,
         d_type    = "rate",
         rate_unit = 123
      ),
      regexp = "Assertion on 'rate_unit' failed: Must be of type 'string', not 'double'."
   )
})

# ---- Rate Formatting Specific Tests ------------------------------------------

test_that("rate formatting produces correct string structure", {
   result <- format_journal_clu(
      central   = 0.0000123,
      lower     = 0.0000098,
      upper     = 0.0000152,
      d_type    = "rate",
      rate_unit = "deaths"
   )

   # Should match pattern: "number deaths (number–number) per magnitude"
   expect_equal(result, "12.3 deaths (9.80–15.2) per 1 million")

   # Should not have percentage signs or "million"/"billion" prefixes
   expect_false(grepl("%|million |billion ", result))
})

test_that("rate formatting with different units", {
   test_data <- list(
      central = 0.0000123,
      lower   = 0.0000098,
      upper   = 0.0000152,
      d_type  = "rate"
   )

   # Test different rate units
   units <- c("deaths", "cases", "events", "hospitalizations")

   for (unit in units) {
      result <- do.call(format_journal_clu, c(test_data, rate_unit = unit))
      expect_true(grepl(unit, result))
      expect_true(grepl("per", result))
   }
})

test_that("rates work with UI_only style", {
   # Test with UI_only style
   new_style("ui_only_test", UI_only = TRUE)

   result <- format_journal_clu(
      central    = 0.0000123,
      lower      = 0.0000098,
      upper      = 0.0000152,
      d_type     = "rate",
      rate_unit  = "deaths",
      style_name = "ui_only_test"
   )

   expect_equal(result, "9.80–15.2 deaths per 1 million")
})

# ---- Rate Wrapper Function Tests ---------------------------------------------

test_that("format_lancet_clu works with rates", {
   result <- format_lancet_clu(
      central   = 0.0000123,
      lower     = 0.0000098,
      upper     = 0.0000152,
      d_type    = "rate",
      rate_unit = "deaths"
   )

   expect_equal(result, "12·3 deaths (9·80–15·2) per 1 million")
})

test_that("format_nature_clu works with rates", {
   result <- format_nature_clu(
      central   = 0.0000123,
      lower     = 0.0000098,
      upper     = 0.0000152,
      d_type    = "rate",
      rate_unit = "cases"
   )

   expect_equal(result, "12.3 cases (9.80–15.2) per 1 million")
})

test_that("format_lancet_df works with rates", {
   df <- data.frame(
      location = "Global",
      mean     = 0.0000123,
      lower    = 0.0000098,
      upper    = 0.0000152
   )

   result <- format_lancet_df(df, d_type = "rate", rate_unit = "deaths")

   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "12·3 deaths (9·80–15·2) per 1 million")
})

test_that("format_nature_df works with rates", {
   df <- data.frame(
      location = "Global",
      mean     = 0.0000123,
      lower    = 0.0000098,
      upper    = 0.0000152
   )

   result <- format_nature_df(df, d_type = "rate", rate_unit = "cases")

   expect_true("clu_fmt" %in% colnames(result))
   expect_equal(result$clu_fmt[1], "12.3 cases (9.80–15.2) per 1 million")
})

# ---- fmt_magnitude with Rates -----------------------------------------------

test_that("fmt_magnitude works with rates", {
   result <- fmt_magnitude(
      x         = 0.0000123,
      d_type    = "rate",
      rate_unit = "deaths"
   )

   expect_equal(result, "12.3 deaths per 1 million")
   expect_true(is.character(result))
})

test_that("fmt_magnitude validates rate_unit", {
   expect_error(
      fmt_magnitude(x = 0.0000123, d_type = "rate"),
      "rate_unit is required"
   )

   # Should not error for non-rate types
   result <- fmt_magnitude(x = 1000000, d_type = "count")
   expect_false(grepl("deaths|cases", result))
})

# ---- fround_rate Function Tests ---------------------------------------------

test_that("fround_rate handles different magnitudes correctly", {
   # Test with different rate magnitudes
   test_cases <- list(
      list(val = 0.0000123, expected_mag = "per 100,000"),
      list(val = 0.000000001, expected_mag = "per 1 billion"),
      list(val = 0.000123, expected_mag = "per 1,000")
   )

   for (case in test_cases) {
      result <- fround_clu_triplet(
         clu = c(case$val, case$val * 0.8, case$val * 1.2),
         d_type = "rate",
         style_name = "nature"
      )

      # Check that formatting works and produces numeric strings
      expect_true(all(grepl("^[0-9.]", result$formatted)))
   }
})

test_that("fround_rate respects rate-specific style parameters", {
   # Test with different rate method settings
   new_style("rate_decimal", rate_method = "decimal", rate_nsmall = 3)
   new_style("rate_int", rate_method     = "int")
   new_style("rate_sigfig", rate_method  = "sigfig", rate_digits_sigfig = 2)

   test_val <- c(0.0000123, 0.0000098, 0.0000152)

   result_decimal <- fround_clu_triplet(test_val, "rate", "rate_decimal")
   result_int <- fround_clu_triplet(test_val, "rate", "rate_int")
   result_sigfig <- fround_clu_triplet(test_val, "rate", "rate_sigfig")

   # Results should be different based on method
   expect_false(identical(result_decimal$formatted, result_int$formatted))
   expect_false(identical(result_decimal$formatted, result_sigfig$formatted))
   expect_false(identical(result_int$formatted, result_sigfig$formatted))
})

# ---- Assertion Function Tests -----------------------------------------------

test_that("assert_rate_unit works correctly", {
   # Should not error for valid rate + unit combinations
   expect_silent(assert_rate_unit("rate", "deaths"))
   expect_silent(assert_rate_unit("rate", "cases"))

   # Should not error for non-rate types (unit ignored)
   expect_silent(assert_rate_unit("prop", NULL))
   expect_silent(assert_rate_unit("count", NULL))
   expect_silent(assert_rate_unit("prop", "ignored"))

   # Should error for rate without unit
   expect_error(
      assert_rate_unit("rate", NULL),
      "rate_unit is required when d_type = 'rate'"
   )

   # Should error for rate with non-string unit
   expect_error(
      assert_rate_unit("rate", 123),
      regexp = "Assertion on 'rate_unit' failed: Must be of type 'string', not 'double'"
   )
})

# ---- Backward Compatibility Tests -------------------------------------------

test_that("existing prop/pp/count functionality unchanged", {
   # Test that existing functionality produces same results

   # Props
   prop_result <- format_journal_clu(
      central = c(0.558, 0.234),
      lower = c(0.507, 0.201),
      upper = c(0.607, 0.267),
      d_type = "prop"
   )
   expect_equal(prop_result[1], "55.8% (50.7–60.7)")
   expect_equal(prop_result[2], "23.4% (20.1–26.7)")

   # Counts
   count_result <- format_journal_clu(
      central = c(55.8e6, 5.67e9),
      lower = c(50.7e6, 5.12e9),
      upper = c(60.7e6, 6.23e9),
      d_type = "count"
   )
   expect_equal(count_result[1], "55.8 million (50.7–60.7)")
   expect_equal(count_result[2], "5.67 billion (5.12–6.23)")

   # Should not contain rate-specific elements
   expect_false(any(grepl("deaths|cases|per", c(prop_result, count_result))))
})

test_that("existing df formatting unchanged", {
   df_prop <- data.frame(
      mean = c(0.558, 0.234),
      lower = c(0.507, 0.201),
      upper = c(0.607, 0.267)
   )

   result <- format_journal_df(df_prop, d_type = "prop")
   expect_equal(ncol(result), 1)  # Only clu_fmt column remains
   expect_equal(nrow(result), 2)
   expect_true("clu_fmt" %in% colnames(result))
   expect_true(all(grepl("%", result$clu_fmt)))
})

# ---- Style Parameter Integration Tests --------------------------------------

test_that("rate styles integrate with existing style system", {
   # Test that rate parameters are properly included in style schemas
   schema <- get_style_schema()

   rate_params <- c("rate_method", "rate_digits_sigfig", "rate_pad_sigfigs", "rate_nsmall")
   expect_true(all(rate_params %in% names(schema)))

   # Test that pre-defined styles include rate parameters
   nature_style <- style_nature()
   lancet_style <- style_lancet()

   expect_true(all(rate_params %in% names(nature_style)))
   expect_true(all(rate_params %in% names(lancet_style)))
})

test_that("custom styles work with rates", {
   new_style(
      "custom_rate",
      rate_method = "decimal",
      rate_digits_sigfig = 4,
      rate_nsmall = 2
   )

   result <- format_journal_clu(
      central    = 0.0000123,
      lower      = 0.0000098,
      upper      = 0.0000152,
      d_type     = "rate",
      rate_unit  = "events",
      style_name = "custom_rate"
   )

   expect_true(grepl("events", result))
   expect_true(grepl("per", result))
   expect_true(is.character(result))
})
