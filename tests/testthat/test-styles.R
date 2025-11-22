# ---- Assertion -----------------------------------------------------------------

test_that("assert_style_schema errors correctly", {
   style_long_kv <- new_style("long_kv")
   style_long_kv$prop_digits_round <- 1:2
   expect_error(
      assert_style_schema(style_long_kv)
      , regexp = "All style_entries must be length 1 key/value pairs: prop_digits_round"
   )

   style_extra_item <- new_style("extra_item")
   style_extra_item$foo <- "bar"
   expect_error(
      assert_style_schema(style_extra_item)
      , regexp = "Style schema is malformed - foo - please inspect the sytle_entry:"
   )

   style_wrong_count_method <- new_style("wrong_count_method")
   style_wrong_count_method$count_method <- "round_half_even"
   expect_error(
      assert_style_schema(style_wrong_count_method)
      , regexp = "Style schema is malformed - count_method - please inspect the sytle_entry:\n +'round_half_even' is not a valid choice for style_entry\\[\\[\"count_method\"\\]\\]"
   )

   style_non_int <- new_style("non_int")
   style_non_int$prop_digits_round <- 2.5
   expect_error(
      assert_style_schema(style_non_int)
      , regexp = "style element 'prop_digits_round' should be integer-ish but is type: 'double', class: numeric"
   )
})
