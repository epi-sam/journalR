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


