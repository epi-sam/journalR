test_that("set_magnitude works", {
   expect_equal(
      set_magnitude(c(1, 1e3, 1e6, 1e9, 1e12, -1e6))
      , data.frame(
         mag         = c("", "", "m", "b", "b", "m")
         , mag_label = c("", "", "million ", "billion ", "billion ", "million ")
         , denom     = c(1, 1, 1e+06, 1e+09, 1e+09, 1e+06)
      )
   )
})
