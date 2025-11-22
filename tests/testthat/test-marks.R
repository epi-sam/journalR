# ---- Marks -----------------------------------------------------------------
test_that("mid_dot works", {
   expect_equal(mid_dot(), "·")
})
test_that("en_dash works", {
   expect_equal(en_dash(), "–")
})
test_that("thin_space works", {
   expect_equal(thin_space(), " ")
})
