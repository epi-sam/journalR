
test_that("switch_strict returns the correct value on success", {
   expect_equal(
      switch_strict(
         "b"
         , a = "apple"
         , b = "banana"
         , c = "cherry"),
      "banana"
   )
})

test_that("switch_strict fails with informative message on invalid option", {
   expect_error(
      switch_strict(
         "x"
         , a = "apple"
         , b = "banana"
         , c = "cherry"
      ),
      # `.*` is required for tests
      # - messages include the name of the function that calls switch_strict
      # - 'eval_bare()' is used for tests
      "Invalid option: x.*Valid options:  c, b, a",
   )
})
test_that("switch_strict fails with informative message on invalid option", {
   expect_error(
      switch_strict(
         "b"
         , apple = "apple"
         , banana = "banana"
         , cherry = "cherry"
      ),
      "Invalid option: b.*Valid options:  cherry, banana, apple",
   )
})
