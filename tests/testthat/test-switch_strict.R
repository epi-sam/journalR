
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
      "'x' is not a valid option.* Valid options are: a, b, c",
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
      "'b' is not a valid option.* Valid options are: apple, banana, cherry",
   )
})
