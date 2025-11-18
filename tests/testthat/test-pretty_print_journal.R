test_that("mid_dot works",
          {
             expect_equal(mid_dot(), "·")
          })
test_that("en_dash works",
          {
             expect_equal(en_dash(), "–")
          })
test_that("thin_space works",
          {
             expect_equal(thin_space(), " ")
          })

test_that("fround works",
          {
             expect_equal(fround(0.123456789), "0·1")
             expect_equal(fround(0.123456789, digits = 3), "0·123")
             expect_equal(fround(0.123456789, digits = 3, nsmall = 4), "0·1230")
          })

test_that("fround_dtype works",
          {
             expect_equal(fround_dtype_lancet(0.123456789), "0·1%")
             expect_equal(fround_dtype_lancet(0.123456789, d_type = "pp"), "0·1 pp")
             expect_equal(fround_dtype_lancet(0.123456789, d_type = "count", digits = 3, nsmall = 4), "0·1230")
          })


test_that("set_magnitude works",
          {
             expect_equal(
                set_magnitude(c(1, 1e3, 1e6, 1e9, 1e12, -1e6))
                , list(
                   mag         = c("", "", "M", "B", "B", "M")
                   , mag_label = c("", "", "million ", "billion ", "billion ", "million ")
                   , denom     = c(1, 1, 1e+06, 1e+09, 1e+09, 1e+06)
                )
             )
          })



test_that("fmt_magnitude works",
          {
             #' fmt_magnitude(123456789) # "123.5 million"
             expect_equal(fmt_magnitude(123456789), "123.5 million")
          })


test_that("format_mean_dt works",
          {
             DT_test <- data.table::data.table(
                location_id = c(1, 2, 3)
                , mean_1990 = c(0.1234, 0.2345, 0.3456)
                , mean_2000 = c(0.2234, 0.3345, 0.4456)
             )

             DT_result <- format_means_dt(DT_test, dtype = "prop")

             DT_expected <- data.table::data.table(
                location_id = c(1, 2, 3)
                , mean_1990 = c("12.3%", "23.5%", "34.6%")
                , mean_2000 = c("22.3%", "33.5%", "44.6%")
             )

             expect_equal(DT_result, DT_expected)
          })

# Integration tests -----

test_that("fround_mag_clu works",
          {
             expect_equal(fround_mag_clu_lancet(clu = c(central = 0.2, lower = 0.1, upper = 0.3), d_type = "prop"), c(central = "0·2", lower = "0·1", upper = "0·3"))
             expect_equal(fround_mag_clu_lancet(clu = c(central = 0.2, lower = -0.1, upper = 0.3), d_type = "pp"), c(central = "0·2", lower = "–0·1", upper = "0·3"))
             expect_equal(fround_mag_clu_lancet(clu = c(central = 9.5e6, lower = 8.9e6, upper = 101e6), d_type = "count"), c(central = "9·50", lower = "8·90", upper = "101"))
             expect_equal(fround_mag_clu_lancet(clu = c(central = 95e6, lower = 96e6, upper = 97e6), d_type = "count"), c(central = "95·0", lower = "96·0", upper = "97·0"))
             # trickier cases - need sig figs even for small numbers
             expect_equal(fround_mag_clu_lancet(clu = c(central = 1, lower = 0.2, upper = 2), d_type = "count"), c(central = "1·00", lower = "0·200", upper = "2·00"))
             expect_equal(fround_mag_clu_lancet(clu = c(central = 10.5, lower = 0.2, upper = 20.3), d_type = "count"), c(central = "10·5", lower = "0·200", upper = "20·3"))
             # rounding edge case
             expect_equal(fround_mag_clu_lancet(clu = c(central = 9995, lower = 9990, upper = 10100), d_type = 'count'), c(central = "10 000", lower = "9990", upper = "10 100"))
          })


test_that("format_lancet_clu works",
          {
             expect_equal(
                format_lancet_clu(central = 0.994, lower = 0.984, upper = 0.998, d_type = "prop")
                , "99·4% (98·4–99·8)"
             )
             expect_equal(format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop")
                          , c("99·4% (98·4–99·8)", "99·4% (98·4–99·8)"))
             expect_equal(format_lancet_clu(central = c(0.994, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "prop")
                          , c("99·4% (–15·0 to 99·8)", "99·4% (98·4–99·8)"))
             expect_equal(format_lancet_clu(central = c(-0.05, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "pp")
                          , c("a decrease of 5·0 pp (–15·0 to 99·8)", "99·4 pp (98·4–99·8)"))
             expect_equal(format_lancet_clu(central = rep(2e6, 2), lower = rep(.5e6, 2), upper = rep(3e6, 2), d_type = "count")
                          , c("2·00 million (0·50–3·00)", "2·00 million (0·50–3·00)"))
             expect_equal(format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop",  digits_round_prop = 4)
                          , c("a decrease of 99·4% (98·4–99·8)", "a decrease of 99·4% (98·4–99·8)"))
             expect_equal(format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop",  digits_round_prop = 4, UI_only = TRUE)
                          , c("98·4–99·8", "98·4–99·8"))
             # rounding edge case
             expect_equal(format_lancet_clu(central = 9995, lower = 9990, upper = 10100, d_type = 'count')
                          , c("10 000 (9990–10 100)"))
          })

test_that("format_nature_clu works",
          {
             expect_equal(
                format_nature_clu(central = 0.994, lower = 0.984, upper = 0.998, d_type = "prop")
                , "99.4% (98.4–99.8)"
             )
             expect_equal(
                format_nature_clu(central = 0.994, lower = -0.984, upper = 0.998, d_type = "prop")
                , "99.4% (-98.4 to 99.8)"
             )
          })


test_that("format_lancet_clu errors correctly", {
   expect_error(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998), d_type = "prop")
      , "Assertion on 'length\\(clu_lengths\\) == 1' failed: Must be TRUE."
   )
   expect_error(
      format_lancet_clu(central = c(0.994, 0.999), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop")
      , "upper is less than/equal to central at index: 2 : \\(0.998 < 0.999\\)"
   )
   expect_error(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "propeller")
      , "Assertion on 'd_type' failed: Must be element of set \\{'prop','pp','count'\\}, but is 'propeller'."
   )

})

# 2025 Oct 28 - format_journal_clu was added as a generic function, and
# format_lancet_clu is just a wrapper for it, so this test catches both, until
# proven otherwise.
test_that("format_lancet_dt and format_nature_dt work",
          {
             DT_count <- data.table::data.table(
                location_did    = rep(1, 2)
                , location_name = rep("Global", 2)
                , mean          = c(55.8e6, 54.7e9)
                , lower         = c(50.7e6, 48.6e9)
                , upper         = c(60.7e6, 59.6e9)
             )

             expect_equal(
                format_lancet_dt(
                   dt          = DT_count,
                   d_type      = "count",
                   central_var = 'mean'
                )
                , structure(
                   list(
                      location_did = c(1, 1),
                      location_name = c("Global", "Global"),
                      clu_fmt = c("55·8 million (50·7–60·7)", "54·7 billion (48·6–59·6)")
                   ),
                   row.names = c(NA, -2L),
                   class = c("data.table", "data.frame")
                )
             )


             expect_equal(
                format_nature_dt(
                   dt            = DT_count
                   , d_type      = "count"
                   , central_var = 'mean'
                )
                , structure(
                   list(
                      location_did = c(1, 1),
                      location_name = c("Global", "Global"),
                      clu_fmt = c(
                         "55.8 million (50.7–60.7)",
                         "54.7 billion (48.6–59.6)"
                      )
                   ),
                   row.names = c(NA, -2L),
                   class = c("data.table", "data.frame")
                )
             )

             DT_prop <- data.table::data.table(
                location_did    = rep(1, 3)
                , location_name = rep("Global", 3)
                , data_space    = c("all_positive", "mixed_negative", "all_negative")
                , mean          = c(.558, -0.1, -0.1)
                , lower         = c(.507, -0.25, -0.2)
                , upper         = c(.607, 1.3, -0.05)
             )

             expect_equal(
                format_lancet_dt(
                   dt          = DT_prop,
                   d_type      = "prop",
                   central_var = 'mean'
                )
                ,
                structure(
                   list(
                      location_did = c(1, 1, 1),
                      location_name = c("Global", "Global", "Global"),
                      data_space = c("all_positive", "mixed_negative", "all_negative"),
                      clu_fmt = c("55·8% (50·7–60·7)", "a decrease of 10·0% (–25·0 to 130·0)", "a decrease of 10·0% (5·0–20·0)"
                      )
                   ),
                   row.names = c(NA, -3L
                   )
                   , class = c("data.table", "data.frame")
                )
             )
          })
