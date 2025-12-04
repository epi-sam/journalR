# ---- Integration - Style-Agnostic -----------------------------------------------------------------

test_that("format_mean_df works", {
   DT_test <- data.table::data.table(
      location_id = c(1, 2, 3)
      , mean_1990 = c(0.1234, 0.2345, 0.3456)
      , mean_2000 = c(0.2234, 0.3345, 0.4456)
   )

   DT_result <- format_means_df(DT_test, d_type = "prop")

   DT_expected <- data.table::data.table(
      location_id = c(1, 2, 3)
      , mean_1990 = c("12.3%", "23.5%", "34.6%")
      , mean_2000 = c("22.3%", "33.5%", "44.6%")
   )

   expect_equal(DT_result, DT_expected)
})



# ---- Integration - Styles -----------------------------------------------------------------

# Lancet styling is hardest to pass - focus here, especially for counts
# Sprinkle in some default Nature styling as well

test_that("fround_clu_triplet works", {
   result1 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 0.2, lower = 0.1, upper = 0.3), d_type = "prop")
   expect_type(result1, "list")
   expect_named(result1, c("formatted", "df_mag_row"))
   expect_equal(result1$formatted, c(central = "20·0", lower = "10·0", upper = "30·0"))
   result2 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = -0.02, lower = -0.1, upper = 0.3), d_type = "pp")
   expect_equal(result2$formatted, c(central = "–2·0", lower = "–10·0", upper = "30·0"))
   result3 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 9.5e6, lower = 8.9e6, upper = 101e6), d_type = "count")
   expect_equal(result3$formatted, c(central = "9·50", lower = "8·90", upper = "101"))
   result4 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 95e6, lower = 94e6, upper = 97e6), d_type = "count")
   expect_equal(result4$formatted, c(central = "95·0", lower = "94·0", upper = "97·0"))
   # trickier cases - need sig figs even for small numbers
   result5 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 1, lower = 0.2, upper = 2), d_type = "count")
   expect_equal(result5$formatted, c(central = "1·00", lower = "0·200", upper = "2·00"))
   result6 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 10.5, lower = 0.2, upper = 20.3), d_type = "count")
   expect_equal(result6$formatted, c(central = "10·5", lower = "0·200", upper = "20·3"))
   # rounding edge case
   result7 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 9995, lower = 9990, upper = 10100), d_type = 'count')
   expect_equal(result7$formatted, c(central = "10 000", lower = "9990", upper = "10 100"))
})


# This is the beating heart of the machine - get these right
test_that("format_lancet_clu works", {
   expect_equal(
      format_lancet_clu(central = 0.994, lower = 0.984, upper = 0.998, d_type = "prop")
      , "99·4% (98·4–99·8)"
   )
   expect_equal(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop")
      , c("99·4% (98·4–99·8)", "99·4% (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "prop")
      , c("99·4% (–15·0 to 99·8)", "99·4% (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = c(-0.05, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "pp")
      , c("a decrease of 5·0 pp (–15·0 to 99·8)", "99·4 pp (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = rep(2e6, 2), lower = rep(.5e6, 2), upper = rep(3e6, 2), d_type = "count")
      , c("2·00 million (0·500–3·00)", "2·00 million (0·500–3·00)")
   )
   expect_equal(
      format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop")
      , c("a decrease of 99·4% (98·4–99·8)", "a decrease of 99·4% (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop")
      , c("a decrease of 99·4% (98·4–99·8)", "a decrease of 99·4% (98·4–99·8)")
   )
   # rounding edge case
   expect_equal(format_lancet_clu(central = 9995, lower = 9990, upper = 10100, d_type = 'count')
                , c("10 000 (9990–10 100)"))
   # round_5_up test
   expect_equal(
      format_journal_clu(central = 1145, lower = 1135, upper = 1155, d_type = 'count'), "1,150 (1,140–1,160)"
   )
   # format_journal_clu(central = -0.1321684, lower = -0.235321, upper = -0.056549, d_type = 'count')
   # format_journal_df(data.frame(mean = -0.1321684, lower = -0.235321, upper = -0.056549), d_type = 'count')
})

test_that("format_nature_clu works", {
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
      , "Column length mismatch:"
   )
   expect_error(
      format_lancet_clu(central = c(0.994, 0.999), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop")
      , "upper is less than/equal to central at index: 2 : \\(0.998 < 0.999\\)"
   )
   expect_error(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "propeller")
      , "'propeller' is not a valid choice."
   )

})

# 2025 Oct 28 - format_journal_clu was added as a generic function, and
# format_lancet_clu is just a wrapper for it, so this test catches both, until
# proven otherwise.
test_that("format_lancet_df and format_nature_df work", {
   DT_count <- data.frame(
      location_did    = rep(1, 2)
      , location_name = rep("Global", 2)
      , mean          = c(55.8e6, 54.7e9)
      , lower         = c(50.7e6, 48.6e9)
      , upper         = c(60.7e6, 59.6e9)
   )

   expect_equal(
      format_lancet_df(
         df          = DT_count,
         d_type      = "count",
      )
      , structure(
         list(
            location_did = c(1, 1),
            location_name = c("Global", "Global"),
            clu_fmt = c("55·8 million (50·7–60·7)", "54·7 billion (48·6–59·6)")
         ),
         row.names = c(NA, -2L),
         class = c("data.frame")
      )
   )


   expect_equal(
      format_nature_df(
         df            = DT_count
         , d_type      = "count"
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
         class = c("data.frame")
      )
   )

   DT_prop <- data.table::data.table(
      location_did    = rep(1, 5)
      , location_name = rep("Global", 5)
      , data_space    = c("all_positive", "mixed_negative", "all_negative", "lower_negative", "cent_low_neg")
      , mean          = c(.558, -0.1, -0.1, 0.05, -0.05)
      , lower         = c(.507, -0.25, -0.2, -0.02, -0.1)
      , upper         = c(.607, 1.3, -0.05, 0.12, 0.1)
   )

   expect_equal(
      format_lancet_df(
         df          = DT_prop,
         d_type      = "prop",
         central_var = 'mean'
      )
      , structure(
         list(
            location_did = rep(1, 5)
            , location_name = rep("Global", 5)
            , data_space = c(
               "all_positive"
               , "mixed_negative"
               , "all_negative"
               , "lower_negative"
               , "cent_low_neg"
            ) , clu_fmt = c(
               "55·8% (50·7–60·7)"
               , "a decrease of 10·0% (–25·0 to 130·0)"
               , "a decrease of 10·0% (5·0–20·0)"
               , "5·0% (–2·0 to 12·0)"
               , "a decrease of 5·0% (–10·0 to 10·0)"
            )
         )
         , row.names = c(NA, -5L)
         , class = c("data.table", "data.frame")
      )
   )
})


test_that("edge case rounding works with and without thousands label", {
   DF <- data.frame(
         data_space = c("thousands", "thousands_edge", "millions", "billions"),
         mean       = c(999000,      999999,           55831000,   5.4717e+12),
         lower      = c(888888,      888888,           50724000,   4.8266e+12),
         upper      = c(2222222,     2222222,          60797000,   5.9786e+12)
   )

   expect_equal(
      DF |> format_journal_df(d_type = "count", style_name = "nature") # |> dput()
      , structure(list(
         data_space = c("thousands", "thousands_edge", "millions", "billions"),
         clu_fmt = c(
            "999,000 (889,000–2,220,000)",
            "1.00 million (0.889–2.22)",
            "55.8 million (50.7–60.8)",
            "5,470 billion (4,830–5,980)"
         )
      ),
      row.names = c(NA, -4L),
      class = "data.frame")
   )


   new_style("lab_thou", count_label_thousands = TRUE)
   expect_equal(
      DF |> format_journal_df(d_type = "count", style_name = "lab_thou") # |> dput()
      , structure(list(
         data_space = c("thousands", "thousands_edge", "millions", "billions"),
         clu_fmt = c(
            "999 thousand (889–2,220)",
            "1.00 million (0.889–2.22)",
            "55.8 million (50.7–60.8)",
            "5,470 billion (4,830–5,980)"
         )
      ),
      row.names = c(NA, -4L),
      class = "data.frame")
   )

   new_style("count_dec", count_method = 'decimal', count_nsmall = 2)
   expect_equal(
      DF |> format_journal_df(d_type = "count", style_name = "count_dec") # |> dput()
      , structure(list(
         data_space = c("thousands", "thousands_edge", "millions", "billions"),
         clu_fmt = c(
            "999,000.00 (888,888.00–2,222,222.00)",
            "999,999.00 (888,888.00–2,222,222.00)",
            "55.83 million (50.72–60.80)",
            "5,471.70 billion (4,826.60–5,978.60)"
         )
      ),
      row.names = c(NA, -4L),
      class = "data.frame")
   )

   new_style("count_int", count_method = 'int', count_nsmall = 2) # nsmall and sigfig logic should be ignored
   expect_equal(
      DF |> format_journal_df(d_type = "count", style_name = "count_int") # |> dput()
      , structure(list(
         data_space = c("thousands", "thousands_edge", "millions", "billions"),
         clu_fmt = c(
            "999,000 (888,888–2,222,222)",
            "999,999 (888,888–2,222,222)",
            "56 million (51–61)",
            "5,472 billion (4,827–5,979)"
         )
      ),
      row.names = c(NA, -4L),
      class = "data.frame")
   )

   new_style("count_int_thou", count_method = 'int', count_nsmall = 2, count_label_thousands = TRUE)
   expect_equal(
      DF |> format_journal_df(d_type = "count", style_name = "count_int_thou") # |> dput()
      , structure(list(
         data_space = c("thousands", "thousands_edge", "millions", "billions"),
         clu_fmt = c(
            "999 thousand (889–2,222)",
            "1,000 thousand (889–2,222)", # Not sure there's a way around this or if it's right as-is, or if I care
            "56 million (51–61)",
            "5,472 billion (4,827–5,979)"
         )
      ),
      row.names = c(NA, -4L),
      class = "data.frame")
   )
})

# ---- Rate Integration Tests ------------------------------------------------

test_that("fround_clu_triplet works with rates", {
   result1 <- fround_clu_triplet(style_name = 'nature', clu = c(central = 0.0000123, lower = 0.0000098, upper = 0.0000152), d_type = "rate")
   expect_equal(result1$formatted, c(central = "12.3", lower = "9.80", upper = "15.2"))

   result2 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 0.0000123, lower = 0.0000098, upper = 0.0000152), d_type = "rate")
   expect_equal(result2$formatted, c(central = "12·3", lower = "9·80", upper = "15·2"))
})

test_that("format_journal_clu works with rates", {
   expect_equal(
      format_journal_clu(central = 0.0000123, lower = 0.0000098, upper = 0.0000152, d_type = "rate", rate_unit = "deaths"),
      "12.3 deaths (9.80–15.2) per 1 million"
   )
   expect_equal(
      format_lancet_clu(central = 0.0000123, lower = 0.0000098, upper = 0.0000152, d_type = "rate", rate_unit = "cases"),
      "12·3 cases (9·80–15·2) per 1 million"
   )
})


