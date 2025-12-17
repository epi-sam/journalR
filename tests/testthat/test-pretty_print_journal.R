# ---- Integration - Style-Agnostic -----------------------------------------------------------------

test_that("format_mean_df works", {
   DT_test <- data.table::data.table(
      location_id = c(1, 2, 3)
      , mean_1990 = c(0.1234, 0.2345, 0.3456)
      , mean_2000 = c(0.2234, 0.3345, 0.4456)
   )

   DT_result <- format_metric_cols(DT_test, metric = "prop")

   DT_expected <- data.table::data.table(
      location_id = c(1, 2, 3)
      , mean_1990 = c("12.3%", "23.5%", "34.6%")
      , mean_2000 = c("22.3%", "33.5%", "44.6%")
   )

   expect_equal(DT_result, DT_expected)

   DT_test <- data.table::data.table(
      location_id = c(1, 2, 3)
      , mean_1990 = c(1234, 02345, 03456)
      , mean_2000 = c(2234, 03345, 04456)
   )

   style_int <- new_style(style_name = "test_style_int", count_nsmall = 0)
   DT_result <- format_metric_cols(DT_test, metric = "count", style_name = "test_style_int")

   DT_expected <- data.table::data.table(
      location_id = c(1, 2, 3)
      , mean_1990 = c("1,230", "2,350", "3,460")
      , mean_2000 = c("2,230", "3,350", "4,460")
   )

   expect_equal(DT_result, DT_expected)
})



# ---- Integration - Styles -----------------------------------------------------------------

# Lancet styling is hardest to pass - focus here, especially for counts
# Sprinkle in some default Nature styling as well

test_that("fround_clu_triplet works", {
   result1 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 0.2, lower = 0.1, upper = 0.3), metric = "prop")
   expect_type(result1, "list")
   expect_named(result1, c("formatted", "df_mag_row"))
   expect_equal(result1$formatted, c(central = "20·0", lower = "10·0", upper = "30·0"))
   result2 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = -0.02, lower = -0.1, upper = 0.3), metric = "pp")
   expect_equal(result2$formatted, c(central = "–2·0", lower = "–10·0", upper = "30·0"))
   result3 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 9.5e6, lower = 8.9e6, upper = 101e6), metric = "count")
   expect_equal(result3$formatted, c(central = "9·50", lower = "8·90", upper = "101"))
   result4 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 95e6, lower = 94e6, upper = 97e6), metric = "count")
   expect_equal(result4$formatted, c(central = "95·0", lower = "94·0", upper = "97·0"))
   # trickier cases - need sig figs even for small numbers
   result5 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 1, lower = 0.2, upper = 2), metric = "count")
   expect_equal(result5$formatted, c(central = "1·00", lower = "0·200", upper = "2·00"))
   result6 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 10.5, lower = 0.2, upper = 20.3), metric = "count")
   expect_equal(result6$formatted, c(central = "10·5", lower = "0·200", upper = "20·3"))
   # rounding edge case
   result7 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 9995, lower = 9990, upper = 10100), metric = 'count')
   expect_equal(result7$formatted, c(central = "10 000", lower = "9990", upper = "10 100"))
})


# This is the beating heart of the machine - get these right
test_that("format_lancet_clu works", {
   expect_equal(
      format_lancet_clu(central = 0.994, lower = 0.984, upper = 0.998, metric = "prop")
      , "99·4% (98·4–99·8)"
   )
   expect_equal(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), metric = "prop")
      , c("99·4% (98·4–99·8)", "99·4% (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), metric = "prop")
      , c("99·4% (–15·0 to 99·8)", "99·4% (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = c(-0.05, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), metric = "pp")
      , c("a decrease of 5·0 pp (–15·0 to 99·8)", "99·4 pp (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = rep(2e6, 2), lower = rep(.5e6, 2), upper = rep(3e6, 2), metric = "count")
      , c("2·00 million (0·500–3·00)", "2·00 million (0·500–3·00)")
   )
   expect_equal(
      format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), metric = "prop")
      , c("a decrease of 99·4% (98·4–99·8)", "a decrease of 99·4% (98·4–99·8)")
   )
   expect_equal(
      format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), metric = "prop")
      , c("a decrease of 99·4% (98·4–99·8)", "a decrease of 99·4% (98·4–99·8)")
   )
   # rounding edge case
   expect_equal(format_lancet_clu(central = 9995, lower = 9990, upper = 10100, metric = 'count')
                , c("10 000 (9990–10 100)"))
   # round_5_up test
   expect_equal(
      format_journal_clu(central = 1145, lower = 1135, upper = 1155, metric = 'count'), "1,150 (1,140–1,160)"
   )
   # format_journal_clu(central = -0.1321684, lower = -0.235321, upper = -0.056549, metric = 'count')
   # format_journal_df(data.frame(mean = -0.1321684, lower = -0.235321, upper = -0.056549), metric = 'count')
})

test_that("format_nature_clu works", {
   expect_equal(
      format_nature_clu(central = 0.994, lower = 0.984, upper = 0.998, metric = "prop")
      , "99.4% (98.4–99.8)"
   )
   expect_equal(
      format_nature_clu(central = 0.994, lower = -0.984, upper = 0.998, metric = "prop")
      , "99.4% (-98.4 to 99.8)"
   )
})


test_that("format_lancet_clu errors correctly", {
   expect_error(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998), metric = "prop")
      , "Column length mismatch:"
   )
   expect_error(
      format_lancet_clu(central = c(0.994, 0.999), lower = c(0.984, 0.984), upper = c(0.998, 0.998), metric = "prop")
      , "upper is less than/equal to central at index: 2 : \\(0.998 < 0.999\\)"
   )
   expect_error(
      format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), metric = "propeller")
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

   new_style("test_ui", UI_only = TRUE)
   expect_equal(
      format_journal_df(
         df         = DT_count,
         metric     = "count",
         style_name = 'test_ui'
      )
      , structure(
         list(
            location_did = c(1, 1),
            location_name = c("Global", "Global"),
            clu_fmt = c("50.7–60.7 million", "48.6–59.6 billion")
         ),
         row.names = c(NA, -2L),
         class = c("data.frame")
      )
   )

   expect_equal(
      format_lancet_df(
         df          = DT_count,
         metric      = "count"
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
         , metric      = "count"
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
         metric      = "prop",
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
      DF |> format_journal_df(metric = "count", style_name = "nature") # |> dput()
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
      DF |> format_journal_df(metric = "count", style_name = "lab_thou") # |> dput()
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
      DF |> format_journal_df(metric = "count", style_name = "count_dec") # |> dput()
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
      DF |> format_journal_df(metric = "count", style_name = "count_int") # |> dput()
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
      DF |> format_journal_df(metric = "count", style_name = "count_int_thou") # |> dput()
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
   result1 <- fround_clu_triplet(style_name = 'nature', clu = c(central = 0.0000123, lower = 0.0000098, upper = 0.0000152), metric = "rate")
   expect_equal(result1$formatted, c(central = "12.3", lower = "9.80", upper = "15.2"))

   result2 <- fround_clu_triplet(style_name = 'lancet', clu = c(central = 0.0000123, lower = 0.0000098, upper = 0.0000152), metric = "rate")
   expect_equal(result2$formatted, c(central = "12·3", lower = "9·80", upper = "15·2"))
})

test_that("format_journal_clu works with rates", {
   expect_equal(
      format_journal_clu(central = 0.0000123, lower = 0.0000098, upper = 0.0000152, metric = "rate", rate_unit = "deaths"),
      "12.3 deaths (9.80–15.2) per 1 million"
   )
   expect_equal(
      format_lancet_clu(central = 0.0000123, lower = 0.0000098, upper = 0.0000152, metric = "rate", rate_unit = "cases"),
      "12·3 cases (9·80–15·2) per 1 million"
   )
})


# ---- format_metric_cols Integration Tests ------------------------------------------------

test_that("format_metric_cols works with prop - typical case", {
   df <- data.frame(
      location = c("A", "B", "C")
      , mean_2020 = c(0.125, 0.456, 0.789)
   )
   result <- format_metric_cols(df, metric = "prop", style_name = "nature")
   expect_equal(result$mean_2020, c("12.5%", "45.6%", "78.9%"))
})

test_that("format_metric_cols works with prop - edge case small values", {
   df <- data.frame(
      location = c("A", "B")
      , mean_2020 = c(0.001, 0.0005)  # Very small proportions
   )
   result <- format_metric_cols(df, metric = "prop", style_name = "nature")
   expect_equal(result$mean_2020, c("0.1%", "0.1%"))  # Rounded to 1 decimal
})

test_that("format_metric_cols works with pp - typical case", {
   df <- data.frame(
      location = c("A", "B")
      , mean_2020 = c(0.05, -0.03)  # 5pp increase, 3pp decrease
   )
   result <- format_metric_cols(df, metric = "pp", style_name = "nature")
   expect_equal(result$mean_2020, c("5.0 pp", "-3.0 pp"))
})

test_that("format_metric_cols works with pp - edge case crossing zero", {
   df <- data.frame(
      location = c("A", "B")
      , mean_2020 = c(0.001, -0.001)  # Very small changes
   )
   result <- format_metric_cols(df, metric = "pp", style_name = "lancet")
   expect_equal(result$mean_2020, c("0·1 pp", "–0·1 pp"))  # Lancet neg mark
})

test_that("format_metric_cols works with count - typical case with Lancet", {
   df <- data.frame(
      location = c("A", "B", "C")
      , mean_2020 = c(1500, 50000, 1.5e6)
   )
   result <- format_metric_cols(df, metric = "count", style_name = "lancet")
   # 1500 should have NO separator (Lancet rule: <= 9999)
   # 50000 should have thin space separator
   # 1.5e6 should be "1.50 million"
   expect_equal(result$mean_2020[1], "1500")
   expect_true(grepl("50\\s*000", result$mean_2020[2]))  # Has separator
   expect_equal(result$mean_2020[3], "1·50 million")
})

test_that("format_metric_cols works with count - Lancet edge case 9999 boundary", {
   df <- data.frame(
      location = c("A", "B", "C", "D")
      , mean_2020 = c(9994, 10000, 9995, 10005)
   )
   result <- format_metric_cols(df, metric = "count", style_name = "lancet")
   # 9999: no separator (Lancet rule)
   # 10000: has separator
   # 9995: rounds to 10000, should get separator
   # 10005: rounds to 10000, has separator
   expect_equal(result$mean_2020[1], "9990")
   expect_true(grepl("10\\s*000", result$mean_2020[2]))  # 10 000 with thin space
   expect_true(grepl("10\\s*000", result$mean_2020[3]))  # 9995 rounds to 10000
   expect_true(grepl("10\\s*000", result$mean_2020[4]))  # 10005 rounds to 10000
})

test_that("format_metric_cols works with rate - typical case", {
   df <- data.frame(
      location = c("A", "B")
      , mean_2020 = c(0.0000123, 0.0000456)
   )

   result_with_unit <- format_metric_cols(df, metric = "rate", rate_unit = "deaths", style_name = "nature")
   expect_equal(
      result_with_unit$mean_2020
      , c(
         "12.3 deaths per 1 million"
         , "45.6 deaths per 1 million"
      )
   )
})

test_that("format_metric_cols works with rate - edge case different magnitudes", {
   df <- data.frame(
      location = c("A", "B")
      , mean_2020 = c(0.00123, 0.0000000456)  # per 1k vs per 1 billion
   )

   result_with_unit <- format_metric_cols(df, metric = "rate", rate_unit = "cases", style_name = "lancet")
   expect_equal(result_with_unit$mean_2020[1], "12·3 cases per 10,000")
   expect_equal(result_with_unit$mean_2020[2], "45·6 cases per 1 billion")
})
