# 2025 Nov 22 - Deprecated - RIP OG formatter - effective but messy
# fround_count <- function(
#       clu
#       , style_name
#       , df_mag
# ) {
#
#    if(any(clu < 0)) stop("Formatting counts under 0 not yet supported: ", toString(clu))
#    checkmate::assert_data_frame(df_mag, nrows = 1)
#
#    style <- get_style(style_name)
#    count_digits_sigfig <- style[["count_digits_sigfig"]]
#    nsmall              <- style[["count_nsmall"]]
#    decimal.mark        <- style[["decimal.mark"]]
#    big.mark_count      <- style[["big.mark_count"]]
#    is_lancet           <- style[["is_lancet"]]
#
#    unlist(
#       lapply(clu, function(x_i){
#
#          # hack for floating point rounding issues
#          epsilon <- 1e-9
#          x_i <- x_i + epsilon
#
#          big.mark_count_og <- data.table::copy(big.mark_count)
#
#          # lancet spec for counts under 10,000
#          if(is_lancet && abs(round(x_i, 0)) <= 9999) {
#             big.mark_count <- ""
#             nsmall <- 0
#          }
#
#          # Need accurate sig figs for numbers <= count_digits_sigfig e.g.
#          # c(10.5, 0.2, 20.3) should become c("10.5", "0.200", "20.3") if
#          # count_digits_sigfig = 3. Ensure number of digits are not counted as
#          # scientific notation e.g. '6e+06' should count as 7 digits, not 5
#          # characters.
#          x_i_rnd <- round(x_i)
#          digits_x_i_whole <- nchar(format(x_i_rnd, scientific = FALSE)) - ifelse(x_i_rnd == 0, 1, 0) # account for zero
#          if(abs(x_i) > 0 & digits_x_i_whole <= count_digits_sigfig) {
#             nsmall <- count_digits_sigfig - digits_x_i_whole
#          }
#
#          # x divided
#          x_i_div <- signif(
#             x        = (x_i / df_mag$denom)
#             , digits = count_digits_sigfig
#          )
#          checkmate::assert_numeric(x_i, len = 1)
#
#          # Ensure e.g. 95.0 million (89.0-101) retains same sigfigs across set of values
#          if(
#             nchar(format(x_i_div, scientific = FALSE)) >= count_digits_sigfig
#             & nsmall > 0
#             & !grepl("\\.", x_i_div)
#          ){
#             nsmall <- 0
#          }
#
#          # Edge case - thousands with high digit precision keep correct nsmall
#          if(
#             df_mag$mag == "t"
#             & nchar(format(x_i_rnd, scientific = FALSE)) >= count_digits_sigfig
#             & nsmall == 0
#          ){
#             nsmall <- count_digits_sigfig - (nchar(x_i_div) - nchar(decimal.mark)) + nchar(decimal.mark)
#          }
#
#          x_i_chr <- format(
#             x              = x_i_div
#             , decimal.mark = decimal.mark
#             , big.mark     = big.mark_count
#             , nsmall       = nsmall
#             , scientific   = FALSE
#          ) |>
#             trimws()
#
#          # catch cases where counts e.g. 9999 would round up to 10000 and we lose the big.mark
#          if(nchar(format(x_i_div, scientific = FALSE)) > digits_x_i_whole){
#             x_i_chr <- format(
#                x              = x_i_div
#                , decimal.mark = decimal.mark
#                , big.mark     = big.mark_count_og
#                , nsmall       = nsmall
#                , scientific   = FALSE
#             ) |>
#                trimws()
#          }
#          checkmate::assert_character(x_i_chr, len = 1)
#
#          # zero pad formatted string to correct sig figs if too short
#          if(
#             nchar(x_i_chr) < (count_digits_sigfig + nchar(decimal.mark))
#             & nsmall > 0
#          ) {
#             # pad   <- required nchar                             - current nchar
#             n_zeros <- (count_digits_sigfig + nchar(decimal.mark)) - nchar(x_i_chr)
#             zeros   <- paste0(rep.int("0", n_zeros), collapse = '')
#             x_i_chr <- sprintf("%s%s", x_i_chr, zeros)
#          }
#
#          return(unname(x_i_chr)) # not sure how extra naming snuck in
#       })
#    )
# }
