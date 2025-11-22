# Recommended Data Flow
# (1) validate + extract style
# (2) scale by df_mag
# (3) format based on chosen method
# (4) apply padding if needed
# (5) assemble into CLU string 'x (l -- u)'

style <- list(
   # count_method = "sigfig"
   count_method = "decimal"
   # count_method = "int"
   , digits_sigfig_count = 3
   , nsmall_count = 3
   # , round_digits = 1
   , decimal.mark = "."
   , big.mark_count = ","
   , pad_count_sigfigs = TRUE
)

clu1 <- c(55800, 50700, 60700)
clu2 <- c(547156000, 48618900, 596168900)
clu3 <- c(central = 11111, lower = 5555, upper = 6666666)
clu4 <- c(central = 5, lower = 1.5, upper = 1234)

df_mag1 <- set_magnitude(clu1[1])
df_mag2 <- set_magnitude(clu2[1])
df_mag3 <- set_magnitude(clu3[1], label_thousands = T, verbose = FALSE)
df_mag3.1 <- set_magnitude(clu3[1], label_thousands = F, verbose = FALSE)
df_mag4 <- set_magnitude(clu4[1])

fround_count(clu1, 'sam', df_mag1)
fround_count(clu2, 'sam', df_mag2)
fround_count(clu3, 'sam', df_mag3)
fround_count(clu3, 'sam', df_mag3.1)
fround_count(clu4, 'sam', df_mag4)

fround_count <- function(
      clu,
      style_name,
      df_mag
) {

   if(any(clu < 0))
      stop("Counts < 0 not yet supported: ", toString(clu))

   checkmate::assert_data_frame(df_mag, nrows = 1)

   # style <- get_style(style_name)

   method        <- style[["count_method"]]
   sigfig        <- style[["digits_sigfig_count"]]
   nsmall        <- style[["nsmall_count"]]
   # round_digits  <- style[["round"]]
   decimal.mark  <- style[["decimal.mark"]]
   big.mark      <- style[["big.mark_count"]]
   force_trail   <- style[["pad_count_sigfigs"]]

   format_one <- function(x) {

      # apply magnitude scaling
      x_sc <- x / df_mag$denom

      # step 2: main formatting logic
      if(method == "sigfig") {

         x_fmt <- signif(x_sc, sigfig)

         x_chr <- format(
            x_fmt,
            scientific   = FALSE,
            decimal.mark = decimal.mark,
            big.mark     = big.mark
         )

         if(force_trail) {

            # this is a problematic search string
            rgx_decimal <- paste0("\\", decimal.mark)

            # split at decimal mark
            if(grepl(rgx_decimal, x_chr)) {
               parts <- strsplit(x_chr, decimal.mark, fixed = TRUE)[[1]]
               int_part    <- parts[1]
               dec_part    <- parts[2]
            } else {
               int_part <- x_chr
               dec_part <- ""
            }

            len_dec_part <- if(dec_part == "") 0 else nchar(dec_part)

            # count digits before decimal (ignore separators)
            digits_before <- nchar(gsub("[^0-9]", "", int_part))

            # how many decimals are needed?
            needed <- max(sigfig - digits_before, 0)

            # ensure we have exactly `needed` decimals
            if(needed > 0) {
               # zeros_to_add <- needed - nchar(dec_part)
               zeros_to_add <- needed - len_dec_part
               if(zeros_to_add > 0) {
                  dec_part <- paste0(dec_part, strrep("0", zeros_to_add))
               }
               x_chr <- paste0(int_part, decimal.mark, dec_part)
            } else {
               # no decimals needed → strip decimals entirely
               x_chr <- int_part
            }
         }

         return(trimws(x_chr))

      }  else if(method == "decimal") {

         x_fmt <- round(x_sc, digits = nsmall)

         x_chr <- format(
            x_fmt,
            nsmall       = nsmall,
            decimal.mark = decimal.mark,
            big.mark     = big.mark,
            scientific   = FALSE
         )

      } else if(method == "int") {

         x_fmt <- round(x_sc, digits = 0)

         x_chr <- format(
            x_fmt,
            decimal.mark = decimal.mark,
            big.mark     = big.mark,
            scientific   = FALSE
         )

      } else {
         stop("Unknown formatting method: ", method)
      }

      trimws(x_chr)
   }

   unname(vapply(clu, format_one, FUN.VALUE = character(1)))
}
