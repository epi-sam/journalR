# started: 2025 Nov 18 15:17:20
# purpose: a set of style-agnostic formatting helpers

# ---- Private -----------------------------------------------------------------

#' Prepare central, lower, upper value triplets for journal presentation
#'
#' Vectorized preparation of central, lower, upper values.
#' Handles negatives, and swaps ordering where necessary.
#' Casting some negatives as positives allows user control to set
#' `style$neg_str_mean` appropriately.
#'
#' non-exported helper
#'
#' @param triplets [matrix]  with rownames 'central', 'lower', 'upper'
#' @param assert_clu_order [lgl: default TRUE] assert that central, lower, upper relationships are valid
#'
#' @returns [num matrix] matrix with rows 'central', 'lower', 'upper' and columns for each triplet set
#'
#' @examples
#' \dontrun{
#' process_clu_triplet_negatives(
#'    central  = c(0.5, -0.2, -0.5, -2)
#'    , lower  = c(0.3, -0.5, -1.0, -3)
#'    , upper  = c(0.7, 0.1, -0.2, -1)
#' )
#' }
process_clu_triplet_negatives <- function(
      triplets
      , assert_clu_order = TRUE
){

   # lists with two shapes for assertions and processing
   # 1. input  - three vectors of equal length (central, lower, upper)
   # 2. output - triplet sets of (central, lower, upper) values for presentation

   checkmate::assert_matrix(
      triplets
      , mode        = "numeric"
      , nrows       = 3
      , any.missing = FALSE
      , row.names   = "strict"
   )
   assert_x_in_y(x = c("central", "lower", "upper"), y = rownames(triplets))

   if(assert_clu_order == TRUE){
      assert_clu_relationship(
         central = triplets["central", ]
         , lower = triplets["lower", ]
         , upper = triplets["upper", ]
      )
   }

   # process negatives
   triplets <- apply(triplets, 2, function(triplet){

      all_neg     <- all(triplet <= 0)
      central_neg <- (triplet["central"] < 0) & !all_neg

      # If just the mean is negative, invert just the mean
      # - style$neg_str_mean handles the text prefixing
      if(central_neg) triplet["central"] <- triplet["central"] * -1

      # If the triplet is all negative, invert and flip upper, lower values
      if(all_neg) {

         triplet <- triplet * -1
         l_temp  <- triplet[["lower"]]
         u_temp  <- triplet[["upper"]]
         triplet[["lower"]] <- u_temp
         triplet[["upper"]] <- l_temp

         if(assert_clu_order == TRUE){
            assert_clu_relationship(
               central = triplet["central"]
               , lower = triplet["lower"]
               , upper = triplet["upper"]
            )
         }

      }

      return(triplet)
   })

   return(triplets)

}

#' Format and round proportion-ish number
#'
#' non-exported helper
#'
#' @param style_name [chr] style name - controls rounding and
#'   formatting.
#' @param clu [num] numeric vector
#'
#' @returns [chr] formatted string
#' @family vector_formats
#'
#' @examples
#' \dontrun{
#' fround_propish(0.123456789, 'nature')
#' }
fround_props <- function(
      clu
      , style_name
){
   style <- get_style(style_name)
   # hack for floating point rounding issues
   epsilon <- 1e-9
   clu <- clu + epsilon
   clu <- clu * 100
   round(x = clu, digits = style$digits_round_prop) |>
      format(nsmall = style$nsmall, decimal.mark = style$decimal.mark) |>
      trimws()
}

#' Format and round count-ish number
#'
#' non-exported helper
#'
#' @param clu [num] numeric triplet of counts (central, lower, upper)
#' @param style_name [chr] style name - controls rounding and
#'   formatting.
#' @param df_mag [data.frame] magnitude df as returned by `set_magnitude()`
#'
#' @returns [chr] formatted string vector
#' @family vector_formats
#'
#' @examples
#' \dontrun{
#' fround_countish(clu = c(12345, 67890, 6.6666e6), df_mag = set_magnitude(12345), style_name = 'nature')
#' }
fround_count <- function(
      clu
      , style_name
      , df_mag
) {

   if(any(clu < 0)) stop("Formatting counts under 0 not yet supported: ", toString(clu))
   checkmate::assert_data_frame(df_mag, nrows = 1)

   style <- get_style(style_name)
   digits_sigfig_count <- style[["digits_sigfig_count"]]
   nsmall              <- style[["nsmall"]]
   decimal.mark        <- style[["decimal.mark"]]
   big.mark_count      <- style[["big.mark_count"]]
   is_lancet           <- style[["is_lancet"]]

   unlist(
      lapply(clu, function(x_i){

         # hack for floating point rounding issues
         epsilon <- 1e-9
         x_i <- x_i + epsilon

         big.mark_count_og <- data.table::copy(big.mark_count)

         # lancet spec for counts under 10,000
         if(is_lancet && abs(round(x_i, 0)) <= 9999) {
            big.mark_count <- ""
            nsmall <- 0
         }

         # Need accurate sig figs for numbers <= digits_sigfig_count e.g.
         # c(10.5, 0.2, 20.3) should become c("10.5", "0.200", "20.3") if
         # digits_sigfig_count = 3. Ensure number of digits are not counted as
         # scientific notation e.g. '6e+06' should count as 7 digits, not 5
         # characters.
         x_i_rnd <- round(x_i)
         digits_x_i_whole <- nchar(format(x_i_rnd, scientific = FALSE)) - ifelse(x_i_rnd == 0, 1, 0) # account for zero
         if(abs(x_i) > 0 & digits_x_i_whole <= digits_sigfig_count) {
            nsmall <- digits_sigfig_count - digits_x_i_whole
         }

         # x divided
         x_i_div <- signif(
            x        = (x_i / df_mag$denom)
            , digits = digits_sigfig_count
         )
         checkmate::assert_numeric(x_i, len = 1)

         # Ensure e.g. 95.0 million (89.0-101) retains same sigfigs across set of values
         if(
            nchar(format(x_i_div, scientific = FALSE)) >= digits_sigfig_count
            & nsmall > 0
            & !grepl("\\.", x_i_div)
         ){
            nsmall <- 0
         }

         # Edge case - thousands with high digit precision keep correct nsmall
         if(
            df_mag$mag == "t"
            & nchar(format(x_i_rnd, scientific = FALSE)) >= digits_sigfig_count
            & nsmall == 0
         ){
            nsmall <- digits_sigfig_count - (nchar(x_i_div) - nchar(decimal.mark)) + nchar(decimal.mark)
         }

         x_i_chr <- format(
            x              = x_i_div
            , decimal.mark = decimal.mark
            , big.mark     = big.mark_count
            , nsmall       = nsmall
            , scientific   = FALSE
         ) |>
            trimws()

         # catch cases where counts e.g. 9999 would round up to 10000 and we lose the big.mark
         if(nchar(format(x_i_div, scientific = FALSE)) > digits_x_i_whole){
            x_i_chr <- format(
               x              = x_i_div
               , decimal.mark = decimal.mark
               , big.mark     = big.mark_count_og
               , nsmall       = nsmall
               , scientific   = FALSE
            ) |>
               trimws()
         }
         checkmate::assert_character(x_i_chr, len = 1)

         # zero pad formatted string to correct sig figs if too short
         if(
            nchar(x_i_chr) < (digits_sigfig_count + nchar(decimal.mark))
            & nsmall > 0
         ) {
            # pad   <- required nchar                             - current nchar
            n_zeros <- (digits_sigfig_count + nchar(decimal.mark)) - nchar(x_i_chr)
            zeros   <- paste0(rep.int("0", n_zeros), collapse = '')
            x_i_chr <- sprintf("%s%s", x_i_chr, zeros)
         }

         return(unname(x_i_chr)) # not sure how extra naming snuck in
      })
   )
}

# ---- Public -----------------------------------------------------------------

#' Format vector of items with Oxford comma
#'
#' @param vec [any] vector of items to format
#' @param sep [chr: default "and"] separator before last item
#'
#' @returns [chr] formatted string with Oxford comma
#' @export
#' @family vector_formats
#'
#' @examples
#' format_oxford_comma(1:2)
#' format_oxford_comma(1:3)
#' format_oxford_comma(1:3, "or")
format_oxford_comma <- function(vec, sep = "and") {
   checkmate::assert_vector(vec, min.len = 1)
   checkmate::assert_string(sep)
   n       <- length(vec)
   set1    <- toString(vec[1:(n - 1)])
   if(n > 2) {
      sep <- sprintf(", %s ", sep)
   } else {
      sep <- sprintf(" %s ", sep)
   }
   set2    <- vec[n]
   str     <- sprintf("%s%s%s", set1, sep, set2)
   return(str)
}

#' Format and round
#'
#' Unaware of data-type or schema, just a hard-coded git-er-done function.
#'
#' @param x [num] numeric vector
#' @param digits [integer] passed to `round()`
#' @param nsmall [integer] passed to `format()`
#' @param decimal.mark [chr] passed to `format()`
#'
#' @return [chr] formatted string
#' @export
#' @family vector_formats
#'
#' @examples
#' fround(0.123456789)
#' fround(0.123456789, digits = 3)
#' fround(0.123456789, digits = 3, nsmall = 4)
fround <- function(x, digits = 1L, nsmall = 1L, decimal.mark = "."){
   # Format and round, no label
   round(x, digits) |>
      format(nsmall = nsmall, decimal.mark = decimal.mark) |>
      trimws()
}



#' Format and round with data-type suffix
#'
#' Unaware of schema, just a hard-coded git-er-done function.
#'
#' @param x [num] numeric value
#' @param d_type [chr c('prop', 'pp', or 'count')] data type - proportion,
#'   percentage point or count
#' @param digits [integer: default 1L] passed to `round()`
#' @param nsmall [integer: default 1L] passed to `format()`
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#'
#' @return [chr] formatted string
#' @export
#' @family vector_formats
#'
#' @examples
#' fround_dtype(0.123456789)
#' fround_dtype(0.123456789, 'pp', 3, 4)
#' fround_dtype(c(55.8346, 123.456789), 'count', 3, 4, ".")
fround_dtype <- function(
      x
      , d_type       = "prop"
      , digits       = 1L
      , nsmall       = 1L
      , decimal.mark = "."
){

   checkmate::assert_numeric(x)
   checkmate::assert_integerish(digits, len = 1, lower = 0)
   checkmate::assert_integerish(nsmall, len = 1, lower = 0)
   checkmate::assert_character(decimal.mark, len = 1)

   # select data-type label
   suffix <- get_data_type_labels()[[d_type]]

   # round and format
   x_fmt <- x  |>
      round(digits) |>
      format(nsmall = nsmall, decimal.mark = decimal.mark) |>
      trimws() |>
      sprintf("%s%s", ... = _ , suffix)
   return(x_fmt)
}

#' Format magnitude
#'
#' Format a numeric vector into a string with specified magnitude (billion,
#' million, thousand).
#'
#' Unaware of schema, just a hard-coded git-er-done function.
#'
#' Caution - thousands magnitude is not Lancet compliant.
#'
#' @param x [num] numeric vector
#' @param digits [int: default 1L] passed to `round()`
#' @param nsmall [int: default 1L] passed to `format()`
#' @param mag [chr c("b", "m", "t")] magnitude (billion, million,
#'   thousand) passed to set_magnitude()
#' @param label_thousands [lgl: default FALSE] allow thousands magnitude?  Not
#'   Lancet-valid. Passed to `set_magnitude()`
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#'
#' @return [chr] formatted string
#' @export
#' @family vector_formats
#' @family magnitudes
#'
#' @examples
#' fmt_magnitude(123456789)
fmt_magnitude <- function(
      x
      , digits          = 1
      , nsmall          = 1
      , decimal.mark    = "."
      , mag             = NULL
      , label_thousands = FALSE
){

   checkmate::assert_numeric(x)
   checkmate::assert_vector(x)
   checkmate::assert_logical(label_thousands, len = 1)
   checkmate::assert_integerish(digits, len = 1, lower = 0)
   checkmate::assert_integerish(nsmall, len = 1, lower = 0)

   df_mag <- set_magnitude(
      x
      , mag             = mag
      , label_thousands = label_thousands
      , verbose         = FALSE
   )

   # hack for floating point rounding issues
   epsilon <- 1e-9
   x <- x + epsilon

   x_fmt <-
      round(x / df_mag$denom, digits = digits) |>
      format(nsmall = nsmall, decimal.mark = decimal.mark) |>
      trimws()|>
      sprintf("%s %s", ... = _, df_mag$mag_label) |>
      trimws()

   return(x_fmt)
}



## Lancet Family  -----------------------------------------------------

#' Format and round with data-type suffix
#'
#' Lancet-specific wrapper for `fround_dtype()`, using mid-dot as decimal mark.
#'
#' @param x [num] numeric value
#' @param d_type [chr c('prop', 'pp', or 'count')] data type - proportion, percentage point or count
#' @param digits [integer: default 1L] passed to `round()`
#' @param nsmall [integer: default 1L] passed to `format()`
#' @param decimal.mark [chr: default mid_dot()] decimal mark passed to `format()`
#'
#' @return [chr] formatted string
#' @export
#' @family vector_formats
#'
#' @examples
#' fround_dtype_lancet(0.123456789)
#' fround_dtype_lancet(0.123456789, 'pp', 3, 4)
#' fround_dtype_lancet(c(55.8346, 123.456789), 'count', 3, 4, ".")
fround_dtype_lancet <- function(
      x
      , d_type       = "prop"
      , digits       = 1L
      , nsmall       = 1L
      , decimal.mark = mid_dot()
){

   fround_dtype(
      x              = x
      , d_type       = d_type
      , digits       = digits
      , nsmall       = nsmall
      , decimal.mark = decimal.mark
   )

}
