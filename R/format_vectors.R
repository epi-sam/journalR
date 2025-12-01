# started: 2025 Nov 18 15:17:20
# purpose: a set of style-agnostic formatting helpers

# ---- Private -----------------------------------------------------------------

#' Prepare central, lower, upper value triplets for journal presentation
#'
#' Vectorized preparation of central, lower, upper values.
#' Handles negatives, and swaps ordering where necessary.
#' Casting some negatives as positives allows user control to set
#' `style$neg_mark_mean` appropriately.
#'
#' non-exported helper
#'
#' @param triplets [matrix]  with rownames 'central', 'lower', 'upper'
#' @param assert_clu_order [lgl: default TRUE] assert that central, lower, upper relationships are valid
#'
#' @returns [num matrix] matrix with rows 'central', 'lower', 'upper' and columns for each triplet set
#' @keywords internal
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
      # - style$neg_mark_mean handles the text prefixing
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
#' "well that was easy, how hard could counts be?"
#'
#' @param clu [num] numeric triplet of proportions (central, lower, upper)
#' @param style_name [chr] style name - controls rounding and
#'   formatting.
#' @param idx [int] row index when called from format_journal_clu(), NULL for standalone
#'
#' @returns [chr] formatted string vector
#' @family vector_formats
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fround_props(c(0.123, 0.100, 0.150), 'nature')
#' }
fround_props <- function(
      clu
      , style_name
      , idx = NULL
){

   # === Get df_mag from state or compute standalone ===
   if (is_df_mag_active()) {
      if (is.null(idx)) {
         stop("idx required when df_mag state is active", call. = FALSE)
      }
      df_mag <- get_df_mag_row(idx)
   } else {
      # Standalone mode: compute magnitude from central value
      df_mag <- set_magnitude(clu[1], d_type = "prop")  # clu[1] is central
   }

   checkmate::assert_data_frame(df_mag, nrows = 1)

   style <- get_style(style_name)

   if (style$round_5_up) {
      clu <- clu + 1e-9
   }

   # Use denominator from df_mag instead of hard-coded * 100
   clu <- clu / df_mag$denom

   round(x = clu, digits = style$prop_digits_round) |>
      format(nsmall = style$prop_nsmall, decimal.mark = style$decimal.mark) |>
      trimws()
}

#' Format and round count-ish number
#'
#' non-exported helper
#'
#' @param clu [num] numeric triplet of counts (central, lower, upper)
#' @param style_name [chr] style name - controls rounding and
#'   formatting.
#' @param idx [int] row index when called from format_journal_clu(), NULL for standalone
#'
#' @returns [chr] formatted string vector
#' @family vector_formats
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fround_count(clu = c(12345, 67890, 6.6666e6), style_name = 'nature', idx = 1)
#' }
fround_count <- function(
      clu,
      style_name,
      idx = NULL
) {

   if(any(clu < 0))
      stop("Counts < 0 not yet supported: ", toString(clu))

   # === NEW: Get df_mag from state or compute standalone ===
   if (is_df_mag_active()) {
      if (is.null(idx)) {
         stop("idx required when df_mag state is active", call. = FALSE)
      }
      df_mag <- get_df_mag_row(idx)
   } else {
      # Standalone mode: compute magnitude from central value
      df_mag <- set_magnitude(clu[1], d_type = "count")  # clu[1] is central
   }

   checkmate::assert_data_frame(df_mag, nrows = 1)

   style <- get_style(style_name)

   method        <- style[["count_method"]]
   sigfig        <- style[["count_digits_sigfig"]]
   nsmall        <- style[["count_nsmall"]]
   big.mark_base <- style[["count_big.mark"]]
   force_trail   <- style[["count_pad_sigfigs"]]
   decimal.mark  <- style[["decimal.mark"]]
   round_5_up    <- style[["round_5_up"]]
   is_lancet     <- style[["is_lancet"]]
   label_thousands <- style[["label_thousands"]]

   # === NEW: Magnitude edge-case detection (SINGLE SOURCE OF TRUTH) ===
   # Check if rounding the central value pushes it across a magnitude boundary
   # e.g., 999,999 -> 1,000,000 should become "1.00 million" not "1,000,000"

   central_val <- clu[1]
   if (round_5_up) {
      central_val <- central_val + 1e-9
   }
   central_scaled <- central_val / df_mag$denom

   central_rounded <- switch_strict(
      method,
      "sigfig"  = signif(central_scaled, sigfig),
      "decimal" = round(central_scaled, digits = nsmall),
      "int"     = round(central_scaled, digits = 0)
   )

   # Rescale to original units to check magnitude
   central_at_original_scale <- central_rounded * df_mag$denom

   # Recalculate magnitude based on rounded value
   df_mag_new <- set_magnitude(
      x = central_at_original_scale,
      d_type = "count",
      mag = NULL,
      label_thousands = label_thousands,
      verbose = FALSE
   )

   # === NEW: Update state if magnitude changed ===
   if (is_df_mag_active() && !identical(df_mag$mag, df_mag_new$mag)) {
      update_df_mag_state(
         idx = idx,
         mag = df_mag_new$mag,
         mag_label = df_mag_new$mag_label,
         denom = df_mag_new$denom
      )
      # Use the new magnitude for formatting
      df_mag <- df_mag_new
   }


   # Use the (possibly updated) df_mag for the actual formatting
   format_one_count <- function(x) {

      # --- 0 guard against Lancet edge case
      x_raw <- data.table::copy(x)

      # --- 1 apply round-5-up rule
      if (round_5_up) {
         x <- x + 1e-9
      }

      # --- 2 scale by magnitude denom
      x_sc <- x / df_mag$denom

      # if(x_sc < 1) browser()

      # --- 3 apply Lancet rule (per value)
      big.mark <- big.mark_base

      if (is_lancet && abs(round(x, 0)) <= 9999) {
         big.mark <- ""
      }

      # --- 4 formatting

      x_chr <- switch_strict(

         method

         , "sigfig" = { # sigfig is the messiest

            x_fmt <- signif(x_sc, sigfig)

            # Lancet edge case
            if (is_lancet && x_raw <= 9999 && x_fmt >= 10000) {
               big.mark <- big.mark_base
            }

            x_chr <- format(
               x_fmt,
               scientific   = FALSE,
               decimal.mark = decimal.mark,
               big.mark     = big.mark
            )

            # --- 5 apply zero-padding logic

            if (force_trail) {

               # split integer & decimal parts
               if (grepl(decimal.mark, x_chr, fixed = TRUE)) {
                  parts <- strsplit(x_chr, decimal.mark, fixed = TRUE)[[1]]
                  int_part <- parts[1]
                  dec_part <- parts[2]
               } else {
                  int_part <- x_chr
                  dec_part <- ""
               }

               # remove separators
               numeric_clean <- gsub("[^0-9]", "", paste0(int_part, dec_part))

               # strip leading zeros – they are not significant
               numeric_clean <- sub("^0+", "", numeric_clean)

               # how many sig figs currently?
               current_sf <- nchar(numeric_clean)

               # how many more needed?
               needed <- max(sigfig - current_sf, 0)

               if (needed > 0) {
                  # ensure a decimal exists
                  if (!grepl(decimal.mark, x_chr, fixed = TRUE)) {
                     x_chr <- paste0(x_chr, decimal.mark)
                     dec_part <- ""
                  }

                  # pad zeros onto decimal side
                  x_chr <- paste0(x_chr, strrep("0", needed))
               }
            }

            # Institute prefers e.g. c(2, 0.5, 3) rounded to 2.00 (0.500-3.00)
            # - Current behavior delivers this output
            # - Build control flow her to allow 2.00 (0.50-3.00) if desired
            # if(grepl(decimal.mark, x_chr)){
            #    x_chr <- substr(x_chr, 1, sigfig + nchar(decimal.mark))
            # }

            return(trimws(x_chr))
         }

         , "decimal" = {

            x_fmt <- round(x_sc, digits = nsmall)

            x_chr <- format(
               x_fmt,
               nsmall       = nsmall,
               decimal.mark = decimal.mark,
               big.mark     = big.mark,
               scientific   = FALSE
            )
            trimws(x_chr)
         }

         , "int" = {

            x_fmt <- round(x_sc, digits = 0)

            x_chr <- format(
               x_fmt,
               decimal.mark = decimal.mark,
               big.mark     = big.mark,
               scientific   = FALSE
            )
            trimws(x_chr)
         }
      )

      trimws(x_chr)
   }

   unname(vapply(clu, format_one_count, FUN.VALUE = character(1)))
}



#' Format and round a single central/lower/upper value set by magnitude without
#' units.
#'
#' `central` could be mean/median/point_estimate. `d_type` is required (count
#' data requires nuanced logic), but labels are not returned.
#'
#' Format and round without unit labeling
#' - Use `format_lancet_clu()` for unit labels
#'
#' @param clu [num] a numeric triplet of three values in central/lower/upper
#'   order.
#' @param d_type [chr c('prop', 'pp', or 'count')] data type - proportion,
#'   percentage point or count
#' @param idx [int] row index when called from format_journal_clu(), NULL for standalone
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'   formatting.
#' @return [chr] formatted string (vectorized)
#' @family styled_formats
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fround_clu_triplet(clu = c(central = 0.2, lower = 0.1, upper = 0.3)
#' , d_type = "prop")
#' fround_clu_triplet(clu = c(central = 0.2, lower = -0.1, upper = 0.3)
#' , d_type = "pp")
#' fround_clu_triplet(clu = c(central = 95e6, lower = 89e6, upper = 101e6)
#' , d_type = "count", idx = 1)
#' fround_clu_triplet(clu = c(central = 95e6, lower = 1e5, upper = 101e9)
#' , d_type = "count", idx = 2)
#' fround_clu_triplet(clu = c(central = 678901, lower = 123456, upper = 6e6)
#' , d_type = "count", idx = 3)
#' }
fround_clu_triplet <- function(
      clu
      , d_type
      , style_name = "nature"
      , idx        = NULL
) {

   style  <- get_style(style_name)
   d_type <- assert_data_type(d_type)

   checkmate::assert_vector(clu, len = 3)
   checkmate::assert_numeric(clu, len = 3)
   if(style$assert_clu_order == TRUE){
      assert_clu_relationship(clu[1], clu[2], clu[3])
   }

   clu_fmt <- switch_strict(
      d_type
      , "prop"  = fround_props(clu = clu, style_name = style_name, idx = idx)
      , "pp"    = fround_props(clu = clu, style_name = style_name, idx = idx)
      , "count" = fround_count(clu = clu, style_name = style_name, idx = idx)
   )

   names(clu_fmt) <- names(clu)

   # replace negative sign
   # - This needs to be done here, not in format_journal_clu() in case
   #   fround_clu_triplet() is called by the user
   clu_fmt <- unlist(lapply(clu_fmt, function(x_i_chr) {
      sub("^-", style$neg_mark_UI, x_i_chr)
   }))

   return(clu_fmt)
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

   set2 <- vec[n]
   str  <- sprintf("%s%s%s", set1, sep, set2)

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
   suffix <- get_data_type_labels(d_type)

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
#' @param d_type [chr] data type: "prop", "pp", "count", "rate" (required)
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
#' fmt_magnitude(123456789, d_type = "count")
fmt_magnitude <- function(
      x
      , d_type
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
      , d_type          = d_type
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
