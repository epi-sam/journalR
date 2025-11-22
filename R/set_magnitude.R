#' Define magnitude, magnitude label and denominator for a vector of numeric
#' values.
#'
#' Support function used on _central_ (e.g. mean) values for later formatting
#' functions.
#'
#' 'Thousands' label is not a Lancet-valid, which uses ddd<narrow-space>ddd
#' format.  See `fround_count()` for details.
#'
#' @param x [num] numeric vector
#' @param mag [chr: default NULL c(NULL, "T", "B", "M")] NULL (auto-detect),
#'   otherwise user-override (not recommended) - (M)illions or (B)illions or
#'   (T)housands (thousands are not Lancet-valid)
#' @param label_thousands [lgl: default FALSE] allow (T)housands magnitude?  Not
#'   Lancet-valid.
#' @param verbose [lgl: default TRUE] warn if label_thousands is TRUE
#'
#' @return [data.frame] with vector elements: mag, mag_label, and denom Each vector
#'   element has one item per length(x)
#' @seealso [fround_count()]
#' @export
#' @family magnitudes
#'
#' @examples
#' set_magnitude(c(1e-6, 1, 1e3, 1e6, 1e9))
set_magnitude <- function(
      x
      , mag             = NULL
      , label_thousands = FALSE
      , verbose         = TRUE
){

   checkmate::assert_numeric(x)
   checkmate::assert_vector(x)
   checkmate::assert_character(mag, len = 1, null.ok = TRUE)
   checkmate::assert_logical(label_thousands, len = 1)
   checkmate::assert_logical(verbose, len = 1)

   if(label_thousands & verbose) warning("'thousands' magnitude is not Lancet-valid")

   # vector of magnitudes
   if(is.null(mag)){
      # auto-detect
      mag <- unlist(lapply(x, function(x_i){
         if      (abs(x_i) >= 1e9)                    mag <- "b"
         else if (abs(x_i) >= 1e6)                    mag <- "m"
         else if (abs(x_i) >= 1e3 && label_thousands) mag <- "t"  # not Lancet-valid
         else mag <- "" # default
      }))

   } else {
      # user override (not recommended)
      mag <- rep.int(mag, length(x))
   }

   mag <- tolower(mag)

   # printable form
   mag_label <- unlist(lapply(mag, function(mag_i){
      switch_strict(
         mag_i
         , "b"    = "billion "
         , "m"    = "million "
         , "t"    = "thousand " # not Lancet-valid
         , .empty = ""
      )
   }))

   denom <- unlist(lapply(mag, function(mag_i){
      switch_strict(
         mag_i
         , "b"    = 1e9
         , "m"    = 1e6
         , "t"    = 1e3 # not Lancet-valid
         , .empty = 1
      )
   }))

   df_mag <- data.frame(mag = mag, mag_label = mag_label, denom = denom)

   return(df_mag)
}



