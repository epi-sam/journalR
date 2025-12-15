# ---- Helper Functions (Internal) --------------------------------------------

#' Set magnitude for proportion-family metrics
#'
#' Proportions and percentage points don't use magnitude scaling.
#' Always returns empty magnitude codes.
#'
#' @param x [num] numeric vector
#' @param mag [chr: default NULL] magnitude override: NULL (auto: use percentage conversion), "raw" (no scaling, use as-is)
#' @param verbose [lgl: default TRUE] verbose warnings
#'
#' @return [data.frame] with columns: mag, mag_label, denom
#' @keywords internal
#' @family magnitudes
#'
set_magnitude_prop <- function(x, mag = NULL, verbose = TRUE) {
   checkmate::assert_numeric(x)
   checkmate::assert_vector(x)
   checkmate::assert_logical(verbose, len = 1)



   # Handle magnitude override
   if (is.null(mag)) {
      if (any(abs(x) > 1)) {
         .ex <- x[abs(x) > 1]
         stop("Proportion values must be between -1 and +1. Found values outside range, e.g.: ", .ex[1], call. = FALSE)
      }
      # Default: convert proportions to percentages (multiply by 100, so divide by 1e-2)
      denom_val <- 1e-2
   } else {
      mag <- tolower(mag)
      if (mag == "as-is") {
         if (any(abs(x) > 100)) {
            .ex <- x[abs(x) > 100]
            stop("Proportion values must be between -100 and +100. Found values outside range, e.g.: ", .ex[1], call. = FALSE)
         }
         # User override: use proportions as-is (no scaling)
         denom_val <- 1
      } else {
         stop("Invalid mag for proportions. Use NULL (default percentage conversion) or 'as-is' (no scaling).", call. = FALSE)
      }
   }

   n <- length(x)
   df_mag <- data.frame(
      mag                = rep("", n)
      , mag_label        = rep("", n)
      , denom            = rep(denom_val, n)
      , stringsAsFactors = FALSE
   )

   return(df_mag)
}

#' Set magnitude for count-space numbers
#'
#' Counts are values >= 1 that are scaled down by dividing by large denominators.
#' E.g., 55,831,000 / 1e6 = 55.831 "million"
#'
#' @param x [num] numeric vector
#' @param mag [chr: default NULL] magnitude override: NULL (auto), "t", "m", "b"
#' @param count_label_thousands [lgl: default FALSE] allow "thousands" magnitude (not Lancet-valid)
#' @param verbose [lgl: default TRUE] verbose warnings
#'
#' @return [data.frame] with columns: mag, mag_label, denom
#' @keywords internal
#' @family magnitudes
#'
set_magnitude_count <- function(x, mag = NULL, count_label_thousands = FALSE, verbose = TRUE) {
   checkmate::assert_numeric(x)
   checkmate::assert_vector(x)
   checkmate::assert_character(mag, len = 1, null.ok = TRUE)
   checkmate::assert_logical(count_label_thousands, len = 1)
   checkmate::assert_logical(verbose, len = 1)

   # Assert count values are greater than or equal to 0
   if (any(x < 0)) {
      .ex <- x[x < 0]
      stop("Count values must be positive or 0. Found non-positive values, e.g.: ", .ex[1], call. = FALSE)
   }

   if (count_label_thousands && verbose) {
      warning("'thousands' magnitude is not Lancet-valid", call. = FALSE)
   }

   # Auto-detect or user override
   if (is.null(mag)) {
      mag <- unlist(lapply(x, function(x_i) {
         if      (abs(x_i) >= 1e9)                          "b"
         else if (abs(x_i) >= 1e6)                          "m"
         else if (abs(x_i) >= 1e3 && count_label_thousands) "t"
         else ""
      }))
   } else {
      mag <- rep.int(mag, length(x))
   }

   mag <- tolower(mag)

   # Generate labels
   mag_label <- unlist(lapply(mag, function(mag_i) {
      switch_strict(
         mag_i
         , "b"    = "billion "
         , "m"    = "million "
         , "t"    = "thousand "
         , .empty = ""
      )
   }))

   # Generate denominators
   denom <- unlist(lapply(mag, function(mag_i) {
      switch_strict(
         mag_i
         , "b"    = 1e9
         , "m"    = 1e6
         , "t"    = 1e3
         , .empty = 1
      )
   }))

   df_mag <- data.frame(
      mag                = mag
      , mag_label        = mag_label
      , denom            = denom
      , stringsAsFactors = FALSE
   )

   return(df_mag)
}

#' Set magnitude for rate-space numbers
#'
#' Rates are values < 1 that are scaled up by dividing by small denominators (reciprocals).
#' E.g., 0.0000123 / 1e-5 = 1.23 "per 100,000"
#'
#' Target: scaled value between 0.1 and 100
#'
#' @param x [num] numeric vector of rates (should be < 1)
#' @param mag [chr: default NULL] magnitude override: NULL (auto),
#'   "per10", "per100", "per1k", "per10k", "per100k",
#'   "per1m", "per10m", "per100m", "per1b", "per10b"
#' @param verbose [lgl: default TRUE] verbose warnings
#'
#' @return [data.frame] with columns: mag, mag_label, denom
#' @keywords internal
#' @family magnitudes
#'
set_magnitude_rate <- function(x, mag = NULL, verbose = TRUE) {
   checkmate::assert_numeric(x)
   checkmate::assert_vector(x)
   checkmate::assert_character(mag, len = 1, null.ok = TRUE)
   checkmate::assert_logical(verbose, len = 1)

   # Assert rate values are positive or 0
   if (any(x < 0)) {
      .ex <- x[x < 0]
      stop("Rate values must be positive or 0. Found non-positive values, e.g.: ", .ex[1], call. = FALSE)
   }

   # Auto-detect or user override
   if (is.null(mag)) {
      mag <- unlist(lapply(x, function(x_i) {

         # Edge case: rate >= 1 should use count, not rate
         if (abs(x_i) >= 1) {
            if (verbose) {
               warning(
                  "Rate value >= 1 detected. Consider using metric='count' instead. Value: ",
                  x_i,
                  call. = FALSE
               )
            }
            return("")
         }

         # Find appropriate "per X" scaling
         # Target: scaled value between 0.1 and 100
         if      (abs(x_i) * 1e10 >= 0.1 && abs(x_i) * 1e10 <= 100) "per10b"
         else if (abs(x_i) * 1e9  >= 0.1 && abs(x_i) * 1e9  <= 100) "per1b"
         else if (abs(x_i) * 1e8  >= 0.1 && abs(x_i) * 1e8  <= 100) "per100m"
         else if (abs(x_i) * 1e7  >= 0.1 && abs(x_i) * 1e7  <= 100) "per10m"
         else if (abs(x_i) * 1e6  >= 0.1 && abs(x_i) * 1e6  <= 100) "per1m"
         else if (abs(x_i) * 1e5  >= 0.1 && abs(x_i) * 1e5  <= 100) "per100k"
         else if (abs(x_i) * 1e4  >= 0.1 && abs(x_i) * 1e4  <= 100) "per10k"
         else if (abs(x_i) * 1e3  >= 0.1 && abs(x_i) * 1e3  <= 100) "per1k"
         else if (abs(x_i) * 1e2  >= 0.1 && abs(x_i) * 1e2  <= 100) "per100"
         else if (abs(x_i) * 1e1  >= 0.1 && abs(x_i) * 1e1  <= 100) "per10"
         else {
            # Value too small even at finest scale
            if (verbose) {
               warning(
                  "Rate value too small even at 'per 10 billion' scale. ",
                  "Scaled value will be < 0.1. Value: ", x_i,
                  call. = FALSE
               )
            }
            "per10b"  # Use finest scale available
         }
      }))
   } else {
      mag <- rep.int(mag, length(x))
   }

   mag <- tolower(mag)

   # Generate labels
   mag_label <- unlist(lapply(mag, function(mag_i) {
      switch_strict(
         mag_i
         , "per10b"  = "per 10 billion"
         , "per1b"   = "per 1 billion"
         , "per100m" = "per 100 million"
         , "per10m"  = "per 10 million"
         , "per1m"   = "per 1 million"
         , "per100k" = "per 100,000"
         , "per10k"  = "per 10,000"
         , "per1k"   = "per 1,000"
         , "per100"  = "per 100"
         , "per10"   = "per 10"
         , .empty    = "per 1"
      )
   }))

   # Generate denominators (reciprocals of "per X")
   denom <- unlist(lapply(mag, function(mag_i) {
      switch_strict(
         mag_i
         , "per10b"  = 1e-10
         , "per1b"   = 1e-9
         , "per100m" = 1e-8
         , "per10m"  = 1e-7
         , "per1m"   = 1e-6
         , "per100k" = 1e-5
         , "per10k"  = 1e-4
         , "per1k"   = 1e-3
         , "per100"  = 1e-2
         , "per10"   = 1e-1
         , .empty    = 1
      )
   }))

   df_mag <- data.frame(
      mag                = mag
      , mag_label        = mag_label
      , denom            = denom
      , stringsAsFactors = FALSE
   )

   return(df_mag)
}

# ---- Main Function (Public) -------------------------------------------------

#' Define magnitude, magnitude label and denominator for a vector of numeric values
#'
#' Support function used on _central_ (e.g. mean) values for later formatting.
#' Routes to appropriate helper based on metric.
#'
#' @param x [num] numeric vector
#' @param metric [chr] metric: "prop", "pp", "count", "rate" (required)
#' @param mag [chr: default NULL] magnitude override (NULL = auto-detect)
#'   - For counts: "t", "m", "b"
#'   - For rates: "per10", "per100", "per1k", ..., "per10b"
#'   - For props/pp: "as-is" (no scaling, use values as provided)
#' @param count_label_thousands [lgl: default FALSE] allow "thousands" magnitude for counts?
#'   Not Lancet-valid.
#' @param verbose [lgl: default TRUE] show warnings?
#'
#' @return [data.frame] with columns: mag, mag_label, denom
#' @export
#' @family magnitudes
#'
#' @examples
#' # Proportions (no scaling)
#' set_magnitude(c(0.5, 0.75), metric = "prop")
#'
#' # Counts
#' set_magnitude(c(1e3, 1e6, 1e9), metric = "count")
#'
#' # Rates
#' set_magnitude(c(0.0000123, 0.0000456), metric = "rate")
set_magnitude <- function(
      x,
      metric,
      mag                   = NULL,
      count_label_thousands = FALSE,
      verbose               = TRUE
) {
   checkmate::assert_vector(x)
   checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)
   checkmate::assert_string(metric, na.ok = FALSE)
   checkmate::assert_character(mag, len = 1, null.ok = TRUE)
   checkmate::assert_logical(count_label_thousands, len = 1)
   checkmate::assert_logical(verbose, len = 1)

   # Validate and normalize metric
   metric <- assert_metric(metric)

   # Route to appropriate helper using switch_strict
   df_mag <- switch_strict(
      metric
      , "prop" = set_magnitude_prop(
         x         = x
         , mag     = mag
         , verbose = verbose
      )
      , "pp" = set_magnitude_prop(
         x         = x
         , mag     = mag
         , verbose = verbose
      )
      , "rate" = set_magnitude_rate(
         x         = x
         , mag     = mag
         , verbose = verbose
      )
      , "count" = set_magnitude_count(
         x                       = x
         , mag                   = mag
         , count_label_thousands = count_label_thousands
         , verbose               = verbose
      )
   )

   return(df_mag)
}



