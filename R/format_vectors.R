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
#' @keywords internal
#'
#' @returns [num matrix] matrix with rows 'central', 'lower', 'upper' and columns for each triplet set
#'
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

#' Validate fround helper return structure
#'
#' Ensures fround_props() and fround_count_rate() return the expected structure:
#' list(formatted = chr[3], df_mag_row = data.frame[1,])
#'
#' non-exported helper
#'
#' @param result [list] Return value from fround_props() or fround_count_rate()
#' @param context [chr] Function name for error message (e.g., "fround_props")
#'
#' @return [NULL] invisibly (stops on validation failure)
#' @family assertions
#' @keywords internal
assert_fround_return_schema <- function(
    result
    , context = "fround helper"
) {

  # Check it's a list
  if (!is.list(result)) {
    stop(
      sprintf(
        "%s must return a list. Got: %s", context, class(result)[1])
      , call. = FALSE
    )
  }

  # Check required names
  required_names <- c("formatted", "df_mag_row")
  if (!all(required_names %in% names(result))) {
    stop(
      sprintf(
        "%s must return list with names: %s. Got: %s"
        , context
        , toString(required_names)
        , toString(names(result))
      )
      , call. = FALSE
    )
  }

  # Check formatted is chr[3]
  if (!is.character(result$formatted) || length(result$formatted) != 3) {
    stop(
      sprintf(
        "%s$formatted must be character vector of length 3. Got: %s[%d]"
        , context
        , class(result$formatted)[1]
        , length(result$formatted)
      )
      , call. = FALSE
    )
  }

  # Check df_mag_row is data.frame[1,] with correct columns
  if (!is.data.frame(result$df_mag_row) || nrow(result$df_mag_row) != 1) {
    stop(
      sprintf(
        "%s$df_mag_row must be single-row data.frame. Got: %s with %d rows"
        , context
        , class(result$df_mag_row)[1]
        , nrow(result$df_mag_row)
      )
      , call. = FALSE
    )
  }

  required_cols <- c("mag", "mag_label", "denom")
  if (!all(required_cols %in% names(result$df_mag_row))) {
    stop(
      sprintf(
        "%s$df_mag_row must have columns: %s. Got: %s"
        , context
        , toString(required_cols)
        , toString(names(result$df_mag_row))
      )
      , call. = FALSE
    )
  }

  invisible(NULL)
}

#' Apply zero-padding to maintain significant figures display
#'
#' Ensures formatted numbers display the requested number of significant figures
#' by padding with zeros on the decimal side.
#'
#' non-exported helper
#'
#' @param x_chr [chr] formatted numeric string
#' @param sigfig [int] target number of significant figures
#' @param decimal.mark [chr] decimal mark character
#'
#' @returns [chr] zero-padded string
#' @keywords internal
apply_sigfig_zero_padding <- function(x_chr, sigfig, decimal.mark) {

  # Split integer & decimal parts
  if (grepl(decimal.mark, x_chr, fixed = TRUE)) {
    parts <- strsplit(x_chr, decimal.mark, fixed = TRUE)[[1]]
    int_part <- parts[1]
    dec_part <- parts[2]
  } else {
    int_part <- x_chr
    dec_part <- ""
  }

  # Remove separators to count significant figures
  numeric_clean <- gsub("[^0-9]", "", sprintf("%s%s", int_part, dec_part))

  # Strip leading zeros – they are not significant
  numeric_clean <- sub("^0+", "", numeric_clean)

  # How many sig figs currently?
  current_sf <- nchar(numeric_clean)

  # How many more needed?
  needed <- max(sigfig - current_sf, 0)

  if (needed > 0) {
    # Ensure a decimal exists
    if (!grepl(decimal.mark, x_chr, fixed = TRUE)) {
      x_chr <- sprintf("%s%s", x_chr, decimal.mark)
      dec_part <- ""
    }

    # Pad zeros onto decimal side
    x_chr <- sprintf("%s%s", x_chr, strrep("0", needed))
  }

  return(x_chr)
}

#' Format value using decimal method
#'
#' non-exported helper
#'
#' @param x_sc [num] scaled numeric value
#' @param nsmall [int] decimal places
#' @param decimal.mark [chr] decimal mark
#' @param big.mark [chr] thousands separator
#'
#' @returns [chr] formatted string
#' @keywords internal
format_decimal <- function(x_sc, nsmall, decimal.mark, big.mark) {
  x_fmt <- round(x_sc, digits = nsmall)
  x_chr <- format(
    x_fmt
    , nsmall       = nsmall
    , decimal.mark = decimal.mark
    , big.mark     = big.mark
    , scientific   = FALSE
  )
  trimws(x_chr)
}

#' Format value using integer method
#'
#' non-exported helper
#'
#' @param x_sc [num] scaled numeric value
#' @param decimal.mark [chr] decimal mark
#' @param big.mark [chr] thousands separator
#'
#' @returns [chr] formatted string
#' @keywords internal
format_int <- function(x_sc, decimal.mark, big.mark) {
  x_fmt <- round(x_sc, digits = 0)
  x_chr <- format(
    x_fmt
    , decimal.mark = decimal.mark
    , big.mark     = big.mark
    , scientific   = FALSE
  )
  trimws(x_chr)
}

#' Helper to assist round_5_up
#'
#' @param x [numeric] vector
#' @param epsilon [numeric] some small value to help e.g. 1225 round up to 1230
#' @family vector_formats
#' @keywords internal
#'
#' @returns [numeric] x +/- epsilon (for pos/neg x)
add_epsilon <- function(x, epsilon = 1e-12){
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(epsilon)

  small_x <- min(abs(x))
  if(small_x == 0) small_x <- 1
  order_mag_small_x <- log10(10^(ceiling(log10(small_x))))
  order_mag_epsilon <- log10(10^(ceiling(log10(epsilon))))

  x_pos <- x > 0
  x_neg <- x < 0


  if(order_mag_epsilon - order_mag_small_x > -3){
    warning(
      sprintf(
        "epsilon must be 3 orders of magnitude smaller (%s, %s) than the smallest supported magnitude unit (%s, %s) (see set_magnitude_rate()) - setting epsilon to 0, round_5_up may not work as expected."
        , epsilon, order_mag_epsilon, small_x, order_mag_small_x
      )
    )
    epsilon <- 0
  }

  # don't add epsilon to 0s
  x[x_pos] <- x[x_pos] + epsilon
  x[x_neg] <- x[x_neg] - epsilon
  return(x)
}

#' Format and round proportion-ish number
#'
#' non-exported helper
#'
#' "well that was easy, how hard could counts be?"
#'
#' @param clu [num] numeric triplet of proportions (central, lower, upper)
#' @param style_name [chr] style name - controls rounding and formatting
#' @param mag [chr: default NULL] magnitude override - see set_magnitude()
#'   - For props/pp: "as-is" (no scaling, use values as provided)
#'   - For counts: "t" (thousand), "m" (million), "b" (billion)
#'   - For rates: "per10", "per100", "per1k", ..., "per10b"
#'
#' @returns [list] with elements:
#'   - formatted: chr[3] - formatted central, lower, upper values
#'   - df_mag_row: data.frame[1,] - magnitude info (mag, mag_label, denom)
#' @family vector_formats
#' @keywords internal
#'
fround_props <- function(clu, style_name, mag = NULL) {

  # Compute magnitude from central value
  df_mag <- set_magnitude(x = clu[1], metric = "prop", mag = mag)
  checkmate::assert_data_frame(df_mag, nrows = 1)

  style <- get_style(style_name)

  if (style$round_5_up) clu <- add_epsilon(clu)

  # Use denominator from df_mag
  clu <- clu / df_mag$denom

  formatted <- round(x = clu, digits = style$prop_digits_round) |>
    format(nsmall = style$prop_nsmall, decimal.mark = style$decimal.mark) |>
    trimws()

  # Replace negative sign with style-specific mark
  formatted <- unlist(lapply(formatted, function(x_i_chr) {
    sub("^-", style$neg_mark_UI, x_i_chr)
  }))

  # Return both formatted values and magnitude info
  result <- list(
    formatted   = formatted,
    df_mag_row  = df_mag
  )

  return(result)
}

#' Format and round count or rate numbers
#'
#' Unified formatting for counts and rates with magnitude scaling.
#' Computes magnitude based on trial-rounded central value.
#'
#' non-exported helper
#'
#' @param clu [num] numeric triplet of counts/rates (central, lower, upper)
#' @param style_name [chr] style name - controls rounding and formatting
#' @param metric [chr]
#' @param mag mag [chr: default NULL] magnitude override - see set_magnitude()
#'   - For props/pp: "as-is" (no scaling, use values as provided)
#'   - For counts: "t" (thousand), "m" (million), "b" (billion)
#'   - For rates: "per10", "per100", "per1k", ..., "per10b"
#'
#' @returns [list] with elements:
#'   - formatted: chr[3] - formatted central, lower, upper values
#'   - df_mag_row: data.frame[1,] - magnitude info (mag, mag_label, denom)
#' @family vector_formats
#' @keywords internal
#'
fround_count_rate <- function(clu, style_name, metric, mag = NULL) {

  # === Input Validation ===
  if (metric == "count" && any(clu < 0)) {
    warning("Counts < 0 not yet supported: ", toString(clu))
  }

  if (metric == "rate" && any(clu <= 0)) {
    stop("Rates <= 0 not supported: ", toString(clu), call. = FALSE)
  }

  # === Extract style parameters ===
  style <- get_style(style_name)

  # Use metric as prefix for parameter names
  method      <- style[[sprintf("%s_method", metric)]]
  sigfig      <- style[[sprintf("%s_digits_sigfig", metric)]]
  nsmall      <- style[[sprintf("%s_nsmall", metric)]]
  force_trail <- style[[sprintf("%s_pad_sigfigs", metric)]]

  # Shared parameters
  big.mark_base         <- style[["count_big.mark"]]
  decimal.mark          <- style[["decimal.mark"]]
  round_5_up            <- style[["round_5_up"]]
  is_lancet             <- style[["is_lancet"]]
  count_label_thousands <- if (metric == "count") {
    style[["count_label_thousands"]]
  } else {
    FALSE
  }

  # === Trial rounding to determine magnitude ===
  # This replaces handle_rounding_magnitude_crossover() by computing
  # magnitude based on what the central value will become after rounding

  central_val <- clu[1]

  # Apply round-5-up rule if needed
  if (round_5_up) {
    # must be small enough not to affect smallest possible rate category (per10bn)
    central_val <- central_val + 1e-12
  }

  # Do trial rounding with arbitrary magnitude (scale = 1)
  central_trial_rounded <- if (metric == "count"){
    switch_strict(
      method
      , "sigfig"  = signif(central_val, sigfig)
      , "decimal" = round(central_val, digits = nsmall)
      , "int"     = round(central_val, digits = 0)
    )
  } else if (metric == "rate"){
    central_val
  }

  # Compute magnitude based on trial-rounded value
  # This automatically handles boundary crossings (e.g., 999,999 -> 1,000,000)
  df_mag <- set_magnitude(
    x                       = central_trial_rounded
    , metric                = metric
    , mag                   = mag
    , count_label_thousands = count_label_thousands
    , verbose               = FALSE
  )

  checkmate::assert_data_frame(df_mag, nrows = 1)

  # === Format each value in the triplet ===
  format_one <- function(x) {

    # Store raw value for Lancet edge case
    x_raw <- x

    # Apply round-5-up rule
    if (round_5_up) x <- add_epsilon(x)

    # Scale by magnitude denom
    x_sc <- x / df_mag$denom

    # Initialize big.mark
    big.mark <- big.mark_base

    # Apply Lancet rule (counts only, per-value)
    if (metric == "count" && is_lancet && abs(round(x, 0)) <= 9999) {
      big.mark <- ""
    }

    # Format according to method
    x_chr <- switch_strict(
      method

      , "sigfig" = {
        x_fmt <- signif(x_sc, sigfig)

        # Lancet edge case: if raw value <= 9999 but rounds to >= 10000
        if (metric == "count" && is_lancet && x_raw <= 9999 && x_fmt >= 10000) {
          big.mark <- big.mark_base
        }

        x_chr <- format(
          x_fmt
          , scientific   = FALSE
          , decimal.mark = decimal.mark
          , big.mark     = big.mark
        )

        # Apply zero-padding if requested
        if (force_trail) {
          x_chr <- apply_sigfig_zero_padding(x_chr, sigfig, decimal.mark)
        }

        trimws(x_chr)
      }

      , "decimal" = {
        format_decimal(x_sc, nsmall, decimal.mark, big.mark)
      }

      , "int" = {
        format_int(x_sc, decimal.mark, big.mark)
      }
    )

    trimws(x_chr)
  }

  formatted <- unname(vapply(clu, format_one, FUN.VALUE = character(1)))

  # Replace negative sign with style-specific mark
  formatted <- unlist(lapply(formatted, function(x_i_chr) {
    sub("^-", style$neg_mark_UI, x_i_chr)
  }))

  # Return both formatted values and magnitude info
  result <- list(
    formatted    = formatted
    , df_mag_row = df_mag
  )

  return(result)
}


#' Format and round a single central/lower/upper value set by magnitude without units
#'
#' `central` could be mean/median/point_estimate. `metric` is required (count
#' data requires nuanced logic), but labels are not returned.
#'
#' Format and round without unit labeling
#' - Use `format_lancet_clu()` for unit labels
#'
#' @param clu [num] a numeric triplet of three values in central/lower/upper order
#' @param metric [chr c('prop', 'pp', 'count', or 'rate')] metric type - proportion,
#'   percentage point, count, or rate
#' @param style_name [chr: default 'nature'] style name - controls rounding and formatting
#' @param mag [chr: default NULL] magnitude override - see set_magnitude()
#'   - For props/pp: "as-is" (no scaling, use values as provided)
#'   - For counts: "t" (thousand), "m" (million), "b" (billion)
#'   - For rates: "per10", "per100", "per1k", ..., "per10b"
#'
#' @return [list] with elements:
#'   - formatted: chr[3] - formatted central, lower, upper values
#'   - df_mag_row: data.frame[1,] - magnitude info (mag, mag_label, denom)
#' @family styled_formats
#' @keywords internal
#'
fround_clu_triplet <- function(
    clu
    , metric
    , style_name = "nature"
    , mag        = NULL
) {

  style  <- get_style(style_name)
  metric <- assert_metric(metric)

  checkmate::assert_vector(clu, len = 3)
  checkmate::assert_numeric(clu, len = 3)

  # fround_clu_triplet() is applied _after_ negative handling in
  # process_clu_triplet_negatives() within format_journal_clu() , so CLU
  # relationships may no longer be valid. This function must remain internal.

  # if(style$assert_clu_order == TRUE){
  #   assert_clu_relationship(clu[1], clu[2], clu[3])
  # }

  # Call appropriate formatting helper
  result <- switch_strict(
    metric
    , "prop"  = fround_props(clu = clu, style_name = style_name, mag = mag)
    , "pp"    = fround_props(clu = clu, style_name = style_name, mag = mag)
    , "count" = fround_count_rate(clu = clu, style_name = style_name, metric = "count", mag = mag)
    , "rate"  = fround_count_rate(clu = clu, style_name = style_name, metric = "rate", mag = mag)
  )

  # Validate return schema
  assert_fround_return_schema(result, context = sprintf("fround helper (metric=%s)", metric))

  # Preserve names on formatted values
  names(result$formatted) <- names(clu)

  return(result)
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
format_oxford_comma <- function(
    vec
    , sep = "and"
) {
  checkmate::assert_vector(vec, min.len = 1)
  checkmate::assert_string(sep)

  n    <- length(vec)
  set1 <- toString(vec[1:(n - 1)])

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
fround <- function(
    x
    , digits       = 1L
    , nsmall       = 1L
    , decimal.mark = "."
){
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
#' @param metric [chr: default 'prop' c('prop', 'pp', 'count', 'rate')]
#' @param digits [integer: default 1L] passed to `round()`
#' @param nsmall [integer: default 1L] passed to `format()`
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#'
#' @return [chr] formatted string
#' @export
#' @family vector_formats
#'
#' @examples
#' fround_metric(0.123456789)
#' fround_metric(0.123456789, 'pp', 3, 4)
#' fround_metric(c(55.8346, 123.456789), 'count', 3, 4, ".")
fround_metric <- function(
    x
    , metric       = "prop"
    , digits       = 1L
    , nsmall       = 1L
    , decimal.mark = "."
){

  checkmate::assert_numeric(x)
  checkmate::assert_integerish(digits, len = 1, lower = 0)
  checkmate::assert_integerish(nsmall, len = 1, lower = 0)
  checkmate::assert_character(decimal.mark, len = 1)

  # select data-type label
  suffix <- get_metric_labels(metric)

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
#' Unaware of styles, just a hard-coded git-er-done function.
#'
#' Caution - thousands magnitude is not Lancet compliant.
#'
#' @param x [num] numeric vector
#' @param metric [chr `c("prop", "pp", "count", "rate")`]
#' @param digits [int: default 1L] passed to `round()`
#' @param nsmall [int: default 1L] passed to `format()`
#' @param mag [chr: default NULL] magnitude override - see set_magnitude()
#'   - For props/pp: "as-is" (no scaling, use values as provided)
#'   - For counts: "t" (thousand), "m" (million), "b" (billion)
#'   - For rates: "per10", "per100", "per1k", ..., "per10b"
#' @param count_label_thousands [lgl: default FALSE] allow thousands magnitude?  Not
#'   Lancet-valid. Passed to `set_magnitude()`
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#' @param rate_unit [chr: default NULL] unit label for rates (e.g., "deaths", "cases").
#'   Required when metric = "rate", ignored otherwise.
#' @param big.mark [chr: default ","] thousands separator passed to `format()`
#'
#' @return [chr] formatted string
#' @export
#' @family vector_formats
#' @family magnitudes
#'
#' @examples
#' fmt_magnitude(123456789, metric = "count")
#' fmt_magnitude(0.0000123, metric = "rate", rate_unit = "deaths")
fmt_magnitude <- function(
    x
    , metric
    , rate_unit             = NULL
    , digits                = 1
    , nsmall                = 1
    , decimal.mark          = "."
    , big.mark              = ","
    , mag                   = NULL
    , count_label_thousands = FALSE
){

  assert_rate_unit(metric, rate_unit)

  checkmate::assert_numeric(x)
  checkmate::assert_vector(x)
  checkmate::assert_logical(count_label_thousands, len = 1)
  checkmate::assert_integerish(digits, len = 1, lower = 0)
  checkmate::assert_integerish(nsmall, len = 1, lower = 0)

  rate_unit_fmt <- if (metric == "rate" && !is.null(rate_unit)) sprintf(" %s", rate_unit) else ""

  df_mag <- set_magnitude(
    x
    , metric                = metric
    , mag                   = mag
    , count_label_thousands = count_label_thousands
    , verbose               = FALSE
  )

  # hack for floating point rounding issues
  epsilon <- 1e-9
  x <- x + epsilon

  x_fmt <-
    round(x / df_mag$denom, digits = digits) |>
    format(nsmall = nsmall, decimal.mark = decimal.mark, big.mark = big.mark) |>
    trimws()|>
    sprintf("%s%s %s", ... = _, rate_unit_fmt, df_mag$mag_label) |>
    trimws()

  return(x_fmt)
}



## Lancet Family  -----------------------------------------------------

#' Format and round with data-type suffix
#'
#' Lancet-specific wrapper for `fround_metric()`, using mid-dot as decimal mark.
#' Retaining for legacy purposes (no Nature equivalent)
#'
#' @param x [num] numeric value
#' @param metric [chr: default 'prop' c('prop', 'pp', 'count', rate)]
#' @param digits [integer: default 1L] passed to `round()`
#' @param nsmall [integer: default 1L] passed to `format()`
#' @param decimal.mark [chr: default mid_dot()] decimal mark passed to `format()`
#'
#' @return [chr] formatted string
#' @export
#' @family vector_formats
#'
#' @examples
#' fround_metric_lancet(0.123456789)
#' fround_metric_lancet(0.123456789, 'pp', 3, 4)
#' fround_metric_lancet(c(55.8346, 123.456789), 'count', 3, 4, ".")
fround_metric_lancet <- function(
    x
    , metric       = "prop"
    , digits       = 1L
    , nsmall       = 1L
    , decimal.mark = mid_dot()
){

  fround_metric(
    x              = x
    , metric       = metric
    , digits       = digits
    , nsmall       = nsmall
    , decimal.mark = decimal.mark
  )

}
