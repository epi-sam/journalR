# started: 2025 Aug 29 13:51:03
# purpose: formatting functions for Lancet journal presentation


#' Format vector of items with Oxford comma
#'
#' @param vec [any] vector of items to format
#'
#' @returns [chr] formatted string with Oxford comma
#' @export
#'
#' @examples
#' format_oxford_comma(1:2) # "1 and 2"
#' format_oxford_comma(1:3) # "1, 2, and 3"
format_oxford_comma <- function(vec) {
   checkmate::assert_vector(vec, min.len = 1)
   n <- length(vec)
   set1 <- toString(vec[1:(n - 1)])
   and_sep <- dplyr::if_else(n > 2, ", and ", " and ")
   set2 <- vec[n]
   str <- sprintf("%s%s%s", set1, and_sep, set2)
   return(str)
}


#' Mid-dot
#'
#' Lancet numeric decimal standard ("·")
#'
#' @returns [chr] mid-dot character ("·")
#' @export
#'
#' @examples mid_dot()
mid_dot <- function() {
   return("\U00B7")
}

#' En-dash
#'
#' Lancet negative/hyphen sign standard ("–")
#'
#' @returns [chr] en-dash character ("–")
#' @export
#'
#' @examples en_dash()
en_dash <- function() {
   # "–"
   return("\U2013")
}


#' Thin space
#'
#' Lancet thin space standard (" ") for thousands separator (instead of commaa
#' ",")
#'
#' @returns [chr] thin space character (" ")
#' @export
#'
#' @examples thin_space()
thin_space <- function() {
   # " "
   return("\U2009")
}


#' Format and round
#'
#' Unaware of data-type
#'
#' @param x [num] numeric vector
#' @param digits [integer] passed to `round()`
#' @param nsmall [integer] passed to `format()`
#'
#' @return [chr] formatted string
#'
#' @examples
#' fround(0.123456789) # "0·1"
#' fround(0.123456789, digits = 3) # "0·123"
#' fround(0.123456789, digits = 3, nsmall = 4) # "0·1230"
fround <- function(x, digits = 1L, nsmall = 1L, decimal.mark = mid_dot()){
   # Format and round, no label
   return(trimws(paste0(format(round(x, digits), nsmall = nsmall, decimal.mark = decimal.mark))))
}

#' Format and round with data-type suffix
#'
#' @param x [num] numeric value
#' @param d_type [chr {'prop', 'pp', or 'count'}] data type - proportion,
#'   percentage point or count
#' @param digits [integer: default 1L] passed to `round()`
#' @param nsmall [integer: default 1L] passed to `format()`
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#'
#' @return [chr] formatted string
#'
#' @examples
#' fround_dtype(0.123456789) # "0.1%"
#' fround_dtype(0.123456789, 'pp', 3, 4) # "0.1230 pp"
#' fround_dtype(c(55.8346, 123.456789), 'count', 3, 4, ".") # "55.8350"  "123.4570"
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
   suffix <- switch_strict(
      d_type
      , "prop" = "%"
      , "pp"   = " pp"
      , "count"    = ""
   )

   # round and format
   x_fmt <- x  %>%
      round(digits) %>%
      format(nsmall = nsmall, decimal.mark = decimal.mark) %>%
      trimws() %>%
      sprintf("%s%s", ., suffix)
   return(x_fmt)
}

#' Define magnitude, magnitude label and denominator for a vector of numeric
#' values
#'
#' Support function used on _mean_ values for later formatting functions
#'
#' Does not use 'thousands' label - not a valid Lancet format, which uses
#' ddd<narrow-space>ddd format.  See fround_mag_clu for that formatting
#'
#' @param x [num] numeric vector
#' @param mag [chr: default NULL {NULL, "B", "M"}] NULL (auto-detect), otherwise
#'   user-override (not recommended) - (M)illions or (B)illions or (T)housands
#'   (thousands are not Lancet-valid)
#' @param allow_thousands [lgl: default FALSE] allow (T)housands magnitude?  Not
#'   Lancet-valid.
#' @param verbose [lgl: default TRUE] warn if allow_thousands is TRUE
#'
#' @return [list] with vector elements: mag, mag_label, and denom Each vector
#'   element has one item per length(x)
#' @export
#'
#' @examples
#' set_magnitude(c(1, 1e3, 1e6))
#' # list(mag = c("", "", "M"), mag_label = c(" ", " ", " million "), denom = c(1, 1, 1e+06))
set_magnitude <- function(
      x
      , mag             = NULL
      , allow_thousands = FALSE
      , verbose         = TRUE
){

   checkmate::assert_numeric(x)
   checkmate::assert_vector(x)
   checkmate::assert_character(mag, len = 1, null.ok = TRUE)
   checkmate::assert_logical(allow_thousands, len = 1)
   checkmate::assert_logical(verbose, len = 1)

   if(allow_thousands & verbose)
      warning("'thousands' magnitude is not Lancet-valid \n   - suppress this warning with verbose = FALSE")

   # vector of magnitudes
   if(is.null(mag)){
      # auto-detect
      mag <- unlist(lapply(x, function(x_i){
         if      (abs(x_i) >= 1e9)                    mag <- "B"
         else if (abs(x_i) >= 1e6)                    mag <- "M"
         else if (abs(x_i) >= 1e3 && allow_thousands) mag <- "T"  # not Lancet-valid
         else mag <- "" # default
      }))

   } else {
      # user override (not recommended)
      mag <- rep.int(mag, length(x))
   }

   mag <- toupper(mag)

   # printable form
   mag_label <- unlist(lapply(mag, function(mag_i){
      switch_strict(
         mag_i
         , "B" = "billion "
         , "M" = "million "
         , "T" = "thousand " # not Lancet-valid
         , .empty = ""
      )
   }))

   denom <- unlist(lapply(mag, function(mag_i){
      switch_strict(
         mag_i
         , "B" = 1e9
         , "M" = 1e6
         , "T" = 1e3 # not Lancet-valid
         , .empty = 1
      )
   }))

   mag_list <- list(mag = mag, mag_label = mag_label, denom = denom)
   unique_element_lengths <- unique(unlist(Map(length, mag_list)))
   checkmate::assert_true(length(unique_element_lengths) == 1)

   return(mag_list)
}



#' Format magnitude
#'
#' Caution - thousands magnitude is not Lancet compliant
#'
#' @param x [num] numeric vector
#' @param digits [int: default 1L] passed to `round()`
#' @param nsmall [int: default 1L] passed to `format()`
#' @param mag [chr {"b", "m", "t"}] magnitude (billion, million, thousand)
#'
#' @return [chr] formatted string
#' @export
#'
#' @examples
#' fmt_magnitude(123456789) # "123.5 million"
fmt_magnitude <- function(
      x
      , digits          = 1
      , nsmall          = 1
      , mag             = "m"
      , allow_thousands = FALSE
){

   checkmate::assert_numeric(x)
   checkmate::assert_integerish(digits, len = 1, lower = 0)
   checkmate::assert_integerish(nsmall, len = 1, lower = 0)
   checkmate::assert_character(mag, len = 1)
   checkmate::assert_logical(allow_thousands, len = 1)

   mag <- tolower(mag)

   if (allow_thousands == FALSE & mag == "t") {
      stop("Thousands magnitude is not Lancet-compliant.  Set allow_thousands = TRUE to override.")
   }

   # find denom
   denom <- switch_strict(
      mag
      , "b" = 1e9
      , "m" = 1e6
      , "t" = 1e3
   )
   # find label
   mag <- switch_strict(
      mag
      , "b" = "billion"
      , "m" = "million"
      , "t" = "thousand"
   )
   # round and format
   x_fmt <-
      round(x / denom, digits = digits) %>%
      format(nsmall = nsmall) %>%
      trimws() %>%
      sprintf("%s %s", ., mag)

   return(x_fmt)
}


#' Format and round central/lower/upper value sets by magnitude _without_ units
#'
#' `central` could be mean/median/point_estimate. d_type is required (count data
#' requires nuanced logic), but labels are not returned.
#'
#' Format and round without unit labeling
#' - Use `format_lancet_clu()` for unit labels
#'
#' Defaults are for lancet labels.
#'
#' Current IHME standards:
#' - https://hub.ihme.washington.edu/spaces/SC/pages/123833253/Rounding+standards
#' - Rates and Percentages - to one decimal place
#' - Counts                - to 3 sig figs
#'
#' @param clu [num] a numeric vector of central/lower/upper
#' @param d_type [chr {'prop', 'pp', or 'count'}] data type - proportion,
#'   percentage point or count
#' @param mag_list [named list] output from `set_magnitude()` - must be based on
#'   **central** value of a central/lower/upper set - central _and_ all UI
#'   values inherit the same scale as the central tendency.
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#' @param negative_sign [chr: default "-"] negative sign
#' @param big.mark_count [chr: default ","] big mark for counts passed to
#'   `format()`
#' @param digits_round_prop [int: default 1L] passed to `round()` for
#'   proportions
#' @param digits_sigfig_count [int: default 3L] passed to `signif()` for counts
#' @param nsmall [int: default 1L] passed to `format()`
#'
#' @return [chr] formatted string (vectorized)
#' @export
#'
#' @examples
#' fround_mag_clu(clu = c(central = 0.2, lower = 0.1, upper = 0.3), d_type = "prop") # "0.2" "0.1" "0.3"
#' fround_mag_clu(clu = c(central = 0.2, lower = -0.1, upper = 0.3), d_type = "pp") # "0.2" "–0.1" "0.3"
#' fround_mag_clu(clu = c(central = 95e6, lower = 89e6, upper = 101e6), d_type = "count") # "95.0"  "89.0"   "101"
#' fround_mag_clu(clu = c(central = 95e6, lower = 96e6, upper = 97e6), d_type = "count") # "95.0"  "96.0"  "97.0"
fround_mag_clu <- function(
      clu
      , d_type
      , mag_list            = set_magnitude(clu[1]) # assuming central is in first position
      , digits_round_prop   = 1L
      , digits_sigfig_count = 3L
      , nsmall              = 1L
      , decimal.mark        = "."
      , negative_sign       = "-"
      , big.mark_count      = ","
      , is_lancet           = FALSE
) {

   checkmate::assert_numeric(clu)
   checkmate::assert_vector(clu, len = 3)
   # not dealing with PERD right now, could be 'mean', 'median', 'max_likelihood' or 'point_estimate'
   # checkmate::assert_vector(clu, names = "named")
   # checkmate::assert_subset(names(clu), choices = c("central", "lower", "upper"))
   checkmate::assert_list(mag_list, names = "named")
   checkmate::assert_names(names(mag_list), must.include = c("denom"))
   checkmate::assert_character(d_type, len = 1)
   checkmate::assert_character(decimal.mark, len = 1)
   checkmate::assert_character(negative_sign, len = 1)
   checkmate::assert_character(big.mark_count, len = 1)
   checkmate::assert_integerish(digits_round_prop, len = 1, lower = 0)
   checkmate::assert_integerish(digits_sigfig_count, len = 1, lower = 0)
   checkmate::assert_integerish(nsmall, len = 1, lower = 0)

   clu_fmt <- switch_strict(
      d_type
      , "prop"  = trimws(format(round(x = clu, digits = digits_round_prop), nsmall = nsmall, decimal.mark = decimal.mark))
      , "pp"    = trimws(format(round(x = clu, digits = digits_round_prop), nsmall = nsmall, decimal.mark = decimal.mark))
      , "count" = {

         if(any(clu < 0)) stop("Formatting counts under 0 not yet supported: ", toString(clu))

         unlist(lapply(clu, function(x_i){

            big.mark_count_og <- data.table::copy(big.mark_count)

            # lancet spec for counts under 10,000
            if(is_lancet && abs(round(x_i, 0)) <= 9999) {
               big.mark_count <- ""
               nsmall <- 0
            }

            # Still need accurate sig figs for numbers <= digits_sigfig_count e.g.
            # c(10.5, 0.2, 20.3) should become c("10.5", "0.200", "20.3") if
            # digits_sigfig_count = 3
            x_i_rnd <- round(x_i)
            digits_x_i_whole <- nchar(x_i_rnd) - ifelse(x_i_rnd == 0, 1, 0) # account for zero
            if(abs(x_i) > 0 & digits_x_i_whole <= digits_sigfig_count) {
               nsmall <- digits_sigfig_count - digits_x_i_whole
            }

            x_i_div <- signif(
               x        = (x_i / mag_list$denom)
               , digits = digits_sigfig_count
            )
            checkmate::assert_numeric(x_i, len = 1)

            # Ensure e.g. 95.0 million (89.0-101) retains same sigfigs across set of values
            if(
               nchar(x_i_div) >= digits_sigfig_count
               & nsmall > 0
               & !grepl("\\.", x_i_div)
            ){
               nsmall <- 0
            }

            x_i_chr <- trimws(
               format(
                  x              = x_i_div
                  , decimal.mark = decimal.mark
                  , big.mark     = big.mark_count
                  , nsmall       = nsmall
               )
            )
            # catch cases where counts e.g. 9999 would round up to 10000 and we lose the big.mark
            if(nchar(x_i_div) > digits_x_i_whole){
               x_i_chr <- trimws(
                  format(
                     x              = x_i_div
                     , decimal.mark = decimal.mark
                     , big.mark     = big.mark_count_og
                     , nsmall       = nsmall
                  )
               )
            }
            checkmate::assert_character(x_i_chr, len = 1)

            # zero pad formatted string to correct sig figs if too short
            if(
               nchar(x_i_chr) < (digits_sigfig_count + nchar(decimal.mark))
               & nsmall > 0
            ) {
               # pad   <- required nchar                             - current nchar
               n_zeros <- (digits_sigfig_count + nchar(decimal.mark)) - nchar(x_i_chr)
               zeros   <- rep.int("0", n_zeros)
               x_i_chr <- sprintf("%s%s", x_i_chr, zeros)
            }

            return(unname(x_i_chr)) # not sure how extra naming snuck in
         }))
      }
   )

   # replace negative sign (with Lancet standard if applicable)
   clu_fmt <- unlist(lapply(clu_fmt, function(x_i_chr){sub("^-", negative_sign, x_i_chr)}))

   return(clu_fmt)
}


# Generic Formatting -----------------------------------------------------------

#' Format central/lower/upper value triplets for journal presentation
#'
#' Defaults are generic.  This function allows special formtting marks to be
#' applied by journal. Use `format_lancet_clu()` for Lancet-specific formatting.
#' Use `format_nature_clu()` for Nature-specific formatting.
#'
#' Takes three vectors as main arguments for data.table-friendly vectorization.
#'
#' `central` could be mean/median/point_estimate
#'
#' Transform c(central = 0.994, lower = 0.984, upper = 0.998) to "99.4%
#' (98.4–99.8)"
#'
#' Accounts for negative values, and UIs that cross zero.  Checks if
#' central/lower/upper values are in the correct order.
#' - https://hub.ihme.washington.edu/spaces/SGT/pages/45675035/WRITING+STYLE+GUIDE#WRITINGSTYLEGUIDE-GBDStyleGuideGBDStyleGuide
#'
#' @param central [num] central/point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param d_type [chr {prop, pp, count}] data type - proportion, percentage
#'   point or count
#' @param digits_round_prop [int: default 1L] number of digits to round proportions/PP
#' @param digits_sigfig_count [int: default 3L] number of significant digits for counts
#' @param nsmall [int: default 1L] number of digits after the decimal point
#' @param decimal.mark [chr: default "."] decimal mark
#' @param negative_sign [chr: default "-"] negative sign
#' @param big.mark_count [chr: default ","] big mark for counts
#' @param mean_neg_text [chr: default "a decrease of "] text to prepend if
#'   central value is negative
#' @param UI_only [lgl: default FALSE] return only the UI?
#' @param UI_text [chr: default ""] text to appear before values inside UI
#'   parentheses
#' @param assert_clu_relationships [lgl: default TRUE] enforce correct relationship between
#'   central/upper/lower values? This should _ALWAYS_ be `TRUE` _UNLESS_ you
#'   only care about the central difference.
#' @param is_lancet [lgl: default FALSE] apply Lancet-specific formatting for counts 9999 and
#'   smaller
#'
#' @return [chr] formatted string vector
#' @export
#'
format_journal_clu <- function(
      central
      , lower
      , upper
      , d_type
      , digits_round_prop        = 1L
      , digits_sigfig_count      = 3L
      , nsmall                   = 1L
      , decimal.mark             = "."
      , negative_sign            = "-"
      , big.mark_count           = ","
      , mean_neg_text            = "a decrease of "
      , UI_only                  = FALSE
      , UI_text                  = ""
      , assert_clu_relationships = TRUE
      , is_lancet                = FALSE
) {

   checkmate::assert_numeric(central, min.len = 1)
   checkmate::assert_numeric(lower)
   checkmate::assert_numeric(upper)
   checkmate::assert_vector(central)
   checkmate::assert_vector(lower)
   checkmate::assert_vector(upper)

   # lists with two shapes for assertions and processing
   # 1. three vectors of equal length (central, lower, upper)
   # 2. list of sets of central/lower/upper triplet values

   clu <- list(central = central, lower = lower, upper = upper)
   triplets <- lapply(seq_along(central), function(i) {
      c(central = central[i], lower = lower[i], upper = upper[i])
   })

   # assert dimensions
   clu_lengths <- unique(unlist(Map(length, clu)))
   checkmate::assert_true(length(clu_lengths) == 1)
   triplet_lengths <- unique(unlist(Map(length, triplets)))
   checkmate::assert_true(length(triplet_lengths) == 1)
   checkmate::assert_true(triplet_lengths == 3)

   checkmate::assert_integerish(digits_round_prop, len = 1, lower = 0)
   checkmate::assert_integerish(nsmall, len = 1, lower = 0)
   checkmate::assert_character(d_type, len = 1)
   checkmate::assert_choice(d_type, choices = c("prop", "pp", "count"))
   checkmate::assert_logical(UI_only, len = 1)
   checkmate::assert_logical(assert_clu_relationships, len = 1)
   checkmate::assert_character(decimal.mark, len = 1)
   checkmate::assert_character(negative_sign, len = 1)
   checkmate::assert_character(big.mark_count, len = 1)
   checkmate::assert_integerish(digits_sigfig_count, len = 1, lower = 0)

   # vectorized assertions
   if(assert_clu_relationships == TRUE){
      assert_x_gte_y(x = upper,   y = central)
      assert_x_gte_y(x = central, y = lower)
      assert_x_gte_y(x = upper,   y = lower) # probably redundant
   }

   # Capture numeric info before character conversion
   # Does UI cross zero? Decide which UI separator to use.
   UI_crosses_zero_vec <- unlist(lapply(triplets, function(triplet) {
      (triplet["lower"] < 0) & (triplet["upper"] > 0)
   })) %>% unname()
   # Is the central value negative?
   decrease_stub <- unlist(lapply(
      central < 0
      , dplyr::if_else, true = mean_neg_text, false = ""
   ))
   # Is the whole set negative?
   all_neg_vec <- unlist(lapply(triplets, function(triplet) all(triplet <= 0)))
   sep_vec     <- dplyr::if_else(
      (UI_crosses_zero_vec == TRUE & all_neg_vec == FALSE)
      , " to ", en_dash()
   )

   # process triplets
   triplets <- lapply(triplets, function(triplet){

      # proportion to percentage
      if(d_type %in% c("prop", "pp")) triplet <- triplet * 100

      all_neg     <- all(triplet <= 0)
      central_neg <- (triplet["central"] < 0) & !all_neg

      # If just the mean is negative, invert just the mean
      if(central_neg) triplet["central"] <- triplet["central"] * -1

      # If the triplet is all negative, invert and flip upper/lower values
      if(all_neg) {
         triplet <- triplet * -1
         l_temp  <- triplet[["lower"]]
         u_temp  <- triplet[["upper"]]
         triplet[["lower"]] <- u_temp
         triplet[["upper"]] <- l_temp
      }
      return(triplet)
   })
   triplets <- lapply(triplets, function(x) {names(x) <- c("central", "lower", "upper"); x})

   # assert any negative inversions left things in good shape
   if(assert_clu_relationships == TRUE){
      assert_x_gte_y(x = upper,   y = central)
      assert_x_gte_y(x = central, y = lower)
      assert_x_gte_y(x = upper,   y = lower) # probably redundant
   }

   mag_list  <- set_magnitude(central)
   mag_label <- mag_list$mag_label
   mag_table <- data.table::as.data.table(mag_list) # easier to use this format here

   # Where the magic happens
   triplets_fmt <- lapply(seq_along(triplets), function(idx){
      triplet_fmt <- fround_mag_clu(
         clu                   = triplets[[idx]]
         , d_type              = d_type
         , mag_list            = as.list(mag_table[idx, ])
         , decimal.mark        = decimal.mark
         , digits_round_prop   = digits_round_prop
         , nsmall              = nsmall
         , big.mark_count      = big.mark_count
         , digits_sigfig_count = digits_sigfig_count
         , negative_sign       = negative_sign
         , is_lancet           = is_lancet
      )
      names(triplet_fmt) <- c("central", "lower", "upper")
      triplet_fmt
   })

   n_sets <- length(triplets_fmt)

   d_type_label <- switch_strict(
      d_type
      , "prop"  = rep.int("%",   n_sets)
      , "pp"    = rep.int(" pp", n_sets)
      , "count" = rep.int("",    n_sets)
   )

   str_vec <- unlist(lapply(seq_along(triplets_fmt), function(i){
      .cen <- triplets_fmt[[i]]['central']
      .upp <- triplets_fmt[[i]]['upper']
      .low <- triplets_fmt[[i]]['lower']
      str <- glue::glue("{decrease_stub[i]}{.cen}{d_type_label[i]} {mag_label[i]}({UI_text}{.low}{sep_vec[i]}{.upp})")

      if (UI_only) {
         str <- glue::glue("{UI_text}{.low}{sep_vec[i]}{.upp}{mag_label[i]}")
      }

      return(str)

   }))

   return(str_vec)
}

#' Return a table with formatted central/lower/upper
#'
#' Assumes a single data-type (d_type) for the whole table (e.g. 'prop', 'pp',
#' 'count')
#'
#' @param dt [data.table]
#' @param d_type [chr {'prop', 'pp', or 'count'}] a single data type
#' @param central_var [chr: default 'mean'] name of central tendency variable
#' @param lower_var [chr: default 'lower'] name of lower bound variable
#' @param upper_var [chr: default 'upper'] name of upper bound variable
#' @param remove_clu [lgl] remove central/lower/upper variables after
#'   formatting?
#' @param assert_clu_relationships [lgl: default TRUE] enforce correct
#'   relationship between central/upper/lower values? This should _ALWAYS_ be
#'   `TRUE` _UNLESS_ you only care about the central difference.
#' @param digits_round_prop [int: default 1L] passed to `round()` for
#'   proportions
#' @param digits_sigfig_count [int: default 3L] passed to `signif()` for counts
#' @param nsmall [int: default 1L] passed to `format()`
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#' @param negative_sign [chr: default "-"] negative sign
#' @param big.mark_count [chr: default "."] big mark for counts passed to
#'   `format()`
#' @param mean_neg_text [chr: default "a decrease of "] text to prepend if
#'   central value is negative
#' @param UI_only [lgl: default FALSE] return only the UI?
#' @param UI_text [chr: default ""] text to appear before values inside UI
#'   parentheses
#' @param is_lancet [lgl] apply Lancet-specific formatting for counts 9999 and
#'   smaller
#'
#' @returns [data.table] copy of input data.table with new 'clu_fmt' column
#' @export
format_journal_dt <- function(
      dt
      , d_type
      , central_var              = "mean"
      , lower_var                = "lower"
      , upper_var                = "upper"
      , remove_clu               = TRUE
      , assert_clu_relationships = TRUE
      , digits_round_prop        = 1L
      , digits_sigfig_count      = 3L
      , nsmall                   = 1L
      , decimal.mark             = "."
      , negative_sign            = "-"
      , big.mark_count           = ""
      , mean_neg_text            = "a decrease of "
      , UI_only                  = FALSE
      , UI_text                  = ""
      , is_lancet                = FALSE
){

   checkmate::assert_data_table(dt)
   checkmate::assert_character(d_type, len = 1)
   checkmate::assert_character(central_var, len = 1)
   checkmate::assert_character(lower_var, len = 1)
   checkmate::assert_character(upper_var, len = 1)
   vars_clu <- c(central_var, lower_var, upper_var)
   assert_x_in_y(vars_clu, colnames(dt))
   assert_x_not_in_y("clu_fmt", colnames(dt))
   checkmate::assert_logical(remove_clu, len = 1)
   checkmate::assert_logical(assert_clu_relationships, len = 1)

   x <- data.table::copy(dt)

   x[, clu_fmt := format_journal_clu(
      central                    = .SD[[central_var]]
      , lower                    = .SD[[lower_var]]
      , upper                    = .SD[[upper_var]]
      , d_type                   = d_type
      , assert_clu_relationships = assert_clu_relationships
      , digits_round_prop        = digits_round_prop
      , digits_sigfig_count      = digits_sigfig_count
      , nsmall                   = nsmall
      , decimal.mark             = decimal.mark
      , negative_sign            = negative_sign
      , big.mark_count           = big.mark_count
      , mean_neg_text            = mean_neg_text
      , UI_only                  = UI_only
      , UI_text                  = UI_text
      , is_lancet                = is_lancet
   )]

   if (remove_clu == TRUE) x[, (vars_clu) := NULL]
   return(x[]) # trick to print if not assigned
}

#' Format means in data.table by data type for presentation.
#'
#' TODO 2025-10-30 - not yet configured with set_magnitude
#'
#' Multiply one or more 'mean_' columns by a scalar and round to specified
#' digits based on a data type.
#'
#' Pairs nicely with `means_year_compare()`
#'
#' Data types:
#' - 'prop'  - proportion (e.g. 0.1234 -> 12.3%)
#' - 'pp'    - percentage point (e.g. 0.1234 -> 12.3 pp)
#' - 'count' - count (e.g. 1234.56 -> 1235)
#'
#' @param DT [data.table] input data.table with one or more 'mean_' columns
#' @param dtype [chr {'prop', 'pp', or 'count'}] a single data type
#' @param scalar [numeric: default 100] scalar to multiply mean values by
#' @param digits [integer: default 1] number of digits to round to
#' @param central_varname [chr: default 'mean'] prefix of mean variable names to
#'   format.  Implemented as e.g. "^mean[_]*" to capture 'mean', 'mean_1990',
#'   'mean_2000', etc.
#'
#' @returns [data.table] copy of input data.table with formatted mean column(s)
#' @export
#'
#' @examples
#' DT <- data.table::data.table(
#'   location_id = c(1, 2, 3)
#'   , mean_1990 = c(0.1234, 0.2345, 0.3456)
#'   , mean_2000 = c(0.2234, 0.3345, 0.4456)
#'  )
#'
#' format_means_dt(DT, dtype = "prop")
format_means_dt <- function(
      DT
      , dtype
      , central_varname = "mean"
      , scalar          = 100
      , digits          = 1
      , decimal.mark    = "."
      , big.mark        = ","
){
   checkmate::assert_data_table(DT)
   checkmate::assert_character(dtype, len = 1)
   checkmate::assert_number(scalar)
   checkmate::assert_integerish(digits, len = 1)

   label <- switch(
      dtype
      , prop  = "%"
      , pp    = " pp"
      , count = ""
      , stop("dtype must be one of 'prop', 'pp' or 'count'")
   )

   mean_varnames <- grep(
      pattern = sprintf("^%s[_]*", central_varname)
      , x     = colnames(DT)
      , value = TRUE
   )

   # hack for floating point rounding issues
   epsilon <- 1e-9

   for (varname in mean_varnames){
      DT[, (varname) := paste0(
         formatC(
            x              = round(get(varname) * scalar + epsilon, digits)
            , format       = "f"
            , digits       = digits
            , big.mark     = big.mark
            , decimal.mark = decimal.mark
         )
         , label
      )]
   }

   DT[]
}


# Lancet Family Formatting -----------------------------------------------------

#' Format and round with data-type suffix
#'
#' @param x [num] numeric value
#' @param d_type [chr c('prop', 'pp', or 'count')] data type - proportion, percentage point or count
#' @param digits [integer: default 1L] passed to `round()`
#' @param nsmall [integer: default 1L] passed to `format()`
#' @param decimal.mark [chr: default mid_d] decimal mark passed to `format()`
#'
#' @return [chr] formatted string
#'
#' @examples
#' fround_dtype_lancet(0.123456789) # "0·1%"
#' fround_dtype_lancet(0.123456789, 'pp', 3, 4) # "0·1230 pp"
#' fround_dtype_lancet(c(55.8346, 123.456789), 'count', 3, 4, ".") # "55.8350"  "123.4570"
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

#' Format and round central/lower/upper value sets by magnitude _without_ units
#'
#' `central` could be mean/median/point_estimate. d_type is required (count data
#' requires nuanced logic), but labels are not returned.
#'
#' Format and round without unit labeling
#' - Use `format_lancet_clu()` for unit labels
#'
#' Defaults are for lancet labels.
#'
#' Current IHME & Lancet standards:
#' - https://hub.ihme.washington.edu/spaces/SC/pages/123833253/Rounding+standards
#' - Rates and Percentages - to one decimal place
#' - Counts                - to 3 sig figs
#'
#' @param clu [num] a numeric vector of central/lower/upper
#' @param d_type [chr c{'prop', 'pp', or 'count'}] data type - proportion,
#'   percentage point or count
#' @param mag_list [named list] output from `set_magnitude()` - must be based on
#'   **central** value of a central/lower/upper set - central _and_ all UI values inherit the
#'   same scale as the central tendency.
#' @param decimal.mark [chr: default mid_dot()] e.g. Lancet standard
#' @param negative_sign [chr: default en_dash()] negative sign Lancet standard
#' @param big.mark_count [chr: default thin_space()] big mark for counts - Lancet specifies narrow
#'   space for counts between 9999 and 1 million
#' @param digits_round_prop [int: default 1L] passed to `round()` for proportions
#' @param digits_sigfig_count [int: default 3L] passed to `signif()` for counts
#' @param nsmall [int: default 1L] passed to `format()`
#'
#' @return [chr] formatted string (vectorized)
#' @export
#'
#' @examples
#' fround_mag_clu(clu = c(central = 0.2, lower = 0.1, upper = 0.3), d_type = "prop") # "0·2" "0·1" "0·3"
#' fround_mag_clu(clu = c(central = 0.2, lower = -0.1, upper = 0.3), d_type = "pp") # "0·2" "–0·1" "0·3"
#' fround_mag_clu(clu = c(central = 95e6, lower = 89e6, upper = 101e6), d_type = "count") # "95·0"  "89·0"   "101"
#' fround_mag_clu(clu = c(central = 95e6, lower = 96e6, upper = 97e6), d_type = "count") # "95·0"  "96·0"  "97·0"
fround_mag_clu_lancet <- function(
      clu
      , d_type
      , mag_list            = set_magnitude(clu[1]) # assuming central is in first position
      , digits_round_prop   = 1L
      , digits_sigfig_count = 3L
      , nsmall              = 1L
      , decimal.mark        = mid_dot()    # lancet centered period
      , negative_sign       = en_dash()    # lancet negative
      , big.mark_count      = thin_space() # lancet narrow space
      , is_lancet           = TRUE
) {

   fround_mag_clu(
      clu                   = clu
      , d_type              = d_type
      , mag_list            = mag_list
      , digits_round_prop   = digits_round_prop
      , digits_sigfig_count = digits_sigfig_count
      , nsmall              = nsmall
      , decimal.mark        = decimal.mark
      , negative_sign       = negative_sign
      , big.mark_count      = big.mark_count
      , is_lancet           = is_lancet
   )
}


#' Lancet Format central/lower/upper data triplets (vectorized)
#'
#' Takes three vectors as main arguments for data.table-friendly vectorization.
#'
#' `central` could be mean/median/point_estimate
#'
#' Transform c(central = 0.994, lower = 0.984, upper = 0.998) to "99.4%
#' (98.4–99.8)"
#'
#' Accounts for negative values, and UIs that cross zero.  Checks if
#' central/lower/upper values are in the correct order.
#' - https://hub.ihme.washington.edu/spaces/SGT/pages/45675035/WRITING+STYLE+GUIDE#WRITINGSTYLEGUIDE-GBDStyleGuideGBDStyleGuide
#'
#' @param central [num] central/point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param d_type [chr {prop, pp, count}] data type - proportion, percentage
#'   point or count
#' @param digits_round_prop [int: default 1L] number of digits to round proportions/PP
#' @param digits_sigfig_count [int: default 3L] number of significant digits for counts
#' @param nsmall [int: default 1L] number of digits after the decimal point
#' @param decimal.mark [chr: default mid_dot()] decimal mark - Lancet specifies mid-dot
#' @param negative_sign [chr: default en_dash()] negative sign
#' @param big.mark_count [chr: default thin_spac] big mark for counts - Lancet specifies narrow
#'   space for counts between 9999 and 1 million
#' @param mean_neg_text [chr: default "a decrease of "] text to prepend if
#'   central is negative e.g. Lancet prefers "a decrease of 99·4\% (98·4–99·8)"
#' @param UI_only [lgl: default FALSE] return only the UI?
#' @param UI_text [chr: default ""] text to appear before values inside UI
#'   parentheses
#' @param assert_clu_relationships [lgl: default TRUE] enforce correct relationship between
#'   central/upper/lower values? This should _ALWAYS_ be `TRUE` _UNLESS_ you
#'   only care about the central difference.
#'
#' @return [chr] formatted string vector
#' @export
#'
#' @examples
#' format_lancet_clu(central = 0.994, lower = 0.984, upper = 0.998, d_type = "prop") # "99.4% (98.4–99.8)"
#' format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop") # "99·4% (98·4–99·8)" "99·4% (98·4–99·8)"
#' format_lancet_clu(central = c(0.994, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "prop") # "1·0% (–0·1 to 1·0)" "1·0% (1·0–1·0)"
#' format_lancet_clu(central = c(-0.05, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "pp") # "1·0 pp (–0·1 to 1·0)" "1·0 pp (1·0–1·0)"
#' format_lancet_clu(central = rep(2, 2), lower = rep(.5, 2), upper = rep(3, 2), d_type = "count")
#' format_lancet_clu(central = rep(2e6, 2), lower = rep(.5e6, 2), upper = rep(3e6, 2), d_type = "count")
#' format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop",  digits_round_prop = 4)
#' format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop",  digits_round_prop = 4, UI_only = T)
#' format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998), d_type = "prop") # fail
#' format_lancet_clu(central = c(0.994, 0.999), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop") # fail
#' format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "propeller") # fail
format_lancet_clu <- function(
      central
      , lower
      , upper
      , d_type
      , digits_round_prop        = 1L
      , digits_sigfig_count      = 3L
      , nsmall                   = 1L
      , decimal.mark             = mid_dot() # lancet centered period
      , negative_sign            = en_dash() # lancet negative
      , big.mark_count           = thin_space() # lancet narrow space
      , mean_neg_text            = "a decrease of "
      , UI_only                  = FALSE
      , UI_text                  = ""
      , assert_clu_relationships = TRUE
      , is_lancet                = TRUE
) {

   format_journal_clu(
      central                    = central
      , lower                    = lower
      , upper                    = upper
      , d_type                   = d_type
      , digits_round_prop        = digits_round_prop
      , digits_sigfig_count      = digits_sigfig_count
      , nsmall                   = nsmall
      , decimal.mark             = decimal.mark
      , negative_sign            = negative_sign
      , big.mark_count           = big.mark_count
      , mean_neg_text            = mean_neg_text
      , UI_only                  = UI_only
      , UI_text                  = UI_text
      , assert_clu_relationships = assert_clu_relationships
      , is_lancet                = is_lancet
   )

}

#' Return a table with formatted central/lower/upper
#'
#' Assumes a single data-type (d_type) for the whole table (e.g. 'prop', 'pp',
#' 'count')
#'
#' @param dt [data.table] with central/lower/upper columns
#' @param d_type [chr {prop', 'pp', 'count'}] data type - proportion, percentage
#'   point or count
#' @param central_var [chr: default 'mean'] name of central tendency e.g.
#'   'point_estimate'
#' @param lower_var [chr: default 'lower']
#' @param upper_var [chr: default 'upper']
#' @param remove_clu [lgl: default TRUE] remove central/lower/upper columns?
#' @param assert_clu_relationships [lgl: default TRUE] enforce correct
#'   relationship between central/upper/lower?
#' @param digits_round_prop [int: default 1L] number of digits to round
#'   proportions/PP
#' @param digits_sigfig_count [int: default 3L] number of significant digits for
#'   counts
#' @param nsmall [int: default 1L] number of digits after the decimal point
#' @param decimal.mark [chr: default mid_dot()] decimal mark - Lancet specifies
#'   mid-dot
#' @param negative_sign [chr: default en_dash()] negative sign
#' @param big.mark_count [chr: default thin_space()] big mark for counts -
#'   Lancet specifies narrow space for counts between 9999 and 1 million
#' @param mean_neg_text [chr: default "a decrease of "] text to prepend if
#'   central is negative
#' @param UI_only [lgl: default FALSE] return only the UI?
#' @param UI_text [chr: default ""] text to appear before vaues inside UI
#'   parentheses
#' @param is_lancet [lgl: default TRUE] apply Lancet-specific formatting for
#'   counts 9999 and smaller
#'
#' @return [data.table] with mean_95_UI_formatted column, and
#'   central/lower/upper columns removed (if specified)
#' @examples
#' DT <- data.table::data.table(
#' location_did = 1
#' , location_name = "Global"
#' , me_name = "vacc_dpt1"
#' , mean = 55.8e6
#' , lower = 50.7e6
#' , upper = 60.7e6
#' )
#' format_lancet_dt(dt = DT, d_type = "count", central_var = 'mean')
#' # location_did location_name   me_name     mean_95_UI_formatted
#' # <num>        <char>    <char>                   <char>
#' #   1:            1        Global vacc_dpt1 55·8 million (50·7–60·7)
format_lancet_dt <- function(
      dt
      , d_type
      , central_var              = "mean"
      , lower_var                = "lower"
      , upper_var                = "upper"
      , remove_clu               = TRUE
      , assert_clu_relationships = TRUE
      , digits_round_prop        = 1L
      , digits_sigfig_count      = 3L
      , nsmall                   = 1L
      , decimal.mark             = mid_dot() # lancet centered period
      , negative_sign            = en_dash() # lancet negative
      , big.mark_count           = thin_space() # lancet narrow space
      , mean_neg_text            = "a decrease of "
      , UI_only                  = FALSE
      , UI_text                  = ""
      , is_lancet                = TRUE
){

   format_journal_dt(
      dt                         = dt
      , d_type                   = d_type
      , central_var              = central_var
      , lower_var                = lower_var
      , upper_var                = upper_var
      , remove_clu               = remove_clu
      , assert_clu_relationships = assert_clu_relationships
      , digits_round_prop        = digits_round_prop
      , digits_sigfig_count      = digits_sigfig_count
      , nsmall                   = nsmall
      , decimal.mark             = decimal.mark
      , negative_sign            = negative_sign
      , big.mark_count           = big.mark_count
      , mean_neg_text            = mean_neg_text
      , UI_only                  = UI_only
      , UI_text                  = UI_text
      , is_lancet                = is_lancet
   )

}

# Nature Family Formatting -----------------------------------------------------

#' Format central/lower/upper sets to Nature CLU format (vectorized)
#'
#' @param central [num] central/point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param d_type [chr {prop, pp, count}] data type - proportion, percentage
#'   point or count
#' @param digits_round_prop [int] number of digits to round proportions/PP
#' @param digits_sigfig_count [int] number of significant digits for counts
#' @param nsmall [int] number of digits after the decimal point
#' @param decimal.mark [chr] decimal mark
#' @param negative_sign [chr] negative sign
#' @param big.mark_count [chr] big mark for counts
#' @param mean_neg_text [chr: default "a decrease of "] text to prepend if all
#'   central/upper/lower triplet values are negative
#' @param UI_only [lgl: default FALSE] return only the UI?
#' @param UI_text [chr: default "95% uncertainty interval, "] text to appear
#'   before values inside UI parentheses
#' @param assert_clu_relationships [lgl] enforce correct relationship between
#'   central/upper/lower values? This should _ALWAYS_ be `TRUE` _UNLESS_ you
#'   only care about the central difference.
#'
#' @return [chr] formatted string vector
#' @export
format_nature_clu <- function(
      central
      , lower
      , upper
      , d_type
      , digits_round_prop        = 1L
      , digits_sigfig_count      = 3L
      , nsmall                   = 1L
      , decimal.mark             = "."
      , negative_sign            = "-"
      , big.mark_count           = ","
      , mean_neg_text            = "a decrease of "
      , UI_only                  = FALSE
      , UI_text                  = ""
      , assert_clu_relationships = TRUE
){
   format_journal_clu(
      central                    = central
      , lower                    = lower
      , upper                    = upper
      , d_type                   = d_type
      , digits_round_prop        = digits_round_prop
      , digits_sigfig_count      = digits_sigfig_count
      , nsmall                   = nsmall
      , decimal.mark             = decimal.mark
      , negative_sign            = negative_sign
      , big.mark_count           = big.mark_count
      , mean_neg_text            = mean_neg_text
      , UI_only                  = UI_only
      , UI_text                  = UI_text
      , assert_clu_relationships = assert_clu_relationships
   )
}


#' Title
#'
#' @param dt [data.table]
#' @param d_type [chr {'prop', 'pp', or 'count'}] a single data type
#' @param central_var [chr: default 'mean'] name of central tendency variable
#' @param lower_var [chr: default 'lower'] name of lower bound variable
#' @param upper_var [chr: default 'upper'] name of upper bound variable
#' @param remove_clu [lgl: default TRUE] remove central/lower/upper variables
#'   after
#' @param assert_clu_relationships [lgl: default TRUE] enforce correct
#'   relationship between
#' @param digits_round_prop [int: default 1L] passed to `round()` for
#'   proportions
#' @param digits_sigfig_count [int: default 3L] passed to `signif()` for counts
#' @param nsmall [int: default 1L] passed to `format()`
#' @param decimal.mark [chr: default "."] decimal mark passed to `format()`
#' @param negative_sign [chr: default "-"] negative sign
#' @param big.mark_count [chr: default ","] big mark for counts passed to
#'   `format()`
#' @param mean_neg_text [chr: default "a decrease of "] text to prepend if
#' @param UI_only [lgl: default FALSE] return only the UI?
#' @param UI_text [chr: default "95% uncertainty interval, "] text to appear
#'   before values inside UI parentheses
#' @param is_lancet [lgl: default FALSE] apply Lancet-specific formatting for
#'   counts 9999 and smaller
#'
#' @returns [data.table] copy of input data.table with new 'clu_fmt' column
#' @export
format_nature_dt <- function(
      dt
      , d_type
      , central_var              = "mean"
      , lower_var                = "lower"
      , upper_var                = "upper"
      , remove_clu               = TRUE
      , assert_clu_relationships = TRUE
      , digits_round_prop        = 1L
      , digits_sigfig_count      = 3L
      , nsmall                   = 1L
      , decimal.mark             = "."
      , negative_sign            = "-"
      , big.mark_count           = ","
      , mean_neg_text            = "a decrease of "
      , UI_only                  = FALSE
      , UI_text                  = ""
      , is_lancet                = FALSE
){

   format_journal_dt(
      dt                         = dt
      , d_type                   = d_type
      , central_var              = central_var
      , lower_var                = lower_var
      , upper_var                = upper_var
      , remove_clu               = remove_clu
      , assert_clu_relationships = assert_clu_relationships
      , digits_round_prop        = digits_round_prop
      , digits_sigfig_count      = digits_sigfig_count
      , nsmall                   = nsmall
      , decimal.mark             = decimal.mark
      , negative_sign            = negative_sign
      , big.mark_count           = big.mark_count
      , mean_neg_text            = mean_neg_text
      , UI_only                  = UI_only
      , UI_text                  = UI_text
   )

}
