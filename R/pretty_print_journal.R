# started: 2025 Aug 29 13:51:03
# purpose: formatting functions for journal presentation

#' Format and round central/lower/upper value sets by magnitude without units.
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
#' @param df_mag [named list] output from `set_magnitude()` - must be based on
#'   central value of a central/lower/upper set - central and all UI values
#'   inherit the same scale as the central tendency.
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'   formatting.
#' @return [chr] formatted string (vectorized)
#' @export
#' @family styled_formats
#'
#' @examples
#' fround_mag_clu(clu = c(central = 0.2, lower = 0.1, upper = 0.3), d_type = "prop")
#' fround_mag_clu(clu = c(central = 0.2, lower = -0.1, upper = 0.3), d_type = "pp")
#' fround_mag_clu(clu = c(central = 95e6, lower = 89e6, upper = 101e6), d_type = "count")
#' fround_mag_clu(clu = c(central = 95e6, lower = 1e5, upper = 101e9), d_type = "count")
#' fround_mag_clu(clu = c(central = 678901, lower = 123456, upper = 6e6), d_type = "count")
fround_mag_clu <- function(
      clu
      , d_type
      , style_name = "nature"
      , df_mag     = set_magnitude(clu[1]) # assuming central is in first position
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
      , "prop"  = fround_props(clu = clu, style_name = style_name)
      , "pp"    = fround_props(clu = clu, style_name = style_name)
      , "count" = fround_count(clu = clu, style_name = style_name, df_mag = df_mag)
   )

   # replace negative sign
   # - This needs to be done here, not in format_journal_clu() in case
   #   fround_mag_clu() is called by the user
   clu_fmt <- unlist(lapply(clu_fmt, function(x_i_chr) {
      sub("^-", style$neg_str_UI, x_i_chr)
   }))

   return(clu_fmt)
}


# Generic Formatting -----------------------------------------------------------

#' Format central, lower, upper value triplets for journal presentation
#'
#' Defaults are generic.  This function allows special formtting marks to be
#' applied by journal. Use `format_lancet_clu()` for Lancet-specific formatting.
#' Use `format_nature_clu()` for Nature-specific formatting.
#'
#' Takes three vectors as main arguments for data.table-friendly vectorization.
#'
#' `central` could be mean, median, point_estimate
#'
#' Transform c(central = 0.994, lower = 0.984, upper = 0.998) to "99.4%
#' (98.4–99.8)"
#'
#' Accounts for negative values, and UIs that cross zero.  Checks if
#' central, lower, upper values are in the correct order.
#'
#' @param central [num] central, point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param d_type [chr c(prop, pp, count)] data type - proportion, percentage
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'  formatting.
#' @return [chr] formatted string vector
#' @export
#' @family styled_formats
#'
#' @examples
#' format_journal_clu(
#'  central = c(0.994, -0.994)
#'  , lower = c(0.984, -0.998)
#'  , upper = c(0.998, -0.984)
#'  , d_type = "prop"
#' )
format_journal_clu <- function(
      central
      , lower
      , upper
      , d_type
      , style_name = "nature"
) {

   d_type <- assert_data_type(d_type)

   style                    <- get_style(style_name)
   neg_str_mean            <- style[["neg_str_mean"]]
   UI_only                  <- style[["UI_only"]]
   UI_text                  <- style[["UI_text"]]
   assert_clu_order <- style[["assert_clu_order"]]
   label_thousands          <- style[["label_thousands"]]

   checkmate::assert_numeric(central, min.len = 1)
   checkmate::assert_numeric(lower)
   checkmate::assert_numeric(upper)
   checkmate::assert_vector(central)
   checkmate::assert_vector(lower)
   checkmate::assert_vector(upper)

   # lists with two shapes for assertions and processing
   # 1. input  - three vectors of equal length (central, lower, upper)
   # 2. output - triplet sets of (central, lower, upper) values for presentation

   clu <- df_strict( # handles equal length assertion
      central = central
      , lower = lower
      , upper = upper
   )

   if(assert_clu_order == TRUE){
      assert_clu_relationship(
         central = clu$central
         , lower = clu$lower
         , upper = clu$upper
      )
   }

   triplets <- t(clu) # transpose for easier processing

   # Magnitude of triplets use raw central value
   # lower and upper inherit central value magnitude scaling
   df_mag  <- set_magnitude(
      # x                 = clu$central
      x                 = triplets["central", ]
      , mag             = NULL
      , label_thousands = label_thousands
      , verbose         = FALSE
   )
   mag_label_vec <- df_mag$mag_label

   # Capture numeric info before character conversion
   # Does UI cross zero? Decide which UI separator to use.
   UI_crosses_zero_vec <- unlist(apply(triplets, 2, function(triplet) {
      (triplet["lower"] < 0) & (triplet["upper"] > 0)
   })) |> unname()

   # Is the central value negative?
   neg_str_mean_vec <- unlist(lapply(triplets["central", ], function(c_i){
      if(c_i < 0) {
         neg_str_mean
      } else {
         ""
      }
   }))

   # Is the whole triplet set negative?
   all_neg_vec <- unlist(apply(triplets, 2, function(triplet) all(triplet <= 0)))

   # Which `(x to/- y)` separator based on triplet set pos/neg relationships
   sep_vec <- data.table::fifelse(
      (UI_crosses_zero_vec == TRUE & all_neg_vec == FALSE)
      , " to ", en_dash()
   )

   # Handle central negatives and full-negative triplet sets
   # - allows style$neg_str_mean to be assigned
   triplets_neg_processed <- process_clu_triplet_negatives(
      triplets = triplets
      , assert_clu_order = assert_clu_order
   )


   # Where the magic happens
   # - Negative signs are styled internally
   triplets_fmt <- lapply(seq_len(ncol(triplets_neg_processed)), function(idx){
      triplet_fmt <- fround_mag_clu(
         clu          = triplets_neg_processed[, idx]
         , d_type     = d_type
         , style_name = style_name
         , df_mag     = df_mag[idx, ]
      )
      # These should be retained, but let's use belt and suspenders
      names(triplet_fmt) <- c("central", "lower", "upper")
      triplet_fmt
   })

   d_type_label <- get_data_type_labels()[[d_type]]

   str_vec <- unlist(lapply(seq_along(triplets_fmt), function(i){
      .cen          <- triplets_fmt[[i]]['central']
      .upp          <- triplets_fmt[[i]]['upper']
      .low          <- triplets_fmt[[i]]['lower']
      .mean_neg_txt <- neg_str_mean_vec[i]
      .mag_label    <- mag_label_vec[i]
      .low_upp_sep  <- sep_vec[i]

      # Building blocks for final string
      str <- glue::glue("{.mean_neg_txt}{.cen}{d_type_label} {.mag_label}({UI_text}{.low}{.low_upp_sep}{.upp})")

      if (UI_only) {
         str <- glue::glue("{UI_text}{.low}{.low_upp_sep}{.upp}{.mag_label}")
      }

      return(str)

   }))

   return(str_vec)
}


#' Return a table with formatted central, lower, upper
#'
#' Assumes a single data-type (d_type) for the whole table (e.g. 'prop', 'pp',
#' 'count')
#'
#' @param df [data.frame, data.table]
#' @param d_type [chr c('prop', 'pp', or 'count')] a single data type
#' @param central_var [chr: default 'mean'] name of central tendency variable
#' @param lower_var [chr: default 'lower'] name of lower bound variable
#' @param upper_var [chr: default 'upper'] name of upper bound variable
#' @param remove_clu_columns [lgl: default TRUE] remove central, lower, upper variables after
#'  formatting?
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'   formatting.
#' @param new_var [chr: default 'clu_fmt'] name of new formatted column
#' @returns [data.frame] data.frame, data.table with new 'clu_fmt' column
#' @export
#' @family styled_formats
#'
#' @examples
#' df <- data.frame(
#'  location_id = c(1, 2, 3)
#'  , mean = c(0.1234, 0, -0.3456)
#'  , lower = c(0.1134, -0.2245, -0.4445)
#'  , upper = c(0.1334, 0.2445, 0.3556)
#' )
#' format_journal_df(df, d_type = "prop")
#'
#' DF <- data.frame(
#'  location_id = c(1, 2, 3)
#'  , mean = c(0.1234, 0, -0.3456)
#'  , lower = c(0.1134, -0.2245, -0.4445)
#'  , upper = c(0.1334, 0.2445, 0.3556)
#' )
#' format_journal_df(DF, d_type = "prop")
format_journal_df <- function(
      df
      , d_type
      , new_var            = "clu_fmt"
      , style_name         = "nature"
      , central_var        = "mean"
      , lower_var          = "lower"
      , upper_var          = "upper"
      , remove_clu_columns = TRUE
){

   d_type <- assert_data_type(d_type)

   checkmate::assert_string(central_var)
   checkmate::assert_string(lower_var)
   checkmate::assert_string(upper_var)
   vars_clu <- c(central_var, lower_var, upper_var)
   assert_x_in_y(vars_clu, colnames(df))
   checkmate::assert_logical(remove_clu_columns, len = 1)

   df <- add_column(
      x         = df
      , varname = new_var
      , vec     = format_journal_clu(
         central      = df[[central_var]]
         , lower      = df[[lower_var]]
         , upper      = df[[upper_var]]
         , d_type     = d_type
         , style_name = style_name
      )
   )

   if (remove_clu_columns == TRUE) df <- drop_columns(x = df, varnames = vars_clu)

   return(df[]) # helps data.table printing
}

#' Format multiple data.frame 'mean_*' columns for presentation (by data type).
#'
#' Format one or more 'mean_' columns by magnitude, data_type, and style.
#'
#' BEWARE: Does not have sophisticated count-type data handling like
#' `format_journal_clu()`.  This is a simple formatter for multiple mean columns.
#' Use with caution.
#'
#' @param df [data.table] input data.table with one or more 'mean_' columns
#' @param d_type [chr c('prop', 'pp', or 'count')] a single data type
#' @param central_var [chr: default 'mean'] prefix of mean variable names to
#'   format.  Implemented as e.g. "^mean[_]*" to capture 'mean', 'mean_1990',
#'   'mean_2000', etc.
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'  formatting.
#'
#' @returns [data.table] copy of input data.table with formatted mean column(s)
#' @export
#' @family styled_formats
#'
#' @examples
#' df <- data.frame(
#'   location_id = c(1, 2, 3)
#'   , mean_1990 = c(100, 1e6, 1e9)
#'   , mean_2000 = c(200, 2e6, 2e-1)
#'  )
#' format_means_df(df, d_type = "count")
format_means_df <- function(
      df
      , d_type
      , central_var = "mean"
      , style_name  = "nature"
){
   checkmate::assert_data_frame(df)
   assert_data_type(d_type)
   checkmate::assert_string(central_var)

   style <- get_style(style_name)

   digits <- switch(
      d_type
      , "prop"  = style[["digits_round_prop"]]
      , "pp"    = style[["digits_round_prop"]]
      , "count" = style[["digits_sigfig_count"]]
   )

   scalar <- switch(
      d_type
      , "prop"  = 100
      , "pp"    = 100
      , "count" = 1
   )

   label <- get_data_type_labels()[[d_type]]

   mean_varnames <- grep(
      pattern = sprintf("^%s[_]+", central_var)
      , x     = colnames(df)
      , value = TRUE
   )

   for (varname in mean_varnames){
      df <- add_column(
         x        = df
         , varname = varname
         , vec     = paste0(
            fmt_magnitude(
               x                 = df[[varname]] * scalar
               , mag             = NULL
               , label_thousands = style[["label_thousands"]]
               , digits          = digits
               , nsmall          = style[["nsmall"]]
            )
            , label
         )
         , overwrite = TRUE
      )
   }

   return(df[])
}

# Lancet Family Formatting -----------------------------------------------------


#' Format and round central, lower, upper value sets by magnitude without units
#' for Lancet journal presentation
#'
#' @param clu [num] a numeric vector of three values in central, lower, upper order.
#' @param d_type [chr c('prop', 'pp', or 'count')] data type - proportion,
#'  percentage point or count.
#' @param df_mag [named list] output from `set_magnitude()` - must be based on
#'  central value of a central, lower, upper set - central and all UI values
#'  inherit the same scale as the central tendency.
#'
#' @returns [chr] formatted string (vectorized)
#' @export
#'
#' @examples
#' fround_mag_clu_lancet(c(central = 0.2, lower = 0.1, upper = 0.3), "prop")
fround_mag_clu_lancet <- function(
      clu
      , d_type
      , df_mag = set_magnitude(clu[1]) # assuming central is in first position
) {
   fround_mag_clu(
      clu          = clu
      , d_type     = d_type
      , style_name = "lancet"
      , df_mag     = df_mag
   )
}

#' Format central, lower, upper value triplets for Lancet journal presentation
#'
#' @param central [num] central, point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param d_type [chr c(prop, pp, count)] data type - proportion, percentage
#'
#' @returns [chr] formatted string vector
#' @export
#' @family styled_formats
#'
#' @examples
#' format_lancet_clu(
#'    central  = c(0.994, -0.994)
#'    , lower  = c(0.984, -0.998)
#'    , upper  = c(0.998, -0.984)
#'    , d_type = "prop"
#' )
format_lancet_clu <- function(
      central
      , lower
      , upper
      , d_type
) {
   format_journal_clu(
      central      = central
      , lower      = lower
      , upper      = upper
      , d_type     = d_type
      , style_name = "lancet"
   )
}

#' Return a table with formatted central, lower, upper for Lancet journal
#'
#' Assumes a single data-type (d_type) for the whole table (e.g. 'prop', 'pp',
#' 'count')
#'
#' @param df [data.table] with central, lower, upper columns
#' @param d_type [chr c(prop', 'pp', 'count')] data type - proportion, percentage
#'   point or count
#' @param central_var [chr: default 'mean'] name of central tendency e.g.
#'   'point_estimate'
#' @param lower_var [chr: default 'lower']
#' @param upper_var [chr: default 'upper']
#' @param new_var [chr: default 'clu_fmt'] name of new formatted column
#' @param remove_clu_columns [lgl: default TRUE] remove central, lower, upper columns?
#'
#' @returns [data.frame, data.table] with mean_95_UI_formatted column, and
#'   central, lower, upper columns removed (if specified)
#' @export
#' @family styled_formats
#'
#' @examples
#' df <- data.frame(
#'    location_did    = 1
#'    , location_name = "Global"
#'    , me_name       = "vacc_dpt1"
#'    , mean          = 55.8e6
#'    , lower         = 50.7e6
#'    , upper         = 60.7e6
#' )
#' format_lancet_df(df = df, d_type = "count", central_var = 'mean')
format_lancet_df <- function(
      df
      , d_type
      , new_var            = "clu_fmt"
      , central_var        = "mean"
      , lower_var          = "lower"
      , upper_var          = "upper"
      , remove_clu_columns = TRUE
){
   format_journal_df(
      df                   = df
      , d_type             = d_type
      , style_name         = "lancet"
      , central_var        = central_var
      , lower_var          = lower_var
      , upper_var          = upper_var
      , new_var            = new_var
      , remove_clu_columns = remove_clu_columns
   )
}


# Nature Family Formatting -----------------------------------------------------

#' Format and round central, lower, upper value sets by magnitude without units
#' for Nature journal presentation.
#'
#' @param clu [num] a numeric vector of three values in central, lower, upper order.
#' @param d_type [chr c('prop', 'pp', or 'count')] data type - proportion,
#'  percentage point or count.
#' @param df_mag [data.frame] output from `set_magnitude()` - must be based on
#'  central value of a central, lower, upper set - central and all UI values
#'  inherit the same scale as the central tendency.
#'
#' @returns [chr] formatted string (vectorized)
#' @export
#'
#' @examples
#' fround_mag_clu_nature(c(central = 0.2, lower = 0.1, upper = 0.3), "prop")
fround_mag_clu_nature <- function(
      clu
      , d_type
      , df_mag = set_magnitude(clu[1]) # assuming central is in first position
) {
   fround_mag_clu(
      clu          = clu
      , d_type     = d_type
      , style_name = "nature"
      , df_mag     = df_mag
   )
}

#' Format central, lower, upper value triplets for Nature journal presentation
#'
#' @param central [num] central, point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param d_type [chr c(prop, pp, count)] data type - proportion, percentage
#'
#' @returns [chr] formatted string vector
#' @export
#' @family styled_formats
#'
#' @examples
#' format_nature_clu(
#'    central  = c(0.994, -0.994)
#'    , lower  = c(0.984, -0.998)
#'    , upper  = c(0.998, -0.984)
#'    , d_type = "prop"
#' )
format_nature_clu <- function(
      central
      , lower
      , upper
      , d_type
) {
   format_journal_clu(
      central      = central
      , lower      = lower
      , upper      = upper
      , d_type     = d_type
      , style_name = "nature"
   )
}

#' Return a table with formatted central, lower, upper for Nature journal
#'
#' @param df [data.table]
#' @param d_type [chr c('prop', 'pp', or 'count')] a single data type
#' @param new_var [chr: default 'clu_fmt'] name of new formatted column
#' @param central_var [chr: default 'mean'] name of central tendency variable
#' @param lower_var [chr: default 'lower'] name of lower bound variable
#' @param upper_var [chr: default 'upper'] name of upper bound variable
#' @param remove_clu_columns [lgl: default TRUE] remove central, lower, upper variables
#'   after
#' @returns [data.table] copy of input data.table with new 'clu_fmt' column
#' @export
#' @family styled_formats
#'
#' @examples
#' df <- data.frame(
#'    location_did    = 1
#'    , location_name = "Global"
#'    , me_name       = "vacc_dpt1"
#'    , mean          = 55.8e6
#'    , lower         = 50.7e6
#'    , upper         = 60.7e6
#' )
#' format_nature_df(df = df, d_type = "count", central_var = 'mean')
format_nature_df <- function(
      df
      , d_type
      , new_var            = "clu_fmt"
      , central_var        = "mean"
      , lower_var          = "lower"
      , upper_var          = "upper"
      , remove_clu_columns = TRUE
){
   format_journal_df(
      df                   = df
      , d_type             = d_type
      , style_name         = "nature"
      , central_var        = central_var
      , lower_var          = lower_var
      , upper_var          = upper_var
      , new_var            = new_var
      , remove_clu_columns = remove_clu_columns
   )
}
