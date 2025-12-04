# started: 2025 Aug 29 13:51:03
# purpose: formatting functions for journal presentation


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
#' Accounts for negative values, and UIs that cross zero.  Checks if central,
#' lower, upper values are in the correct order.
#'
#' @param central [num] central, point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param metric [chr c(prop, pp, count, rate)] metric - proportion,
#'   percentage point, count, or rate
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'   formatting.
#' @param rate_unit [chr: default NULL] rate unit label (required when metric = 'rate')
#'   - Examples: "deaths", "cases", "events", "births"
#' @return [chr] formatted string vector
#' @export
#' @family styled_formats
#' @examples
#' format_journal_clu(
#'  central = c(0.994, -0.994)
#'  , lower = c(0.984, -0.998)
#'  , upper = c(0.998, -0.984)
#'  , metric = "prop"
#' )
#'
#' # Rate formatting with rate_unit
#' format_journal_clu(
#'   central   = 0.0000123,
#'   lower     = 0.0000098,
#'   upper     = 0.0000152,
#'   metric    = "rate",
#'   rate_unit = "deaths"
#' )
format_journal_clu <- function(
      central
      , lower
      , upper
      , metric
      , style_name = "nature"
      , rate_unit  = NULL
) {

   metric <- assert_metric(metric)
   assert_rate_unit(metric, rate_unit)

   style                  <- get_style(style_name)
   neg_mark_mean          <- style[["neg_mark_mean"]]
   UI_only                <- style[["UI_only"]]
   UI_text                <- style[["UI_text"]]
   assert_clu_order       <- style[["assert_clu_order"]]
   count_label_thousands  <- style[["count_label_thousands"]]
   round_5_up             <- style[["round_5_up"]]

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

   n <- nrow(clu)

   # === REMOVED: All state management ===
   # - init_df_mag_state(n)
   # - on.exit(flush_df_mag_state(), add = TRUE)
   # - Thread safety code (data.table::setDTthreads)
   # - Initial set_magnitude() call
   # - set_df_mag_state(df_mag)

   if(assert_clu_order == TRUE){
      assert_clu_relationship(
         central = clu$central
         , lower = clu$lower
         , upper = clu$upper
      )
   }

   triplets <- t(clu) # transpose for easier processing

   # Capture numeric info before character conversion
   # Does UI cross zero? Decide which UI separator to use.
   UI_crosses_zero_vec <- unlist(apply(triplets, 2, function(triplet) {
      (triplet["lower"] < 0) & (triplet["upper"] > 0)
   })) |> unname()

   # Is the central value negative?
   neg_str_mean_vec <- unlist(lapply(triplets["central", ], function(c_i){
      if(c_i < 0) {
         neg_mark_mean
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
   # - allows style$neg_mark_mean to be assigned
   triplets_neg_processed <- process_clu_triplet_negatives(
      triplets = triplets
      , assert_clu_order = assert_clu_order
   )

   # === NEW: Format triplets and accumulate df_mag ===
   # Each call returns list(formatted = chr[3], df_mag_row = df[1,])
   results_list <- lapply(seq_len(ncol(triplets_neg_processed)), function(idx){
      result <- fround_clu_triplet(
         clu          = triplets_neg_processed[, idx]
         , metric     = metric
         , style_name = style_name
      )

      # Validate schema
      assert_fround_return_schema(
         result,
         context = sprintf("fround_clu_triplet (triplet %d)", idx)
      )

      # Ensure names are present
      names(result$formatted) <- c("central", "lower", "upper")

      return(result)
   })

   # Extract formatted values
   triplets_fmt <- lapply(results_list, function(r) r$formatted)

   # Accumulate df_mag rows
   df_mag <- do.call(rbind, lapply(results_list, function(r) r$df_mag_row))
   rownames(df_mag) <- NULL  # Clean up row names

   # === END NEW ===

   metric_label <- get_metric_labels(metric)

   is_rate_type <- (metric == "rate")

   str_vec <- unlist(lapply(seq_along(triplets_fmt), function(i){
      .cen          <- triplets_fmt[[i]]['central']
      .upp          <- triplets_fmt[[i]]['upper']
      .low          <- triplets_fmt[[i]]['lower']
      .mean_neg_txt <- neg_str_mean_vec[i]
      .mag_label    <- df_mag$mag_label[i]
      .low_upp_sep  <- sep_vec[i]

      # Define rate-specific components (conditionally populated)
      if (is_rate_type) {
         rate_unit_fmt    <- sprintf(" %s", rate_unit)   # " deaths " or " cases "
         .rate_mag_label  <- sprintf(" %s", .mag_label)   # " per 100,000"
         .count_mag_label <- ""                           # empty for rates
         .type_label      <- ""                           # empty for rates
      } else {
         rate_unit_fmt    <- ""                           # empty for non-rates
         .rate_mag_label  <- ""                           # empty for non-rates
         .count_mag_label <- .mag_label                   # "million " for counts
         .type_label      <- metric_label                 # "%" for props
      }

      # Single glue template handles all types
      str <- glue::glue(
         "{.mean_neg_txt}{.cen}{rate_unit_fmt}{.type_label} {.count_mag_label}({UI_text}{.low}{.low_upp_sep}{.upp}){.rate_mag_label}"
      )

      if (UI_only) {
         str <- glue::glue(
            "{UI_text}{.low}{.low_upp_sep}{.upp}{rate_unit_fmt}{.count_mag_label}{.rate_mag_label}"
         )
      }

      return(str)

   }))

   return(str_vec)
}


#' Return a table with formatted central, lower, upper
#'
#' Assumes a single data-type (metric) for the whole table (e.g. 'prop', 'pp',
#' 'count')
#'
#' @param df [data.frame, data.table]
#' @param metric [chr c('prop', 'pp', 'count', 'rate')] a single metric
#' @param central_var [chr: default 'mean'] name of central tendency variable
#' @param lower_var [chr: default 'lower'] name of lower bound variable
#' @param upper_var [chr: default 'upper'] name of upper bound variable
#' @param remove_clu_columns [lgl: default TRUE] remove central, lower, upper
#'   variables after formatting?
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'   formatting.
#' @param rate_unit [chr: default NULL] rate unit label (required when metric = 'rate')
#'   - Examples: "deaths", "cases", "events", "births"
#' @param new_var [chr: default 'clu_fmt'] name of new formatted column
#'
#' @returns [data.frame] data.frame, data.table with new 'clu_fmt' column
#' @export
#' @family styled_formats
#'
#' @examples
#' df <- data.frame(
#'  location_id = c(1, 2, 3)
#'  , mean      = c(0.1234, 0, -0.3456)
#'  , lower     = c(0.1134, -0.2245, -0.4445)
#'  , upper     = c(0.1334, 0.2445, 0.3556)
#' )
#' format_journal_df(df, metric = "prop")
#'
#' # Rate formatting example
#' rate_df <- data.frame(
#'   location = c("Global", "USA"),
#'   mean     = c(0.0000123, 0.0000456),
#'   lower    = c(0.0000098, 0.0000401),
#'   upper    = c(0.0000152, 0.0000512)
#' )
#' format_journal_df(rate_df, metric = "rate", rate_unit = "deaths")
format_journal_df <- function(
      df
      , metric
      , new_var            = "clu_fmt"
      , style_name         = "nature"
      , central_var        = "mean"
      , lower_var          = "lower"
      , upper_var          = "upper"
      , remove_clu_columns = TRUE
      , rate_unit          = NULL
){

   metric <- assert_metric(metric)

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
         , metric     = metric
         , style_name = style_name
         , rate_unit  = rate_unit
      )
   )

   if (remove_clu_columns == TRUE) df <- drop_columns(x = df, varnames = vars_clu)

   return(df[]) # helps data.table printing
}

#' Format multiple data.frame 'mean_*' columns for presentation (by metric).
#'
#' Format one or more 'mean_' columns by magnitude, metric, and style.
#'
#' BEWARE: Does not have sophisticated count-type data handling like
#' `format_journal_clu()`.  This is a simple formatter for multiple mean
#' columns. Use with caution.
#'
#' @param df [data.table] input data.table with one or more 'mean_' columns
#' @param metric [chr c('prop', 'pp', 'count', 'rate')] a single metric
#' @param central_var [chr: default 'mean'] prefix of mean variable names to
#'   format.  Implemented as e.g. "^mean[_]*" to capture 'mean', 'mean_1990',
#'   'mean_2000', etc.
#' @param style_name [chr: default 'nature'] style name - controls rounding and
#'   formatting.
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
#' format_means_df(df, metric = "count")
format_means_df <- function(
      df
      , metric
      , central_var = "mean"
      , style_name  = "nature"
){
   checkmate::assert_data_frame(df)
   assert_metric(metric)
   checkmate::assert_string(central_var)

   style <- get_style(style_name)

   digits <- get_style_item_by_metric(
      style_name   = style_name
      , style_item = "digits"
      , metric     = metric
   )
   n_small <- get_style_item_by_metric(
      style_name   = style_name
      , style_item = "n_small"
      , metric     = metric
   )

   label <- get_metric_labels(metric)

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
               x                       = df[[varname]]
               , mag                   = NULL
               , count_label_thousands = style[["count_label_thousands"]]
               , digits                = digits
               , nsmall                = n_small
               , metric                = metric
            )
            , label
         )
         , overwrite = TRUE
      )
   }

   return(df[])
}

# Lancet Family Formatting -----------------------------------------------------


#' Format central, lower, upper value triplets for Lancet journal presentation
#'
#' @param central [num] central, point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param metric [chr c(prop, pp, count, rate)] metric - proportion,
#'   percentage point, count, or rate
#' @param rate_unit [chr: default NULL] rate unit label (required when metric = 'rate')
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
#'    , metric = "prop"
#' )
#'
#' # Rate example with Lancet formatting
#' format_lancet_clu(
#'   central   = 0.0000123,
#'   lower     = 0.0000098,
#'   upper     = 0.0000152,
#'   metric    = "rate",
#'   rate_unit = "deaths"
#' )
format_lancet_clu <- function(
      central
      , lower
      , upper
      , metric
      , rate_unit = NULL
) {
   format_journal_clu(
      central      = central
      , lower      = lower
      , upper      = upper
      , metric     = metric
      , style_name = "lancet"
      , rate_unit  = rate_unit
   )
}

#' Return a table with formatted central, lower, upper for Lancet journal
#'
#' Assumes a single data-type (metric) for the whole table (e.g. 'prop', 'pp',
#' 'count')
#'
#' @param df [data.table] with central, lower, upper columns
#' @param metric [chr c('prop', 'pp', 'count', 'rate')] metric - proportion,
#'   percentage point, count, or rate
#' @param central_var [chr: default 'mean'] name of central tendency e.g.
#'   'point_estimate'
#' @param lower_var [chr: default 'lower']
#' @param upper_var [chr: default 'upper']
#' @param new_var [chr: default 'clu_fmt'] name of new formatted column
#' @param remove_clu_columns [lgl: default TRUE] remove central, lower, upper
#'   columns after formatting?
#' @param rate_unit [chr: default NULL] rate unit label (required when metric = 'rate')
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
#' format_lancet_df(df = df, metric = "count", central_var = 'mean')
format_lancet_df <- function(
      df
      , metric
      , new_var            = "clu_fmt"
      , central_var        = "mean"
      , lower_var          = "lower"
      , upper_var          = "upper"
      , remove_clu_columns = TRUE
      , rate_unit          = NULL
){
   format_journal_df(
      df                   = df
      , metric             = metric
      , style_name         = "lancet"
      , central_var        = central_var
      , lower_var          = lower_var
      , upper_var          = upper_var
      , new_var            = new_var
      , remove_clu_columns = remove_clu_columns
      , rate_unit          = rate_unit
   )
}


# Nature Family Formatting -----------------------------------------------------


#' Format central, lower, upper value triplets for Nature journal presentation
#'
#' @param central [num] central, point_estimate value vector
#' @param lower [num] lower bound vector
#' @param upper [num] upper bound vector
#' @param metric [chr c(prop, pp, count, rate)] metric - proportion,
#'   percentage point, count, or rate
#' @param rate_unit [chr: default NULL] rate unit label (required when metric = 'rate')
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
#'    , metric = "prop"
#' )
#'
#' # Rate example with Nature formatting
#' format_nature_clu(
#'   central   = 0.0000123,
#'   lower     = 0.0000098,
#'   upper     = 0.0000152,
#'   metric    = "rate",
#'   rate_unit = "cases"
#' )
format_nature_clu <- function(
      central
      , lower
      , upper
      , metric
      , rate_unit = NULL
) {
   format_journal_clu(
      central      = central
      , lower      = lower
      , upper      = upper
      , metric     = metric
      , style_name = "nature"
      , rate_unit  = rate_unit
   )
}

#' Return a table with formatted central, lower, upper for Nature journal
#'
#' @param df [data.table]
#' @param metric [chr c('prop', 'pp', 'count', 'rate')] a single metric
#' @param new_var [chr: default 'clu_fmt'] name of new formatted column
#' @param central_var [chr: default 'mean'] name of central tendency variable
#' @param lower_var [chr: default 'lower'] name of lower bound variable
#' @param upper_var [chr: default 'upper'] name of upper bound variable
#' @param remove_clu_columns [lgl: default TRUE] remove central, lower, upper
#'   columns after formatting?
#' @param rate_unit [chr: default NULL] rate unit label (required when metric = 'rate')
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
#' format_nature_df(df = df, metric = "count", central_var = 'mean')
format_nature_df <- function(
      df
      , metric
      , new_var            = "clu_fmt"
      , central_var        = "mean"
      , lower_var          = "lower"
      , upper_var          = "upper"
      , remove_clu_columns = TRUE
      , rate_unit          = NULL
){
   format_journal_df(
      df                   = df
      , metric             = metric
      , style_name         = "nature"
      , central_var        = central_var
      , lower_var          = lower_var
      , upper_var          = upper_var
      , new_var            = new_var
      , remove_clu_columns = remove_clu_columns
      , rate_unit          = rate_unit
   )
}
