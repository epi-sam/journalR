#' Get style schema
#'
#' Centrally managed definition for all required journal format styles.
#'
#' @returns [list] named list of style elements and their expected types
#' @export
#' @family styles
#'
#' @examples
#' get_style_schema()
get_style_schema <- function(){
   list(
      # Proportion parameters
      prop_digits_round       = "integer"
      , prop_nsmall           = "integer"

      # Count parameters
      , count_method          = "character"
      , count_digits_sigfig   = "integer"
      , count_pad_sigfigs     = "logical"
      , count_nsmall          = "integer"
      , count_big.mark        = "character"
      , count_label_thousands = "logical"

      # Rate parameters
      , rate_method           = "character"
      , rate_digits_sigfig    = "integer"
      , rate_pad_sigfigs      = "logical"
      , rate_nsmall           = "integer"

      # Shared parameters
      , decimal.mark          = "character"
      , neg_mark_mean         = "character"
      , neg_mark_UI           = "character"
      , UI_text               = "character"
      , UI_only               = "logical"
      , assert_clu_order      = "logical"
      , is_lancet             = "logical"
      , round_5_up            = "logical"
   )
}


#' Assert style schema
#'
#' Validates that a style entry conforms to the expected schema defined in
#' `get_style_schema()`.
#'
#' @param style_entry [list] named list representing a style entry
#'
#' @returns [list] invisible validated style_entry
#' @keywords internal
#' @family assertions
#' @family styles
assert_style_schema <- function(style_entry){

   require_args(style_entry)

   checkmate::assert_list(style_entry, names = "named")

   # Assert all entry elements are present
   style_schema <- get_style_schema()
   assert_x_in_y(names(style_schema), names(style_entry))

   # Assert choice subsets for some key items
   tryCatch(
      assert_set_choice(style_entry[["count_method"]], c("sigfig", "decimal", "int"))
      , error = function(e)
      {
         stop(
            sprintf(
               "Style schema is malformed - %s - please inspect the sytle_entry:\n  "
               , "count_method"
            )
            , conditionMessage(e)
         )
      }
   )

   # Validate rate_method
   tryCatch(
      assert_set_choice(style_entry[["rate_method"]], c("sigfig", "decimal", "int"))
      , error = function(e)
      {
         stop(
            sprintf(
               "Style schema is malformed - %s - please inspect the sytle_entry:\n  "
               , "rate_method"
            )
            , conditionMessage(e)
         )
      }
   )

   # Assert metrics
   lapply(seq_along(style_entry), function(i){
      x              <- style_entry[[i]]
      xname          <- names(style_entry)[i]
      xtype_expected <- style_schema[[xname]]
      xtype_actual   <- typeof(x)
      xclass_actual  <- class(x)

      tryCatch(
         assert_set_choice(xname, choices = names(style_schema))
         , error = function(e)
         {
            stop(
               sprintf(
                  "Style schema is malformed - %s - please inspect the sytle_entry:\n  "
                  , xname
               )
               , conditionMessage(e)
            )
         }
      )

      tryCatch(
         checkmate::assert_scalar(x)
         , error = function(e)
         {
            stop(
               sprintf(
                  "All style_entries must be length 1 key/value pairs: %s\n  "
                  , xname
               )
               , conditionMessage(e)
            )
         }
      )

      # Check for integerish values, and convert to integer gracefully if found
      if(xtype_expected == "integer"){
         x_is_int <- FALSE
         if(xtype_actual == "integer"){
            x_is_int <- TRUE
         } else if (xtype_actual == "double") {
            # check for integerish
            if(x == floor(x)){
               x_is_int <- TRUE
            }
         }

         if(x_is_int) {
            x <- as.integer(x)
         } else {
            stop(
               sprintf(
                  "style element '%s' should be integer-ish but is type: '%s', class: %s"
                  , xname
                  , xtype_actual
                  , toString(xclass_actual)
               )
            )
         }
         # so far integers always set number of formatting digits
         checkmate::assert_integer(x, lower = 0)
      } # end of integer handling

      checkmate::assert_class(x, classes = xtype_expected, null.ok = FALSE)
   })

   invisible(style_entry)
}


#' Set a new style by list
#'
#' @param style_name [chr] name of the style to set
#' @param style_entry [list] named list representing the style entry
#'
#' @returns [chr] invisible vector of input objects, to allow easier un-locking
#' @export
#' @family styles
#'
#' @examples
#' set_style(
#'    style_name    = "my_style"
#'    , style_entry = list(
#'       prop_digits_round       = 1
#'       , prop_nsmall           = 1
#'       , count_method          = "sigfig"
#'       , count_digits_sigfig   = 3
#'       , count_pad_sigfigs     = TRUE
#'       , count_nsmall          = 1
#'       , count_big.mark        = ","
#'       , count_label_thousands = FALSE
#'       , rate_method           = "sigfig"
#'       , rate_digits_sigfig    = 3
#'       , rate_pad_sigfigs      = TRUE
#'       , rate_nsmall           = 1
#'       , decimal.mark          = "."
#'       , neg_mark_mean         = "-"
#'       , neg_mark_UI           = "-"
#'       , UI_only               = FALSE
#'       , UI_text               = ""
#'       , assert_clu_order      = TRUE
#'       , is_lancet             = FALSE
#'       , round_5_up            = TRUE
#'    )
#' )
set_style <- function(style_name, style_entry){
   # checkmate::assert_string(style_name) # done within set_dict_format()
   assert_style_schema(style_entry)
   set_dict_format(dict_name = style_name, dict_entry = style_entry)
}


#' Make a new style by args
#'
#' Wrapper function to create and set a new style in one step.
#'
#' @param style_name [chr] name of the style to set
#' @param prop_digits_round [int: default 1] number of digits to round proportions to
#' @param prop_nsmall [int: default 1] minimum number of digits to the right of the decimal point - proportions
#' @param count_method [chr: c("sigfig", "decimal", "int")] choose how to report counts - prioritize sigfigs across mean/lower/upper, hard-set decimals, or leave numbers in integer space.
#' @param count_digits_sigfig [int: default 3] number of significant figures for counts
#' @param count_nsmall [int: default 1] passed to `format()` if `count_method` == 'decimal'
#' @param count_pad_sigfigs [lgl: default TRUE] signif(5.00, 3) is "5" - do you want to pad the trailing 0s back on - usually TRUE?
#' @param decimal.mark [chr: default "."] decimal mark e.g. "." or `mid_dot()` for Lancet.
#' @param neg_mark_mean [chr: default "-"] string to describe central value negatives - e.g. "-1 (-2 to 4)" could become "Negtive 1 (-2 to 4)"
#' @param neg_mark_UI [chr: default "-"] string to describe negative sign in UI brackets e.g. "1 (-2 to 4)" could become "1 (--2 to 4)" (en-dash)
#' @param count_big.mark [chr: default ","] character to use for counts thousand, million, billion separator e.g. ","
#' @param UI_only [lgl: default FALSE] Return only UI from `format_journal_df()` family functions?
#' @param UI_text [chr: default ""] Text to appear inside UI brackets before numbers e.g. "2 (1 -- 4)" could become "2 (95\%UI 1 -- 4)"
#' @param assert_clu_order [lgl: default TRUE] whether to assert CLU relationships (ensure lower < central < upper)
#' @param is_lancet [lgl: default FALSE] TRUE to handle edge-case Lancet count formatting policies
#' @param count_label_thousands [lgl: default FALSE] whether format counts as e.g. 10,000 as '10 thousand'
#' @param rate_method [chr: c("sigfig", "decimal", "int")] choose how to report rates - prioritize sigfigs across mean/lower/upper, hard-set decimals, or leave numbers in integer space.
#' @param rate_digits_sigfig [int: default 3] number of significant figures for rates
#' @param rate_pad_sigfigs [lgl: default TRUE] signif(5.00, 3) is "5" - do you want to pad the trailing 0s back on for rates - usually TRUE?
#' @param rate_nsmall [int: default 1] passed to `format()` if `rate_method` == 'decimal'
#' @param round_5_up [lgl: default TRUE] In R, `round(1245, 3)` is "1240".  Do you want to round to "1250" instead? Default TRUE to conform with common expectations.
#'
#' @returns [chr] invisible vector of input objects
#' @export
#' @family styles
#' @family styled_formats
#'
#' @examples
#' new_style(style_name = "my_style")
new_style <- function(
      style_name
      , prop_digits_round     = 1
      , prop_nsmall           = 1
      , count_method          = "sigfig"
      , count_digits_sigfig   = 3
      , count_pad_sigfigs     = TRUE
      , count_nsmall          = 1
      , count_big.mark        = ","
      , count_label_thousands = FALSE
      , rate_method           = "sigfig"
      , rate_digits_sigfig    = 3
      , rate_pad_sigfigs      = TRUE
      , rate_nsmall           = 1
      , decimal.mark          = "."
      , neg_mark_mean         = "-"
      , neg_mark_UI           = "-"
      , UI_only               = FALSE
      , UI_text               = ""
      , assert_clu_order      = TRUE
      , is_lancet             = FALSE
      , round_5_up            = TRUE
){
   set_style(
      style_name    = style_name
      , style_entry = list(
         prop_digits_round       = prop_digits_round
         , prop_nsmall           = prop_nsmall
         , count_method          = count_method
         , count_digits_sigfig   = count_digits_sigfig
         , count_pad_sigfigs     = count_pad_sigfigs
         , count_nsmall          = count_nsmall
         , count_big.mark        = count_big.mark
         , count_label_thousands = count_label_thousands
         , rate_method           = rate_method
         , rate_digits_sigfig    = rate_digits_sigfig
         , rate_pad_sigfigs      = rate_pad_sigfigs
         , rate_nsmall           = rate_nsmall
         , decimal.mark          = decimal.mark
         , neg_mark_mean         = neg_mark_mean
         , neg_mark_UI           = neg_mark_UI
         , UI_only               = UI_only
         , UI_text               = UI_text
         , assert_clu_order      = assert_clu_order
         , is_lancet             = is_lancet
         , round_5_up            = round_5_up
      )
   )
}

#' Get a style from the styles dictionary
#'
#' Accessor function to retrieve a style from the package's
#' styles dictionary.
#'
#' @param style_name [chr] name of the style to retrieve
#'
#' @returns [list] the requested style as a named list
#' @export
#' @family styles
#'
#' @examples
#' get_style("lancet")
get_style <- function(style_name) {
   get_dict_format(dict_name = style_name) |>
      assert_style_schema()
}


# ---- Pre-Defined Styles ------------------------------------------------------

#' Nature style schema
#'
#' The default style for the package.
#'
#' Pre-defined style schema for Nature journal formatting.
#'
#' @returns [list] named list representing the nature style
#' @export
#' @family styles
#'
#' @examples
#' style_nature()
style_nature <- function(){
   assert_style_schema(
      list(
         prop_digits_round       = 1
         , prop_nsmall           = 1
         , count_method          = "sigfig"
         , count_digits_sigfig   = 3
         , count_pad_sigfigs     = TRUE
         , count_nsmall          = 1
         , decimal.mark          = "."
         , count_big.mark        = ","
         , count_label_thousands = FALSE
         , rate_method           = "sigfig"
         , rate_digits_sigfig    = 3
         , rate_pad_sigfigs      = TRUE
         , rate_nsmall           = 1
         , neg_mark_mean         = "-"
         , neg_mark_UI           = "-"
         , UI_text               = ""
         , UI_only               = FALSE
         , assert_clu_order      = TRUE
         , is_lancet             = FALSE
         , round_5_up            = TRUE
      )
   )
}

#' Lancet style schema
#'
#' Pre-defined style schema for Lancet journal formatting
#'
#' @returns [list] named list representing the lancet style
#' @export
#' @family styles
#'
#' @examples
#' style_lancet()
style_lancet <- function(){
   assert_style_schema(
      list(
         prop_digits_round       = 1
         , prop_nsmall           = 1
         , count_method          = "sigfig"
         , count_pad_sigfigs     = TRUE
         , count_digits_sigfig   = 3
         , count_nsmall          = 1
         , decimal.mark          = mid_dot()
         , count_big.mark        = thin_space()
         , count_label_thousands = FALSE
         , rate_method           = "sigfig"
         , rate_digits_sigfig    = 3
         , rate_pad_sigfigs      = TRUE
         , rate_nsmall           = 1
         , neg_mark_mean         = "a decrease of "
         , neg_mark_UI           = en_dash()
         , UI_text               = ""
         , UI_only               = FALSE
         , assert_clu_order      = TRUE
         , is_lancet             = TRUE
         , round_5_up            = TRUE
      )
   )
}
