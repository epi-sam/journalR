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
      prop_digits_round     = "integer"
      , prop_nsmall         = "integer"
      , count_method        = "character"
      , count_digits_sigfig = "integer"
      , count_pad_sigfigs   = "logical"
      , count_nsmall        = "integer"
      , count_big.mark      = "character"
      , decimal.mark        = "character"
      , neg_mark_mean        = "character"
      , neg_mark_UI          = "character"
      , UI_text             = "character"
      , UI_only             = "logical"
      , assert_clu_order    = "logical"
      , label_thousands     = "logical"
      , is_lancet           = "logical"
      , round5up            = "logical"
   )
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
#'       prop_digits_round     = 2
#'       , count_digits_sigfig = 3
#'       , count_method        = "sigfig"
#'       , count_pad_sigfigs   = TRUE
#'       , prop_nsmall         = 1
#'       , count_nsmall        = 1
#'       , decimal.mark        = "."
#'       , neg_mark_UI          = "-"
#'       , count_big.mark      = ","
#'       , neg_mark_mean        = "a decrease of"
#'       , UI_only             = FALSE
#'       , UI_text             = ""
#'       , assert_clu_order    = TRUE
#'       , is_lancet           = FALSE
#'       , label_thousands     = FALSE
#'       , round5up            = TRUE
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
#' @param label_thousands [lgl: default FALSE] whether format counts as e.g. 10,000 as '10 thousand'
#' @param round5up [lgl: default TRUE] In R, `round(1245, 3)` is "1240".  Do you want to round to "1250" instead? Default TRUE to conform with common expectations.
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
      , prop_digits_round   = 1
      , prop_nsmall         = 1
      , count_method        = "sigfig"
      , count_digits_sigfig = 3
      , count_pad_sigfigs   = TRUE
      , count_nsmall        = 1
      , count_big.mark      = ","
      , decimal.mark        = "."
      , neg_mark_mean        = "-"
      , neg_mark_UI          = "-"
      , UI_only             = FALSE
      , UI_text             = ""
      , assert_clu_order    = TRUE
      , is_lancet           = FALSE
      , label_thousands     = FALSE
      , round5up            = TRUE
){
   set_style(
      style_name    = style_name
      , style_entry = list(
         prop_digits_round     = prop_digits_round
         , prop_nsmall         = prop_nsmall
         , count_method        = count_method
         , count_digits_sigfig = count_digits_sigfig
         , count_pad_sigfigs   = count_pad_sigfigs
         , count_nsmall        = count_nsmall
         , count_big.mark      = count_big.mark
         , decimal.mark        = decimal.mark
         , neg_mark_mean        = neg_mark_mean
         , neg_mark_UI          = neg_mark_UI
         , UI_only             = UI_only
         , UI_text             = UI_text
         , assert_clu_order    = assert_clu_order
         , is_lancet           = is_lancet
         , label_thousands     = label_thousands
         , round5up            = round5up
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
         prop_digits_round     = 1
         , prop_nsmall         = 1
         , count_method        = "sigfig"
         , count_digits_sigfig = 3
         , count_pad_sigfigs   = TRUE
         , count_nsmall        = 1
         , decimal.mark        = "."
         , count_big.mark      = ","
         , neg_mark_mean        = "-"
         , neg_mark_UI          = "-"
         , UI_text             = ""
         , UI_only             = FALSE
         , assert_clu_order    = TRUE
         , label_thousands     = FALSE
         , is_lancet           = FALSE
         , round5up            = TRUE
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
         prop_digits_round     = 1
         , prop_nsmall         = 1
         , count_method        = "sigfig"
         , count_pad_sigfigs   = TRUE
         , count_digits_sigfig = 3
         , count_nsmall        = 1
         , decimal.mark        = mid_dot()
         , count_big.mark      = thin_space()
         , neg_mark_mean        = "a decrease of "
         , neg_mark_UI          = en_dash()
         , UI_text             = ""
         , UI_only             = FALSE
         , assert_clu_order    = TRUE
         , label_thousands     = FALSE
         , is_lancet           = TRUE
         , round5up            = TRUE
      )
   )
}
