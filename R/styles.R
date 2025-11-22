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
      digits_round_prop     = "integer"
      , nsmall_prop         = "integer"
      , method_count        = "character"
      , digits_sigfig_count = "integer"
      , pad_count_sigfigs   = "logical"
      , nsmall_count        = "integer"
      , big.mark_count      = "character"
      , decimal.mark        = "character"
      , neg_str_mean        = "character"
      , neg_str_UI          = "character"
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
#'       digits_round_prop     = 2
#'       , digits_sigfig_count = 3
#'       , method_count        = "sigfig"
#'       , pad_count_sigfigs   = TRUE
#'       , nsmall_prop         = 1
#'       , nsmall_count        = 1
#'       , decimal.mark        = "."
#'       , neg_str_UI          = "-"
#'       , big.mark_count      = ","
#'       , neg_str_mean        = "a decrease of"
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
#' @param digits_round_prop [int] number of digits to round proportions to
#' @param digits_sigfig_count [int] number of significant figures for counts
#' @param nsmall_prop [int] minimum number of digits to the right of the decimal point - proportions
#' @param decimal.mark [chr] decimal mark
#' @param neg_str_UI [chr] character to use for negative sign
#' @param big.mark_count [chr] character to use for counts thousand, million, billion separator
#' @param neg_str_mean [chr] text to use when describing negative mean differences
#' @param UI_only [lgl] whether to format for UI only
#' @param UI_text [chr] text to use for UI formatting
#' @param assert_clu_order [lgl] whether to assert CLU relationships (ensure lower < central < upper)
#' @param is_lancet [lgl] whether the style is for Lancet formatting - controls specific edge-case behaviors
#' @param label_thousands [lgl] whether format counts as e.g. 10,000 as '10 thousand'
#'
#' @returns [chr] invisible vector of input objects, to allow easier un-locking
#' @export
#' @family styles
#'
#' @examples
#' new_style(
#'   style_name            = "my_style"
#'   , digits_round_prop   = 1
#'   , digits_sigfig_count = 1
#'   , pad_count_sigfigs   = TRUE
#'   , nsmall_prop         = 1
#'   , nsmall_count        = 1
#'   , decimal.mark        = "."
#'   , neg_str_UI          = "-"
#'   , big.mark_count      = ","
#'   , neg_str_mean        = "a decrease of"
#'   , UI_only             = FALSE
#'   , UI_text             = ""
#'   , assert_clu_order    = TRUE
#'   , is_lancet           = FALSE
#'   , label_thousands     = FALSE
#'   , round5up            = TRUE
#' )
new_style <- function(
      style_name
      , digits_round_prop
      , nsmall_prop
      , method_count
      , digits_sigfig_count
      , pad_count_sigfigs
      , nsmall_count
      , decimal.mark
      , neg_str_UI
      , big.mark_count
      , neg_str_mean
      , UI_only
      , UI_text
      , assert_clu_order
      , is_lancet
      , label_thousands
      , round5up
){
   set_style(
      style_name    = style_name
      , style_entry = list(
         digits_round_prop     = digits_round_prop
         , nsmall_prop         = nsmall_prop
         , method_count        = method_count
         , digits_sigfig_count = digits_sigfig_count
         , pad_count_sigfigs   = pad_count_sigfigs
         , nsmall_count        = nsmall_count
         , big.mark_count      = big.mark_count
         , decimal.mark        = decimal.mark
         , neg_str_mean        = neg_str_mean
         , neg_str_UI          = neg_str_UI
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
         digits_round_prop     = 1
         , nsmall_prop         = 1
         , method_count        = "sigfig"
         , digits_sigfig_count = 3
         , pad_count_sigfigs   = TRUE
         , nsmall_count        = 1
         , decimal.mark        = "."
         , big.mark_count      = ","
         , neg_str_mean        = "-"
         , neg_str_UI          = "-"
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
         digits_round_prop     = 1
         , nsmall_prop         = 1
         , method_count        = "sigfig"
         , pad_count_sigfigs   = TRUE
         , digits_sigfig_count = 3
         , nsmall_count        = 1
         , decimal.mark        = mid_dot()
         , big.mark_count      = thin_space()
         , neg_str_mean        = "a decrease of "
         , neg_str_UI          = en_dash()
         , UI_text             = ""
         , UI_only             = FALSE
         , assert_clu_order    = TRUE
         , label_thousands     = FALSE
         , is_lancet           = TRUE
         , round5up            = TRUE
      )
   )
}
