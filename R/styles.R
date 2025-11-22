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

#' Get style schema
#'
#' Centrally managed definition for all required journal format styles.
#'
#' @returns [list] named list of style elements and their expected types
#' @family styles
#'
#' @examples
#' \dontrun{
#' get_style_schema()
#' }
get_style_schema <- function(){
   list(
      digits_round_prop          = "integer"
      , digits_sigfig_count      = "integer"
      , nsmall                   = "integer"
      , decimal.mark             = "character"
      , neg_str_UI            = "character"
      , big.mark_count           = "character"
      , neg_str_mean            = "character"
      , UI_only                  = "logical"
      , UI_text                  = "character"
      , assert_clu_relationships = "logical"
      , is_lancet                = "logical"
      , allow_thousands          = "logical"
   )
}

#' Set a new style schema
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
#'       digits_round_prop          = 2
#'       , digits_sigfig_count      = 3
#'       , nsmall                   = 1
#'       , decimal.mark             = "."
#'       , neg_str_UI            = "-"
#'       , big.mark_count           = ","
#'       , neg_str_mean            = "a decrease of"
#'       , UI_only                  = FALSE
#'       , UI_text                  = ""
#'       , assert_clu_relationships = TRUE
#'       , is_lancet                = FALSE
#'       , allow_thousands          = FALSE
#'    )
#' )
set_style <- function(style_name, style_entry){
   # checkmate::assert_string(style_name) # done within set_dict_format()
   assert_style_schema(style_entry)
   set_dict_format(dict_name = style_name, dict_entry = style_entry)
}

#' Create a new style
#'
#' Wrapper function to create and set a new style in one step.
#'
#' @param style_name [chr] name of the style to set
#' @param digits_round_prop [int] number of digits to round proportions to
#' @param digits_sigfig_count [int] number of significant figures for counts
#' @param nsmall [int] minimum number of digits to the right of the decimal point
#' @param decimal.mark [chr] decimal mark
#' @param neg_str_UI [chr] character to use for negative sign
#' @param big.mark_count [chr] character to use for counts thousand, million, billion separator
#' @param neg_str_mean [chr] text to use when describing negative mean differences
#' @param UI_only [lgl] whether to format for UI only
#' @param UI_text [chr] text to use for UI formatting
#' @param assert_clu_relationships [lgl] whether to assert CLU relationships (ensure lower < central < upper)
#' @param is_lancet [lgl] whether the style is for Lancet formatting - controls specific edge-case behaviors
#' @param allow_thousands [lgl] whether format counts as e.g. 10,000 as '10 thousand'
#'
#' @returns [chr] invisible vector of input objects, to allow easier un-locking
#' @export
#' @family styles
#'
#' @examples
#' new_style(
#'   style_name    = "my_style"
#'   , digits_round_prop        = 1
#'   , digits_sigfig_count      = 1
#'   , nsmall                   = 1
#'   , decimal.mark             = "."
#'   , neg_str_UI            = "-"
#'   , big.mark_count           = ","
#'   , neg_str_mean            = "a decrease of"
#'   , UI_only                  = FALSE
#'   , UI_text                  = ""
#'   , assert_clu_relationships = TRUE
#'   , is_lancet                = FALSE
#'   , allow_thousands          = FALSE
#' )
new_style <- function(
      style_name
      , digits_round_prop
      , digits_sigfig_count
      , nsmall
      , decimal.mark
      , neg_str_UI
      , big.mark_count
      , neg_str_mean
      , UI_only
      , UI_text
      , assert_clu_relationships
      , is_lancet
      , allow_thousands
){
   set_style(
      style_name    = style_name
      , style_entry = list(
         digits_round_prop          = digits_round_prop
         , digits_sigfig_count      = digits_sigfig_count
         , nsmall                   = nsmall
         , decimal.mark             = decimal.mark
         , neg_str_UI            = neg_str_UI
         , big.mark_count           = big.mark_count
         , neg_str_mean            = neg_str_mean
         , UI_only                  = UI_only
         , UI_text                  = UI_text
         , assert_clu_relationships = assert_clu_relationships
         , is_lancet                = is_lancet
         , allow_thousands          = allow_thousands
      )
   )
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
         digits_round_prop          = 1
         , nsmall                   = 1
         , digits_sigfig_count      = 3
         , decimal.mark             = "."
         , neg_str_UI            = "-"
         , big.mark_count           = ","
         , neg_str_mean            = "-"
         , UI_only                  = FALSE
         , UI_text                  = ""
         , assert_clu_relationships = TRUE
         , is_lancet                = FALSE
         , allow_thousands          = FALSE
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
         digits_round_prop          = 1
         , nsmall                   = 1
         , digits_sigfig_count      = 3
         , decimal.mark             = mid_dot()
         , neg_str_UI            = en_dash()
         , big.mark_count           = thin_space()
         , neg_str_mean            = "a decrease of "
         , UI_only                  = FALSE
         , UI_text                  = ""
         , assert_clu_relationships = TRUE
         , is_lancet                = TRUE
         , allow_thousands          = FALSE
      )
   )
}
