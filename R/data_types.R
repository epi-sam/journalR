#' Get data types
#'
#' Centrally managed definition for all allowed data types.
#'
#' @returns [chr] vector of allowed data types
#' @export
#' @family data_types
#'
#' @examples
#' get_data_types()
get_data_types <- function(){
   c(
      "prop"
      , "pp"
      , "count"
   )
}

#' Get data type labels
#'
#' Centrally managed definition for all data type labels.
#'
#' @returns [list] named list of data type labels
#' @export
#' @family data_types
#'
#' @examples
#' get_data_type_labels()
return_data_type_labels <- function(){
   dtype_labels <- list(
      prop    = "%"
      , pp    = " pp"
      , count = ""
   )
   lapply(names(dtype_labels), assert_data_type)
   return(dtype_labels)
}

get_data_type_labels <- function(d_type){
   return_data_type_labels()[[d_type]]
}

get_style_item_by_data_type <- function(style_name, style_item, d_type){

   style <- get_style(style_name)

   switch_strict(
      style_item

      , "digits" = {
         switch_strict(
            d_type
            , "prop"  = style[["digits_round_prop"]]
            , "pp"    = style[["digits_round_prop"]]
            , "count" = style[["digits_sigfig_count"]]
         )
      }
      , "scalar" = {
         switch_strict(
            d_type
            , "prop"  = 100
            , "pp"    = 100
            , "count" = 1
         )
      }
      , "n_small" = {
         switch_strict(
            d_type
            , "prop"  = style[["nsmall_prop"]]
            , "pp"    = style[["nsmall_prop"]]
            , "count" = style[["nsmall_count"]]
         )
      }
   )

}
