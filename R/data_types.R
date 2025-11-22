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

return_data_type_labels <- function(){
   dtype_labels <- list(
        prop  = "%"
      , pp    = " pp"
      , count = ""
   )
   lapply(names(dtype_labels), assert_data_type)
   return(dtype_labels)
}

#' Get data type labels
#'
#' Centrally managed definition for all data type labels.
#'
#' @param d_type [chr]
#'
#' @returns [list] named list of data type labels
#' @export
#' @family data_types
#'
#' @examples
#' get_data_type_labels('prop')
get_data_type_labels <- function(d_type){
   assert_data_type(d_type)
   return_data_type_labels()[[d_type]]
}

#' Helper for `format_means_df()`, perhaps others
#'
#' @param style_name [chr] name of a style
#' @param style_item [chr] a style list item name
#' @param d_type [chr] a valid data type
#'
#' @returns [scalar] some style item, type may vary
get_style_item_by_data_type <- function(style_name, style_item, d_type){

   style  <- get_style(style_name)
   d_type <- assert_data_type(d_type)

   switch_strict(

      style_item

      , "digits" = {
         switch_strict(
            d_type
            , "prop"  = style[["prop_digits_round"]]
            , "pp"    = style[["prop_digits_round"]]
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
