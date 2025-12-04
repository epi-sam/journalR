#' Get data types
#'
#' Centrally managed definition for all allowed data types.
#'
#' @returns [chr] vector of allowed data types
#' @export
#' @family metrics
#'
#' @examples
#' get_metrics()
get_metrics <- function(){
   c(
      "prop"
      , "pp"
      , "count"
      , "rate"
   )
}

# Internal helper function for get_metric_labels
# @keywords internal
return_metric_labels <- function(){
   metric_labels <- list(
      prop    = "%"
      , pp    = " pp"
      , count = ""
      , rate  = ""
   )
   lapply(names(metric_labels), assert_metric)
   return(metric_labels)
}

#' Get data type labels
#'
#' Centrally managed definition for all data type labels.
#'
#' @param metric [chr]
#'
#' @returns [list] named list of data type labels
#' @export
#' @family metrics
#'
#' @examples
#' get_metric_labels('prop')
get_metric_labels <- function(metric){
   assert_metric(metric)
   return_metric_labels()[[metric]]
}

#' Helper for `format_means_df()`, perhaps others
#'
#' @param style_name [chr] name of a style
#' @param style_item [chr] a style list item name
#' @param metric [chr] a valid metric type
#'
#' @returns [scalar] some style item, type may vary
#' @keywords internal
get_style_item_by_metric <- function(style_name, style_item, metric){

   style  <- get_style(style_name)
   metric <- assert_metric(metric)

   switch_strict(

      style_item

      , "digits" = {
         switch_strict(
            metric
            , "prop"  = style[["prop_digits_round"]]
            , "pp"    = style[["prop_digits_round"]]
            , "count" = style[["count_digits_sigfig"]]
            , "rate"  = style[["rate_digits_sigfig"]]
         )
      }

      , "n_small" = {
         switch_strict(
            metric
            , "prop"  = style[["prop_nsmall"]]
            , "pp"    = style[["prop_nsmall"]]
            , "count" = style[["count_nsmall"]]
            , "rate"  = style[["rate_nsmall"]]
         )
      }
   )

}
