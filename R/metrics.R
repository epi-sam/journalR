#' Get metrics
#'
#' Centrally managed definition for all allowed metrics.
#'
#' @returns [chr] vector of allowed metrics
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

#' Get metric labels
#'
#' Centrally managed definition for all metric labels.
#'
#' @param metric [chr]
#'
#' @returns [list] named list of metric labels
#' @export
#' @family metrics
#'
#' @examples
#' get_metric_labels('prop')
get_metric_labels <- function(metric){
   assert_metric(metric)
   return_metric_labels()[[metric]]
}

#' Helper for `format_metric_cols()`, perhaps others
#'
#' @param style_name [chr] name of a style
#' @param style_item [chr] a style list item name
#' @param metric [chr] a valid metric type
#' @keywords internal
#'
#' @returns [scalar] some style item, type may vary
get_style_item_by_metric <- function(style_name, style_item, metric){

   style  <- get_style(style_name)
   metric <- assert_metric(metric)

   item <- switch(

      style_item

      , "digits" = {
         switch(
            metric
            , "prop"  = style[["prop_digits_round"]]
            , "pp"    = style[["prop_digits_round"]]
            , "count" = style[["count_digits_sigfig"]]
            , "rate"  = style[["rate_digits_sigfig"]]
         )
      }

      , "n_small" = {
         switch(
            metric
            , "prop"  = style[["prop_nsmall"]]
            , "pp"    = style[["prop_nsmall"]]
            , "count" = style[["count_nsmall"]]
            , "rate"  = style[["rate_nsmall"]]
         )
      }

      , "invert_all_neg_UI" = {
         switch(
            metric
            , "prop"  = style[["prop_invert_all_neg_UI"]]
            , "pp"    = style[["prop_invert_all_neg_UI"]]
            , "count" = style[["count_invert_all_neg_UI"]]
            , "rate"  = style[["rate_invert_all_neg_UI"]]
         )
      }
   )

   if(is.null(item)){
      stop(
         sprintf(
            "No style item found for style_name: '%s', style_item: '%s', metric: '%s'"
            , style_name
            , style_item
            , metric
         )
      )
   }

   return(item)

}
