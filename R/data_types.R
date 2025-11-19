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
get_data_type_labels <- function(){
   dtype_labels <- list(
      prop    = "%"
      , pp    = " pp"
      , count = ""
   )
   lapply(names(dtype_labels), assert_data_type)
   return(dtype_labels)
}
