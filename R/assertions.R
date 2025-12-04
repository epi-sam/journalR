# No docstring for this function - internal utility
# @keywords internal
require_args <- function(...) {
   args <- as.list(match.call())[-1]
   for (arg in args) {
      arg_name <- deparse(arg)
      val <- tryCatch(eval(arg, parent.frame()), error = function(e) NULL)
      if (is.null(val))
         stop(sprintf("'%s' is required with no default", arg_name), call. = FALSE)
   }
}

#' Assert all elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @return [none] stop if any elements of x are not in y
#' @family assertions
#' @keywords internal
#'
assert_x_in_y <- function(x, y){
   checkmate::assert_vector(x)
   checkmate::assert_vector(y)
   absent <- setdiff(x, y)
   if (length(absent) > 0) {
      x_name <- deparse(substitute(x))
      y_name <- deparse(substitute(y))
      stop(sprintf("required in %s but absent in %s: %s", x_name, y_name, toString(absent)))
   }
}

#' Assert no elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @returns [none] stop if any elements of x are in y
#' @family assertions
#' @keywords internal
#'
assert_x_not_in_y <- function(x, y){
   checkmate::assert_vector(x, null.ok = TRUE)
   checkmate::assert_vector(y)
   x_name  <- deparse(substitute(x))
   y_name  <- deparse(substitute(y))
   present <- intersect(x, y)
   if (length(present) > 0) {
      stop(sprintf("forbidden in %s but present in %s: %s", x_name, y_name, toString(present)))
   }
}

#' Assert set choice
#'
#' @param x [scalar] some scalar value
#' @param choices [vector] vector of allowed choices
#'
#' @returns [scalar] invisible validated x
#' @family assertions
#' @keywords internal
assert_set_choice <- function(x, choices){
   checkmate::assert_scalar(x)
   checkmate::assert_vector(choices)
   # instead of checkmate, format choices with newlines - easier to read
   xname <- deparse(substitute(x))
   if(! x %in% choices){
      stop(
         sprintf(
            "'%s' is not a valid choice for %s. \nValid options are:\n   %s"
            , x
            , xname
            , paste0(choices, collapse = "\n   ")
         )
      )
   }
   invisible(x)
}


#' Assert data type
#'
#' Validates that a given data type is among the allowed types.
#'
#' @param metric [chr] metric type to validate
#'
#' @returns [chr] invisible validated metric
#' @family assertions
#' @keywords internal
assert_metric <- function(metric){
   require_args(metric)
   assert_set_choice(x = metric, choices = get_metrics())
   invisible(metric)
}


#' Assert Greater Than or Equal To
#'
#' @param x [num]eric vector
#' @param y [num]eric vector
#'
#' @returns [none] stop if any elements of x are greater than y
#' @family assertions
#' @keywords internal
assert_x_gte_y <- function(x, y){
   checkmate::assert_numeric(x, any.missing = FALSE)
   checkmate::assert_numeric(y, any.missing = FALSE)
   x_name <- deparse(substitute(x))
   y_name <- deparse(substitute(y))
   if(any(x < y)){
      bad_idx   <- which(x < y)
      offenders <- paste0("(", x[bad_idx], " < ", y[bad_idx], ")")
      stop(sprintf("%s is less than/equal to %s at index: %s : %s", x_name, y_name, paste0(bad_idx, collapse = ", "), toString(offenders)))
   }
}

#' Assert CLU relationships
#'
#' Validates that the relationships between central, lower, and upper values
#' are consistent with CLU (Central, Lower, Upper) conventions:
#' - upper >= central
#' - central >= lower
#' - upper >= lower
#'
#' @param central [num] vector of central values
#' @param lower [num] vector of lower bound values
#' @param upper [num] vector of upper bound values
#'
#' @returns [none] stop if any of the CLU relationships are violated
#' @family assertions
#' @keywords internal
assert_clu_relationship <- function(central, lower, upper){
   assert_x_gte_y(x = upper,   y = central)
   assert_x_gte_y(x = central, y = lower)
   assert_x_gte_y(x = upper,   y = lower) # probably redundant
}


#' Assert rate unit parameter
#'
#' Validates that when metric is "rate", rate_unit is provided and is a string.
#' When metric is not "rate", rate_unit is ignored.
#'
#' @param metric [chr] metric type
#' @param rate_unit [chr or NULL] rate unit parameter
#'
#' @returns [none] stop if metric is "rate" and rate_unit is missing/invalid
#' @family assertions
#' @keywords internal
assert_rate_unit <- function(metric, rate_unit) {
   checkmate::assert_string(metric)

   if (metric == "rate") {
      if (is.null(rate_unit)) {
         stop("rate_unit is required when metric = 'rate' (e.g., 'cases', 'deaths')", call. = FALSE)
      }
      checkmate::assert_string(rate_unit)
   }

   invisible(NULL)
}
