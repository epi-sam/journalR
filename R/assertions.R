#' Assert all elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @return [none] stop if any elements of x are not in y
#' @export
#' @family assertions
#' @encoding UTF-8
#'
#' @examples
#' assert_x_in_y(c("a", "b"), c("a", "b", "c"))
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
#' @export
#' @family assertions
#' @encoding UTF-8
#'
#' @examples
#' assert_x_not_in_y(c("a", "b"), c("c", "d", "e"))
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
#' @export
#' @family assertions
#' @encoding UTF-8
#'
#' @examples
#' assert_set_choice("a", letters)
assert_set_choice <- function(x, choices){
   checkmate::assert_scalar(x)
   checkmate::assert_vector(choices)
   # instead of checkmate, format choices with newlines - easier to read
   if(! x %in% choices){
      stop(
         sprintf(
            "'%s' is not a valid choice. \nValid options are:\n   %s"
            , x
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
#' @param d_type [chr] data type to validate
#'
#' @returns [chr] invisible validated d_type
#' @export
#' @family assertions
#' @encoding UTF-8
#'
#' @examples
#' assert_data_type("prop")
assert_data_type <- function(d_type){
   assert_set_choice(x = d_type, choices = get_data_types())
   invisible(d_type)
}


#' Assert style schema
#'
#' Validates that a style entry conforms to the expected schema defined in
#' get_style_schema().
#'
#' @param style_entry [list] named list representing a style entry
#'
#' @returns [list] invisible validated style_entry
#' @export
#' @family assertions
#' @encoding UTF-8
#'
#' @examples
#' assert_style_schema(
#'    list(
#'       digits_round_prop          = 2
#'       , digits_sigfig_count      = 3
#'       , nsmall                   = 1
#'       , decimal.mark             = "."
#'       , negative_sign            = "-"
#'       , big.mark_count           = ","
#'       , mean_neg_text            = "a decrease of"
#'       , UI_only                  = FALSE
#'       , UI_text                  = ""
#'       , assert_clu_relationships = TRUE
#'       , is_lancet                = FALSE
#'    )
#' )
assert_style_schema <- function(style_entry){

   checkmate::assert_list(style_entry, names = "named")
   style_schema <- get_style_schema()

   lapply(seq_along(style_entry), function(i){
      x              <- style_entry[[i]]
      xname          <- names(style_entry)[i]
      xtype_expected <- style_schema[[xname]]
      xtype_actual   <- typeof(x)
      xclass_actual  <- class(x)

      assert_set_choice(xname, choices = names(style_schema))
      checkmate::assert_scalar(x)

      # assert data types, checking for integerish values, and converting to
      # integer gracefully when found
      if(xtype_expected == "integer"){
         x_is_int <- FALSE
         if(xtype_actual == "integer"){
            x_is_int <- TRUE
         } else if(xtype_actual == "double"){
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
                  "style element '%s' should be integer(ish) but is of type '%s' (class: %s)"
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

   return(style_entry)
}

#' Assert Greater Than or Equal To
#'
#' @param x [num]eric vector
#' @param y [num]eric vector
#'
#' @returns [none] stop if any elements of x are greater than y
#' @export
#' @family assertions
#' @encoding UTF-8
#'
#' @examples
#' lower = c(1, 2, 3)
#' mean = c(3, 3, 3)
#' assert_x_gte_y(mean, lower)
#' mean = c(1, 2, 4)
#' # assert_x_gte_y(lower, mean) # stop
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
#' @export
#' @family assertions
#'
#' @examples
#' assert_clu_relationship(c(5, 10, 15), c(3, 8, 12), c(7, 12, 18))
assert_clu_relationship <- function(central, lower, upper){
   assert_x_gte_y(x = upper,   y = central)
   assert_x_gte_y(x = central, y = lower)
   assert_x_gte_y(x = upper,   y = lower) # probably redundant
}
