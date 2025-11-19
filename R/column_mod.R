# started: 2025 Sep 08 11:48:43
# purpose: Suite of functions to handle columns from data.frames or
# data.tables, preserving class and modifying in place if data.table.

#' @importFrom data.table :=
NULL

#' Add/overwrite a column in a data.frame or data.table, preserving class and
#' modifying in place if data.table.
#'
#' @param x [data.frame or data.table]
#' @param vec [any] vector of values to add as new column
#' @param varname [chr] column name for new column
#' @param overwrite [lgl: default FALSE] overwrite existing column if TRUE
#'
#' @returns [data.frame or data.table] input `x` with new column added
#' @export
#' @family column_mods
#'
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3])
#' df <- add_column(df, "c", c(TRUE, FALSE, TRUE))
#' print(df)
#' class(df)
#' dt <- data.table::data.table(a = 1:3, b = letters[1:3])
#' add_column(dt, "c", c(TRUE, FALSE, TRUE)) # modified in place
#' print(dt)
#' class(dt)
add_column <- function(x, varname, vec, overwrite = FALSE){
   checkmate::assert_data_frame(x)
   checkmate::assert_string(varname)
   checkmate::assert_vector(vec, len = nrow(x))
   checkmate::assert_logical(overwrite, len = 1)
   if(!isTRUE(overwrite)) assert_x_not_in_y(varname, colnames(x))
   if(inherits(x, "data.table")){
      x <- data.table::copy(x) # avoid in-place modification
      x[, (varname) := vec]
   } else {
      x[[varname]] <- vec
   }
   return(x)
}

#' Drop a column from a data.frame or data.table, preserving class and
#' modifying in place if data.table.
#'
#' @param x [data.frame or data.table]
#' @param varname [chr] column name to drop
#'
#' @returns [data.frame or data.table] input `x` with column dropped
#' @export
#' @family column_mods
#'
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3], c = c(TRUE, FALSE, TRUE))
#' df <- drop_column(df, "c")
#' print(df)
#' class(df)
#' dt <- data.table::data.table(a = 1:3, b = letters[1:3], c = c(TRUE, FALSE, TRUE))
#' drop_column(dt, "c") # modified in place
#' print(dt)
#' class(dt)
drop_column <- function(x, varname){
   checkmate::assert_data_frame(x)
   checkmate::assert_string(varname)
   assert_set_choice(varname, colnames(x))
   if(inherits(x, "data.table")){
      x[, (varname) := NULL]
   } else {
      x[[varname]] <- NULL
   }
   return(x)
}

#' Vectorized version of drop_column()
#'
#' @param x [data.frame or data.table]
#' @param varnames [chr vector] column names to drop
#'
#' @returns [data.frame or data.table] input `x` with columns dropped
#' @export
#' @family column_mods
#'
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3], c = c(TRUE, FALSE, TRUE))
#' df <- drop_columns(df, c("b", "c"))
drop_columns <- function(x, varnames){
   checkmate::assert_data_frame(x)
   checkmate::assert_character(varnames)
   checkmate::assert_vector(varnames, min.len = 1)
   varnames <- unique(varnames)
   for(v in varnames){
      x <- drop_column(x, v)
   }
   return(x)
}
