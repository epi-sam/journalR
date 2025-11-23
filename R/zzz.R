# ---- Init -----------------------------------------------------------------

# Create a package-level environment
# This is created when the package is loaded and persists for the session
# CRAN compliant: no global assignment, scoped to package namespace
# - accessor functions can modify it, but it is not directly accessible to users
# - the user might create a _shadow_ copy in their .GlobalEnv, but the package
#   version is still safe
.dict_formats <- new.env(parent = emptyenv())

# Initialize dictionaries when package loads
.onLoad <- function(libname, pkgname) {
   # Initialize preset dictionaries
   .dict_formats[['lancet']] <- style_lancet()
   .dict_formats[['nature']] <- style_nature()
   presets <- ls(envir = .dict_formats)
   lock_some_bindings(objs  = presets, env = .dict_formats)

   # CANNOT do this, or the user won't be able to update
   # - even if bindings = FALSE, the environment itself is locked to new entries
   # lockEnvironment(.dict_formats, bindings = FALSE)
}

.datatable.aware=TRUE

# ---- Accessors -----------------------------------------------------------------

#' Get the package environment
#'
#' For internal use.
#'
#' @returns [env] the package's dictionary environment
#'
#' @examples
#' \dontrun{
#' get_dict_formats()
#' }
get_dict_formats <- function() {
   .dict_formats
}


#' Get all pre-assigned .dict_formats names
#'
#' @returns [chr] names of all pre-assigned dictionaries in .dict_formats
#'
#' @examples
#' \dontrun{
#' get_dict_formats_names()
#' }
get_dict_formats_names <- function(){
   names(get_dict_formats())
}

#' Get dictionary by name
#'
#' Accessor function to retrieve a format dictionary from the package's
#' dictionary environment.
#'
#' @param dict_name [chr] name of the dictionary to retrieve
#'
#' @returns [any] the requested dictionary
#'
#' @examples
#' \dontrun{
#' get_dict_format("lancet")
#' }
get_dict_format <- function(dict_name) {
   checkmate::assert_string(dict_name)
   avail_names <- get_dict_formats_names()
   if (!dict_name %in% avail_names) {
      stop(
         sprintf(
            "Dictionary '%s' not found. Available dictionaries are:\n  %s"
            , dict_name
            , toString(avail_names)
         )
      )
   }
   return(.dict_formats[[dict_name]])
}

#' Set dictionary by name
#'
#' Mutator function to set a format dictionary in the package's
#' dictionary environment.
#'
#' Dictionary entries should only be settable with the accessor functions.
#'
#' @param dict_name [chr] name of the dictionary to set
#' @param dict_entry [any] value to assign to the dictionary
#'
#' @returns [chr] invisible vector of input objects, to allow easier un-locking
#'
#' @examples
#' \dontrun{
#' set_dict_format("my_dict", list(a = 1, b = 2))
#' }
set_dict_format <- function(dict_name, dict_entry) {
   checkmate::assert_string(dict_name)
   # done within schema validation, keep this one as general as possible
   # checkmate::assert_list(dict_entry, names = "named")
   # unlock_some_bindings(objs = dict_name, env = .dict_formats)
   .dict_formats[[dict_name]] <- dict_entry
   # lock_some_bindings(dict_name, .dict_formats)
}


# ---- df_mag State Management Functions ----------------------------------------

#' @title df_mag State Management Functions
#' @description
#' Manage df_mag state during vectorized count formatting in format_journal_clu().
#' State is stored in .dict_formats alongside style dictionaries.
#' @name df_mag_state
#' @keywords internal
NULL

#' Initialize df_mag state
#' @param n Number of rows (triplets) to process
#' @return NULL invisibly
#' @keywords internal
init_df_mag_state <- function(n) {
  stopifnot(is.numeric(n), length(n) == 1, n > 0, n == floor(n))
  
  env <- .dict_formats
  
  # Warn if overwriting active state (indicates nesting bug)
  if (isTRUE(env[["df_mag_active"]])) {
    warning(
      "df_mag state already active. ",
      "Nested calls to format_journal_clu() are not supported. ",
      "Reinitializing state.",
      call. = FALSE
    )
  }
  
  # Create empty df_mag structure matching set_magnitude() output
  env[["df_mag"]] <- data.frame(
    mag = rep(NA_character_, n),
    mag_label = rep(NA_character_, n),
    denom = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
  
  env[["df_mag_active"]] <- TRUE
  
  invisible(NULL)
}

#' Store a complete df_mag data.frame in state
#' @param df_mag data.frame as returned by set_magnitude()
#' @return NULL invisibly
#' @keywords internal
set_df_mag_state <- function(df_mag) {
  checkmate::assert_data_frame(df_mag)
  env <- .dict_formats
  if (!is_df_mag_active()) {
    stop("Cannot set df_mag state: not initialized. Call init_df_mag_state() first.",
         call. = FALSE)
  }
  env[["df_mag"]] <- df_mag
  invisible(NULL)
}

#' Check if df_mag state is active
#' @return Logical
#' @keywords internal
is_df_mag_active <- function() {
  env <- .dict_formats
  isTRUE(env[["df_mag_active"]])
}

#' Get current df_mag state
#' @return df_mag data.frame or NULL
#' @keywords internal
get_df_mag_state <- function() {
  env <- .dict_formats
  if (is_df_mag_active()) {
    return(env[["df_mag"]])
  }
  NULL
}

#' Get a single row from df_mag state
#' @param idx Row index (1-based)
#' @return Single-row data.frame
#' @keywords internal
get_df_mag_row <- function(idx) {
  if (!is_df_mag_active()) {
    stop("df_mag state not active", call. = FALSE)
  }
  env <- .dict_formats
  n_rows <- nrow(env[["df_mag"]])
  if (!is.numeric(idx) || length(idx) != 1 || idx < 1 || idx > n_rows) {
    stop(sprintf("idx must be in range [1, %d], got: %s", n_rows, idx),
         call. = FALSE)
  }
  env[["df_mag"]][idx, , drop = FALSE]
}

#' Update df_mag state at specific index
#'
#' Called by fround_count() when magnitude edge case is detected.
#' Field names match set_magnitude() output: mag, mag_label, denom.
#'
#' @param idx Integer. The row index (1-based) to update.
#' @param mag Character. Magnitude code: "", "t", "m", or "b".
#' @param mag_label Character. Printable label with trailing space.
#' @param denom Numeric. Denominator for scaling (1, 1e3, 1e6, 1e9).
#' @return NULL invisibly
#' @keywords internal
update_df_mag_state <- function(idx,
                                 mag = NULL,
                                 mag_label = NULL,
                                 denom = NULL) {
  
  env <- .dict_formats
  
  # Validate state is active
  if (!is_df_mag_active()) {
    stop(
      "Cannot update df_mag state: state not initialized. ",
      "Call init_df_mag_state() first.",
      call. = FALSE
    )
  }
  
  # Validate index
  n_rows <- nrow(env[["df_mag"]])
  if (!is.numeric(idx) || length(idx) != 1 || idx < 1 || idx > n_rows) {
    stop(
      sprintf("idx must be integer in range [1, %d], got: %s", n_rows, idx),
      call. = FALSE
    )
  }
  
  # Update non-NULL fields (matches set_magnitude() output columns)
  if (!is.null(mag)) {
    env[["df_mag"]]$mag[idx] <- mag
  }
  if (!is.null(mag_label)) {
    env[["df_mag"]]$mag_label[idx] <- mag_label
  }
  if (!is.null(denom)) {
    env[["df_mag"]]$denom[idx] <- denom
  }
  
  invisible(NULL)
}

#' Flush df_mag state
#' @return The df_mag data.frame that was stored (before clearing), or NULL
#' @keywords internal
flush_df_mag_state <- function() {
  env <- .dict_formats
  
  # Capture current state
  result <- env[["df_mag"]]
  
  # Clear state
  env[["df_mag"]] <- NULL
  env[["df_mag_active"]] <- FALSE
  
  return(result)
}

