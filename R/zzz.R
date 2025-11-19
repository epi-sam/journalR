# ---- Init -----------------------------------------------------------------

# Create a package-level environment
# This is created when the package is loaded and persists for the session
# CRAN compliant: no global assignment, scoped to package namespace
# - accessor functions can modify it, but it is not directly accessible to users
# - the user might create a _shadow_ copy in their .GlobalEnv, but the package
#   version is still safe
# .dict_formats <- new.env(parent = emptyenv())

# Initialize dictionaries when package loads
.onLoad <- function(libname, pkgname) {

   # requireNamespace("checkmate", quietly = TRUE)
   # Initialize preset dictionaries
   ns <- asNamespace(pkgname)
   .dict_formats <- new.env(parent = emptyenv())
   .dict_formats[['lancet']] <- style_lancet()
   .dict_formats[['nature']] <- style_nature()
   presets <- ls(envir = .dict_formats)
   # lock_some_bindings(objs  = presets, env = .dict_formats)
   assign(".dict_formats", .dict_formats, envir = ns)

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

