#' Lock select bindings in the global environment
#'
#' Safely tries to lock the specified object bindings in the given environment.
#'
#' @param objs [chr] a vector of object names to lock
#' @param env [environment] the environment to lock the bindings in
#'
#' @returns [chr] invisible vector of input objects, to allow easier un-locking
#'
#' @family bindings
lock_some_bindings <- function(objs, env = globalenv()){
   checkmate::assert_character(objs)
   for(x in objs){
      try({if(exists(x, envir = env) && !bindingIsLocked(x, env)) lockBinding(x, env)})
   }
   return(invisible(objs))
}

#' #' Unlock select bindings in the global environment
#' #'
#' #' Safely tries to unlock the specified object bindings in the given environment.
#' #'
#' #' @param objs [chr] a vector of object names to unlock
#' #' @param env [environment] the environment to unlock the bindings in
#' #'
#' #' @returns [chr] invisible vector of input objects, to allow easier re-locking
#' #'
#' #' @family bindings
#' #' @examples
#' #' unlock_some_bindings(c("my_var1", "my_var2"))
#' unlock_some_bindings <- function(objs, env = globalenv()){
#'    checkmate::assert_character(objs)
#'    for(x in objs){
#'       try({if(exists(x, envir = env) && bindingIsLocked(x, env)) unlockBinding(x, env)})
#'    }
#'    return(invisible(objs))
#' }
#'
#' #' Unlock ALL bindings in the global environment
#' #'
#' #' Safely tries to unlock all object bindings in the given environment.
#' #'
#' #' @param env [environment] the environment to unlock the bindings in
#' #'
#' #' @returns [chr] invisible vector of input objects, to allow easier re-locking
#' #'
#' #' @family bindings
#' #' @examples
#' #' unlock_all_bindings()
#' unlock_all_bindings <- function(env = globalenv()){
#'    unlock_some_bindings(ls(envir = env), env)
#' }

