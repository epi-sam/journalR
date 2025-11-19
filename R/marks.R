#' Mid-dot
#'
#' Lancet numeric decimal standard
#'
#' @returns [chr] mid-dot character
#' @export
#' @family marks
#'
#' @examples
#' mid_dot()
mid_dot <- function() {
   return("\U00B7")
}

#' En-dash
#'
#' Lancet negative/hyphen sign standard
#'
#' @returns [chr] en-dash character
#' @export
#' @family marks
#'
#' @examples
#' en_dash()
en_dash <- function() {
   return("\U2013")
}


#' Thin space
#'
#' Lancet thin space standard for thousands separator (instead of comma
#' ",")
#'
#' @returns [chr] thin space character
#' @export
#' @family marks
#'
#' @examples
#' thin_space()
thin_space <- function() {
   return("\U2009")
}
