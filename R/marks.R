#' Mid-dot
#'
#' Lancet numeric decimal standard: "\eqn{\cdot}" ("\\U00B7")
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
#' Standard for "x -- y" and Lancet negative: ("\\U2013")
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
#' Lancet thin space separator for counts 10,000 -- 999,9999 instead of comma
#' ",": ("\\U2009")
#'
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

