
#' Small-capital
#'
#' @description A utility function to turn lower case character into small capitals
#' @param x A character strings to be turned into small capitals
#'
#' @return A character vector
#' @export
#'
#' @examples
#' scaps("happiness is a desired goal")
scaps <- function(x = NULL) {
  paste("<span style = 'font-variant:small-caps;'>", x, "</span>", sep = "")
}
