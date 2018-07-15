
#' Small-capital
#'
#' @description A utility function to turn lower case character into small capitals
#' @param concept
#'
#' @return A character vector
#' @export
#'
#' @examples
#' scaps("happiness is a desired goal")
scaps <- function(concept = NULL) {
  paste("<span style = 'font-variant:small-caps;'>", concept, "</span>", sep = "")
}
