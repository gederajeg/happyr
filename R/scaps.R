
#' Small-capital
#'
#' @description A utility function to turn lower case character into small capitals
#' @param x A character strings to be turned into small capitals
#'
#' @return A character vector
#' @export
#' @references Rajeg, G. P. W. (2018). Scapr [R]. \url{https://doi.org/10.5281/zenodo.1327273}. \url{https://gederajeg.github.io/scapr/} (Original work published 2018)

#'
#' @examples
#' scaps("happiness is a desired goal")
scaps <- function(x = NULL) {
  paste("<span style = 'font-variant:small-caps;'>", x, "</span>", sep = "")
}
