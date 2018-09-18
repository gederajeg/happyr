#' Retrieve the top-10 lexically diverse metaphors
#'
#' @description This function calls the script to output the top-10 metaphors that have high index of lexical creativity in their linguistic manifestation.
#'     The lexical creativity is derived from the \code{type-per-token ratio (TTR)} measure (Rajeg, 2018, Chapter 6).
#' @param df_ttr_out the tibble as the output of \code{\link{ttr}}.
#' @param min_token minimum token frequency of the metaphors.
#' @param top_n_limit limit for presenting the top-n most lexically diverse metaphors.
#'
#' @return A tibble data frame.
#' @export
#'
#' @examples
#' # get the ttr table
#' ttr_metaphor <- ttr(phd_data_metaphor,
#'                     metaphor_var = "metaphors",
#'                     lexunit_var = "lu",
#'                     float_digits = 2)
#'
#' # get the lexically diverse metaphors
#' get_lexically_diverse_metaphors(df_ttr_out = ttr_metaphor,
#'                                 min_token = 3,
#'                                 top_n_limit = 10)
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr quo
#' @importFrom dplyr desc
#' @importFrom dplyr top_n
#' @importFrom rlang .data
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia.
get_lexically_diverse_metaphors <- function(df_ttr_out = NULL, min_token = 3, top_n_limit = 10) {
  assertthat::assert_that(!is.null(df_ttr_out), msg = "The `df_ttr_out` argument is NULL; please specify it with the data frame output of `ttr()`!")
  creative_df <- dplyr::filter(df_ttr_out, .data$token >= min_token)
  creative_df <- dplyr::arrange(creative_df, dplyr::desc(.data$type_per_token_lu))
  creative_df <- dplyr::top_n(creative_df, top_n_limit, .data$type_per_token_lu)
  return(creative_df)

}
