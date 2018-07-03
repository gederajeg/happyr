#' Perform the frequency profile measures in Chapter 5 and Chapter 6
#'
#' @description Generate a set of basic frequency profiles for the metaphors: (i) token frequency, (ii) type frequency, and (iii) type per token ratio (TTR).
#' @param df the data frame for the database of the thesis (\code{phd_data_metaphor.rda}).
#' @param metaphor_var character string of the column name for the metaphor variable in the data frame (i.e., \code{"metaphors"}).
#' @param lexunit_var character string of the column name for the lexical unit variable in the data frame (i.e., \code{"lu"}).
#' @param float_digits integer indicating the retained floating points from the calculation. The default is \code{2}.
#' @return A tibble data frame (\code{tbl_df}) sorted in decreasing order of the \code{token} frequency of the metaphors.
#' @examples
#' ttr_metaphor <- ttr(df = phd_data_metaphor,
#'                     metaphor_var = "metaphors",
#'                     lexunit_var = "lu",
#'                     float_digits = 2)
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for happiness}. PhD Thesis. Monash University. Melbourne, Australia.
#' @importFrom dplyr enquo
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom stringr str_to_lower
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @export
ttr <- function(df = NULL, metaphor_var = "metaphors", lexunit_var = "lu", float_digits = 2) {
  metaphor_q <- rlang::sym(metaphor_var)
  lu_q <- rlang::sym(lexunit_var)

  df_out <- df %>%
    dplyr::group_by(!!metaphor_q) %>%
    dplyr::summarise(token = n(),
                     type_lu = dplyr::n_distinct(!!lu_q))
  df_out <- dplyr::ungroup(df_out) %>%
    dplyr::mutate(perc_token = round(.data$token/sum(.data$token) * 100, float_digits),
                  perc_type_lu = round(.data$type_lu/sum(.data$type_lu) * 100, float_digits),
                  type_per_token_lu = round((.data$type_lu/.data$token) * 100, float_digits)) %>%
    dplyr::arrange(dplyr::desc(.data$token))

  colnames(df_out) <- stringr::str_to_lower(colnames(df_out))
  return(df_out)
}
