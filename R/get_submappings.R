#' Retrieve the submappings of the metaphors overall or by synonyms/words
#'
#' @description A utility function to get the descriptive data for the submappings of a metaphor (for the aggregated synonyms data or for a given synonym, if specified).
#'    This function is used in Rajeg's (2018, Chapter 5 & 6).
#' @param metaphor character strings of regular expressions for the metaphor.
#' @param word character strings of regular expressions for the happiness synonyms.
#' @param df the data frame (\code{phd_data_metaphor}).
#' @param metaphor_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param submet_var character string for the column name of the submapping variable (i.e., \code{"submappings"}).
#' @param synonym_var character string for the column name of the synonyms variable (i.e., \code{"synonyms"}).
#' @param lexunit_var character string for the column name of the lexical unit variable (i.e., \code{"lu"}).
#'
#' @return A tibble/data frame of the descriptive statistics of the submappings, namely the raw and relative (i.e. percentages of the) token and type frequencies.
#' @export
#'
#' @examples
#' submet <- get_submappings(metaphor = "possessable object$", df = phd_data_metaphor)
#'
#' @importFrom dplyr enquo
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom dplyr n
#' @importFrom dplyr top_n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr ungroup
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang sym
#' @importFrom purrr is_null
#' @importFrom stringr str_detect
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for happiness} (PhD Thesis). Monash University. Melbourne, Australia.

get_submappings <- function(metaphor = NULL,
                            word = NULL,
                            df = NULL,
                            metaphor_var = "metaphors",
                            submet_var = "submappings",
                            synonym_var = "synonyms",
                            lexunit_var = "lu") {

  # generate symbols for string-input variables
  meta_q <- rlang::sym(metaphor_var)
  syn_q <- rlang::sym(synonym_var)
  lu_q <- rlang::sym(lexunit_var)
  submet_q <- rlang::sym(submet_var)

  # retrieve the relevant rows and variables
  if (purrr::is_null(word) == TRUE) {
    d <- df %>%
      dplyr::filter(stringr::str_detect(!!meta_q, metaphor)) %>%
      dplyr::group_by(!!submet_q)
  } else {
    d <- df %>%
      dplyr::filter(stringr::str_detect(!!meta_q, metaphor),
                    stringr::str_detect(!!syn_q, word)) %>%
      dplyr::group_by(!!submet_q, !!syn_q)
  }

  # compute the results
  N <- dplyr::quo(n)
  type <- dplyr::quo(type)
  perc <- dplyr::quo(perc)
  type_perc <- dplyr::quo(type_perc)

  res <- d %>%
    dplyr::summarise(!!dplyr::quo_name(N) := n(),
                     !!dplyr::quo_name(type) := dplyr::n_distinct(!!lu_q))

  if (purrr::is_null(word) == TRUE) {
    res %>%
      dplyr::mutate(!!dplyr::quo_name(perc) := round(!!dplyr::quo(n/sum(n) * 100), 2),
                    !!dplyr::quo_name(type_perc) := round(!!dplyr::quo(type/sum(type) * 100), 2)) %>%
      dplyr::arrange(dplyr::desc(!!N)) %>%
      dplyr::ungroup()
  } else {
    res %>%
      dplyr::group_by(!!syn_q) %>%
      dplyr::mutate(!!dplyr::quo_name(perc) := round(!!dplyr::quo(n/sum(n) * 100), 2),
                    !!dplyr::quo_name(type_perc) := round(!!dplyr::quo(type/sum(type) * 100), 2)) %>%
      dplyr::arrange(!!syn_q, dplyr::desc(!!N)) %>%
      ungroup()
  }
}
