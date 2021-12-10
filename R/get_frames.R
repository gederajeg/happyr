#' Retrieve the descriptive distributional statistics of source frames
#'
#' @description A utility function to get the descriptive statistics for the source frames of a metaphor (for the aggregated synonyms data or for a given synonym, if specified).
#'    This function is used in Rajeg (2019, Chapter 5 & 6).
#' @param metaphor character strings of regular expressions for the metaphor.
#' @param word character strings of regular expressions for the happiness synonyms.
#' @param df the data frame (\code{phd_data_metaphor}).
#' @param metaphor_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param frame_var character string for the column name of the source frame variable (i.e., \code{"source_frames"}).
#' @param synonym_var character string for the column name of the synonyms variable (i.e., \code{"synonyms"}).
#' @param lexunit_var character string for the column name of the lexical unit variable (i.e., \code{"lu"}).
#'
#' @return A tibble/data frame of the descriptive statistics of the frames,
#'     namely the raw and relative (i.e. percentages of the) token and type frequencies.
#'     If the \code{word} argument is specified,
#'     both the \code{source_frames} and the \code{synonyms} variable are grouped
#'     for calculating the token and type frequencies, while percentages of these are summarised per word.
#' @export
#' @examples
#'
#' # for aggregated data across all synonyms
#' get_frames(metaphor = "desired goal",
#'            df = phd_data_metaphor)
#'
#' # for a synonym
#' get_frames(metaphor = "desired goal",
#'            word = "^kesenangan",
#'            df = phd_data_metaphor)
#'
#' # for synonyms with the same root (e.g. "senang" and "kesenangan")
#' get_frames(metaphor = "desired goal",
#'            word = "senang",
#'            df = phd_data_metaphor)
#'
#' # for synonyms in the nominalised forms
#' get_frames(metaphor = "desired goal",
#'            word = "^(kesenangan|kegembiraan)",
#'            df = phd_data_metaphor)
#'
#' @references Rajeg, G. P. W. (2019). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia. \url{https://doi.org/10.26180/5cac231a97fb1}.
#' @importFrom dplyr enquo
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom dplyr n
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
#' @importFrom purrr is_null
#' @importFrom stringr str_detect
#' @importFrom assertthat assert_that
get_frames <- function(metaphor = NULL,
                       word = NULL,
                       df = NULL,
                       frame_var = "source_frames",
                       metaphor_var = "metaphors",
                       synonym_var = "synonyms",
                       lexunit_var = "lu") {

  assertthat::assert_that(!is.null(df), msg = "The `df` argument is NULL; please specify it with `phd_data_metaphor`!")

  # generate symbols for string-input variables
  lu_q <- rlang::sym(lexunit_var)
  syn_q <- rlang::sym(synonym_var)
  meta_q <- rlang::sym(metaphor_var)
  frame_q <- rlang::sym(frame_var)
  N <- dplyr::quo(n)
  type <- dplyr::quo(type)
  perc <- dplyr::quo(perc)
  type_perc <- dplyr::quo(type_perc)
  frames <- dplyr::quo_name(dplyr::quo(frames))
  synonyms <- dplyr::quo_name(dplyr::quo(synonyms))

  if(purrr::is_null(word) == TRUE) {
    df %>%
      dplyr::filter(stringr::str_detect(!!meta_q, metaphor)) %>%
      dplyr::group_by(!!frame_q) %>%
      dplyr::summarise(!!dplyr::quo_name(N) := n(),
                       !!dplyr::quo_name(type) := dplyr::n_distinct(!!lu_q)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!dplyr::quo_name(perc) := round(!!dplyr::quo(n/sum(n) * 100), 2),
                    !!dplyr::quo_name(type_perc) := round(!!dplyr::quo(type/sum(type) * 100), 2)) %>%
      dplyr::select(!!frames := !!frame_q, dplyr::everything()) %>%
      dplyr::arrange(dplyr::desc(!!N))

  } else {
    df %>%
      dplyr::filter(stringr::str_detect(!!meta_q, metaphor)) %>%
      dplyr::filter(stringr::str_detect(!!syn_q, word)) %>%
      dplyr::group_by(!!frame_q, !!syn_q) %>%
      dplyr::summarise(!!dplyr::quo_name(N) := n(),
                       !!dplyr::quo_name(type) := dplyr::n_distinct(!!lu_q)) %>%
      dplyr::group_by(!!syn_q) %>%
      dplyr::mutate(!!dplyr::quo_name(perc) := round(!!dplyr::quo(n/sum(n) * 100), 2),
                    !!dplyr::quo_name(type_perc) := round(!!dplyr::quo(type/sum(type) * 100), 2)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!frames := !!frame_q, !!synonyms := !!syn_q, dplyr::everything()) %>%
      dplyr::arrange(!!dplyr::quo(synonyms), dplyr::desc(!!N))
  }
}
