#' Select a (set of) frequency profile column(s) for a metaphor
#'
#' @param metaphor character string of regular expressions for the metaphors.
#' @param ... any bare variable/column name of the frequency profiles from \code{ttr} function.
#' @param metaphor_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param df data frame storing the results of \code{\link{ttr}}.
#'
#' @return A tibble data frame
#' @export
#'
#' @examples
#' # perform ttr
#' ttr_metaphor <- ttr(phd_data_metaphor,
#'                     metaphor_var = "metaphors",
#'                     lexunit_var = "lu",
#'                     float_digits = 2)
#'
#' # retrieve the profile
#' get_meta_freq_profiles("food", # regex for the metaphor
#'                        token, type_lu, type_per_token_lu, # inputs for the dot-dot-dot (...)
#'                        metaphor_var = "metaphors",
#'                        df = ttr_metaphor)
#' @importFrom dplyr quos
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia.
get_meta_freq_profiles <- function(metaphor = NULL, ..., metaphor_var = "metaphors", df = NULL) {
  m_var <- rlang::sym(metaphor_var)
  freq_profiles <- dplyr::quos(...)
  tb_temp <- dplyr::filter(df, stringr::str_detect(!!m_var, metaphor))
  tb_temp <- dplyr::select(tb_temp, !!m_var, !!!freq_profiles)
  return(tb_temp)
}
