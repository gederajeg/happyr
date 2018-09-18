#' Retrieve the conventionalised Lexical Units of a metaphor
#'
#' @description The function helps filter out LUs of a metaphor that can be considered as conventional linguistic instances (cf. Vergara Wilson, 2014, for the way to determine CIC, which stands for \emph{"conventionalised instances of constructions"}).
#' @param metaphor regular expressions for the relevant metaphor.
#' @param df_ttr_out the tibble as the output of \code{\link{ttr}}.
#'
#' @return A tibble data frame
#'
#' @examples
#' ttr_metaphor <- ttr(df = phd_data_metaphor,
#'                     metaphor_var = "metaphors",
#'                     lexunit_var = "lu",
#'                     float_digits = 2)
#'
#' get_cic_meta_lu("possessable", ttr_metaphor)
#'
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom utils data
#' @export
#' @references
#' \itemize{
#' \item Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia.
#' \item Vergara Wilson, D. (2014). \emph{Categorization and Constructional Change in Spanish Expressions of "becoming"}. Leiden: Brill.
#' }
get_cic_meta_lu <- function(metaphor = NULL, df_ttr_out = NULL) {

  assertthat::assert_that(!is.null(df_ttr_out), msg = "The `df_ttr_out` argument is NULL; please specify it with the data frame output of `ttr()`!")
  token_per_type_lu <- dplyr::quo(token_per_type_lu)
  cic_lu_limit <- dplyr::quo(cic_lu_limit)
  df_cic_out <- df_ttr_out %>%
    dplyr::mutate(!!dplyr::quo_name(token_per_type_lu) := round(.data$token/.data$type_lu, 2),
                  !!dplyr::quo_name(cic_lu_limit) := round(.data$token_per_type_lu))
  cic_limit <- df_cic_out %>%
    dplyr::filter(stringr::str_detect(.data$metaphors, metaphor)) %>%
    dplyr::pull(!!cic_lu_limit)
  df_counter <- get_lu_table(metaphor = metaphor, df = phd_data_metaphor, top_n_only = FALSE)
  df_counter <- df_counter %>%
    dplyr::filter(.data$N >= cic_limit)
  return(df_counter)
}
