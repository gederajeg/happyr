#' Retrieve the frequency of Lexical units and the evoked submappings
#'
#' @description The function generates a table of frequency and percentages of Lexical units evoking certain submappings of a metaphor.
#' @param metaphor regular expressions for the relevant metaphor.
#' @param word character strings of regular expressions for the HAPPINESS synonyms.
#' @param print_all logical; print all output
#' @param top_n logical; limit to the top-n LUs
#' @param limit integer; maximum rows to be printed from the top-n LUs
#' @param df the data frame (\code{phd_data_metaphor}).
#' @param metaphor_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param submet_var character string for the column name of the submapping variable (i.e., \code{"submappings"}).
#' @param synonym_var character string for the column name of the synonyms variable (i.e., \code{"synonyms"}).
#' @param lexunit_var character string for the column name of the lexical unit variable (i.e., \code{"lu"}).
#' @param lexunit_gloss_var character string for the column name of the gloss of the lexical units (i.e., \code{"lu_gloss"}).
#'
#' @return A tibble containing the submappings, LUs and the glosses,
#'     frequency of each LUs of the metaphor,
#'     percentage of each LUs in the metaphor,
#'     and percentage of each LUs by the evoked submappings.
#' @export
#' @importFrom dplyr top_n
#'
#' @examples
#' # for all synonyms
#' get_lu_submappings_table(metaphor = "liquid in a container",
#'                          df = phd_data_metaphor)
#'
#' # for \emph{kegembiraan} 'joy'
#' get_lu_submappings_table(metaphor = "liquid in a container",
#'                          word = "kegembiraan",
#'                          df = phd_data_metaphor)
get_lu_submappings_table <- function(metaphor = NULL,
                                     word = NULL,
                                     print_all=FALSE,
                                     top_n=FALSE,
                                     limit = 10,
                                     df = NULL,
                                     metaphor_var = "metaphors",
                                     submet_var = "submappings",
                                     synonym_var = "synonyms",
                                     lexunit_var = "lu",
                                     lexunit_gloss_var = "lu_gloss") {
  m_var <- rlang::sym(metaphor_var)
  sub_var <- rlang::sym(submet_var)
  syn_var <- rlang::sym(synonym_var)
  lu_var <- rlang::sym(lexunit_var)
  lu_g_var <- rlang::sym(lexunit_gloss_var)

  if (purrr::is_null(word)) { # if the synonym is not specified
    out <- df %>%
      dplyr::filter(stringr::str_detect(!!m_var, metaphor)) %>%
      dplyr::count(!!sub_var, !!lu_var, !!lu_g_var)

  } else {
    out <- df %>%
      dplyr::filter(stringr::str_detect(!!m_var, metaphor),
                    stringr::str_detect(!!syn_var, word)) %>%
      dplyr::count(!!syn_var, !!sub_var, !!lu_var, !!lu_g_var)
  }

  out <- out %>%
    dplyr::mutate(perc_expr_overall = round(.data$n/sum(.data$n) * 100, 2)) %>%
    dplyr::group_by(!!sub_var) %>%
    dplyr::mutate(perc_expr_by_submappings = round(.data$n/sum(.data$n) * 100, 2)) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::ungroup()
  if (top_n == TRUE) {
    out %>%
      dplyr::top_n(limit, n) %>%
      dplyr::arrange(!!sub_var, dplyr::desc(.data$n))
  } else {
    if (print_all == TRUE) {
      out %>%
        dplyr::arrange(!!sub_var, dplyr::desc(.data$n)) %>%
        print(n="Inf")
    } else if (print_all == FALSE) {
      out %>%
        dplyr::arrange(!!sub_var, dplyr::desc(.data$n))
    }
  }
}
