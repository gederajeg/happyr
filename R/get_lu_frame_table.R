#' Retrieve the (top-n most frequent) Lexical Units evoking the source frames of a metaphor
#'
#' @description The function is designed to easily retrieve a tibble data frame of the top-n most frequent frame-evoking Lexical Units (LU) of the prominent metaphors discussed in Chapter 5 and 6.
#' @param metaphor regular expressions for the relevant metaphor.
#' @param word character strings of regular expressions for the HAPPINESS synonyms.
#' @param print_all logical; print all output
#' @param top_n logical; limit to the top-n LUs
#' @param limit integer; maximum rows to be printed from the top-n LUs
#' @param df the data frame (\code{phd_data_metaphor}).
#' @param metaphor_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param frame_var character string for the column name of the source frame variable (i.e., \code{"source_frames"}).
#' @param synonym_var character string for the column name of the synonyms variable (i.e., \code{"synonyms"}).
#' @param lexunit_var character string for the column name of the lexical unit variable (i.e., \code{"lu"}).
#' @param lexunit_gloss_var character string for the column name of the gloss of the lexical units (i.e., \code{"lu_gloss"}).
#'
#' @return A tibble/data frame of the descriptive statistics of the LUs and the frames, including the gloss of the LUs.
#' @importFrom rlang sym
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # for the aggregated data
#' get_lu_frame_table(metaphor = "possessable",
#'                    top_n = TRUE,
#'                    limit = 20,
#'                    df = phd_data_metaphor)
#'
#' # for a specific synonym
#' get_lu_frame_table(metaphor = "possessable",
#'                    word = "^bahagia$",
#'                    top_n = TRUE,
#'                    limit = 20,
#'                    df = phd_data_metaphor)
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for happiness} (PhD Thesis). Monash University. Melbourne, Australia.
get_lu_frame_table <- function(metaphor = NULL,
                               word = NULL,
                               print_all = FALSE,
                               top_n = FALSE,
                               limit = 10,
                               df = NULL,
                               metaphor_var = "metaphors",
                               frame_var = "source_frames",
                               synonym_var = "synonyms",
                               lexunit_var = "lu",
                               lexunit_gloss_var = "lu_gloss") {

  # vars
  meta_var <- rlang::sym(metaphor_var)
  frame_var <- rlang::sym(frame_var)
  syn_var <- rlang::sym(synonym_var)
  lu_var <- rlang::sym(lexunit_var)
  gloss_var <- rlang::sym(lexunit_gloss_var)

  if (purrr::is_null(word) == TRUE) {
    out <- df %>%
      filter(str_detect(!!meta_var, metaphor)) %>%
      group_by(!!frame_var, !!lu_var, !!gloss_var) %>%
      summarise(!!dplyr::quo_name(dplyr::quo(n)) := n())
  } else {
    out <- df %>%
      filter(str_detect(!!meta_var, metaphor), str_detect(!!syn_var, word)) %>%
      group_by(!!frame_var, !!lu_var, !!gloss_var, !!syn_var) %>%
      summarise(!!dplyr::quo_name(dplyr::quo(n)) := n())
  }

  #output quosures
  perc_overall <- dplyr::quo(perc_overall)
  perc_by_frames <- dplyr::quo(perc_by_frames)
  Lexical_units <- dplyr::quo(Lexical_units)
  Frames <- dplyr::quo(Frames)
  Glosses <- dplyr::quo(Glosses)
  N <- dplyr::quo(N)
  Perc_by_frames <- dplyr::quo(Perc_by_frames)
  Perc_overall <- dplyr::quo(Perc_overall)

  out <- out %>%
    ungroup() %>%
    mutate(!!quo_name(perc_overall) := round(!!quo(n/sum(n) * 100), 2)) %>%
    group_by(!!frame_var) %>%
    mutate(!!quo_name(perc_by_frames) := round(!!quo(n/sum(n) * 100), 2)) %>%
    ungroup() %>%
    arrange(desc(!!quo(n))) %>%
    mutate(!!quo_name(Lexical_units) := stringr::str_c("*", !!lu_var, "*", sep = "")) %>%
    select(!!quo(Lexical_units),
           !!quo_name(Frames) := !!frame_var,
           !!quo_name(Glosses) := !!gloss_var,
           !!quo_name(N) := !!quo(n),
           !!quo_name(Perc_by_frames) := !!quo(perc_by_frames),
           !!quo_name(Perc_overall) := !!quo(perc_overall))

  if (top_n == TRUE) {
    out1 <- out %>%
      dplyr::top_n(limit, !!quo(N))
    out1 %>%
      print(n = "Inf")
  } else {
    if (print_all == TRUE) {
      out %>%
        print(n = "Inf")
    } else if (print_all == FALSE) {
      out
    }
  }
}
