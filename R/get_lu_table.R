#' Retrieve the (top-n most frequent) Lexical Units of a metaphor
#'
#' @description The function is designed to easily retrieve a tibble data frame of the top-n most frequent Lexical Units (LU) of the prominent metaphors discussed in Chapter 5 and 6.
#' @param metaphor regular expressions for the relevant metaphor.
#' @param top_n_only logical; the default is \code{TRUE} with the number specified in \code{top_n_limit} argument. If \code{FALSE}, it prints all the LU for the metaphor.
#' @param top_n_limit integer; how many most frequent LU should be printed? The default is \code{10}.
#' @param df the data frame for the thesis (\code{phd_data_metaphor}). This argument is not put first because the main attention when using the function is on varying the metaphor (hence, coming first) rather than on varying the database.
#' @param submapping_perc logical; wheter to include the percentages of the LU by submappings. If \code{TRUE}, the function only print out the percentages of the LU by submappings.
#'     To include the submapping types, specify the \code{incl_submappings} argument to \code{TRUE} as well (the default is \code{FALSE}).
#' @param incl_submappings logical; whether to include the submappings inferred from the LU (\code{TRUE}) or not (\code{FALSE} -- the default).
#' @return A tbl_df.
#' @examples
#' get_lu_table(metaphor = "destination", df = phd_data_metaphor)
#' @importFrom stringr regex
#' @importFrom dplyr top_n
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @export

get_lu_table <- function(metaphor = "regular expressions",
                         top_n_only = TRUE,
                         top_n_limit = 10,
                         df = NULL,
                         submapping_perc = FALSE,
                         incl_submappings = FALSE) {

  assertthat::assert_that(!is.null(df), msg = "The `df` argument is NULL; please specify it with `phd_data_metaphor`!")

  perc_overall <- dplyr::quo(perc_overall)
  perc_by_submappings <- dplyr::quo(perc_by_submappings)

  if (submapping_perc == FALSE) {
    out <- df %>%
      dplyr::filter(stringr::str_detect(.data$metaphors, stringr::regex(metaphor, ignore_case = TRUE))) %>%
      dplyr::count(.data$lu, .data$lu_gloss, sort = TRUE) %>%
      dplyr::select(Lexical_units = .data$lu, Gloss = .data$lu_gloss, n = n) %>%
      dplyr::mutate(Lexical_units = stringr::str_c("*", .data$Lexical_units, "*", sep = ""),
                    Perc_overall = round(n/sum(n)*100, 2))
    if (top_n_only==TRUE) {
      out <- out %>%
        dplyr::top_n(top_n_limit, n) %>%
        dplyr::rename(N = n)
      return(out)
    } else {
      out <- out %>%
        dplyr::rename(N = n)
      return(out)
    }
  } else { # if one wishes to include the percentage of the submappings of a metaphor
    out <- df %>%
      dplyr::filter(stringr::str_detect(.data$metaphors, stringr::regex(metaphor, ignore_case = TRUE))) %>%
      dplyr::count(.data$submappings, .data$lu, .data$lu_gloss, sort = TRUE) %>%
      dplyr::mutate(!!dplyr::quo_name(perc_overall) := round(n/sum(n)*100, 2)) %>%
      dplyr::group_by(.data$submappings) %>%
      dplyr::mutate(!!dplyr::quo_name(perc_by_submappings) := round(n/sum(n)*100, 2)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(n))
    if (incl_submappings == FALSE) {
      out <- out %>%
        dplyr::select(Lexical_units = .data$lu,
                      Gloss = .data$lu_gloss,
                      n = .data$n,
                      .data$perc_overall,
                      .data$perc_by_submappings) %>%
        dplyr::mutate(Lexical_units = stringr::str_c("*", .data$Lexical_units, "*", sep = "")) %>%
        dplyr::rename(Perc_overall = perc_overall,
                      Perc_by_submappings = perc_by_submappings)
    } else {
      out <- out %>%
        dplyr::select(Submappings = .data$submappings,
                      Lexical_units = .data$lu,
                      Gloss = .data$lu_gloss,
                      dplyr::everything()) %>%
        dplyr::mutate(Lexical_units = stringr::str_c("*", .data$Lexical_units, "*", sep = ""),
                      Submappings = stringr::str_c('<span style="font-variant:small-caps;">', .data$Submappings, '</span>', sep = '')) %>%
        dplyr::rename(Perc_overall = perc_overall,
                      Perc_by_submappings = perc_by_submappings)
    }

    if (top_n_only==TRUE) {
      out <- out %>%
        dplyr::top_n(top_n_limit, n) %>%
        dplyr::rename(N = n)
      return(out)
    } else {
      out <- out %>%
        dplyr::rename(N = n)
      return(out)
    }
  }


}
