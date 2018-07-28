#' Attracted items from MDCA
#'
#' @description A function to retrieve list of \emph{attracted} items for a construction from the results of \code{\link{mdca}}.
#' @param df output data frame from \code{link{mdca}}.
#' @param filter_by character string indicating whether the output of \code{link{mdca}} is filtered according to the \emph{construction} variable (i.e. \code{"cxn"}) or the \emph{collexemes/collocates} variable (i.e. \code{"colloc"}).
#'     If it is left \code{NULL} (the default), the data is filtered according to the value specified in the \code{min_assocstr} argument.
#' @param cxn_var character string for the column name of the construction variable (i.e., \code{"synonyms"}).
#' @param cxn_type character strings of regular expressions for the HAPPINESS synonyms.
#' @param coll_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param coll_type character strings of regular expressions for the conceptual metaphors.
#' @param min_assocstr numeric threshold for the minimum collostruction strength. By default it is set to \code{2}.
#'
#' @return A tibble/data frame
#' @export
#'
#' @examples
#' # for distinctive metaphors
#' mdca_res <- mdca(df = phd_data_metaphor,
#'                  cxn_var = "synonyms",
#'                  coll_var = "metaphors",
#'                  correct_holm = TRUE,
#'                  concise_output = TRUE,
#'                  already_count_table = FALSE,
#'                  assocstr_digits = 3)
#' # retrieve distinctive/attracted metaphors for "kebahagiaan".
#' mdca_attr(df = mdca_res,
#'           filter_by = "cxn",
#'           cxn_var = "synonyms",
#'           cxn_type = "kebahagiaan",
#'           min_assocstr = 2)
#'
mdca_attr <- function(df = NULL,
                      filter_by = "cxn",
                      cxn_var = "synonyms",
                      cxn_type = NULL,
                      coll_var = NULL,
                      coll_type = NULL,
                      min_assocstr = 2) {

  assertthat::assert_that(!is.null(df), msg = "The `df` argument is NULL; please specify it with the data frame output from `mdca()`!")

  if (is.null(filter_by)) {
    message("The output of `mdca()` is filtered by the given minimum AssocStr threshold (specified in the `min_assocstr` argument), but not according to the Construction or Collocation/Collexeme variables!\nIf you want to filter it according to the Construction, specify `filter_by` with 'cxn' (or 'colloc' for Collocation/Collexeme) and `cxn_type` with the character name of the Construction (e.g., 'kebahagiaan')\nFor Collocation filtering, specify the `coll_type` argument with the character name of a collocate/collexeme!\n")
    x <- df %>%
      dplyr::filter(.data$assocstr >= min_assocstr)
    return(x)
  } else if (filter_by == 'cxn') {
    # message("Filtering the output of `mdca()` by the value of the Construction variable (e.g., those in the 'synonyms' column)...\n")
    if (is.null(cxn_var)) {
      stop("NO input for `cxn_var` argument! Specify it with the column name of the Construction variable (e.g., 'synonyms') from the `mdca()` output!\n")
    } else if (is.null(cxn_var) == FALSE) {
      if (is.null(cxn_type)) {
        stop("NO input for `cxn_type` argument! Specify it with the name of the Construction (e.g., the values in the 'synonyms' column from the `mdca()` output)!\n")
      } else if (is.null(cxn_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$assocstr >= min_assocstr,
                        stringr::str_detect(!!rlang::sym(cxn_var), cxn_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), dplyr::desc(.data$assocstr))
      }
    }
  } else if (filter_by == 'colloc') {
    # message("Filtering the output of `mdca()` by the value of the Collocation/Collexeme variable (e.g., those in the 'metaphors' column)...\n")
    if (is.null(coll_var)) {
      stop("NO input for `coll_var` argument! Specify it with the column name of the Collocation/Collexeme variable (e.g., 'metaphors') from the `mdca()` output!\n")
    } else if (is.null(coll_var) == FALSE) {
      if (is.null(coll_type)) {
        stop("NO input for `coll_type` argument! Specify it with the name of the Collocation/Collexeme (e.g., the values in the 'metaphors' column from the `mdca()` output)!\n")
      } else if (is.null(coll_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$assocstr >= min_assocstr,
                        stringr::str_detect(!!rlang::sym(coll_var), coll_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), dplyr::desc(.data$assocstr))
      }
    }
  }
  return(x)
}


#' Repelled items from MDCA
#'
#' @description A function to retrieve list of \emph{repelled} items for a construction from the results of \code{\link{mdca}}.
#' @param df output data frame from \code{link{mdca}}.
#' @param filter_by character string indicating whether the output of \code{link{mdca}} is filtered according to the \emph{construction} variable (i.e. \code{"cxn"}) or the \emph{collexemes/collocates} variable (i.e. \code{"colloc"}).
#'     If it is left \code{NULL} (the default), the data is filtered according to the value specified in the \code{min_assocstr} argument.
#' @param cxn_var character string for the column name of the construction variable (i.e., \code{"synonyms"}).
#' @param cxn_type character strings of regular expressions for the HAPPINESS synonyms.
#' @param coll_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param coll_type character strings of regular expressions for the conceptual metaphors.
#' @param min_assocstr numeric threshold for the minimum collostruction strength. By default it is set to \code{-2}.
#'
#' @return A tibble/data frame
#' @export
#'
#' @examples
#' # for distinctive metaphors
#' mdca_res <- mdca(df = phd_data_metaphor,
#'                  cxn_var = "synonyms",
#'                  coll_var = "metaphors",
#'                  correct_holm = TRUE,
#'                  concise_output = TRUE,
#'                  already_count_table = FALSE,
#'                  assocstr_digits = 3)
#' # retrieve the repelled metaphors for "kebahagiaan".
#' mdca_repel(df = mdca_res,
#'           filter_by = "cxn",
#'           cxn_var = "synonyms",
#'           cxn_type = "kebahagiaan",
#'           min_assocstr = -2)
#'
mdca_repel <- function(df = NULL,
                      filter_by = "cxn",
                      cxn_var = "synonyms",
                      cxn_type = NULL,
                      coll_var = NULL,
                      coll_type = NULL,
                      min_assocstr = -2) {

  assertthat::assert_that(!is.null(df), msg = "The `df` argument is NULL; please specify it with the data frame output from `mdca()`!")

  if (is.null(filter_by)) {
    message("The output of `mdca()` is filtered by the given minimum AssocStr threshold (specified in the `min_assocstr` argument), but not according to the Construction or Collocation/Collexeme variables!\nIf you want to filter it according to the Construction, specify `filter_by` with 'cxn' (or 'colloc' for Collocation/Collexeme) and `cxn_type` with the character name of the Construction (e.g., 'kebahagiaan')\nFor Collocation filtering, specify the `coll_type` argument with the character name of a collocate/collexeme!\n")
    x <- df %>%
      dplyr::filter(.data$assocstr <= min_assocstr)
    return(x)
  } else if (filter_by == 'cxn') {
    # message("Filtering the output of `mdca()` by the value of the Construction variable (e.g., those in the 'synonyms' column)...\n")
    if (is.null(cxn_var)) {
      stop("NO input for `cxn_var` argument! Specify it with the column name of the Construction variable (e.g., 'synonyms') from the `mdca()` output!\n")
    } else if (is.null(cxn_var) == FALSE) {
      if (is.null(cxn_type)) {
        stop("NO input for `cxn_type` argument! Specify it with the name of the Construction (e.g., the values in the 'synonyms' column from the `mdca()` output)!\n")
      } else if (is.null(cxn_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$assocstr <= min_assocstr,
                        stringr::str_detect(!!rlang::sym(cxn_var), cxn_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), .data$assocstr)
      }
    }
  } else if (filter_by == 'colloc') {
    # message("Filtering the output of `mdca()` by the value of the Collocation/Collexeme variable (e.g., those in the 'metaphors' column)...\n")
    if (is.null(coll_var)) {
      stop("NO input for `coll_var` argument! Specify it with the column name of the Collocation/Collexeme variable (e.g., 'metaphors') from the `mdca()` output!\n")
    } else if (is.null(coll_var) == FALSE) {
      if (is.null(coll_type)) {
        stop("NO input for `coll_type` argument! Specify it with the name of the Collocation/Collexeme (e.g., the values in the 'metaphors' column from the `mdca()` output)!\n")
      } else if (is.null(coll_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$assocstr <= min_assocstr,
                        stringr::str_detect(!!rlang::sym(coll_var), coll_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), .data$assocstr)
      }
    }
  }
  return(x)
}
