#' Attracted items from MDCA
#'
#' @description A function to retrieve list of \emph{attracted} items for a construction from the results of \code{\link{mdca}}.
#' @param df output data frame from \code{link{mdca}}.
#' @param filter_by character string indicating whether the output of \code{link{mdca}} is filtered according to the \emph{construction} variable (i.e. \code{"cxn"}) or the \emph{collexemes/collocates} variable (i.e. \code{"colloc"}).
#'     If it is left \code{NULL} (the default), the data is filtered according to the value specified in the \code{min_collstr} argument.
#' @param cxn_var character string for the column name of the construction variable (i.e., \code{"synonyms"}).
#' @param cxn_type character strings of regular expressions for the HAPPINESS synonyms.
#' @param coll_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param coll_type character strings of regular expressions for the conceptual metaphors.
#' @param min_collstr numeric threshold for the minimum collostruction strength. By default it is set to \code{2}.
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
#'                  collstr_digits = 3)
#' # retrieve distinctive/attracted metaphors for "kebahagiaan".
#' mdca_attr(df = mdca_res,
#'           filter_by = "cxn",
#'           cxn_var = "synonyms",
#'           cxn_type = "kebahagiaan",
#'           min_collstr = 2)
#'
mdca_attr <- function(df = NULL,
                      filter_by = "cxn",
                      cxn_var = "synonyms",
                      cxn_type = NULL,
                      coll_var = NULL,
                      coll_type = NULL,
                      min_collstr = 2) {
  if (is.null(filter_by)) {
    cat("Filtered by the given minimun CollStr threshold!\n")
    x <- df %>%
      dplyr::filter(.data$coll.str >= min_collstr)
    return(x)
  } else if (filter_by == 'cxn') {
    if (is.null(cxn_var)) {
      cat("NO input for 'cxn.var' argument!\n")
    } else if (is.null(cxn_var) == FALSE) {
      if (is.null(cxn_type)) {
        cat("NO input for 'cxn.type' argument!\n")
      } else if (is.null(cxn_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$coll.str >= min_collstr,
                        stringr::str_detect(!!rlang::sym(cxn_var), cxn_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), dplyr::desc(.data$coll.str)) %>%
          as.data.frame()
      }
    }
  } else if (filter_by == 'colloc') {
    if (is.null(coll_var)) {
      cat("NO input for 'colloc.var' argument!\n")
    } else if (is.null(coll_var) == FALSE) {
      if (is.null(coll_type)) {
        cat("NO input for 'colloc.type' argument!\n")
      } else if (is.null(coll_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$coll.str >= min_collstr,
                        stringr::str_detect(!!rlang::sym(coll_var), coll_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), dplyr::desc(.data$coll.str)) %>%
          as.data.frame()
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
#'     If it is left \code{NULL} (the default), the data is filtered according to the value specified in the \code{min_collstr} argument.
#' @param cxn_var character string for the column name of the construction variable (i.e., \code{"synonyms"}).
#' @param cxn_type character strings of regular expressions for the HAPPINESS synonyms.
#' @param coll_var character string for the column name of the metaphor variable (i.e., \code{"metaphors"}).
#' @param coll_type character strings of regular expressions for the conceptual metaphors.
#' @param min_collstr numeric threshold for the minimum collostruction strength. By default it is set to \code{-2}.
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
#'                  collstr_digits = 3)
#' # retrieve the repelled metaphors for "kebahagiaan".
#' mdca_repel(df = mdca_res,
#'           filter_by = "cxn",
#'           cxn_var = "synonyms",
#'           cxn_type = "kebahagiaan",
#'           min_collstr = -2)
#'
mdca_repel <- function(df = NULL,
                      filter_by = "cxn",
                      cxn_var = "synonyms",
                      cxn_type = NULL,
                      coll_var = NULL,
                      coll_type = NULL,
                      min_collstr = -2) {
  if (is.null(filter_by)) {
    cat("Filtered by the given minimun CollStr threshold!\n")
    x <- df %>%
      dplyr::filter(.data$coll.str <= min_collstr)
    return(x)
  } else if (filter_by == 'cxn') {
    if (is.null(cxn_var)) {
      cat("NO input for 'cxn.var' argument!\n")
    } else if (is.null(cxn_var) == FALSE) {
      if (is.null(cxn_type)) {
        cat("NO input for 'cxn.type' argument!\n")
      } else if (is.null(cxn_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$coll.str <= min_collstr,
                        stringr::str_detect(!!rlang::sym(cxn_var), cxn_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), .data$coll.str) %>%
          as.data.frame()
      }
    }
  } else if (filter_by == 'colloc') {
    if (is.null(coll_var)) {
      cat("NO input for 'colloc.var' argument!\n")
    } else if (is.null(coll_var) == FALSE) {
      if (is.null(coll_type)) {
        cat("NO input for 'colloc.type' argument!\n")
      } else if (is.null(coll_type) == FALSE) {
        x <- df %>%
          dplyr::filter(.data$coll.str <= min_collstr,
                        stringr::str_detect(!!rlang::sym(coll_var), coll_type)) %>%
          dplyr::arrange(!!rlang::sym(cxn_var), .data$coll.str) %>%
          as.data.frame()
      }
    }
  }
  return(x)
}
