#' Multiple distinctive collexeme analysis (MDCA)
#'
#' @description Function to perform \emph{Multiple Distinctive Collexeme Analysis} (MDCA) in Rajeg's (2018) Chapter 7.
#' @param df the data frame for the thesis (\code{phd_data_metaphor}) included in the package.
#' @param cxn_var character strings for the column name for the \code{constructions} variable, in this case, the "\code{synonyms}" column.
#' @param coll_var character strings for the column name for the \code{collocates} variable, in this case, the "\code{metaphors}" column.
#' @param already_count_table logical; the default is \code{FALSE} indicating \code{mdca} takes raw input data frame for observation-per-row format as in the case of \code{phd_data_metaphor}.
#'     When it is \code{TRUE}, it expects tidy co-occurrence count between values of \code{var_cxn} and \code{var_coll} with three columns:
#'     \tabular{rrr}{
#'      synonyms \tab metaphors \tab n\cr
#'          kesenangan \tab happiness is a possessable object \tab 182\cr
#'          kebahagiaan \tab happiness is a possessable object \tab 181\cr
#'          ... \tab ... \tab ...
#'     }
#' @param assocstr_digits integer for the floating points/digits of the \emph{Association Strength}. The default is \code{3L}.
#' @param correct_holm logical; the default is \code{TRUE} for performing Holm's correction method of the \emph{p}-value (cf. Gries, 2009, p. 249).
#' @param concise_output logical; if \code{TRUE} (the default), \code{mdca} outputs the following columns:
#'     \itemize{
#'         \item \code{metaphors}
#'         \item \code{synonyms}
#'         \item \code{n} (for the \emph{observed} co-occurrence frequency between \code{metaphors} and the \code{synonyms}).
#'         \item \code{exp} (for the \emph{expected} co-occurrence frequency between \code{metaphors} and the \code{synonyms}).
#'         \item \code{p_binomial} (the one-tailed \emph{p}-value of the \emph{Binomial Test}).
#'         \item \code{assocstr} (the log10 transformed values of the \emph{p}-value of the \emph{Binomial Test}. The \code{assocstr} values are \emph{positive} when \code{n} is higher than the \code{exp} frequency, and they are \emph{negative} when otherwise.).
#'         \item \code{p_holm} (when \code{correct_holm} is \code{TRUE})
#'         \item \code{dec} (significance decision after Holm's correction) (when \code{correct_holm} is \code{TRUE})
#'     }
#' @details The \code{mdca} function is built on top of the core members of the \code{tidyverse} suit of packages.
#'     The computation of the \emph{Association Strength} is based on the \code{\link[stats]{binom.test}} function (cf. Hilpert, 2006).
#'     The computation of the corrected \emph{p}-value of the one-tailed Binomial Test with Holm's method is performed using \code{\link[stats]{p.adjust}}.
#' @return A tbl_df (cf. the \code{concise_output}).
#' @examples
#' # for distinctive metaphors
#' mdca_res <- mdca(df = phd_data_metaphor,
#'                  cxn_var = "synonyms",
#'                  coll_var = "metaphors",
#'                  correct_holm = TRUE,
#'                  concise_output = TRUE,
#'                  already_count_table = FALSE,
#'                  assocstr_digits = 3L)
#'
#' # for distinctive 4-window span collocates
#' data("colloc_input_data")
#' mdca_colloc <- mdca(df = colloc_input_data,
#'                     cxn_var = "synonyms",
#'                     coll_var = "collocates",
#'                     correct_holm = TRUE,
#'                     concise_output = TRUE,
#'                     already_count_table = FALSE,
#'                     assocstr_digits = 3L)

#' @importFrom dplyr group_by_
#' @importFrom dplyr if_else
#' @importFrom tidyr complete_
#' @importFrom stats binom.test
#' @importFrom stats p.adjust
#' @importFrom purrr pmap
#' @importFrom purrr map_dbl
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom dplyr rename_
#' @importFrom dplyr select_
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr matches
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @references
#'     \itemize{
#'         \item Gries, S. T. (2009). \emph{Statistics for linguistics with R: A practical introduction}. Berlin: Mouton de Gruyter.
#'         \item Hilpert, M. (2006). Distinctive collexeme analysis and diachrony. \emph{Corpus Linguistics and Linguistic Theory}, \emph{2}(2), 243–256.
#'         \item Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia.
#'     }
#' @export
mdca <- function(df = NULL,
                 cxn_var = "synonyms",
                 coll_var = "metaphors",
                 already_count_table = FALSE,
                 assocstr_digits = 3L,
                 correct_holm = TRUE,
                 concise_output = TRUE) {

  assertthat::assert_that(!is.null(df), msg = "The `df` argument is NULL; please specify it with the data frame input!")

  # quiets concerns of R CMD check re: the .'s that appear in pipelines
  # . <- "shut_up" not needed as all pipes have been removed

  # columns names for tidyeval
  cxn_var <- rlang::sym(cxn_var)
  coll_var <- rlang::sym(coll_var)

  # function IF MDCA starts here
  # cross-tab the relevant variables
  if (already_count_table == FALSE) {
    co_occ_tb <- dplyr::count(df, !!cxn_var, !!coll_var)
    co_occ_tb <- tidyr::complete(co_occ_tb, !!coll_var, !!cxn_var, fill = list(n = 0))
  } else {
    co_occ_tb <- tidyr::complete(df, !!coll_var, !!cxn_var, fill = list(n = 0))
  }

  # get the total freq of the CxN
  cxn_sum <- dplyr::summarise(dplyr::group_by(co_occ_tb, !!cxn_var), cxn_sum = sum(.data$n))

  # left join the total freq of the CxN to the data base
  co_occ_tb <- dplyr::left_join(co_occ_tb, cxn_sum, by = dplyr::quo_name(cxn_var))

  # get the total freq of the COLLOCATES
  colloc_sum <- dplyr::summarise(dplyr::group_by(co_occ_tb, !!coll_var), colloc_sum = sum(.data$n))

  # left join the total freq of the COLLOCATES to the data base
  co_occ_tb <- dplyr::left_join(co_occ_tb, colloc_sum, by = dplyr::quo_name(coll_var))

  # get the total database token/sum of the database
  if (already_count_table == FALSE) {
    co_occ_tb <- dplyr::mutate(co_occ_tb, dbase_token = dim(df)[1])
  } else {
    co_occ_tb <- dplyr::mutate(co_occ_tb, dbase_token = sum(.data$n))
  }

  # get the EXPECTED FREQUENCY, EXPECTED PROBABILITY, OBS_EXP DIFFERENCE, and BINOMIAL ALTERNATIVES
  co_occ_tb <- dplyr::mutate(co_occ_tb, exp = (cxn_sum * colloc_sum)/.data$dbase_token, # exp.freq
                             exp_prob = exp/colloc_sum, # exp_prob
                             obs_exp = '=',
                             obs_exp = dplyr::if_else(n > exp, '>', .data$obs_exp), # obs_exp diff.
                             obs_exp = dplyr::if_else(n < exp, '<', .data$obs_exp), # obs_exp diff.
                             alt = dplyr::if_else(n >= exp, 'greater', 'less')) # obs_exp diff.

  # compute the ONE-TAIL EXACT BINOMIAL TEST and ASSOCIATION STRENGTH VALUE
  p_binomial <- dplyr::quo(p_binomial)
  assocstr <- dplyr::quo(assocstr)
  abs_assocstr <- dplyr::quo(abs_assocstr)
  co_occ_tb <- tidyr::nest(dplyr::group_by(co_occ_tb, !!coll_var, !!cxn_var))
  co_occ_tb <- dplyr::mutate(co_occ_tb,
                             !!dplyr::quo_name(p_binomial) := purrr::map_dbl(data, binomial_test)) # binomial test
  co_occ_tb <- tidyr::nest(dplyr::group_by(tidyr::unnest(co_occ_tb), !!coll_var, !!cxn_var))
  co_occ_tb <- dplyr::mutate(co_occ_tb,
                             !!dplyr::quo_name(assocstr) := purrr::map_dbl(data, assoc_strength, 3L), # association strength
                             !!dplyr::quo_name(abs_assocstr) := abs(.data$assocstr))
  co_occ_tb <- tidyr::unnest(co_occ_tb)

  # get the sum of absolute deviation
  dbase_to_left_join <- dplyr::group_by(co_occ_tb, !!coll_var)
  ## generate a sum_abs_dev for the COLLOCATES
  dbase_to_left_join <- dplyr::summarise(dbase_to_left_join, sum_abs_dev = sum(.data$abs_assocstr))
  dbase_to_left_join <- dplyr::ungroup(dbase_to_left_join)
  co_occ_tb <- dplyr::left_join(co_occ_tb, dbase_to_left_join, by = dplyr::quo_name(coll_var))

  ## get the CxN with the largest deviation
  df_for_largest_dev <- split(co_occ_tb, co_occ_tb[, 1])
  df_for_largest_dev_res <- purrr::map(df_for_largest_dev, function(lrg_dev) dplyr::filter(lrg_dev, lrg_dev$abs_assocstr == max(lrg_dev$abs_assocstr)))
  df_for_largest_dev_res <- purrr::map_df(df_for_largest_dev_res, function(lrg_dev) dplyr::ungroup(lrg_dev))
  vars_to_select <- stringr::str_c(dplyr::quo_name(coll_var), dplyr::quo_name(cxn_var), 'abs_assocstr', sep = '|')
  df_for_largest_dev_res <- dplyr::select(df_for_largest_dev_res,
                                          dplyr::matches(vars_to_select))
  df_for_largest_dev_res <- dplyr::rename(df_for_largest_dev_res, largest_dev = !!cxn_var)
  df_for_largest_dev_res <- dplyr::select(df_for_largest_dev_res, -!!abs_assocstr)
  rm(df_for_largest_dev)

  ## left_join the largest dev. CxN
  co_occ_tb <- dplyr::ungroup(co_occ_tb)
  co_occ_tb <- dplyr::select(co_occ_tb, -!!abs_assocstr)
  co_occ_tb <- dplyr::left_join(co_occ_tb, df_for_largest_dev_res, by = dplyr::quo_name(coll_var))

  # compute HOLM'S ADJUSTED P-VALUE
  # cf. http://rcompanion.org/rcompanion/f_01.html for example with `p.adjust()`
  co_occ_tb <- dplyr::mutate(co_occ_tb,
                             p_holm = stats::p.adjust(p = .data$p_binomial, method = "holm"),
                             dec = "ns", # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(.data$p_holm < 0.1, "ms", .data$dec), # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(.data$p_holm < 0.05, "*", .data$dec), # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(.data$p_holm < 0.01, "**", .data$dec), # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(.data$p_holm < 0.001, "***", .data$dec)) # from Gries' (2004) HCFA script
  # Gries, Stefan Th. 2004. HCFA 3.2. A program for R. URL: <http://www.linguistics.ucsb.edu/faculty/stgries/>
  # Gries' HCFA script is available from the following book:
  # Gries, Stefan Th. (2009). Statistics for linguistics with R: A practical introduction. Berlin: Mouton de Gruyter.

  # outputting the results
  if (concise_output == TRUE) {
    if (correct_holm == FALSE) {
      x <- dplyr::select(co_occ_tb,
                         !!coll_var,
                         !!cxn_var,
                         !!rlang::sym('n'),
                         !!rlang::sym('exp'),
                         !!rlang::sym('assocstr'),
                         !!rlang::sym('p_binomial'))
      x <- dplyr::mutate(x,
                         p_binomial = format(.data$p_binomial, digits = assocstr_digits + 1L))
      return(x)
    } else {
      x <- dplyr::select(co_occ_tb,
                         !!coll_var,
                         !!cxn_var,
                         !!rlang::sym('n'),
                         !!rlang::sym('exp'),
                         !!rlang::sym('assocstr'),
                         !!rlang::sym('p_binomial'),
                         !!rlang::sym('p_holm'),
                         !!rlang::sym('dec'))
      x <- dplyr::mutate(x, p_holm = format(.data$p_holm, digits = assocstr_digits + 1L),
                         p_binomial = format(.data$p_binomial, digits = assocstr_digits + 1L))
      return(x)
    }
  } else {
    x <- dplyr::select(co_occ_tb,
                       !!coll_var,
                       !!cxn_var,
                       !!rlang::sym('n'),
                       !!rlang::sym('exp'),
                       !!rlang::sym('assocstr'),
                       !!rlang::sym('p_binomial'),
                       !!rlang::sym('p_holm'),
                       !!rlang::sym('dec'),
                       !!rlang::sym('sum_abs_dev'),
                       !!rlang::sym('largest_dev'),
                       dplyr::everything()
    )
    return(x)
  }
}

#' Binomial test
#'
#' @description A utility function to perform one-tailed Binomial Test in a row-wise, tidy fashion.
#'     It is called internally by \code{\link{mdca}}.
#' @param df A nested/list data frame containing input data for the Binomial Test
#'
#' @return A double numeric vector of p-value from the Binomial Test
#' @importFrom stats binom.test

binomial_test <- function(df) {
  p_binomial <- stats::binom.test(df$n, df$colloc_sum, df$exp_prob, df$alt)$p.value
  return(p_binomial)
}

#' Association strength
#'
#' @description A utility function to derive association strength value in a row-wise, tidy fashion.
#'     It is derived via log-transformation to the base of 10L of the Binomial Test p-value.
#'     The function is called internally by \code{\link{mdca}}.
#' @param df A nested/list data frame containing input data for transforming the p-value of the Binomial Test into association strength value
#' @param assocstr_digit Integer for the floating points/digits of the \emph{Association Strength}. The default is \code{3L}, which is passed-on from \code{\link{mdca}}.
#'
#' @return A double numeric vector of association strength
#' @importFrom dplyr if_else

assoc_strength <- function(df, assocstr_digit = assocstr_digit) {
  assocstr <- dplyr::if_else(df$n >= df$exp, round(-log10(df$p_binomial), assocstr_digit), round(log10(df$p_binomial), assocstr_digit))
  return(assocstr)
}
