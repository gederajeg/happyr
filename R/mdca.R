#' Multiple distinctive collexeme analysis (MDCA)
#'
#' @description Function to perform \emph{Multiple Distinctive Collexeme Analysis} (MDCA) in Rajeg (2019, Chapter 7).
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
#'
#' If \code{concise_output} is \code{FALSE}, \code{mdca} returns the total tokens in the data, total frequency of each collexeme/collocate, total frequency of each construction, the sum of absolute deviation of the collexeme/collocate, the construction name, showing the largest deviation from the expected, co-occurrence frequency with the collexeme, expected probability of the co-occurrence, and the direction of the deviation from the expected frequency (i.e., whether a collexeme is attracted or repelled by a construction).
#' @details The \code{mdca} function is built on top of the core members of the \code{tidyverse} suit of packages.
#'     The computation of the \emph{Association Strength} is based on the \code{\link[stats]{dbinom}} function (Gries, 2009, pp. 41-42; cf. Hilpert, 2006). The computation of the corrected \emph{p}-value of the one-tailed Binomial Test with Holm's method is performed using \code{\link[stats]{p.adjust}}.
#'
#'     There is a well-known interactive R script to perform MDCA by Stefan Th. Gries that is called \href{http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html}{\emph{Coll.analysis 3.5}} (Gries, 2014). The script includes the other codes to compute the family of methods of \emph{Collostructional Analyses}. The \code{mdca} function in happyr aims to achieve the same analytical goal as that in \emph{Coll.analysis 3.5}, but is designed differently in terms of its usage and the internal codes, as it is based on the \href{https://www.tidyverse.org/}{tidyverse}.
#'
#'     \code{mdca} allows users to have input and output data frame directly in the R environment, primarily enabling them to write interactive document in R Markdown in relation to MDCA. Moreover, happyr provides two functions dedicated to handle the output of \code{mdca} to retrieve the \emph{distinctive/attracted} and \emph{repelled} collexemes/collocates for a given construction. In contrast, Stefan Gries' script has two options to either print the output into (i) terminal or (ii) into external plain text, which requires post-processing of the results, mostly on a spreadsheet.
#'
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
#'                    cxn_var = "synonyms",
#'                    coll_var = "collocates",
#'                    correct_holm = TRUE,
#'                    concise_output = TRUE,
#'                    already_count_table = FALSE,
#'                    assocstr_digits = 3L)

#' @importFrom dplyr group_by_
#' @importFrom dplyr if_else
#' @importFrom tidyr complete_
#' @importFrom stats p.adjust
#' @importFrom stats dbinom
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
#'         \item Gries, S. T. (2014). Coll.analysis 3.5. A script for R to compute perform collostructional analyses. \url{http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/index.html}.
#'         \item Hilpert, M. (2006). Distinctive collexeme analysis and diachrony. \emph{Corpus Linguistics and Linguistic Theory}, \emph{2}(2), 243–256.
#'         \item Rajeg, G. P. W. (2019). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia. \url{https://doi.org/10.26180/5cac231a97fb1}.
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

  # columns names for tidyeval
  cxn_var <- rlang::sym(cxn_var)
  coll_var <- rlang::sym(coll_var)
  cxn_sum <- dplyr::quo(cxn_sum)
  colloc_sum <- dplyr::quo(colloc_sum)
  dbase_token <- dplyr::quo(dbase_token)
  p_binomial <- dplyr::quo(p_binomial)
  p_holm <- dplyr::quo(p_holm)
  dec <- dplyr::quo(dec)
  assocstr <- dplyr::quo(assocstr)
  exp <- dplyr::quo(exp)
  exp_prob <- dplyr::quo(exp_prob)
  alt <- dplyr::quo(alt)
  obs_exp <- dplyr::quo(obs_exp)
  abs_assocstr <- dplyr::quo(abs_assocstr)

  # function IF MDCA starts here
  # cross-tab the relevant variables
  if (already_count_table == FALSE) {
    co_occ_tb <- dplyr::count(df, !!cxn_var, !!coll_var)
    co_occ_tb <- tidyr::complete(co_occ_tb, !!coll_var, !!cxn_var, fill = list(n = 0L))
  } else {
    co_occ_tb <- tidyr::complete(df, !!coll_var, !!cxn_var, fill = list(n = 0L))
  }

  # get the total database token/sum of the database
  if (already_count_table == FALSE) {
    co_occ_tb <- dplyr::mutate(co_occ_tb, !!dplyr::quo_name(dbase_token) := dim(df)[1])
  } else {
    co_occ_tb <- dplyr::mutate(co_occ_tb, !!dplyr::quo_name(dbase_token) := sum(.data$n))
  }

  # get the total freq. of the construction/node word
  co_occ_tb <- dplyr::mutate(dplyr::group_by(co_occ_tb, !!cxn_var),
                             !!dplyr::quo_name(cxn_sum) := sum(.data$n))

  # get the total freq. of the collocates/collexemes/context words
  co_occ_tb <- dplyr::mutate(dplyr::group_by(co_occ_tb, !!coll_var),
                             !!dplyr::quo_name(colloc_sum) := sum(.data$n))

  # get the exp.freq and exp.prob
  co_occ_tb <- dplyr::mutate(dplyr::ungroup(co_occ_tb),
                             !!dplyr::quo_name(exp) := (cxn_sum * colloc_sum)/.data$dbase_token,
                             !!dplyr::quo_name(exp_prob) := exp/colloc_sum,
                             !!dplyr::quo_name(obs_exp) := '=',
                             obs_exp = dplyr::if_else(n > exp, '>', .data$obs_exp), # obs_exp diff.
                             obs_exp = dplyr::if_else(n < exp, '<', .data$obs_exp), # obs_exp diff.
                             !!dplyr::quo_name(alt) := dplyr::if_else(n >= exp, 'greater', 'less'))

  # binomial test function
  binomial_test <- function(n, colloc_sum, exp_prob, alt) {
    if (alt == "greater") {
      pbin <- sum(dbinom(n:colloc_sum, colloc_sum, exp_prob))
    } else {
      pbin <- sum(dbinom(0:n, colloc_sum, exp_prob))
    }
    return(pbin)
  }

  # association strength function
  assoc_strength <- function(n, exp, p_binomial, assocstr_digit = assocstr_digit) {
    assocstr <- dplyr::if_else(n >= exp,
                               round(-log10(p_binomial), assocstr_digit),
                               round(log10(p_binomial), assocstr_digit))
    return(assocstr)
  }

  # run binomial test, association strength computation, and Holm's adjustment
  # cf. http://rcompanion.org/rcompanion/f_01.html for example with `p.adjust()`
  co_occ_tb <- dplyr::mutate(co_occ_tb,
                             !!dplyr::quo_name(p_binomial) := purrr::pmap_dbl(list(n, colloc_sum, exp_prob, alt),
                                                                              binomial_test),
                             !!dplyr::quo_name(assocstr) := purrr::pmap_dbl(list(n, exp, p_binomial),
                                                                            assoc_strength, assocstr_digits),
                             !!dplyr::quo_name(abs_assocstr) := abs(.data$assocstr),
                             !!dplyr::quo_name(p_holm) := stats::p.adjust(p_binomial, "holm"),
                             !!dplyr::quo_name(dec) := "ns", # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(p_holm < 0.1, "ms", dec), # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(p_holm < 0.05, "*", dec), # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(p_holm < 0.01, "**", dec), # from Gries' (2004) HCFA script
                             dec = dplyr::if_else(p_holm < 0.001, "***", dec))
  # Gries, Stefan Th. 2004. HCFA 3.2. A program for R. URL: <http://www.linguistics.ucsb.edu/faculty/stgries/>
  # Gries' HCFA script is available from the following book:
  # Gries, Stefan Th. (2009). Statistics for linguistics with R: A practical introduction. Berlin: Mouton de Gruyter.

  # get the sum of absolute deviation
  dbase_to_left_join <- dplyr::group_by(co_occ_tb, !!coll_var)
  ## generate a sum_abs_dev for the COLLOCATES
  dbase_to_left_join <- dplyr::summarise(dbase_to_left_join, sum_abs_dev = sum(.data$abs_assocstr))
  dbase_to_left_join <- dplyr::ungroup(dbase_to_left_join)
  co_occ_tb <- dplyr::left_join(co_occ_tb, dbase_to_left_join, by = dplyr::quo_name(coll_var))

  ## get the CxN with the largest deviation
  df_for_largest_dev <- split(co_occ_tb, co_occ_tb[, 1])
  df_for_largest_dev_res <- purrr::map_df(df_for_largest_dev, function(lrg_dev) dplyr::filter(dplyr::ungroup(lrg_dev), lrg_dev$abs_assocstr == max(lrg_dev$abs_assocstr)))
  # df_for_largest_dev_res <- purrr::map_df(df_for_largest_dev_res, function(lrg_dev) dplyr::ungroup(lrg_dev))
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
