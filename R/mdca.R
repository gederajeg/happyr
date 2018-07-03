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
#' @param collstr_digits integer for the floating points/digits of the \emph{Collostruction Strength}. The default is \code{3}.
#' @param correct_holm logical; the default is \code{TRUE} for performing Holm's correction method of the \emph{p}-value (cf. Gries, 2009, p. 249).
#' @param concise_output logical; if \code{TRUE} (the default), \code{mdca} outputs the following columns:
#'     \itemize{
#'         \item \code{metaphors}
#'         \item \code{synonyms}
#'         \item \code{n} (for the \emph{observed} co-occurrence frequency between \code{metaphors} and the \code{synonyms}).
#'         \item \code{exp} (for the \emph{expected} co-occurrence frequency between \code{metaphors} and the \code{synonyms}).
#'         \item \code{p.bin} (the one-tailed \emph{p}-value of the \emph{Binomial Test}).
#'         \item \code{coll.str} (the log10 transformed values of the \emph{p}-value of the \emph{Binomial Test}. The \code{coll.str} values are \emph{positive} when \code{n} is higher than the \code{exp} frequency, and they are \emph{negative} when otherwise.).
#'         \item \code{p.holm} (when \code{correct_holm} is \code{TRUE})
#'         \item \code{dec} (significance decision after Holm's correction) (when \code{correct_holm} is \code{TRUE})
#'     }
#' @details The \code{mdca} function is built on top of the core members of the \code{tidyverse} suit of packages.
#'     The computation of the \emph{Collostruction Strength} is based on the \code{\link[stats]{binom.test}} function (cf. Hilpert, 2006).
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
#'                  collstr_digits = 3)
#'\dontrun{
#' # for distinctive 4-window span collocates
#' data("colloc_input_data")
#' mdca_colloc <- mdca(df = colloc_input_data,
#'                     cxn_var = "synonyms",
#'                     coll_var = "collocates",
#'                     correct_holm = TRUE,
#'                     concise_output = TRUE,
#'                     already_count_table = FALSE,
#'                     collstr_digits = 3)
#'}

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
#' @references
#'     \itemize{
#'         \item Gries, S. T. (2009). \emph{Statistics for linguistics with R: A practical introduction}. Berlin: Mouton de Gruyter.
#'         \item Hilpert, M. (2006). Distinctive collexeme analysis and diachrony. \emph{Corpus Linguistics and Linguistic Theory}, \emph{2}(2), 243–256.
#'         \item Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for happiness} (PhD Thesis). Monash University. Melbourne, Australia.
#'     }
#' @export
mdca <- function(df = NULL,
                 cxn_var = "synonyms",
                 coll_var = "metaphors",
                 already_count_table = FALSE,
                 collstr_digits = 3,
                 correct_holm = TRUE,
                 concise_output = TRUE) {
  # quiets concerns of R CMD check re: the .'s that appear in pipelines
  . <- "shut_up"

  # columns names for tidyeval
  cxn_var <- rlang::sym(cxn_var)
  coll_var <- rlang::sym(coll_var)

  # function IF MDCA starts here
  # cross-tab the relevant variables
  if (already_count_table == FALSE) {
    co.occ.tab <- df %>%
      dplyr::count(!!cxn_var, !!coll_var) %>%
      tidyr::complete(!!coll_var, !!cxn_var, fill = list(n = 0L))
  } else {
    co.occ.tab <- df %>%
      tidyr::complete(!!coll_var, !!cxn_var, fill = list(n = 0L))
  }

  # get the total freq of the CxN
  cxn.sum <- co.occ.tab %>%
    dplyr::group_by(!!cxn_var) %>%
    dplyr::summarise(cxn.sum = sum(.data$n))

  # left join the total freq of the CxN to the data base
  co.occ.tab <- co.occ.tab %>%
    dplyr::left_join(cxn.sum, by = dplyr::quo_name(cxn_var))

  # get the total freq of the COLLOCATES
  colloc.sum <- co.occ.tab %>%
    dplyr::group_by(!!coll_var) %>%
    dplyr::summarise(colloc.sum = sum(.data$n))

  # left join the total freq of the COLLOCATES to the data base
  co.occ.tab <- co.occ.tab %>%
    dplyr::left_join(colloc.sum, by = dplyr::quo_name(coll_var))

  # get the total database token/sum of the database
  if (already_count_table == FALSE) {
    co.occ.tab <- co.occ.tab %>%
      dplyr::mutate(dbase.token = dim(df)[1])
  } else {
    co.occ.tab <- co.occ.tab %>%
      dplyr::mutate(dbase.token = sum(.data$n))
  }

  # get the EXPECTED FREQUENCY, EXPECTED PROBABILITY, OBS_EXP DIFFERENCE, and BINOMIAL ALTERNATIVES
  co.occ.tab <- co.occ.tab %>%
    dplyr::mutate(exp = (cxn.sum * colloc.sum)/.data$dbase.token, # exp.freq
                  exp.prob = exp/colloc.sum, # exp.prob
                  obs.exp = '=',
                  obs.exp = dplyr::if_else(n > exp, '>', .data$obs.exp), # obs.exp diff.
                  obs.exp = dplyr::if_else(n < exp, '<', .data$obs.exp), # obs.exp diff.
                  alt = dplyr::if_else(n >= exp, 'greater', 'less')) # obs.exp diff.

  # compute the ONE-TAIL EXACT BINOMIAL TEST and COLLOSTRUCTION STRENGTH VALUE
  co.occ.tab <- co.occ.tab %>%
    dplyr::mutate(p.bin = dplyr::if_else(n >= exp,
                                         list(n, colloc.sum, .data$exp.prob) %>%
                                           purrr::pmap(binom.test, alternative = 'greater') %>%
                                           purrr::map_dbl(., 'p.value'),
                                           list(n, colloc.sum, .data$exp.prob) %>%
                                           purrr::pmap(binom.test, alternative = 'less') %>%
                                           purrr::map_dbl(., 'p.value')),
                  coll.str = dplyr::if_else(n >= exp,
                                            round(-log10(.data$p.bin), collstr_digits), # generate COLL.STR
                                            round(log10(.data$p.bin), collstr_digits)),
                  abs.collstr = abs(.data$coll.str))

  # get the sum of absolute deviation
  co.occ.tab <- co.occ.tab %>%
    dplyr::left_join(co.occ.tab %>%
                       dplyr::group_by(!!coll_var) %>%
                       dplyr::summarise(sum.abs.dev = sum(.data$abs.collstr)) %>% # generate a SUM.ABS.DEV for the COLLOCATES
                dplyr::ungroup(),
              by = dplyr::quo_name(coll_var))

  # get the CxN with the largest deviation
  largest.dev <- co.occ.tab %>%
    split(.[,1]) %>%
    purrr::map(~dplyr::filter(., .$abs.collstr == max(.$abs.collstr))) %>%
    purrr::map_df(~dplyr::ungroup(.) %>%
                    dplyr::select(., dplyr::matches(stringr::str_c(dplyr::quo_name(coll_var), dplyr::quo_name(cxn_var), 'abs.collstr', sep = '|')))) %>%
    dplyr::rename(largest.dev = !!cxn_var)

  # left_join the largest dev. CxN
  #co.occ.tab <- co.occ.tab %>%
  # ungroup() %>%
  #select(-abs.collstr) %>%
  #left_join(largest.dev, by = var_coll)

  # compute HOLM'S ADJUSTED P-VALUE
  co.occ.tab <- co.occ.tab %>%
    dplyr::mutate(p.holm = stats::p.adjust(p = .data$p.bin, method = "holm"), # cf. http://rcompanion.org/rcompanion/f_01.html for example with p.adjust()
                  dec = "ns", # from Gries' (2004) HCFA script
                  dec = dplyr::if_else(.data$p.holm < 0.1, "ms", .data$dec), # from Gries' (2004) HCFA script
                  dec = dplyr::if_else(.data$p.holm < 0.05, "*", .data$dec), # from Gries' (2004) HCFA script
                  dec = dplyr::if_else(.data$p.holm < 0.01, "**", .data$dec), # from Gries' (2004) HCFA script
                  dec = dplyr::if_else(.data$p.holm < 0.001, "***", .data$dec)) # from Gries' (2004) HCFA script

  # outputting the results
  if (concise_output == TRUE) {
    if (correct_holm == FALSE) {
      x <- co.occ.tab %>%
        dplyr::select(!!coll_var, !!cxn_var, !!rlang::sym('n'), !!rlang::sym('exp'), !!rlang::sym('coll.str'), !!rlang::sym('p.bin')) %>%
        dplyr::mutate(p.bin = format(.data$p.bin, digits = 4))
      return(x)
    } else {
      x <- co.occ.tab %>%
        dplyr::select(!!coll_var, !!cxn_var, !!rlang::sym('n'), !!rlang::sym('exp'), !!rlang::sym('coll.str'), !!rlang::sym('p.bin'), !!rlang::sym('p.holm'), !!rlang::sym('dec')) %>%
        dplyr::mutate(p.holm = format(.data$p.holm, digits = 4),
                      p.bin = format(.data$p.bin, digits = 4))
      return(x)
    }
  } else {
    return(co.occ.tab)
  }
}
