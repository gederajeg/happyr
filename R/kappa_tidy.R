#' Calculate Kappas for interrater agreement trial
#'
#' @description The function performs Kappas for interrater agreement between two-raters, the author of the thesis and one other coder.
#'     The calculation is based on the \code{\link[irr]{kappa2}} function from the \emph{irr} package (Gamer et al, 2012).
#'     All scripts to use this function to perform the calculation for each interrater agreement trial in Rajeg (2019, Chapter 3) are shown in the \strong{Examples} section below.
#' @param df The input data frame for each interrater trial tasks
#' @param var_names character strings of regular expressions for the columns involved in the calculation of the Kappa score (i.e. the interrater agreement score).
#' @param split_by by default it is specified by "synonyms" since Kappas are computed for classification for data per synonyms included in the trial.
#' @param round character strings indicating whether the Kappas are for the "pre-discussion" or "post-discussion" round.
#'
#' @return A tibble/data frame
#'
#' @importFrom tibble enframe
#' @importFrom dplyr matches
#' @importFrom dplyr select
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom purrr map
#' @importFrom irr kappa2
#' @export
#' @references
#'     \itemize{
#'         \item Gamer, M., Lemon, J., & Singh, P. (2012). irr: Various Coefficients of Interrater Reliability and Agreement. R package version 0.84.
#'         \item Rajeg, G. P. W. (2019). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia. \url{https://doi.org/10.26180/5cac231a97fb1}.
#'     }
#' @examples
#'
#' # kappa calculation for the constructional patterns
#' ## pre-discussion
#' irr_cxn <- kappa_tidy(df = df_cxn_pattern,
#'                       var_names = "^pattern",
#'                       round = "pre_disc")
#' irr_cxn # check the output format
#' mean(irr_cxn[["kappa"]]) # mean
#' sd(irr_cxn[["kappa"]]) # SD
#'
#'
#' # kappa calculation for the metaphoricity
#' ## pre-discussion
#' irr_use_1 <- kappa_tidy(df = df_meta_use_1st,
#'                        var_names = "^use_(coder|author)$",
#'                        round = "pre_disc")
#' mean(irr_use_1[["kappa"]]) # mean
#' sd(irr_use_1[["kappa"]]) # SD
#'
#' ## post-discussion
#' irr_use_2 <- kappa_tidy(df = df_meta_use_2nd,
#'                         var_names = "^use_2nd_",
#'                         round = "post_disc")
#' mean(irr_use_2[["kappa"]]) # mean
#' sd(irr_use_2[["kappa"]]) # SD
#'
#' # interrater agreement for the conceptual metaphors
#' ## pre-discussion
#' irr_cm_1 <- kappa_tidy(df = df_cm,
#'                        var_names = "_pre$",
#'                        round = "pre_disc")
#' mean(irr_cm_1[["kappa"]]) # mean
#' sd(irr_cm_1[["kappa"]]) # SD
#'
#' ## post-discussion
#' irr_cm_2 <- kappa_tidy(df = df_cm,
#'                        var_names = "_post$",
#'                        round = "post_disc")
#' mean(irr_cm_2[["kappa"]]) # mean
#' sd(irr_cm_2[["kappa"]]) # SD
#'
kappa_tidy <- function(
  df = NULL,
  var_names = "pattern_(coder|author)",
  split_by = "synonyms",
  round = "pre_disc") {

  # codes for processing the kappa values per synonyms
  df_split <- split(df, df[[split_by]])
  df_split <- purrr::map(df_split, function(df) dplyr::select(df, dplyr::matches(var_names)))
  irr_res <- purrr::map(df_split, function(df) irr::kappa2(df))
  irr_res <- tibble::enframe(irr_res)

  # get the list results into a tidy data frame
  kappa <- purrr::map_dbl(irr_res$value, function(x) x$value)
  z_score <- purrr::map_dbl(irr_res$value, function(x) x$statistic)
  cases <- purrr::map_int(irr_res$value, function(x) x$subjects)
  rater <- purrr::map_int(irr_res$value, function(x) x$raters)
  pvalue <- purrr::map_dbl(irr_res$value, function(x) x$p.value)
  words <- irr_res$name
  df_out <- tibble::tibble(words,
                           rater,
                           cases,
                           kappa,
                           round = round)
  return(df_out)

} # end of `function()`
