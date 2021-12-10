#' Perform the frequency profiles of metaphors in Rajeg (2019, Chapter 5 and Chapter 6)
#'
#' @description Generate a set of basic frequency profiles of (i) token frequency, (ii) type frequency, and (iii) type per token ratio (TTR) of a particular schema (e.g., conceptual metaphor as a conceptual schema, or word-formation pattern as a morphological, constructional schema).
#' @param df the raw data frame containing the schemas and their linguistic instantiations (e.g., the \code{phd_data_metaphor.rda} in the case of the thesis).
#' @param schema_var character string of the column name for the schema variable in the data frame (i.e., \code{"metaphors"} in the \code{phd_data_metaphor.rda}).
#' @param lexunit_var character string of the column name for the lexical unit variable realising the schema in the data frame (i.e., \code{"lu"} in the \code{phd_data_metaphor.rda}).
#' @param float_digits integer indicating the retained floating points from the calculation. The default is \code{2}.
#' @return A tibble data frame (\code{tbl_df}) sorted in decreasing order of the \code{token} frequency of the schemas.
#' @details As mentioned above, the \code{ttr()} function can be extended beyond its use for the thesis in Rajeg (2019). It can be used to generate the three frequency profiles of a set of morphological constructional schemas in relation to their manifesting words. For instance, we can use \code{ttr()} to determine the type and type/token ratio of two word-formation patterns, contrasting their productivity. This can be done as long as the input \code{df} contains raw data with two columns: one representing the morphological schema and the other one representing the linguistic instantiations of each of the schema.
#' @examples
#' ttr_metaphor <- ttr(df = phd_data_metaphor,
#'                     schema_var = "metaphors",
#'                     lexunit_var = "lu",
#'                     float_digits = 2)
#' @references Rajeg, G. P. W. (2019). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS}. PhD Thesis. Monash University. Melbourne, Australia. \url{https://doi.org/10.26180/5cac231a97fb1}.
#' @importFrom dplyr enquo
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom stringr str_to_lower
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @export
ttr <- function(df = NULL, schema_var = "metaphors", lexunit_var = "lu", float_digits = 2) {

  assertthat::assert_that(!is.null(df), msg = "The `df` argument is NULL; please specify it with `phd_data_metaphor`!")

  schema_q <- rlang::sym(schema_var)
  lu_q <- rlang::sym(lexunit_var)

  df_out <- df %>%
    dplyr::group_by(!!schema_q) %>%
    dplyr::summarise(token = n(),
                     type_lu = dplyr::n_distinct(!!lu_q))
  df_out <- dplyr::ungroup(df_out) %>%
    dplyr::mutate(perc_token = round(.data$token/sum(.data$token) * 100, float_digits),
                  perc_type_lu = round(.data$type_lu/sum(.data$type_lu) * 100, float_digits),
                  type_per_token_lu = round((.data$type_lu/.data$token) * 100, float_digits)) %>%
    dplyr::arrange(dplyr::desc(.data$token))

  colnames(df_out) <- stringr::str_to_lower(colnames(df_out))
  return(df_out)
}
