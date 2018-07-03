#' Plot most frequent body-part terms
#'
#' @description A function to call the script to generate the Body-part barplot in Rajeg (2018, Chapter 5).
#' @param df the data frame for the thesis (\code{phd_data_metaphor}).
#'
#' @return Barplot based on \code{ggplot2} package
#' @export
#'
#' @examples
#' plot_body_part(df = phd_data_metaphor)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_text
#' @importFrom stats reorder
#' @importFrom dplyr %>%
#' @importFrom dplyr quo
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr quo_name
#' @importFrom stringr str_c
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for happiness} (PhD Thesis). Monash University. Melbourne, Australia.
plot_body_part <- function(df = NULL) {
  bp_gloss <- tibble::tibble(gloss = c('chest/bosom', 'self', 'liver', 'eyes', 'face; lit. front', 'body', 'face', 'face; lit. surface of a sandstone', 'deepest part of the heart', 'lips', 'mouth', 'body; bodily'),
                             body_part_terms = c('dada', 'diri', 'hati', 'mata', 'muka', 'tubuh', 'wajah', 'paras', 'lubuk kalbu', 'bibir', 'mulut', 'jasmani'))

  body_part_inclusion <- dplyr::quo(body_part_inclusion)
  body_part_terms <- dplyr::quo(body_part_terms)
  body_parts <- dplyr::quo(body_parts)
  gloss <- dplyr::quo(gloss)
  bdy_parts_df <- df %>%
    dplyr::filter(!!body_part_inclusion %in% c('y')) %>%
    dplyr::count(!!body_part_terms, sort = TRUE) %>%
    dplyr::left_join(bp_gloss, by = 'body_part_terms') %>%
    dplyr::mutate(!!dplyr::quo_name(body_parts) := stringr::str_c(.data$body_part_terms, "__'", .data$gloss, "'", sep = ""))
  bdy_parts_df %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(body_parts, n), y = n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = 'Body-part terms', y = 'Token frequency') +
    #axis_theme +
    ggplot2::geom_text(ggplot2::aes(label = n),
                      hjust = dplyr::if_else(bdy_parts_df$n > 4, 1.25, -0.3),
                      colour = dplyr::if_else(bdy_parts_df$n > 4, "white", "black"))
}
