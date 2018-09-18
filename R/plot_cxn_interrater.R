#' Top constructional patterns
#'
#' @description A function to generate Figure 3.1 in Rajeg (2018).
#'     The figure shows the proportion of the most frequent
#'     constructional patterns agreed during the interrater agreement trial.
#' @param df data frame for the plot included in the package, that is \code{top_cxn_data}.
#'
#' @return A ggplot image
#' @export
#'
#' @examples
#' plot_cxn_interrater(df = top_cxn_data)
#'
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_grey
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 position_fill
#' @importFrom stats reorder
#'
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS}. PhD Thesis. Monash University. Melbourne, Australia.

plot_cxn_interrater <- function(df = NULL) {
  synonyms <- dplyr::quo(synonyms)
  cxn_pattern <- dplyr::quo(cxn_pattern)
    ggplot2::ggplot(data = df,
                    ggplot2::aes(x = synonyms,
                                 y = n,
                                 fill = stats::reorder(cxn_pattern, -n),
                                 group = stats::reorder(cxn_pattern, -n))) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Synonyms",
                  fill = "Constructional patterns",
                  y = "Proportion") +
    ggplot2::scale_fill_grey(start = 0.1, end = 0.95) +
    ggplot2::geom_text(ggplot2::aes(label = n),
                       position = ggplot2::position_fill(0.9),
                       colour = rep(c('black', 'black', 'black', 'white'), 3),
                       vjust = 1.5,
                       size = 2.35)
}
