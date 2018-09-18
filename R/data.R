#' The main metaphor data frame
#'
#' This data is used for the \emph{token frequency}, \emph{type frequency} and \emph{type-per-token ratio} analyses in Rajeg (2018, Ch. 5 and Ch. 6), and for the \emph{Multiple Distinctive Collexeme Analysis} in Rajeg (2018, Ch. 7). The data consists of the following variables:
#' \describe{
#' \item{corpus}{Name of the corpus files}
#' \item{sentence_id}{Character strings of the sentence id (or line id for \emph{WebCorp} and \emph{Sketch Engine} data) in which the happiness synonyms are found in the corpus}
#' \item{synonyms}{Character strings of the synonyms}
#' \item{m_expr}{Metaphorical expressions containing the synonyms, that is, the metaphorical patterns}
#' \item{m_expr_gloss}{English gloss of the metaphorical patterns}
#' \item{lu}{Source-frame lexical units (henceforth, LUs) in the metaphorical patterns}
#' \item{lu_gloss}{English gloss of the source-frame LUs}
#' \item{lu_phrase_type}{Syntactic category of the source-frame LUs (e.g., verb, adjective)}
#' \item{voice_verbal_lu}{Grammatical voice of the verbal, source-frame LUs in the metaphorical patterns}
#' \item{source_frames}{The source frame categories evoked by the source-frame LUs in the metaphorical patterns}
#' \item{metaphors}{Conceptual metaphors evoked by the metaphorical patterns. This is based on grouping similar frame-role-mapping of the target-frame words in the source-frames}
#' \item{submappings}{Submappings of the metaphors}
#' \item{body_part_inclusion}{Character strings indicating whether the metaphorical patterns include/mention body-part terms (\code{"y"} for 'yes'), person (\code{"person"}), others type (\code{"others"}), or none (\code{"NA"})}
#' \item{body_part_terms}{Character strings of the body-part terms}
#' \item{sentence_match}{The sentence-match from which the metaphorical patterns of the synonyms are extracted}
#' }
#' @references
#' Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia.
"phd_data_metaphor"

#' Gloss for the distinctive collocates
#'
#' A tibble data frame containing gloss for the distinctive collocates of the synonyms presented in Rajeg (2018, Ch.7).
#' @references
#' Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia.
"dist_colloc_gloss"

#' Distinctive collocates data
#'
#' The input data for distinctive collocates analysis in Rajeg (2018, Ch.7). The collocates data are words co-occurring within the span of 4 words to the right and left of the happiness near-synonyms. Following Gevaert (2007, p. 197), the collocates that are part of the studied synonyms are excluded because they may neutralise the distinctive features of the near-synonyms when the goal is to find semantic differences between these near-synonyms.
#'
#' The data consists of the following two variables:
#' \describe{
#' \item{synonyms}{The ten near-synonyms under studied. Note that only results of the nominalised, \emph{ke- -an} forms are presented in the thesis, but not the root forms.}
#' \item{collocates}{The window-span collocates of the synonyms}
#' }
#' @references
#' \itemize{
#' \item{Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS} (PhD Thesis). Monash University. Melbourne, Australia.}
#' \item{Gevaert, C. (2007). \emph{The history of ANGER: The lexical field of ANGER from Old to Early Modern English} (PhD thesis). Katholieke Universiteit Leuven, Leuven.
#'}
#'}
#'
"colloc_input_data"

#' Interrater data for conceptual metaphors
#'
#' The input data for interrater agreement trial for the conceptual metaphors.
#' The data include classifications in the pre-discussion and post discussion rounds.
"df_cm"

#' Interrater data for metaphorical usages of the pattern (1st round)
#'
#' The input data for interrater agreement trial for determining whether the constructional patterns of the happiness words are metaphorical or not.
#' The data include classifications in the pre-discussion round.
"df_meta_use_1st"

#' Interrater data for metaphorical usages of the pattern (2nd round)
#'
#' The input data for interrater agreement trial for determining whether the constructional patterns of the happiness words are metaphorical or not.
#' The data include classifications in the post-discussion round.
"df_meta_use_2nd"

#' Interrater data for the constructional patterns
#'
#' The input data for interrater agreement trial for determining the constructional patterns of the happiness words in their concordance lines.
#' The data include classifications in the pre-discussion round only since we already arrived at a nearly perfect agreement before the discussion.
"df_cxn_pattern"

#' Interrater data for the top constructional patterns
#'
#' The input data for \emph{Figure 3-1} in Rajeg (2018).
#' The barplot shows the distribution of constructional patterns during interrater agreement trial whose frequency of occurrences are at least five (5).
#' @references Rajeg, G. P. W. (2018). \emph{Metaphorical profiles and near-synonyms: A corpus-based study of Indonesian words for HAPPINESS}. PhD Thesis. Monash University. Melbourne, Australia.
"top_cxn_data"




