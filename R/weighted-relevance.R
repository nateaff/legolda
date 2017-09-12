# Functions for computing relevance and top terms of 
# an LDA model based on the relevance scores
 
#' Compute Weighted Sum of Vectors
#'
#' @param x A vector
#' @param y A vector
#' @return A vector 
#' @export
weight <- function(x, y, wt) {
  if (wt > 1 || wt < 0) stop("wt should be a valid probability")
  (wt * x) + (1 - wt) * (x / y)
}

# Takes a model output by topicmodels::LDA and
# returns the top nterms based on a weighted relevance score
 
#' Compute the Top Terms of an LDA Model
#'
#' Computes a relevance score using the weighted sum 
#' of the document term score and corpus term freqeuncy.
#'
#' @param model An LDA model produced by the topicmodels package.
#' @param lambda A float. Percent to weight each term.
#' @param nterms An integer. The number of top terms to return.
#' @param freq A table of word per document frequency counts. (TODO)
#' @return A tibble of the top 'nterms' terms for each topic.
#' @export
top_terms <- function(model, lambda, nterms, freq) {
  
  # Coerce for tidytext
  if(class(model) == "LDA_VEM"){
    class(model) <- "LDA"
  }

  topics <- tidytext::tidy(model, matrix = "beta") %>%
    right_join(freq, by = c("term" = "rgba")) %>%
    mutate(relevance = legolda::weight(beta, percent, lambda)) %>%
    arrange(topic, desc(relevance))

  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(nterms, relevance) %>%
    ungroup() %>%
    mutate(topic_name = paste0("Topic ", topic)) %>%
    arrange(topic, -relevance) %>%
    # Keep colors sorted for faceting
    mutate(order = row_number())

  top_terms
}
