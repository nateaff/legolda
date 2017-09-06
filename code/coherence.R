# Test out measure of coherence from the
# SpeedReader package
library(SpeedReader)
library(purrr)
library(tidytext)
# library()

library(legolda)

lda_models <- readRDS("~/devel/R-proj/lego-lda/inst/data/lda_models_all.RDS"))

set_topics <- lda_models %>%
  map(function(x) {
    class(x) <- "LDA"; x
  }) %>%
  map(tidytext::tidy, matrix = "gamma")

legolda::load_data(sample_data = FALSE)
legolda::tidy_colorsets()

# Total frequency used in relevance score
freq <- set_colors %>%
  count(rgba) %>%
  mutate(percent = n / nrow(set_colors))

# Parameters for looking at a particular model
model <- lda_models[[1]]
lambda <- 0.7
topn <- 10

top_terms <- top_terms(model, lambda, topn)

coherence <- function(model, nterms, dtmat, lambda = 0.7, freq) {
  top_terms <- top_terms(model, lambda, nterms, freq)
  terms <- attr(dtmat, "dimnames")$Terms
  coherence <- SpeedReader::topic_coherence(top_terms$term, dtmat, vocabulary = terms)
  unlist(coherence)
}

# Error Freq
from_cache <- TRUE
if (!from_cache) {
  # ldas <- tibble(models = lda_models, k = map(lda_models, function(x) x@k))
  ldas <- list(k = map(lda_models, ~.x@k))

  coh10 <- lda_models %>%
    purrr::map(coherence, dtmat = dtmat, nterms = 10, freq)
  coh5 <- lda_models %>%
    purrr::map(coherence, dtmat = dtmat, nterms = 5, freq)
  coh3 <- lda_models %>%
    purrr::map(coherence, dtmat = dtmat, nterms = 3, freq)

  coh <- list(coh3, coh5, coh10) %>%
    lapply(., unlist) %>%
    do.call(cbind, .) %>%
    data.frame()

  coh$ntopics <- unlist(ldas)

  names(coh) <- c("3", "5", "10", "ntopics")
  cohdf <- tidyr::gather(coh, key = topn, value = coherence, -ntopics)
  saveRDS(cohdf, here::here("inst", "data", "coherence.RDS"))
} # end if

# @knitr coherence-score

cohdf <- readRDS(here::here("inst", "data", "coherence.RDS"))
cohdf$topn <- forcats::fct_inorder(cohdf$topn)
# TODO: sort number of terms in order

cohdf %>%
  ggplot(aes(x = ntopics, y = coherence, group = topn)) +
  geom_point(aes(colour = topn, group = topn), size = 2) +
  geom_line(aes(color = topn)) +
  scale_color_manual(values = pal21(), guide = guide_legend(title = "Number of terms")) +
  scale_x_continuous(breaks = cohdf$ntopics) +
  labs(
    x = "Number of topics", y = "Coherence",
    title = "Term coherence of LDA models",
    subtitle = "Coherence scores (higher is better) for top 3, 5 and 10 terms"
  ) +
  theme_scatter(bgcol= "#f8f8f8")
