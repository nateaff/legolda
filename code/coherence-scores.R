# Compute topic coherence using topic_coherence 
# function from SpeedReader package

library(SpeedReader)
library(purrr)
library(tidytext)
library(legolda)

lda_models <- get_lda_models()
legolda::create_tables()

# Total frequency used in relevance score
freq <- set_colors %>%
  count(rgba) %>%
  mutate(percent = n / nrow(set_colors))

# Parameters for looking at a particular model
model <- lda_models[[1]]
# Weighted relevance as used in LDAvis package
lambda <- 0.7
top_num = 10

coherence <- function(model, nterms, dtmat, lambda = 0.7, freq) {
  top_terms <- top_terms(model, lambda, nterms, freq)
  terms <- attr(dtmat, "dimnames")$Terms
  coherence <- SpeedReader::topic_coherence(top_terms$term, dtmat, vocabulary = terms)
  unlist(coherence)
}
# A test
# top_terms <- top_terms(model, lambda, top_num, freq)

# topic_coherence takes a matrix
dtmat <- as.matrix(dtm)

coh_tbl <- tibble(model = lda_models) %>% 
mutate(ntopics = legolda::get_topic_numbers(model)) %>%
mutate("3" = purrr::map(model, coherence, nterms = 3, dtmat = dtmat, freq =freq)) %>%
mutate("5" = purrr::map(model, coherence, nterms = 5, dtmat = dtmat,  freq = freq)) %>%
mutate("10" = purrr::map(model, coherence, nterms = 10, dtmat = dtmat,  freq = freq))

# TODO: finish fixing
test <- coh_tbl %>% 
select(-model) 
# tidyr::unnest(ntopics)

# Use cached version
from_cache <- TRUE

# This is a moderately long script
if (!from_cache) {
  dtmat <- as.matrix(dtm)

coh_tbl <- tibble(model = lda_models) %>% 
mutate(ntopics = legolda::get_topic_numbers(model)) %>%
mutate("3" = purrr::map(model, coherence, nterms = 3, dtmat = dtmat, freq =freq)) %>%
mutate("5" = purrr::map(model, coherence, nterms = 5, dtmat = dtmat,  freq = freq)) %>%
mutate("10" = purrr::map(model, coherence, nterms = 10,dtmat = dtmat,  freq = freq))

coh_tbl <- coh_tbl[,-1] %>% tidyr::unnest %>% tidyr::gather 

  # ldas <- list(k = map(lda_models, ~.x@k))
  # # 
  # coh10 <- lda_models %>%
  #   purrr::map(coherence, dtmat = dtmat, nterms = 10, freq)
  # coh5 <- lda_models %>%
  #   purrr::map(coherence, dtmat = dtmat, nterms = 5, freq)
  # coh3 <- lda_models %>%
  #   purrr::map(coherence, dtmat = dtmat, nterms = 3, freq)

  # coh <- list(coh3, coh5, coh10) %>%
  #   lapply(., unlist) %>%
  #   do.call(cbind, .) %>%
  #   data.frame()

  # coh$ntopics <- unlist(ldas)

  # names(coh) <- c("3", "5", "10", "ntopics")
  # coh_tbl <- tidyr::gather(coh, key = topn, value = coherence, -ntopics)
  saveRDS(coh_tbl, here::here("inst", "data", "coherence.RDS"))
} # end if

# @knitr coherence-score

coh_tbl <- readRDS(here::here("inst", "data", "coherence.RDS"))
coh_tbl$topn <- forcats::fct_inorder(coh_tbl$topn)
# TODO: sort number of terms in order

coh_tbl %>%
  ggplot(aes(x = ntopics, y = coherence, group = topn)) +
  geom_point(aes(colour = topn, group = topn), size = 2) +
  geom_line(aes(color = topn)) +
  scale_color_manual(values = pal21(), guide = guide_legend(title = "Number of terms")) +
  scale_x_continuous(breaks = coh_tbl$ntopics) +
  labs(
    x = "Number of topics", y = "Coherence",
    title = "Term coherence of LDA models",
    subtitle = "Coherence scores (higher is better) for top 3, 5 and 10 terms"
  ) +
  theme_scatter(bgcol= "#f8f8f8")
