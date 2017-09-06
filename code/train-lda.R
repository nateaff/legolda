
# Cross validate model on sequence of topics
# Perplexity is used as a measure of model fitness
# The LDA model fitting is compute intensive.
# Try training on a sample to estimate compute times

# library(purrr)
# library(tidytext)
# library(topicmodels)

## @knitr setup

library(dplyr)
library(ggplot2)
library(tidytext)
library(parallel)
library(topicmodels)
# library(purrr)
from_cache <- TRUE

legolda::load_data(sample_data = FALSE)
legolda::tidy_colorsets()

# saveRDS(dtm, "dtm.RDS")

## @knitr k-fold-cv

if (!from_cache) {
  # A test on a sample showed better performance in the 30-60 range
  n_topics <- c(10, 20, seq(30, 60, by = 5), 75, 100)

  # Create train and test sets
  dtm5 <- kfold(dtm, 5)
  cv5_models <- list()

  # Train models using default controls settings
  # VEM with estimated alpha
  for (k in seq_along(n_topics)) {
    cat("Topic no. ", k, "\n")
    cv5_models[[k]] <- dtm5 %>%
      mutate(
        model = mclapply(
          train, LDA,
          control = list(seed = 1, verbose = 1),
          k = n_topics[k]
        ),
        mc.cores = (detectCores() - 1)
      ) %>%
      mutate(perplexity = purrr::map2(model, test, perplexity))
  }
  # Create tidy dataframe of results
  perplexities <- cv5_models %>%
    purrr::map_df("perplexity") %>%
    mutate(n_topic = n_topics) %>%
    tidyr::gather(key = fold, value = perplexity, -n_topic)


  #  saveRDS(perplexities, here::here("inst", "data", "perplexity_all.RDS"))
  #  saveRDS(cv5_models, here::here("inst", "data", "models_all2.RDS"))
} # end if

## @knitr load-results
perplexities <- readRDS(here::here("inst", "data", "perplexity_all.RDS"))


## @knitr cv-result-plot
perplexities %>%
  ggplot(aes(n_topic, perplexity)) +
  geom_point(aes(colour = fold), size = 2) +
  geom_line(aes(n_topic, perplexity, group = fold, colour = fold)) +
  scale_color_manual(values = pal21(), guide = guide_legend(title = "Folds")) +
  geom_smooth(se = FALSE, colour = "#2f2f2a") +
  scale_x_continuous(breaks = perplexities$n_topic) +
  labs(
    x = "Number of topics", y = "Perplexity",
    title = "Perplexities scores for LDA models",
    subtitle = "Perplexity (lower is better) of holdout sets for 5-fold cross-validation"
  ) +
  legolda::theme_scatter(bgcol = "#f8f8f8") 


## @knitr lda-models
ntopics <- c(20, 30, 35, 40, 50, 75, 100)
# Train LDA models on full data set
if (!from_cache) {
  lda_models <- c(20, 30, 35, 40, 50, 75, 100) %>%
    purrr::map(LDA, x = dtm, control = list(seed = 1))
}

## @knitr other-evaluation

lda_models <- readRDS("~/devel/R-proj/lego-lda/inst/data/lda_models_all.RDS")
lda_metrics <- legolda::score_models(lda_models, dtm, topics = ntopics)

plot_lda_scores(lda_metrics, title)
