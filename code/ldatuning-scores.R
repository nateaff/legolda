# File loads and scores lda models using the lda_tuning package

## @knitr ldatuning-scores

lda_models <- readRDS(here::here("inst", "data", "lda_models_all.RDS"))
lda_metrics <- legolda::score_models(lda_models, dtm, topics = ntopics)

plot_lda_scores(lda_metrics, title)
