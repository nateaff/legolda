
## @knitr kmeans-compute-score
library(ggplot2)
library(dplyr)
library(waffle)
library(tm)
library(tidytext)
library(parallel)

from_cache <- TRUE

if (!from_cache) {
  legolda::load_data(sample_data = FALSE)
  legolda::tidy_colorsets()

  n_topics <- c(20, 30, 40, 40, 50, 60, 75, 100)

  tf_idf <- tm::weightTfIdf(m = dtm, normalize = TRUE)
  tf_idf_mat <- as.matrix(tf_idf)

  tf_idf_norm <- tf_idf_mat / apply(tf_idf_mat, 1, function(x) sum(x ^ 2) ^ 0.5)

  kmclust <- n_topics %>%
    mclapply(
      ., function(x) kmeans(
        tf_idf_norm,
        centers = x,
        iter.max = 50
      ),
      mc.cores = (detectCores() - 1)
    ) # %>% tidy

  kmdf <- tibble(k = list(n_topics), model = kmclust)
  kmtidy <- kmdf %>%
    mutate(km = purrr:::map(model, broom::tidy))


  # Update where root_id is added to theme_df
  root_ids <- theme_df %>%
    mutate(root_id = ifelse(!is.na(parent_id), parent_id, id)) %>%
    select(id, root_id) %>%
    right_join(sets_df, by = c("id" = "theme_id")) %>%
    right_join(total_words) %>%
    select(root_id)


  km_rand <- apply(
    kmtidy, 1,
    function(x) clues::adjustedRand(x$model$cluster, root_ids$root_id)
  ) %>%
    t() %>%
    data.frame()

  km_rand$k <- n_topics

  km_rand <- km_rand %>%
    tidyr::gather(key = Score, value = score, -k)
  saveRDS(km_rand, here::here("inst", "data", "kmeans_scores.RDS"))
} # end if

## @knitr lda-compute-external-score

# Cluster based on LDA
library(topicmodels)
library(ggplot2)
library(dplyr)
library(waffle)

lda_models <- readRDS(here::here("inst", "data", "lda_models_all.RDS"))
length(lda_models)

top_cluster <- function(x) posterior(x)$topics %>%
  apply(., 1, which.max)

lda_clust <- lda_models %>%
  lapply(., top_cluster)

lda_rand <- lda_clust %>%
  lapply(., function(x) clues::adjustedRand(x, root_ids$root_id)) %>%
  do.call(rbind, .) %>%
  data.frame()

lda_rand$k <- n_topics
lda_rand_score <- lda_rand %>%
  tidyr::gather(key = Score, value = score, -k)

saveRDS(lda_rand_score, here::here("inst", "data", "lda_rand_score.RDS"))
saveRDS(lda_clust, here::here("inst", "data", "lda_clusters.RDS"))

## @knitr plot-external-scores
library(ggplot2)
library(dplyr)

km_rand <- readRDS(here::here("inst", "data", "kmeans_scores.RDS"))
lda_rand_score <- readRDS(here::here("inst", "data", "lda_rand_score.RDS"))

rand_scores <- rbind(km_rand, lda_rand_score)
rand_scores$type <- rep(c("Kmeans", "LDA"), each = 35)

rand_scores %>%
  ggplot(aes(x = k, y = score)) +
  geom_point(aes(color = metric), size = 1.5) +
  geom_line(aes(color = metric)) +
  scale_color_manual(values = pal21(), guide = guide_legend(title = "Score")) +
  scale_x_continuous(breaks = unique(km_rand$k)) +
  labs(
    x = "Number of topics",
    y = "Score",
    title = "Cluster scores based existing labels",
    subtitle = paste0("Scores (higher is better) based on",
     "comparison to each set's parent theme id")
  ) +
  theme_scatter(bgcol = "#f8f8f8") +
  facet_wrap(~type)

## @knitr plot-cluster

# The remaining files rely on this one 
# TODO: Move this set of plots elsewhere
library(forcats)
library(purrr)
library(legolda)

if(!exists("set_colors")){
  load_data(sample_data = FALSE)
}

lda_models <- readRDS(here::here("inst", "data", "lda_models_all.RDS"))

model_num = 3
# Get top 2 colors for each topic
lda_models <- lda_models %>%
  purrr::map(function(x) {
    class(x) <- "LDA"
    x
  }) 

# Total frequency used in relevance score
word_freq <- set_colors %>%
  count(rgba) %>%
  mutate(percent = n / nrow(set_colors))

lambda = 0.5
nterms = 2 

top_colors <- top_terms(lda_models[[model_num]], lambda, nterms, word_freq) %>% 
  mutate(topic_name = forcats::fct_inorder(factor(topic_name)))

# Blend two hex colors  
blend <- function(df, scale = 100){ 
  pos <- df$beta[2] * scale
  colorRampPalette(c(df$term[1], df$term[2]))(100)[2]
}

# Generate palette that is a blend of the top two  
topic_pal <- top_colors %>% split(.$topic) %>% 
  purrr::map(blend) %>% unlist

vec_2_function(topic_pal)
devtools::load_all()


## @knitr plot-topic-distribution 
library(legolda)
library(forcats)
library(dplyr)
library(ggplot2) 

model_num = 3

# Assumes topic_pal is available
lda_clust <- readRDS(here::here("inst", "data", "lda_clusters.RDS"))

# Model with 30 clusters
set_clust <- tibble(set_num = names(lda_clust[[model_num]]), 
              topic_id = lda_clust[[model_num]]) %>% 
            left_join(sets_df) %>% 
            arrange(topic_id) %>%  
            # mutate(topic_name = fct_inorder(factor(paste0("Topic ", topic_id)))) %>%
            mutate(topic_id = fct_inorder(factor(topic_id)))

topic_pal <- topic_palette()

# Get count per topic
set_clust <- set_clust %>% 
count(topic_id) %>%
arrange(n) %>%
mutate(order = row_number())

gg <- set_clust %>% ggplot(aes(x = topic_id, y = n, 
          fill = topic_id, 
          group = topic_id)) 
gg <- gg + geom_col(size = 0.8)  
# scale_color_manual(values = topic_pal) +
gg <- gg + scale_fill_manual(values = topic_pal) 
# gg <- gg + scale_x_continuous(breaks = set_clust$topic_id )  
gg <- gg + labs(x = "Topic number", 
                y = "Sets in topic", 
                title = "Color topic frequency") 
gg <- gg + theme_bar(bgcol = "#c8c6c4") 
# gg <- gg + coord_flip() 
gg <- gg + theme(legend.position = "none") 
gg <- gg + theme(panel.grid.major.x = element_blank())     
gg <- gg + theme(panel.grid.major.y = element_line(color = "#f8f6f4", size = 0.2))
gg

## @knitr plot-topic-timeline
model_num = 3

# Assumes topic_pal is available
lda_clust <- readRDS(here::here("inst", "data", "lda_clusters.RDS"))

# Model with 30 clusters
set_clust <- tibble(set_num = names(lda_clust[[model_num]]), 
              topic_id = lda_clust[[model_num]]) %>% 
            left_join(sets_df) %>% 
            arrange(topic_id) %>%  
            # mutate(topic_name = fct_inorder(factor(paste0("Topic ", topic_id)))) %>%
            mutate(topic_id = fct_inorder(factor(topic_id)))

topic_pal <- topic_palette()

# Get count by year and topic
set_clust %>% 
group_by(topic_id, year) %>% 
count(topic_id) %>% 
ggplot(aes(x = year, y = n, 
    group = topic_id, 
    color = topic_id)) +
geom_line(aes(color = factor(topic_id)), size = 0.8) + 
geom_area(aes(fill = topic_id, alpha = 1)) +   
scale_color_manual(values = topic_pal) +
scale_fill_manual(values = topic_pal) + 
labs(x = "Year set released", 
  y = "Sets in topic", 
  title = "Number of Lego sets per color topic, 1954-2017") +
facet_wrap(~topic_id, nrow = 8) +
theme_scatter(bgcol = "#c8c6c4", grid_col = "#f8f6f4") + 
theme(legend.position = "none")



