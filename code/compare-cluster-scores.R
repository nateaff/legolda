# Cluster scoring functions are in \cdoe{cluster-scores.R}

# Computes kmeans clusters for each topic number
# Computes clusters for LDA models using a hard cut-off on topic distribution
# Uses several cluster scoring methods from the \{clues} package to compare
# cluster scors for kmeans and LDA clusters to the parent_id of each set

## @knitr plot-external-scores

n_topics <- legolda::get_topic_numbers(lda_models)

# Label sets by top topic probability
lda_clust <- legolda::get_lda_clusters(lda_models)

# Score using clues::adjustedRand
lda_score <- legolda::score_lda(lda_clust, total_words)
kmeans_score <- legolda::score_kmeans(n_topics, from_cache = FALSE)

# Combine
rand_scores <- rbind(kmeans_score, lda_score)
rand_scores$type <- rep(c("Kmeans", "LDA"), each = 5*length(n_topics))

rand_scores %>%
  ggplot(aes(x = k, y = score)) +
  geom_point(aes(color = metric), size = 1.5) +
  geom_line(aes(color = metric)) +
  scale_color_manual(values = pal21(), guide = guide_legend(title = "Score")) +
  scale_x_continuous(breaks = unique(kmeans_score$k)) +
  labs(
    x = "Number of topics",
    y = "Score",
    title = "Cluster scores based existing labels",
    subtitle = paste0("Scores (higher is better) based on",
     "comparison to each set's parent theme id")
  ) +
  theme_scatter(bgcol = "#f8f8f8") +
  facet_wrap(~type)