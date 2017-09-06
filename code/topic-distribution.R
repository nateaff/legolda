# Example from http://tidytextmining.com/nasa.html
# Look at how well sets are assigned to topics

## @knitr load-distributions
library(dplyr)
library(tidytext)
library(purrr)

lda_models <- readRDS("~/devel/R-proj/lego-lda/inst/data/lda_models_all.RDS")
n_topics <- c(20, 30, 40, 50, 60, 75, 100)

gdf <- lda_models %>%
  map(function(x) {
    class(x) <- "LDA"
    x
  }) %>%
  map(tidytext::tidy, matrix = "gamma")

for (k in 1:length(n_topics)) {
  gdf[[k]]$n_topic <- rep(n_topics[[k]], nrow(gdf[[k]]))
}

gdf <- do.call(rbind, gdf)

## @knitr plot-all-topics
gdf %>%
  ggplot(aes(gamma)) +
  geom_histogram(aes(fill = factor(n_topic))) +
  scale_y_log10() +
  scale_fill_manual(values = pal21()) +
  labs(
    title = "Distribution of probabilities for all topics",
    y = "Number of documents", x = expression(gamma)
  ) +
  facet_wrap(~n_topic) +
  legolda::theme_bar(bgcol = "#f8f8f8")

## @knitr plot-30-topics

# For a set number of topics, what is the distribution
# of documents over that topic
topic_n <- 40
pal <- colorRampPalette(pal21())(topic_n)


gdf %>%
  filter(n_topic == topic_n & log(gamma, 10) > -Inf) %>%
  ggplot() +
  geom_histogram(aes(gamma, fill = factor(topic))) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = c(.25, 0.5, .75)) + 
  scale_y_log10() +
  labs(
    title = paste0("Topic probability distribution for 40 topics", topic_n, " topics"),
    subtitle = paste0("Topic probability per set arranged by topic"),
    y = "Documents per bin",
    x = "Probability bin"
  ) +
  facet_wrap(~topic, ncol = 8) +
  legolda::theme_bar(bgcol = "#f8f8f8") 
 


## @knitr plot-60-topics

# For a set number of topics, what is the distribution
# of documents over that topic
topic_n <- 60
pal <- colorRampPalette(pal21())(topic_n)

breaks <- c(0.5)

gdf %>%
  filter(n_topic == topic_n & log(gamma, 10) > -Inf) %>%
  ggplot() +
  geom_histogram(aes(gamma, fill = factor(topic))) +
  scale_fill_manual(values = pal) +
  scale_y_log10() +
  scale_y_continuous(breaks = breaks) +
  labs(
    title = paste0("Set distribution over ", topic_n, " topics"),
    y = "Number of documents per bin",
    x = "Probability bin"
  ) +
  facet_wrap(~topic, ncol = 10) +
  legolda::theme_bar(bgcol = "#f8f8f8") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )


## @knitr plot-other

# For each topic, how many documents might be sorted
# into that topic, using gamma > .50
breaks = c(0.5)

gt50 <- gdf %>%
  group_by(topic, n_topic) %>%
  filter(gamma > 0.5 & n_topic >= 50 & n_topic <= 60) %>%
  count()

gt50 %>%
  ggplot(aes(x = topic, y = n, fill = factor(n_topic))) +
  geom_col() +
  scale_fill_manual(values = pal21()) +
  scale_y_continuous(breaks = breaks) +
  # labs(
  #      title = paste0("Set distribution over ", topic_n, " topics"),
  #      y = "Number of documents per bin",
  #      x = "Probability bin") +
  facet_wrap(~n_topic, scales = "free") +
  legolda::theme_bar(bgcol = "#f8f8f8") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray30", size = 0.1)
  )
