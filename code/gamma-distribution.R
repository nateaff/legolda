# Example from http://tidytextmining.com/nasa.html
# Look at how well sets are assigned to topics

## @knitr load-distributions
library(dplyr)
library(tidytext)
library(purrr)

lda_models <- readRDS("~/devel/R-proj/lego-lda/inst/data/lda_models_all.RDS")

gamma_tbl <- lda_models %>%
  map(function(x) {
    class(x) <- "LDA"
    x
  }) %>% tibble(model = .) %>% 
  mutate(topic_num = map_int(model, ~.x@k)) %>% 
  mutate(gamma = map(model, tidytext::tidy, matrix = "gamma"))

gamma_tbl <- gamma_tbl %>% tidyr::unnest(gamma)

## @knitr plot-all-topics
gamma_tbl %>%
  ggplot(aes(gamma)) +
  geom_histogram(aes(fill = factor(topic_num))) +
  scale_y_log10() +
  scale_fill_manual(values = pal21()) +
  labs(
    title = "Distribution of probabilities for all topics",
    y = "Number of documents", x = expression(gamma)
  ) +
  facet_wrap(~topic_num) +
  legolda::theme_bar(bgcol = "#f8f8f8")

## @knitr plot-40-topics

# For a set number of topics, what is the distribution
# of documents over that topic
topic_n = 40
pal <- colorRampPalette(pal21())(topic_n)

gamma_tbl %>%
  filter(topic_num == topic_n & log(gamma, 10) > -Inf) %>%
  ggplot() +
  geom_histogram(aes(gamma, fill = factor(topic))) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = c(.25, 0.5, .75)) + 
  scale_y_log10() +
  labs(
    title = paste0("Set distribution over ", topic_n, " topics"),
    y = "Sets per bin",
    x = "Topic probability bin"
  ) +
  facet_wrap(~topic, ncol = 8) +
  legolda::theme_bar(bgcol = "#f8f8f8") 
 


## @knitr plot-60-topics

# For a set number of topics, what is the distribution
# of documents over that topic
topic_n <- 60
pal <- colorRampPalette(pal21())(topic_n)

breaks <- c(0.5)

gamma_tbl %>%
  filter(topic_num == topic & log(gamma, 10) > -Inf) %>%
  ggplot() +
  geom_histogram(aes(gamma, fill = factor(topic))) +
  scale_fill_manual(values = pal) +
  scale_y_log10() +
  scale_y_continuous(breaks = breaks) +
  labs(
    title = paste0("Set distribution over ", topic, " topics"),
    y = "Number of documents per bin",
    x = "Probability bin"
  ) +
  facet_wrap(~topic_num, ncol = 10) +
  legolda::theme_bar(bgcol = "#f8f8f8") +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )


## @knitr plot-other

# For each topic, how many documents might be sorted
# into that topic, using gamma > .50
breaks = c(0.5)

gt50 <- gamma_tbl %>%
  group_by(topic, topic_num) %>%
  filter(gamma > 0.5 & topic_num >= 50 & topic_num <= 60) %>%
  count()

gt50 %>%
  ggplot(aes(x = topic, y = n, fill = factor(topic_num))) +
  geom_col() +
  scale_fill_manual(values = pal21()) +
  scale_y_continuous(breaks = breaks) +
  # labs(
  #      title = paste0("Set distribution over ", topic, " topics"),
  #      y = "Number of documents per bin",
  #      x = "Probability bin") +
  facet_wrap(~topic_num, scales = "free") +
  legolda::theme_bar(bgcol = "#f8f8f8") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray30", size = 0.1)
  )
