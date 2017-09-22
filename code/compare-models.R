# Compare LDA models
## @knitr color-distribution
library(dplyr)
library(ggplot2)

if(!exists("set_colors")){
  legolda::load_csv(sample_data = FALSE)
  legolda::create_tables(sample_data = FALSE)
}

lda_models <- readRDS(here::here("inst", "data", "lda_models_all.RDS"))

set_topics <- lda_models %>%
  purrr::map(function(x) {
    class(x) <- "LDA"
    x
  }) %>%
  purrr::map(tidytext::tidy, matrix = "gamma")

# Total frequency used in relevance score
word_freq <- set_colors %>%
  count(rgba) %>%
  mutate(percent = n / nrow(set_colors))

# Create palette
pal <- unique(set_colors$rgba)
names(pal) <- unique(pal)

# Plot weighted relevance of terms/colors for each topic
plot_relevance <- function(top_terms, bgcol) {
 
  ntopics <- max(top_terms$topic)
  subtitle <- paste0("Weighted color distribution for ", ntopics, " topics")

  top_terms %>%
    ggplot(aes(x = -order, y = relevance, fill = term)) +
    labs(
      x = "", y = "Color relevance to topic",
      title = "Lego color topics",
      subtitle = subtitle
    ) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free", nrow = 5) +
    scale_fill_manual(values = pal) +
    coord_flip() +
    theme_bar(bgcol) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

top_list <- lda_models %>% 
  purrr::map(legolda::top_terms, lambda = 0.7, nterms = 7, freq = word_freq)

bgcol = "#a8a4a2"

plot_relevance(top_list[[2]], bgcol) # 20 topics
plot_relevance(top_list[[3]], bgcol) # 40 topics
plot_relevance(top_list[[5]], bgcol) # 60 topics

## @knitr topic-table-2-2

# Which sets are most associated with a topic
model_num <- 2
topic_num <- 2

view_topic <- set_topics[[model_num]] %>%
  filter(topic == topic_num) %>%
  arrange(desc(gamma)) %>%
  head(50) %>%
  mutate(set_num = document, gamma = round(gamma, 2)) %>%
  left_join(sets_df, by = "set_num") %>%
  mutate(set_name = stringr::str_sub(name, 1, 20)) %>%
  select(topic, gamma, set_name, set_num, theme_id, year, num_parts) %>%
  left_join(theme_df, by = c("theme_id" = "id")) %>%
  mutate(theme_name = name) %>%
  select(topic, gamma, set_name, set_num, theme_name, theme_id, year, num_parts)

knitr::kable(view_topic, caption = paste0("Sets most associated with topic", topic_num))

saveRDS(view_topic, here::here("inst", "data", "view-topic-2-2.RDS"))
  
## @knitr topic-waffle-2-2
model_num <- 2
topic_num <- 2

library(waffle)
plot_topic <- set_topics[[model_num]] %>%
  dplyr::filter(topic == topic_num) %>%
  dplyr::arrange(desc(gamma)) %>%
  head(20)


waffle_prep <- function(document, sets) {
  document$set_num <- document$document
  document %>%
    left_join(sets, by = "set_num") %>%
    select(set_num, name, theme, year, rgba) %>%
    group_by(theme, name, set_num, year) %>%
    tidyr::nest() %>%
    mutate(counts = purrr::map(data, table))
}

bgcol <- "#e8e4e2"
w1 <- waffle_prep(plot_topic[1, ], set_colors)
w2 <- waffle_prep(plot_topic[4, ], set_colors)
w3 <- waffle_prep(plot_topic[6, ], set_colors)
w4 <- waffle_prep(plot_topic[7, ], set_colors)

waffle::iron(
  waff(w1, size = 0.5, rows = 1, nchr = 20, bgcol = bgcol),
  waff(w2, size = 2,   rows = 1, nchr = 13, bgcol = bgcol),
  waff(w3, size = 0.2, rows = 2, nchr = 20, bgcol = bgcol),
  waff(w4, size = 0.2, rows = 4, nchr = 20, bgcol = bgcol)
)



## @knitr topic-waffle-3-2
model_num <- 3
topic_num <- 2

library(waffle)
plot_topic <- set_topics[[model_num]] %>%
  dplyr::filter(topic == topic_num) %>%
  dplyr::arrange(desc(gamma)) %>%
  head(20)


waffle_prep <- function(document, sets) {
  document$set_num <- document$document
  document %>%
    left_join(sets, by = "set_num") %>%
    select(set_num, name, theme, year, rgba) %>%
    group_by(theme, name, set_num, year) %>%
    tidyr::nest() %>%
    mutate(counts = purrr::map(data, table))
}

bgcol <- "#e8e4e2"
w1 <- waffle_prep(plot_topic[1, ], set_colors)
w2 <- waffle_prep(plot_topic[4, ], set_colors)
w3 <- waffle_prep(plot_topic[6, ], set_colors)
w4 <- waffle_prep(plot_topic[8, ], set_colors)

waffle::iron(
  waff(w1, size = 0.5, rows = 1, nchr = 20, bgcol = bgcol),
  waff(w2, size = 2,   rows = 4, nchr = 13, bgcol = bgcol),
  waff(w3, size = 0.2, rows = 3, nchr = 20, bgcol = bgcol),
  waff(w4, size = 0.2, rows = 2, nchr = 20, bgcol = bgcol)
)


## @knitr topic-table-3-2
# Which sets are most associated with a topic
model_num <- 3
topic_num <- 2

view_topic <- set_topics[[model_num]] %>%
  filter(topic == topic_num) %>%
  arrange(desc(gamma)) %>%
  head(50) %>%
  mutate(set_num = document, gamma = round(gamma, 2)) %>%
  left_join(sets_df, by = "set_num") %>%
  mutate(set_name = stringr::str_sub(name, 1, 20)) %>%
  select(topic, gamma, set_name, set_num, theme_id, year, num_parts) %>%
  left_join(theme_df, by = c("theme_id" = "id")) %>%
  mutate(theme_name = name) %>%
  select(topic, gamma, set_name, set_num, theme_name, theme_id, year, num_parts)

knitr::kable(view_topic, caption = "Sets most associated with topic 37")

saveRDS(view_topic, here::here("inst", "data", "view-topic-3-2.RDS"))


## @knitr topic-waffle-3-32
# Which sets are most associated with a topic
model_num <- 3
topic_num <- 32

plot_topic <- set_topics[[model_num]] %>%
  dplyr::filter(topic == topic_num) %>%
  dplyr::arrange(desc(gamma)) %>%
  head(10)
# plot_topic

waffle_prep <- function(document, sets) {
  document$set_num <- document$document
  document %>%
    left_join(sets, by = "set_num") %>%
    select(set_num, name, theme, year, rgba) %>%
    group_by(theme, name, set_num, year) %>%
    tidyr::nest() %>%
    mutate(counts = purrr::map(data, table))
}

w1 <- waffle_prep(plot_topic[1, ], set_colors)
w2 <- waffle_prep(plot_topic[4, ], set_colors)
w3 <- waffle_prep(plot_topic[6, ], set_colors)
w4 <- waffle_prep(plot_topic[7, ], set_colors)

waffle::iron(
  waff(w1,  size = 0.5, rows = 1, nchr = 20, bgcol = bgcol),
  waff(w2,  size = 0.5, rows = 4, nchr = 18, bgcol = bgcol),
  waff(w3,  size = 0.5, rows = 1, nchr = 19, bgcol = bgcol),
  waff(w4,  size = 0.5, rows = 4, nchr = 20, bgcol = bgcol)
)

## @knitr topic-table-3-32
model_num <- 3
topic_num <- 32

view_topic <- set_topics[[model_num]] %>%
  filter(topic == topic_num) %>%
  arrange(desc(gamma)) %>%
  head(50) %>%
  mutate(set_num = document, gamma = round(gamma, 2)) %>%
  left_join(sets_df, by = "set_num") %>%
  mutate(set_name = stringr::str_sub(name, 1, 20)) %>%
  select(topic, gamma, set_name, set_num, theme_id, year, num_parts) %>%
  left_join(theme_df, by = c("theme_id" = "id")) %>%
  mutate(theme_name = name) %>%
  select(topic, gamma, set_name, set_num, theme_name, theme_id, year, num_parts)

saveRDS(view_topic, here::here("inst", "data", "view-topic-32-3.RDS"))

knitr::kable(view_topic, caption = paste0("Sets most associated with topic ", topic_num))





