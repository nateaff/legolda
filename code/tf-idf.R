# This script:
# 1. Computes TF-IDF
# 2. Plots high TF-IDF 'terms', i.e. colors
# 3. Plots waffle plot samples of the high scoring 'documents', i.e. sets
# 4. Repeats steps 2. and 3. fro the low TF-IDF scores

## @knitr compute-tf-idf
library(ggplot2)
library(dplyr)
library(waffle)
library(tm)
library(tidytext)
library(purrr)

pal <- unique(set_colors$rgba)
names(pal) <- unique(pal)

# Compute tf-idf.
# For exemplar see Tidytext : http://tidytextmining.com/tfidf.html
set_words <- set_colors %>%
  count(name, set_num, rgba, sort = TRUE) %>%
  ungroup()

total_words <- set_words %>%
  group_by(set_num) %>%
  summarize(total = sum(n))

set_words <- left_join(set_words, total_words, by = "set_num")

set_words <- set_words %>%
  tidytext::bind_tf_idf(rgba, set_num, n)

## @knitr top-tf-idf-plot

# High TF-IDF scores
top <- set_words %>%
  arrange(tf_idf) %>%
  mutate(rgba = factor(rgba, levels = unique(rgba))) %>%
  # Remove sand brick and sets with few pieces
  filter(n > 20, rgba != "#D67572FF") %>%
  tail(50) %>%
  arrange(desc(tf_idf))

top <- top[match(unique(top$rgba), top$rgba), ] %>%
  arrange(tf_idf)
top$rgba <- factor(top$rgba, top$rgba)
top <- tail(top, 10)

# Plot highest tf-idf sets
top %>%
  ggplot() +
  geom_bar(
    aes(x = rgba, y = tf_idf, fill = rgba),
    stat = "identity",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = top$name) +
  labs(
    x = "Set name and color",
    y = "TF-IDF score",
    title = "Colors distinct to a set",
    subtitle = paste0("Set and brick color combinations with high TF-IDF scores")
  ) +
  coord_flip() +
  legolda::theme_bar()

## @knitr top-tf-idf-sets
top %>%
  select(set_num) %>%
  left_join(set_colors) %>%
  select(name, theme, year, set_num, rgba) %>%
  group_by(theme, name, set_num, year) %>%
  tidyr::nest() %>%
  mutate(counts = map(data, table)) -> out

# Waffle plots
iron(
  waff(out[10, ], size = 0.1, nrow = 35, pad = 0, nchr = 20),
  waff(out[9, ], size = 0.2, nrow = 3, pad = 0, nchr = 20),
  waff(out[8, ], size = 0.2, nrow = 3, pad = 0, nchr = 17),
  waff(out[7, ], size = 0.2, nrow = 6, pad = 0, nchr = 12)
)

## @knitr low-tf-idf-plot
# Set-color combinations with low TF-IDF
low <- set_words %>%
  arrange(tf_idf) %>%
  mutate(rgba = factor(rgba, levels = unique(rgba))) %>%
  # Remove red sand brick sets
  filter(n > 20, rgba != "#D67572FF") %>%
  head(200) %>% 
  filter(!is.na(rgba)) %>%
  arrange(tf_idf)

# Remove
low <- low[match(unique(low$rgba), low$rgba), ]
low$rgba <- factor(low$rgba, low$rgba)
low <- low[1:10, ]

low %>%
  ggplot() +
  geom_bar(
    aes(x = rgba, y = tf_idf, fill = rgba),
    stat = "identity",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = low$name) +
  labs(
    x = "Set name and color",
    y = "TF-IDF",
    title = "Not very suprising colors",
    subtitle = "Set and brick color combinations with low TF-IDF scores"
  ) +
  coord_flip() +
  legolda::theme_bar()



low %>%
  select(set_num) %>%
  left_join(set_colors) %>%
  select(name, theme, year, set_num, rgba) %>%
  group_by(theme, name, set_num, year) %>%
  tidyr::nest() %>%
  mutate(counts = map(data, table)) -> out

## @knitr low-tf-idf-plots1,
iron(
  waff(out[10, ], pad = 0, size = 0.1, nrow = 30, nchr = 17),
  waff(out[9, ], pad = 0, size = 0.1, nrow = 36, nchr = 20),
  waff(out[8, ], size = 0.1, nrow = 45, nchr = 15)
)

## @knitr low-tf-idf-plots2
iron(
  waff(out[7, ], size = 0.2, nrow = 45, nchr = 18)
)
