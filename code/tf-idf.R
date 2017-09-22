# This script 
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

# Compute tf-idf.
load_csv(sample_data = FALSE)
create_tables()

## @knitr top-tf-idf-plot
pal <- unique(set_colors$rgba)
names(pal) <- unique(pal)

# High TF-IDF scores
top <- set_words %>%
  arrange(tf_idf) %>%
  mutate(rgba = factor(rgba, levels = unique(rgba))) %>%
  # Remove sand brick and sets with few pieces
  filter(n > 20, rgba != "#D67572FF") %>%
  tail(50) %>%
  arrange(desc(tf_idf))

# Hackery
top <- top[match(unique(top$rgba), top$rgba), ] %>%
  arrange(tf_idf)
top$rgba <- factor(top$rgba, top$rgba)
top <- tail(top, 10)

bgcol = "#e8e4e2"
# Plot highest tf-idf sets
top %>%
  ggplot() +
  geom_bar(
    aes(x = rgba, y = tf_idf, fill = rgba),
    stat = "identity",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = top$set_name) +
  labs(
    x = "Set name and color",
    y = "TF-IDF score",
    title = "Colors distinct to a set",
    subtitle = paste0("Set and brick color combinations with high TF-IDF scores")
  ) +
  geom_hline(yintercept = c(0, 1, 2, 3), size = 1.5, col = bgcol) +
  coord_flip() +
  legolda::theme_bar()


## @knitr top-tf-idf-sets
bgcol <- "#e8e4e2"

top %>%
  select(set_num) %>%
  left_join(set_colors) %>%
  select(set_name, theme, year, set_num, rgba) %>%
  group_by(theme, set_name, set_num, year) %>%
  tidyr::nest() %>%
  mutate(counts = purrr::map(data, table)) -> out


# Waffle plots
iron(
  waff(out[10, ], size = 0.01, rows = 30, nchr = 20, bgcol = bgcol),
  waff(out[9, ],  size = 0.2, rows = 3,  nchr = 20, bgcol = bgcol),
  waff(out[8, ],  size = 0.2, rows = 3,  nchr = 17, bgcol = bgcol),
  waff(out[7, ],  size = 0.2, rows = 10,  nchr = 12, bgcol = bgcol)
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
  scale_x_discrete(labels = low$set_name) +
  labs(
    x = "Set name and color",
    y = "TF-IDF",
    title = "Not very suprising colors",
    subtitle = "Set and brick color combinations with low TF-IDF scores"
  ) +
   geom_hline(yintercept = c(0, 0.003, 0.006, 0.009), size = 1.5, col = bgcol) +
  coord_flip() +
  legolda::theme_bar()

## @knitr low-tf-idf-sets1
low %>%
  select(set_num) %>%
  left_join(set_colors) %>%
  select(set_name, theme, year, set_num, rgba) %>%
  group_by(theme, set_name, set_num, year) %>%
  tidyr::nest() %>%
  mutate(counts = purrr::map(data, table)) -> out


iron(
  waff(out[10, ], size = 0.1, rows = 30, nchr = 17, bgcol = bgcol),
  waff(out[9, ],  size = 0.1, rows = 36, nchr = 20, bgcol = bgcol)
)

## @knitr low-tf-idf-sets2
iron(
  waff(out[8, ], size = 0.1, rows = 45, nchr = 15, bgcol = bgcol), 
  waff(out[7, ], size = 0.2, rows = 45, nchr = 18, bgcol = bgcol)
)
