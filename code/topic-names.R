# Name topics based on word frequency or tf-idf 
library(purrr)
library(dplyr)
library(forcats)
library(ggplot2)

legolda::load_data(sample_data = FALSE)
legolda::tidy_colorsets()

lda_models <- readRDS(here::here("inst", "data", "lda_models_all.RDS"))

# Tidy dataframe with sets assinged t
set_topics <- lda_models %>%
  map(function(x) {
    class(x) <- "LDA"; x
  }) %>%
  map(tidytext::tidy, matrix = "gamma")



# Assumes topic_pal is available
# Depends on (TODO: campare-clusters)
# See Ch 1 Text mining with R
model_num = 3
lda_clust <- readRDS(here::here("inst", "data", "lda_clusters.RDS"))


set_clust <- tibble(set_num = names(lda_clust[[model_num]]), 
              topic_id = lda_clust[[model_num]]) %>% 
            left_join(sets_df) %>% 
            arrange(topic_id) %>%  
            # mutate(topic_name = fct_inorder(factor(paste0("Topic ", topic_id)))) %>%
            mutate(topic_id = fct_inorder(factor(topic_id)))

# Tidy tokenization and remove stop words 
title_words <- set_clust %>% select(topic_id, name) %>% 
  tidytext::unnest_tokens(word, name) %>% 
  anti_join(tidytext::stop_words) %>%
  count(topic_id, word, sort = TRUE) %>%
  ungroup

total_words <- title_words %>% 
  group_by(topic_id) %>% 
  summarize(total = sum(n))

title_words <- left_join(title_words, total_words) %>% 
               tidytext::bind_tf_idf(word, topic_id, n)

title_words

plot_words <- title_words %>%
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))))

top3 <- plot_words %>% 
  group_by(topic_id) %>%
  top_n(3, tf_idf) %>%
  arrange(topic_id, desc(tf_idf)) %>%
  ungroup 

tnames <- top3 %>% split(., top3$topic_id) %>%
  map_chr( ~paste(.x$word, collapse=" ")) %>% 
  map_chr( ~stringr::str_sub(.x, 1, 32)) %>% 
  tibble(tname = ., id = names(.)) %>% 
  mutate(topic_name = paste0(id, ": ", tname)) %>%
  select(topic_name)

tnames
 
saveRDS(tnames, here::here("inst", "data", "topic_names.RDS")) 


# join 





