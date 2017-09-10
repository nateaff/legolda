# Plots of the 

## @knitr plot-topic-distribution 
 
# Set model num. Models: 20, 30, 40, 50, 60, 75, 100
model_num = 3
# Label sets by top topic probability
lda_clust <- get_lda_clusters(lda_models)

set_clust <- tibble(set_num = names(lda_clust[[model_num]]), 
              topic_id = lda_clust[[model_num]]) %>% 
            left_join(sets_df) %>% 
            arrange(topic_id) %>%  
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
gg <- gg + scale_fill_manual(values = topic_pal) 
gg <- gg + labs(x = "Topic number", 
                y = "Sets in topic", 
                title = "Color topic frequency") 
gg <- gg + theme_bar(bgcol = "#c8c6c4") 
gg <- gg + theme(legend.position = "none") 
gg <- gg + theme(panel.grid.major.x = element_blank())     
gg <- gg + theme(panel.grid.major.y = element_line(color = "#f8f6f4", size = 0.2))
gg


## @knitr plot-topic-timeline
model_num = 3

# Assumes topic_pal is available
lda_clust <- lda_clust <- get_lda_clusters(lda_models)

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

