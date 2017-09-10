# Waffle plots for the final model 

## @knitr plot-final-topics
library(forcats)
library(purrr)
library(grid)
library(gridExtra) 

if(!exists("set_colors")){
  cat("holler \n")
  load_data(sample_data = FALSE)
}

lda_models <- readRDS(here::here("inst", "data", "lda_models_all.RDS"))

model_num = 3
ntopics = lda_models[[model_num]]@k 

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
nterms = 15
top_colors <- top_terms(lda_models[[model_num]], lambda, nterms, word_freq) %>% 
  mutate(topic_name = forcats::fct_inorder(factor(topic_name))) %>%
  mutate(rep = round(beta*100)) %>%  
  select(topic, term, rep)

# Expand by the beta weight of the color
top_colors <- top_colors[rep(seq(nrow(top_colors)), top_colors$rep), 1:3]

# Get counts for waffle plot. Wrapper needs name in name column
tp <- top_colors %>% count(topic, term) %>% 
  mutate(name = fct_inorder(factor(paste0("Topic ", topic))), 
    counts = n)

tpnames <- get_topic_names(lda_models[[model_num]])
tp$name <- tpnames$topic_name

waff_topic <- function(data, ntopic, col) {
  p <- data %>% filter(topic == ntopic) 
  wp <- waffle2(
          p$counts, 
          title = p$name, 
          colors = p$term, 
          rows = 5, size = 0.3, 
          grout_color = col) 
  wp + theme_waff(col)
}

bgcol <- "#c8c4c2"

pp <- lapply(1:ntopics, function(x) waff_topic(data = tp, ntopic = x, col = bgcol))

grid.draw(grobTree(rectGrob(gp=gpar(fill= bgcol, lwd=0)),
   do.call(grid.arrange, c(pp, ncol = 5) )))
