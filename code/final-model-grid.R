# Waffle plots for the final model 

## @knitr plot-final-topics
library(forcats)
library(purrr)
library(grid)
library(ggplot2)
library(gridExtra) 

if(!exists("set_colors")){
  cat("Loading data \n")
  legolda::load_csv(sample_data = FALSE)
  legolda::create_tables()
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
nterms = 50
top_colors <- top_terms(lda_models[[model_num]], lambda, nterms, word_freq) %>% 
  mutate(topic_name = forcats::fct_inorder(factor(topic_name))) %>%
  mutate(rep = round(beta*100)) %>%  
  select(topic, term, rep)

# Expand by the beta weight of the color
top_colors <- top_colors[rep(seq(nrow(top_colors)), top_colors$rep), 1:3]

# Get counts for waffle plot. Wrapper needs name in name column
tp <- top_colors %>% count(topic, term) %>% 
      rename(count = n)

tpnames <- get_topic_names(lda_models[[model_num]])
tp <- tp %>% left_join(tpnames) %>% 
      rename(name = topic_name)


waff_topic <- function(data, ntopic, col) {
  p <- data %>% filter(topic == ntopic) 
  wp <- waffle2(
          p$count, 
          title = p$name, 
          colors = p$term, 
          rows = 5, size = 0.3, 
          grout_color = col) 
  # wp <- wp + theme_waff(col) 
  wp <- wp + theme(legend.position = "none")
  wp <- wp + theme(
      # panel.spacing = unit(1.2, "lines"),
      plot.title = element_text(
        size = 16,
        # family = "Roboto Condensed",
        face = "plain",
        color = "gray5"
      ),
      plot.subtitle = element_text(
        color = "gray10",
        face = "plain",
        size = 11
      ),
      axis.title = element_text(
        size = 11 ,
        color = "gray15"
      ),
      axis.text = element_blank(),
      plot.caption = element_text(
        face = "italic",
        size = 9,
        color = "gray25"
      )
    )
  # wp <- wp + theme_waff(bgcol = col, modify_text = FALSE)
  # wp <- wp + theme(plot.title = element_text(size = 10, face = "bold"))
  wp
}

bgcol <- "#787472"

pp <- map(1:ntopics, ~waff_topic(data = tp, ntopic = .x, col = bgcol))

pdf(here::here("docs", "figure", "final-grid-plot.pdf"), width = 13, height = 15)

grid.draw(
  grobTree(
    rectGrob(gp=gpar(fill= bgcol, lwd=0)),
    do.call(arrangeGrob, c(pp, ncol = 4) 
     )))


dev.off()
