# Name topics based on word frequency or tf-idf 

 
#' Collapse String List 
#'
#' Collapses a list of strings but limits the number of strings 
#'  collapsed to max_n
#'
#' @param max_n An integer. Maximum number of strings to collapse.
#' @param len An integer. Length over which to use only the first max_n strings.
#' @return A string which is a comma separated list of collapsed strings. 
str_collapse <- function(str_list, len, max_n = 3) {
  ifelse(stringr::str_length(paste(str_list, collapse = " ")) <= len,
          paste(str_list, collapse = " "), 
          paste(str_list[1:max_n], collapse=" "))  
}

test_str_collapse <- function(){
  letters[1:26]
  out <- str_collapse(list(letters[1:26]), 2, 17)
  stringr::str_length(out) == 17
}


 
#' Get topic names based on an LDA model 
#'
#' 
#'
#' @param model The LDA model which will have names created for its 
#'  topics 
#' @return A tibble with the single column `topic_name`
get_topic_names <- function(model){

  class(model) <- "LDA"

  lda_clust <- get_lda_clusters(list(model)) %>% 
              .$clust %>% 
              unlist 

  set_clust <- tibble(set_num = names(lda_clust), 
                topic_id = lda_clust) %>% 
              left_join(sets_df) %>% 
              arrange(topic_id) %>%  
              mutate(topic_id = forcats::fct_inorder(factor(topic_id)))

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

  plot_words <- title_words %>%
    arrange(desc(tf_idf)) %>% 
    mutate(word = factor(word, levels = rev(unique(word))))

  top3 <- plot_words %>% 
    group_by(topic_id) %>%
    top_n(3, tf_idf) %>%
    arrange(topic_id, desc(tf_idf)) %>%
    ungroup 

  tnames <- top3 %>% split(., top3$topic_id) %>%
    purrr::map_chr(~str_collapse(.x$word, len = 26)) %>%
    # map_chr( ~stringr::str_sub(.x, 1, 32)) %>% 
    tibble(tname = ., id = names(.)) %>% 
    mutate(topic_name = paste0(id, "  ", tname)) %>% 
    mutate(topic = as.numeric(id)) %>% 
    select(topic, topic_name) 

  tnames
}






