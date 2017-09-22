# Connect to database and load all lego sets
 
#' Retrieve and Munge Lego Datasets
#'
#' Function relies on a connection being returned by the 
#'  connect function.
#'
#' @param sample_data Logical. Only return a sample.
#' @param n_samples An integer. The number of samples to return.
#'
#' @return set_colors is assigned the returned data set 
#' @export
load_csv <- function(sample_data = TRUE, n_samples = 10000) {
library(dplyr)

  # Check themes
  cat("Loading datasets from CSV files \n")
    # Get tables
  themes <- readr::read_csv(here::here("data", "themes.csv"))
  sets <- readr::read_csv(here::here("data","sets.csv"))
  inventories <- readr::read_csv(here::here("data", "inventories.csv"))
  inventory_parts <- readr::read_csv(here::here("data","inventory_parts.csv"))
  colors <- readr::read_csv(here::here("data","colors.csv"))

  cat("Assigning themes to theme_df \n")
  theme_df <<- themes
  sets_df <<- sets

  # Combine columns 
  set_colors <- themes %>%
      # Set root_id parent or to id if it is a top level theme
      mutate(root_id = ifelse(!is.na(parent_id), parent_id, id), 
            theme = name, theme_id = id) %>%
      select(theme, theme_id, root_id) %>%
      right_join(sets, by = "theme_id") %>%
      mutate(set_name = stringr::str_sub(name, 1, 20)) %>%
      select(set_num, set_name, year, root_id, theme_id, theme) %>%
      right_join(inventories, by = "set_num") %>%
      right_join(inventory_parts, by = c("id" = "inventory_id")) %>%
      left_join(colors, by = c("color_id" = "id")) %>%
      mutate(rgba = ifelse(is_trans == "t", paste0("#", rgb, "80"),paste0("#", rgb, "FF"))) %>%    
      select(set_num, set_name, theme_id, theme, root_id, rgba, year, quantity) 


  # Expand rows by brick quantity
  set_colors <<- set_colors[rep(seq(nrow(set_colors)), set_colors$quantity), ] %>% 
      select(-quantity)

  sampled_status <- "full set"
  if (sample_data) {
    cat("Taking ", n_samples, " rows sample of set inventories")
    set_colors <- set_colors %>%
      sample_n(n_samples)
    sampled <- "sampled"
  }

 cat(paste0("Assigning ", sampled_status, " set inventories to 'set_colors' \n"))
 # A check from an earlier debugging stage
 if (sum(is.na(set_colors$root_id)) > 0) stop("NA values in root_id")

}


assign_text <- function(arg) {
  cat(paste0("Assigning values to ", deparse(substitute(arg)), "\n"))
}

# Load set_words, dtm to environment
 
#' Load Derived Lego Datasets to Environment
#'
#' Loads 'set_words' and the document term matrix 'dtm'.
#' The 'set_colors' data needs to be in the golbal environment
#'
#' @export
create_tables <- function(sample_data = TRUE) {
  if(!exists("set_colors")){
    cat("Loading table 'set_colors'")
    legolda::load_csv(sample_data = sample_data)
  }

  set_words <- set_colors %>%  
    dplyr::count(set_name, set_num, rgba, sort = TRUE) %>% 
    dplyr::ungroup( )

  total_words <- set_words %>%  
    group_by(set_num) %>%
    summarize(total = sum(n))
  
  total_words <<- theme_df %>%
  mutate(root_id = ifelse(!is.na(parent_id), parent_id, id)) %>%
  select(id, root_id) %>%
  right_join(sets_df, by = c("id" = "theme_id")) %>%
  right_join(total_words)

  assign_text(total_words)

  set_words <- left_join(set_words, total_words, by = "set_num")

  cat("Assigning tidy set and color dataframe to 'set_words' \n")
  set_words <<- set_words %>%
    tidytext::bind_tf_idf(rgba, set_num, n)

  cat("Creating sparse document term matrix (tm-package) and assigning to 'dtm' \n")
  dtm <<- set_words %>%
    tidytext::cast_dtm(set_num, rgba, n)
}


get_lda_models <- function(){
  readRDS(here::here("inst", "data", "lda_models_all.RDS"))
}