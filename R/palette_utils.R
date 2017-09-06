#' Generate a pallete from a color vector
#' 
#' @param vec A vector to convert ot a function 
#' @param fname A string. The name of the function
#'   that is created 
#' @return Writes a file to a local R directory. Assumes 
#'  that the function is called from a package directory.
#' @export
vec_2_function <- function(vec){
  vec_names <- names(vec)

  sink(here::here("R", "topic_palette.R"))
  cat("#' Generated Topic Palette \n")
  cat("#' ")
  cat("#' @export \n")
  cat("topic_palette <- function() { \n \n")
  cat("# Palette colors \n")
  cat("pal <- c(")
  cat(stringr::str_c(paste0("'", vec, "'", collapse = ", ")))
  cat(") \n")
  cat("topic_names <- c(")  
  cat(stringr::str_c(paste0("'", vec_names, "'", collapse = ", ")))
  cat(") \n")
  cat("names(pal) <- topic_names \n")    
  cat("pal \n")
  cat("}") 
  sink()
}

#' Palette based on topic 21 from 35? topic model 
#' @export
pal21 <- function() {
  c("#9B9A5AFF", "#CC702AFF", "#078BC9FF", "#F8BB3DFF",
    "#4B9F4AFF", "#A95500FF", "#D09168FF", "#9FC3E9FF", 
    "#F08F1CFF", "#CFE2F7FF")
}
