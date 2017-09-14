# Themes for waffle plots, bar plots and scatter plot

# Themes
 
#' Waffle Plot Theme
#'
#'
#' @return A ggplot2 theme
#' @export
theme_waff <- function(bgcol = "#e8e4e2") {
  tm <- theme_minimal()
  tm <- tm + theme(
      legend.position = "none",
      plot.background = element_rect(fill = bgcol, color = bgcol),
      panel.background = element_rect(fill = bgcol, color = bgcol),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.spacing = unit(1.2, "lines"),
      plot.title = element_text(
        family = "Lato",
        size = 14,
        face = "bold",
        color = "gray5"
      ),
      plot.subtitle = element_text(
        family = "Roboto Condensed",
        color = "gray10",
        face = "plain",
        size = 11
      ),
      axis.title = element_text(
        family = "Roboto Condensed",
        size = 11 ,
        color = "gray15"
      ),
      axis.text = element_blank(),
      plot.caption = element_text(
        family = "Roboto Condensed",
        face = "italic",
        size = 9,
        color = "gray25"
      )
    )
}


#' Bar Plot Theme
#'
#'
#' @return A ggplot2 theme
#' @export
theme_bar <- function(bgcol = "#e8e4e2", grid_col = "gray25") {
  theme_minimal() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = bgcol, color = bgcol),
      panel.background = element_rect(fill = bgcol, color = bgcol),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(color = grid_col, size = 0.15),
      # panel.spacing = unit(1.2, "lines"),
      plot.title = element_text(
        family = "Lato",
        size = 16,
        face = "bold",
        color = "gray5"
      ),
      plot.subtitle = element_text(
        color = "gray10",
        face = "plain",
        size = 10
      ),
      axis.title = element_text(
        family = "Roboto Condensed",
        size = 9,
        color = "gray15"
      ),
      # axis.text = element_blank(),
      plot.caption = element_text(
        family = "Roboto Condensed",
        face = "italic",
        size = 9,
        color = "gray25"
      )
    )
}


#' Scatter Plot Theme
#'
#'
#' @return a ggplot2 theme
#' @export
theme_scatter <- function(bgcol = "#e8e4e2", grid_col = "gray25") {
  theme_bar(bgcol) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(
        family = "Roboto Condensed",
        size = 9,
        color = "gray15"),
      legend.title = element_text(
        family = "Roboto Condensed",
        size = 9,
        color = "gray5"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(color = grid_col, size = 0.15),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(color = grid_col, size = 0.15)
    )
}



#' Dark Bar Plot Theme
#'
#'
#' @return A ggplot2 theme
#' @export
theme_dark_bar <- function(bgcol = "#686462", grid_col = "white") {
  theme_minimal() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = bgcol, color = bgcol),
      panel.background = element_rect(fill = bgcol, color = bgcol),
      panel.grid.minor.y = element_blank(),
      # panel.grid.major.x = element_line(color = grid_col, size = 0.15),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      # panel.spacing = unit(1.2, "lines"),
      plot.title = element_text(
        family = "Roboto",
        size = 20,
        face = "bold",
        color = "gray5"
      ),
      plot.subtitle = element_text(
        family = "Roboto Condensed",
        color = "black",
        face = "plain",
        size = 11
      ),
      # axis.text.x = element_text(
      #   family = "Roboto Condensed",
      #   face = "plain",
      #   size = 10,
      #   color = "gray5"
      #   ), 
      axis.title = element_text(
        family = "Roboto Condensed",
        face = "bold",
        size = 11,
        color = "gray5"
      ),
      axis.text = element_text(
        family = "Roboto Condensed",
        face = "plain",
        size = 9,
        color = "gray80"
      ),
      # axis.text = element_blank(),
      plot.caption = element_text(
        family = "Roboto Condensed",
        face = "italic",
        size = 9,
        color = "gray5"
      )
    )
}














