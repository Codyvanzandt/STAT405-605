library(tidyverse)
library(grid)

# data <- read_csv("./data/killer_plot_data.csv")

plot_word_timeseries <- function(data, book_title, smoothing_coefficient=2/3, show_line=FALSE, remove_overlap=FALSE){
  title_data <- data %>%
    filter(title==book_title) %>%
    mutate(date = row_number())
  
  title_checkouts_smoothed <- lowess(title_data$checkouts, f=smoothing_coefficient)
  
  output_plot <- plot_base_graph(title_checkouts_smoothed$x, title_checkouts_smoothed$y)
  
  col <- ifelse(
    title_data$rating < 3, "firebrick",
    ifelse(
      title_data$rating > 3 & title_data$rating < 5, "gold3", "darkgreen"
    )
  )
  grid.text(title_data$word, x=title_checkouts_smoothed$x, title_checkouts_smoothed$y,
            default.units="native", gp=gpar(fontsize=10, col=col), check.overlap=remove_overlap)
  
  if(show_line){
    grid.lines(title_checkouts_smoothed$x, title_checkouts_smoothed$y, default.units="native")  
  }
  
  output_plot
  
}

plot_base_graph <- function(x, y){
  grid.newpage()
  vp <- plotViewport(margins = c(5.1, 4.1, 4.1, 2.1))
  pushViewport(vp)
  pushViewport(dataViewport(x, y))
  grid.rect()
  grid.xaxis()
  grid.yaxis()
}


