library(tidyverse)
library(grid)

# data <- read_csv("./data/killer_plot_data.csv")

plot_base_graph <- function(x, y){
  grid.newpage()
  vp <- plotViewport(margins = c(5.1, 4.1, 4.1, 2.1))
  pushViewport(vp)
  pushViewport(dataViewport(x, y))
  grid.rect()
}

plot_word_timeseries <- function(data, book_title, smoothing_coefficient=2/3, show_line=TRUE, remove_overlap=FALSE){
  
  title_data <- data %>%
    filter(title==book_title)
  
  title_checkouts_smoothed <- lowess(title_data$checkouts, f=smoothing_coefficient)
  
  # Get base plot
  output_plot <- plot_base_graph(title_checkouts_smoothed$x, title_checkouts_smoothed$y)
  
  # Plot axes
  grid.xaxis(at = title_checkouts_smoothed$x[seq.int(1, nrow(title_data), length.out = 8)], label = format(as.Date(title_data$date), "%b %Y")[seq.int(1, nrow(title_data), length.out = 8)], gp = gpar(fontsize = 10))
  grid.yaxis(gp = gpar(fontsize = 10))
  
  # Plot axes labels
  grid.text("Month", y = unit(-3, "lines"))
  grid.text("Number of Checkouts", x = unit(-3.4, "lines"), rot = 90)
  
  # Plot title
  grid.text(book_title, y = min(title_checkouts_smoothed$y) + 1.1 * (max(title_checkouts_smoothed$y) - min(title_checkouts_smoothed$y)), default.units="native", gp = gpar(fontsize = 16))
  
  # Plot line
  if(show_line){
    grid.lines(title_checkouts_smoothed$x, title_checkouts_smoothed$y, default.units="native",
               gp=gpar(alpha = 0.25, lty = 2, lwd = 2))  
  }
  
  # Get word colors
  col <- ifelse(
    title_data$rating < 3, "firebrick",
    ifelse(
      title_data$rating > 3 & title_data$rating < 5, "gold3", "darkgreen"
    )
  )
  
  # Plots words
  low <- data$log_odds_weighted[data$title==book_title]
  fontsizes <- 6 + 10 * (low - min(low)) / (max(low) - min(low))
  
  grid.text(title_data$word, x = title_checkouts_smoothed$x, y = title_checkouts_smoothed$y,
            default.units="native", gp=gpar(fontsize=fontsizes, col=col), check.overlap=remove_overlap)
  
  output_plot
  
}
