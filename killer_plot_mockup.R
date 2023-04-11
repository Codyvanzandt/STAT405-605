library(tidyverse)
source("killer_plot_candles.R")

set.seed(12294)

mock_data <- data.frame(
  x = 1:60,
  y = cumsum(runif(60, -1, 1)),
  words = read_csv("random_words.csv")$words,
  distinctiveness = runif(60, 5, 30)
)

plot_base_graph(mock_data)
# grid.lines(mock_data$x, mock_data$y, default.units="native")
grid.text(mock_data$words, x=mock_data$x, y=mock_data$y,
          default.units="native", gp=gpar(fontsize=8), check.overlap=TRUE)
