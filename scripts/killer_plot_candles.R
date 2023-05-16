library(grid)
library(tidyverse)
library(scales)

fake_data <- data.frame(x=1:300,
                        y=cumsum(runif(300, -1, 1))
                        )

candle_x <- seq(10, 300, by=5)
fake_candle_data <- data.frame(
  x = candle_x,
  y = fake_data$y[candle_x],
  value = runif(length(candle_x), -1, 1)
)

plot_candle_graph <- function(data, candle_data, candle_width, max_candle_height){
  # data assumed to contain $x and $y
  # candle_data assumed to contain $x, $y, and $value
  plot_base_graph(data)
  candle_heights <- compute_candle_height(candle_data, max_candle_height)
  plot_candles(candle_heights, candle_width)
  grid.lines(data$x, data$y, default.units="native")
}

plot_base_graph <- function(data){
  # expects data$x and data$y
  grid.newpage()
  vp <- plotViewport(margins = c(5.1, 4.1, 4.1, 2.1))
  pushViewport(vp)
  pushViewport(dataViewport(data$x, data$y))
  grid.rect()
  grid.xaxis()
  grid.yaxis()
}

plot_candles <- function(data, width){
  # expects data$x, data$y, data$height, data$direction

  up_candles <- data %>% filter(direction == "up")
  down_candles <- data %>% filter(direction == "down")
  
  grid.rect(up_candles$x, up_candles$y, width=width, height=data$height,
            just=c("centre", "bottom"),
            default.units = "native",
            gp=gpar(fill="darkgreen"))
  
  grid.rect(down_candles$x, down_candles$y, width=width, height=data$height,
            just=c("centre", "top"),
            default.units = "native",
            gp=gpar(fill="firebrick"))
}

compute_candle_height <- function(candle_data, max_candle_height){
  candle_data["direction"] <- ifelse(candle_data$value >= 0, "up", "down")
  candle_data["height"] <- max_candle_height * abs(candle_data$value)
  candle_data
}


# For best results, set width = distance between candles
# plot_candle_graph(fake_data, fake_candle_data, 5, 3)

