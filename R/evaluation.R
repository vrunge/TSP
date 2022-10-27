##  GPL-3 License
## Copyright (c) 2022 Vincent Runge

#' city plot
#'
#' @description plotting the cities and the proposed best tour
#' @param x the proposed best tour
#' @param ... Other parameters
#' @param data matrix of positions for cities in (x,y)
#' @param main title for the plot
#' @param value to plot the length value of the proposed tour
#' @return plot 2D of the cities
plot.TSP <- function(x,..., data, main = "", value = "")
{
  plot(data[,1], data[,2], xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", main = main, asp = 1,
       xaxt = "n", yaxt = "n")
  n <- length(x)
  col_fun <- colorRamp(c("blue", "red"))
  rgb_cols <- col_fun(1:n / n)
  cols <- rgb(rgb_cols, maxColorValue = 256)
  for(i in 1:(n-1)){segments(x0 = data[x[i],1],
                                        y0 = data[x[i],2],
                                        x1 = data[x[i+1],1],
                                        y1 = data[x[i+1],2], lwd = 1, col = cols[i])}
  segments(x0 = data[x[length(x)],1],
           y0 = data[x[length(x)],2],
           x1 = data[x[1],1],
           y1 = data[x[1],2], lwd = 1, col = cols[n])
  title(sub=value, adj=1, line=0.5, font=4)
}


#' Tour length value
#'
#' @description Computing the tour length value
#' @param tour the proposed best tour
#' @param data matrix of positions for cities in (x,y)
#' @return the tour length
tour_length <- function(tour, data)
{
  return(sum(apply(apply(data[c(tour, tour[1]),],2,function(x) diff(x)^2), 1 ,function(x) sqrt(sum(x)))))
}






