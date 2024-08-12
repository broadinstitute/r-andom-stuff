# Create smoothed vector from input data by averaging 3 consecutive values
mean3 <- function(data) {
  n <- length(data)
  if (n < 2) {
    return(data)
  }
  first <- data[1]
  last <- data[n]
  (c(data[2:n], last) + data + c(first, data[1:(n - 1)])) / 3
}

mean3_df <- function(data) {
  data.frame(lapply(data, mean3))
}

plot_cols <- function(data) {
  col1 <- data[,1]
  x_min <- 1
  x_max <- length(col1)
  y_min <- min(col1)
  y_max <- max(col1)
  for (col in data[-1]) {
    y_min <- min(y_min, min(col))
    y_max <- max(y_max, max(col))
  }
  plot(0, 0, xlim = c(x_min, x_max), ylim = c(y_min, y_max), type = "n")
  for (i in 1:(length(data))) {
    lines(data[,i], col = i)
  }
}
