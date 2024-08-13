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

# Apply smooting to all values in a data frame
mean3.df <- function(data) {
  data.frame(lapply(data, mean3))
}

# Plot all values in a data frame as lines over index
plot.cols <- function(data, y_min = NULL, y_max = NULL) {
  col1 <- data[,1]
  x_min <- 1
  x_max <- length(col1)
  if (is.null(y_min) || is.null(y_max)) {
    y_min_auto <- min(col1)
    y_max_auto <- max(col1)
    for (col in data[-1]) {
      y_min_auto <- min(y_min_auto, min(col))
      y_max_auto <- max(y_max_auto, max(col))
    }
    if (is.null(y_min)) {
      y_min <- y_min_auto
    }
    if (is.null(y_max)) {
      y_max <- y_max_auto
    }
  }
  plot(0, 0, xlim = c(x_min, x_max), ylim = c(y_min, y_max), type = "n")
  for (i in 1:(length(data))) {
    lines(data[,i], col = i)
  }
}
