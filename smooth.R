# Create smoothed vector from input data by averaging 3 consecutive values
mean3 <- function(data) {
    n <- length(data)
    if (n < 2) {
        return(data)
    }
    first <- data[1]
    last <- data[n]
    (c(data, last, last) + c(first, data, last) + c(first, first, data))[2:(n+1)] / 3
}
