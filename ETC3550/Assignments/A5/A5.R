library(fpp3, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)

ses_func <- function(y, forecast = 1, alpha_input = 0) {
    smooth_function <- function(y, alpha, level) {
        n <- length(y)
        levels <- numeric(n + 1)
        levels[1] <- level
        for (t in 1:n) {
            levels[t + 1] <- alpha * y[t] + (1 - alpha) * levels[t]
        }
        return(levels[n + 1])
    }

    # Finds the best alpha value for given dataset
    alpha_cal <- function(y, alpha_g, level) {
        # adjust incrementor for more fined alpha value at the cost of time
        alpha_incrementor <- 10^(-5)
        sse_best <- Inf

        # Do while loop
        repeat{
            n <- length(y)
            levels <- numeric(n + 1)
            levels[1] <- level
            for (t in 1:n) {
                levels[t + 1] <- alpha_g * y[t] + (1 - alpha_g) * levels[t]
            }
            yhat <- levels[-(n + 1)]
            sse <- sum((y - yhat)^2)
            if (sse > sse_best) {
                break
            }
            alpha_g <- alpha_g + alpha_incrementor
            sse_best <- sse
        }
        return(alpha_g)
    }
    if (forecast == 1) {
        return(smooth_function(y, alpha_cal(y, alpha_input, y[1]), y[1]))
    }
    while (forecast > 1) {
        print(forecast)
        y <- append(
            y,
            smooth_function(y, alpha_cal(y, alpha_input, y[1]), y[1])
        )
        forecast <- forecast - 1
    }
    return(y[length(y)])
}