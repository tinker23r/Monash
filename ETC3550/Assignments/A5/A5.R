library(fpp3, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)

ses_func <- function(y, forecast = 1, alpha_input = 0) {
    smooth_function <- function(y, alpha, level, sse_mode = FALSE) {
        n <- length(y)
        levels <- numeric(n + 1)

        levels[1] <- alpha * y[1] + (1 - alpha) * level

        for (t in 1:n) {
            levels[t + 1] <- alpha * y[t] + (1 - alpha) * levels[t]
        }
        # Returns Sum of Squared Errors
        if (sse_mode) {
            return(sum((y - levels[-(n + 1)])^2))
        }

        # Returns yhat_(t+h)
        return(levels[n + 1])
    }

    sse <- function(par, y) {
        return(smooth_function(y, par[1], par[2], TRUE))
    }

    best_val <- optim(
        par = c(
            alpha = alpha_input,
            level = y[1]
        ),
        fn = sse,
        y = y
    )

    # Forecast you return the same value for any value of h
    # Since the formula for SES shows that h has no influence
    # on the final output
    return(smooth_function(y, best_val$par[1], best_val$par[2]))
}