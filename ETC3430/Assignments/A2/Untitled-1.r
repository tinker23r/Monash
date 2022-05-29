rm(list = ls())
library(knitr)
library(tidyverse)
set.seed(31455034)
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

# Limiting to Years 1975 - 2015 & Ages 0 - 99
death <- read.delim(file = "Data/Deaths_1x1.txt", sep = "") %>%
    filter(1975 <= Year, Year <= 2015, Age != "110+") %>%
    select(-Female, -Male) %>%
    mutate(Age = strtoi(Age)) %>%
    filter(Age < 100)


# Limiting to Years 1975 - 2015 & Ages 0 - 99
exposure <- read.delim(file = "Data/Exposures_1x1.txt", sep = "") %>%
    filter(1975 <= Year, Year <= 2015, Age != "110+") %>%
    select(-Female, -Male) %>%
    mutate(Age = strtoi(Age)) %>%
    filter(Age < 100)

age <- max(death$Age) - min(death$Age) + 1
year <- 2015 - 1975 + 1

# Death Array
d <- array(NA, c(age, year))

# Exposure Array
e <- array(NA, c(age, year))

# Central Death Rate Array
# m_(x, t) ≈ d_(x,t) / E_(x,t)
m <- array(NA, c(age, year))

# Dataset age incrementer
h <- 1

for (t in 1:year) {
    for (x in 1:age) {
        d[x, t] <- death$Total[h]
        e[x, t] <- exposure$Total[h]
        m[x, t] <- d[x, t] / e[x, t]
        h <- h + 1
    }
}

nom_upper <- 0.9
nom_lower <- 0.1
hids <- 3
epoch <- 100000
testing_years <- c(5, 15, 25)

x <- array(NA, c(2, 100))
mx <- max(log(m[, testing_years]))
mn <- min(log(m[, testing_years]))
t <- (log(m[, testing_years]) - mn) / (mx - mn) * (nom_upper - nom_lower) + nom_lower


n <- age
y <- array(NA, c(hids, n))
z <- numeric()

logistic <- function(s) {
    if (s > 700) {
        logistic <- 1
    }
    if (s <= 700) {
        logistic <- exp(s) / (1 + exp(s))
    }
    logistic
}

# Initialisation

# Error Function
e <- numeric()

# a: weights between input layer and hidden layer
# a: uniform distribution (-1,1)
a <- array(runif(3 * hids, -1, 1), c(3, hids))


# b: {-1, +1}
b <- -1 + rbinom(hids + 1, 1, 0.5) * 2

# tuning parameters
# epsilon: learning rate
epsilona <- array(0.5, c(3, hids))
epsilonb <- rep(0.5, hids + 1)
# lambda, phi, theta: update learning rate
lambda <- 0.1
phi <- 0.5
theta <- 0.7

# hidden nodes; fitted target value (output)
for (k in 1:n) {
    for (j in 1:hids) {
        y[j, k] <- logistic(a[3, j] + a[1, j] * x[1, k] + a[2, j] * x[2, k])
    }
    z[k] <- logistic(b[hids + 1] + sum(b[1:hids] * y[1:hids, k]))
}

# estimation
# epoch: number of iterations
for (h in 1:epoch) {
    # da, db: derivative
    # activation function: logistic function
    # f(s) = exp(s)/(1+exp(s))
    da <- array(0, c(3, hids))
    db <- rep(0, hids + 1)
    # calculate derivatives with respect to a and b
    for (k in 1:n) {
        p <- (z[k] - t[k]) * z[k] * (1 - z[k]) / n
        for (j in 1:hids) {
            db[j] <- db[j] + p * y[j, k]
        }
        db[hids + 1] <- db[hids + 1] + p
        for (j in 1:hids) {
            q <- (z[k] - t[k]) * z[k] * (1 - z[k]) * b[j] * y[j, k] * (1 - y[j, k]) / n
            da[1, j] <- da[1, j] + q * x[1, k]
            da[2, j] <- da[2, j] + q * x[2, k]
            da[3, j] <- da[3, j] + q
        }
    }
    # update learning rate
    if (h > 1) {
        for (i in 1:3) {
            for (j in 1:hids) {
                if (ga[i, j] * da[i, j] > 0) {
                    epsilona[i, j] <- epsilona[i, j] + lambda
                }
                if (ga[i, j] * da[i, j] <= 0) {
                    epsilona[i, j] <- epsilona[i, j] * phi
                }
            }
        }
        for (j in 1:(hids + 1)) {
            if (gb[j] * db[j] > 0) {
                epsilonb[j] <- epsilonb[j] + lambda
            }
            if (gb[j] * db[j] <= 0) {
                epsilonb[j] <- epsilonb[j] * phi
            }
        }
    }
    # update parameters a and b (gradient descent)
    a <- a - epsilona * da
    b <- b - epsilonb * db
    # update accumulated information
    if (h == 1) {
        ga <- da
        gb <- db
    }
    if (h > 1) {
        ga <- theta * ga + (1 - theta) * da
        gb <- theta * gb + (1 - theta) * db
    }
    # update values of hidden nodes and fitted target
    for (k in 1:n) {
        for (j in 1:hids) {
            y[j, k] <- logistic(a[3, j] + a[1, j] * x[1, k] + a[2, j] * x[2, k])
        }
        z[k] <- logistic(b[hids + 1] + sum(b[1:hids] * y[1:hids, k]))
    }
    # calculate error function
    e[h] <- 0.5 * sum((z - t)^2) / n

    if (h %% 10000 == 0) {
        print(paste0("Epoch: ", h))
    }
}

plot(
    c(0:99), (z - 0.1) / 0.8 * (mx - mn) + mn,
    xlab = "age", ylab = "log death rate 2018",
    ylim = c(-10, -2), type = "l", lty = 3
)

lines(c(0:99), (t - 0.1) / 0.8 * (mx - mn) + mn)