##### 10.1 #####
setwd("/Users/stranger/Desktop/Monash/Miscellanea/Tutoring/2022_S1/ETC3430/Week 10")
raw <- read.table("Australia Data.txt", header = TRUE)
age <- 90
year <- 49
d <- array(NA, c(age, year))
e <- array(NA, c(age, year))
m <- array(NA, c(age, year))
h <- 1
for (t in 1:year) {
  for (x in 1:age) {
    d[x, t] <- raw$dxMale[h]
    e[x, t] <- raw$exMale[h]
    m[x, t] <- d[x, t] / e[x, t]
    h <- h + 1
  }
  h <- h + 21
}

# normalizaiton
# neural network is sensitive to scale
x <- c(1:90) / 100
mx <- max(log(m[, year]))
mn <- min(log(m[, year]))
t <- (log(m[, year]) - mn) / (mx - mn) * (0.9 - 0.1) + 0.1

# y: hidden nodes
n <- 90
hids <- 1
epoch <- 100000
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

e <- numeric()
# a: parameters between input layer and hidden layer
a <- array(NA, c(2, hids))
# intialization of parameters
# a: uniform distribution (-1,1)
for (i in 1:2) {
  for (j in 1:hids) {
    a[i, j] <- runif(1, -1, 1)
  }
}
# b: {-1, +1}
b <- -1 + rbinom(hids + 1, 1, 0.5) * 2
# b = sample(c(-1,1),hids+1)

# tuning parameters
# epsilon: learning rate
epsilona <- array(0.5, c(2, hids))
epsilonb <- rep(0.5, hids + 1)
# lambda, phi, theta: update learning rate
lambda <- 0.1
phi <- 0.5
theta <- 0.7

# hidden nodes; fitted target value (output)
for (k in 1:n) {
  for (j in 1:hids) {
    y[j, k] <- logistic(a[2, j] + a[1, j] * x[k])
  }
  z[k] <- logistic(b[hids + 1] + sum(b[1:hids] * y[1:hids, k]))
}

# estimation
# epoch: number of iterations
for (h in 1:epoch) {
  # da, db: derivative
  # activation function: logistic function
  # f(s) = exp(s)/(1+exp(s))
  da <- array(0, c(2, hids))
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
      da[1, j] <- da[1, j] + q * x[k]
      da[2, j] <- da[2, j] + q
    }
  }
  # update learning rate
  if (h > 1) {
    for (i in 1:2) {
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
      y[j, k] <- logistic(a[2, j] + a[1, j] * x[k])
    }
    z[k] <- logistic(b[hids + 1] + sum(b[1:hids] * y[1:hids, k]))
  }
  # calculate error function
  e[h] <- 0.5 * sum((z - t)^2) / n

  if (h %% 10000 == 0) {
    print(paste0("Epoch: ", h))
  }
}

plot(c(0:89), (z - 0.1) / 0.8 * (mx - mn) + mn, xlab = "age", ylab = "male log death rate 2018", ylim = c(-10, -2), type = "l", lty = 3)
lines(c(0:89), (t - 0.1) / 0.8 * (mx - mn) + mn)

##### 10.2 #####
raw <- read.table("Individual Claims.txt", header = TRUE)
x <- array(NA, c(2, 100))
t <- numeric()
for (k in 1:100) {
  t[k] <- raw$Line[k]
  x[1, k] <- raw$Claim[k] / 60000
  x[2, k] <- raw$Time[k] / 5
}

n <- 100
hids <- 5
epoch <- 100000
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

e <- numeric()
a <- array(NA, c(3, hids))
for (i in 1:3) {
  for (j in 1:hids) {
    a[i, j] <- runif(1, -1, 1)
  }
}
b <- -1 + rbinom(hids + 1, 1, 0.5) * 2
epsilona <- array(0.5, c(3, hids))
epsilonb <- rep(0.5, hids + 1)
lambda <- 0.1
phi <- 0.5
theta <- 0.7
for (k in 1:n) {
  for (j in 1:hids) {
    y[j, k] <- logistic(a[3, j] + a[1, j] * x[1, k] + a[2, j] * x[2, k])
  }
  z[k] <- logistic(b[hids + 1] + sum(b[1:hids] * y[1:hids, k]))
}

for (h in 1:epoch) {
  da <- array(0, c(3, hids))
  db <- rep(0, hids + 1)
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

  a <- a - epsilona * da
  b <- b - epsilonb * db
  if (h == 1) {
    ga <- da
    gb <- db
  }
  if (h > 1) {
    ga <- theta * ga + (1 - theta) * da
    gb <- theta * gb + (1 - theta) * db
  }
  for (k in 1:n) {
    for (j in 1:hids) {
      y[j, k] <- logistic(a[3, j] + a[1, j] * x[1, k] + a[2, j] * x[2, k])
    }
    z[k] <- logistic(b[hids + 1] + sum(b[1:hids] * y[1:hids, k]))
  }
  e[h] <- 0.5 * sum((z - t)^2) / n

  if (h %% 10000 == 0) {
    print(paste0("Epoch: ", h))
  }
}

plot(c(1:epoch), e, xlab = "epoch", ylab = "error", type = "l")
plot(x[1, 1:50] * 60000, x[2, 1:50] * 5,
  xlab = "Claim Size", ylab = "Time to Settlement",
  xlim = c(0, 60000), ylim = c(0, 5), pch = 16, col = 4
)
points(x[1, 51:100] * 60000, x[2, 51:100] * 5, pch = 16, col = 2)

temp1 <- numeric()
temp2 <- numeric()
temp3 <- numeric()
temp4 <- numeric()
for (k in 1:100) {
  if (z[k] < 0.5) {
    temp1 <- c(temp1, x[1, k])
    temp2 <- c(temp2, x[2, k])
  }
  if (z[k] > 0.5) {
    temp3 <- c(temp3, x[1, k])
    temp4 <- c(temp4, x[2, k])
  }
}
plot(c(1:epoch), e, xlab = "epoch", ylab = "error", type = "l")

par(mfrow = c(1, 2))
plot(x[1, 1:50] * 60000, x[2, 1:50] * 5,
  xlab = "Claim Size", ylab = "Time to Settlement",
  xlim = c(0, 60000), ylim = c(0, 5), pch = 16, col = 4
)
points(x[1, 51:100] * 60000, x[2, 51:100] * 5, pch = 16, col = 2)

plot(temp1 * 60000, temp2 * 5,
  xlab = "Claim Size", ylab = "Time to Settlement",
  xlim = c(0, 60000), ylim = c(0, 5), pch = 16, col = 4
)
points(temp3 * 60000, temp4 * 5, pch = 16, col = 2)