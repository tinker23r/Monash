# rm(list = ls())
# ### 9.1 ###
# death <- read.delim(file = "Data/Deaths_1x1.txt", sep = "") %>%
#     filter(1975 <= Year, Year <= 2015, Age != "110+") %>%
#     select(-Female, -Male) %>%
#     mutate(Age = strtoi(Age)) %>%
#     filter(Age < 100)

# exposure <- read.delim(file = "Data/Exposures_1x1.txt", sep = "") %>%
#     filter(1975 <= Year, Year <= 2015, Age != "110+") %>%
#     select(-Female, -Male) %>%
#     mutate(Age = strtoi(Age)) %>%
#     filter(Age < 100)

# age <- 100
# year <- 2015 - 1975 + 1
# d <- array(NA, c(age, year))
# e <- array(NA, c(age, year))
# m <- array(NA, c(age, year + 45))
# # death$Total %>% head()
# h <- 1
# for (t in 1:year) {
#     for (x in 1:age) {
#         d[x, t] <- death$Total[h]
#         e[x, t] <- exposure$Total[h]
#         m[x, t] <- d[x, t] / e[x, t]
#         h <- h + 1
#     }
#     # h <- h + 11
# }

# # a
# # ax
# # bx
# # kt
# a <- numeric()
# for (x in 1:age) {
#     a[x] = mean(log(m[x, 1:year]))
# }

# info <- array(NA, c(age, year))
# # log(m[x,t] - a[x])
# # dmeaned mortality data
# for (x in 1:age) {
#     for (t in 1:year) {
#         info[x, t] <- log(m[x, t]) - a[x]
#     }
# }

# pca <- svd(info, 1, 1)
# # X = UDV'
# # U: orthogonal matrix
# # V: orthogonal matrix
# # D: diagonal matrix
# b <- pca$u / sum(pca$u)
# k <- sum(pca$u) * pca$d[1] * pca$v

# # b
# plot(c(0:99), a, xlab = "age", ylab = "a(x)", xlim = c(0, 100), type = "l")
# plot(c(0:99), b, xlab = "age", ylab = "b(x)", xlim = c(0, 100), type = "l")
# plot(c(1975:2015), k[1:year], xlab = "year", ylab = "k(t)", xlim = c(1970, 2020), type = "l")

# # c
# # point forecast
# # k[year+t] = mu + k[year+t-1] + e[t]
# # log(m[x,year+t]) = a[x] + b[x]k[year+t]
# # m[x,year+t] = exp(a[x] + b[x]k[year+t])
# mu <- (k[year] - k[1]) / (year - 1)
# for (t in 1:32) {
#     k[year + t] <- k[year + t - 1] + mu
#     for (x in 1:age) {
#         m[x, year + t] <- exp(a[x] + b[x] * k[year + t])
#     }
# }

# # interval forecast/prediction interval
# # k[t] - k[t-1] = mu + e[t]
# # var(k[t] - k[t-1]) = var(mu + e[t]) = var(e[t])
# # sd(c(k[2]-1k[1], k[3]-k[2], ......, k[49]-k[48]))
# sigma <- sd(k[2:year] - k[1:(year - 1)])

# mf <- array(NA, c(age, year + 45, 1000))
# kf <- array(NA, c(year + 45, 1000))
# # 1000 simulations
# # rnorm(1): simulated error with mean 0 and variance 1
# # sigma*rnorm(1)
# for (z in 1:1000) {
#     mf[1:age, 1:year, z] <- m[1:age, 1:year]
#     kf[1:year, z] <- k[1:year]
#     for (t in 1:45) {
#         kf[year + t, z] <- kf[year + t - 1, z] + mu + sigma * rnorm(1)
#         for (x in 1:age) {
#             mf[x, year + t, z] <- exp(a[x] + b[x] * kf[year + t, z])
#         }
#     }
# }

# #  30
# # 95% preidciton interval
# # 97.5% quantile: upper end
# # 2.5% quantile: loweer end
# upper <- numeric()
# lower <- numeric()
# for (t in 1:32) {
#     upper[t] <- quantile(mf[31, year + t, 1:1000], 0.975)
#     lower[t] <- quantile(mf[31, year + t, 1:1000], 0.025)
# }

# plot(c(1970:2018), log(m[31, 1:year]), xlab = "year", ylab = "log m(30,t)", xlim = c(1970, 2050), type = "l", ylim = c(-9, -7))
# lines(c(2019:2050), log(m[31, (year + 1):(year + 32)]), lty = 2, col = 2)
# lines(c(2019:2050), log(upper), lty = 3, col = 2)
# lines(c(2019:2050), log(lower), lty = 3, col = 2)

# # 65
# upper <- numeric()
# lower <- numeric()
# for (t in 1:32) {
#     upper[t] <- quantile(mf[66, year + t, 1:1000], 0.975)
#     lower[t] <- quantile(mf[66, year + t, 1:1000], 0.025)
# }

# plot(c(1970:2018), log(m[66, 1:year]), xlab = "year", ylab = "log m(30,t)", xlim = c(1970, 2050), type = "l", ylim = c(-6.8, -3.8))
# lines(c(2019:2050), log(m[66, (year + 1):(year + 32)]), lty = 2, col = 2)
# lines(c(2019:2050), log(upper), lty = 3, col = 2)
# lines(c(2019:2050), log(lower), lty = 3, col = 2)

rm(list = ls())
### 9.2 ###
raw <- read.table("Australia Data.txt", header = TRUE)
age <- 30
year <- 49
d <- array(NA, c(age, year))
e <- array(NA, c(age, year))
m <- array(NA, c(age, year))
q <- array(NA, c(age, year + 32))
h <- 61
for (t in 1:year) {
    for (x in 1:age) {
        d[x, t] <- raw$dxMale[h]
        e[x, t] <- raw$exMale[h]
        m[x, t] <- d[x, t] / e[x, t]
        q[x, t] <- 1 - exp(-m[x, t])
        h <- h + 1
    }
    h <- h + 81
}

# a
xbar <- mean(c(60:89))
co <- c(60:89) - xbar
kappa_1 <- numeric()
kappa_2 <- numeric()
for (t in 1:year) {
    fit <- lm(log(q[1:age, t] / (1 - q[1:age, t])) ~ co)
    kappa_1[t] <- fit$coef[1]
    kappa_2[t] <- fit$coef[2]
}

# b
# logit(q[x,t]) = k1[t] + k2[t](x-xbar) + error[x,t]
plot(c(1970:2018), kappa_1[1:year], xlab = "year", ylab = "k1(t)", xlim = c(1970, 2020), type = "l")
plot(c(1970:2018), kappa_2[1:year], xlab = "year", ylab = "k2(t)", xlim = c(1970, 2020), type = "l")

# c
# bivaraite random walk with drift
# k1[t] = mu1 + k1[t-1] + e1[t]
# k2[t] = mu2 + k2[t-1] + e2[t]
# cov(e1[t], e2[t]) exists
mu1 <- (kappa_1[year] - kappa_1[1]) / (year - 1)
mu2 <- (kappa_2[year] - kappa_2[1]) / (year - 1)

# point forecast
# y = logit(q[x,t]) = log(q[x,t]/(1-q[x,t]))
# q[x,t] = exp(y)/(1+exp(y))
for (t in 1:32) {
    kappa_1[year + t] <- kappa_1[year + t - 1] + mu1
    kappa_2[year + t] <- kappa_2[year + t - 1] + mu2
    for (x in 1:age) {
        temp <- exp(kappa_1[year + t] + kappa_2[year + t] * (x + 59 - xbar))
        q[x, year + t] <- temp / (1 + temp)
    }
}

# prediction interval
# k1[t] - k1[t-1] - mu1 = e1[t]
# k2[t] - k2[t-1] - mu2 = e2[t]
# cov(e1[t],e2[t]) = cov(k1[t] - k1[t-1] - mu1, k2[t] - k2[t-1] - mu2) = cov(k1[t] - k1[t-1], k2[t] - k2[t-1])
# corr(e1[t],e2[t]) = corr(k1[t] - k1[t-1], k2[t] - k2[t-1])
sigma1 <- sd(kappa_1[2:year] - kappa_1[1:(year - 1)])
sigma2 <- sd(kappa_2[2:year] - kappa_2[1:(year - 1)])
sigma12 <- cor(kappa_1[2:year] - kappa_1[1:(year - 1)], kappa_2[2:year] - kappa_2[1:(year - 1)])

qf <- array(NA, c(age, year + 32, 1000))
k1f <- array(NA, c(year + 32, 1000))
k2f <- array(NA, c(year + 32, 1000))
for (z in 1:1000) {
    qf[1:age, 1:year, z] <- q[1:age, 1:year]
    k1f[1:year, z] <- kappa_1[1:year]
    k2f[1:year, z] <- kappa_2[1:year]
    for (t in 1:32) {
        z1 <- rnorm(1)
        z2 <- sigma12 * z1 + sqrt(1 - sigma12^2) * rnorm(1)
        k1f[year + t, z] <- k1f[year + t - 1, z] + mu1 + sigma1 * z1
        k2f[year + t, z] <- k2f[year + t - 1, z] + mu2 + sigma2 * z2
        for (x in 1:age) {
            temp <- exp(k1f[year + t, z] + k2f[year + t, z] * (x + 59 - xbar))
            qf[x, year + t, z] <- temp / (1 + temp)
        }
    }
}

upper <- numeric()
lower <- numeric()
for (t in 1:45) {
    upper[t] <- quantile(qf[6, year + t, 1:1000], 0.95)
    lower[t] <- quantile(qf[6, year + t, 1:1000], 0.05)
}

# d
plot(c(1975:2015), log(q[6, 1:year]), xlab = "year", ylab = "log q(60,t)", xlim = c(1970, 2050), type = "l", ylim = c(-6.5, -3))
lines(c(2016:2060), log(q[6, (year + 1):(year + 45)]), lty = 2, col = 2)
lines(c(2016:2060), log(upper), lty = 3, col = 2)
lines(c(2016:2060), log(lower), lty = 3, col = 2)