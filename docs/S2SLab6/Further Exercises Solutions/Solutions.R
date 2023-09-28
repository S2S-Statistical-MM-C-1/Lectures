#sample from Poisson distribution
m <- 10000

means_pois_2 <- numeric(m)
means_pois_5 <- numeric(m)
means_pois_10 <- numeric(m)
means_pois_50 <- numeric(m)
means_pois_100 <- numeric(m)

for(i in 1:m){
  means_pois_2[i] <- mean(rpois(n = 2, lambda = 3))
}

for(i in 1:m){
  means_pois_5[i] <- mean(rpois(n = 5, lambda = 3))
}

for(i in 1:m){
  means_pois_10[i] <- mean(rpois(n = 10, lambda = 3))
}

for(i in 1:m){
  means_pois_50[i] <- mean(rpois(n = 50, lambda = 3))
}

for(i in 1:m){
  means_pois_100[i] <- mean(rpois(n = 100, lambda = 3))
}

means <- data.frame(mean = c(means_pois_2, means_pois_5, means_pois_10,
                             means_pois_50, means_pois_100),
                    size = rep(c("n = 2", "n = 5", "n = 10", "n = 50", "n = 100"), each = m))

means$size <- factor(means$size, levels = c("n = 2", "n = 5", "n = 10", "n = 50", "n = 100"))

library(ggplot2)

ggplot(data = subset(means, subset = (size == "n = 2"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(0, 8)) +
  labs(title = "Sample size n = 2",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)/sqrt(2)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means, subset = (size == "n = 5"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(0, 8)) +
  labs(title = "Sample size n = 5",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)/sqrt(5)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means, subset = (size == "n = 10"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(1, 6)) +
  labs(title = "Sample size n = 10",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)/sqrt(10)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means, subset = (size == "n = 50"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(2, 4)) +
  labs(title = "Sample size n = 50",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)/sqrt(50)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means, subset = (size == "n = 100"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(2, 4)) +
  labs(title = "Sample size n = 100",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 3, sd = sqrt(3)/sqrt(100)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)



#sample from Binomial distribution Bi(10, 0.5)
m <- 10000

means_binom_2 <- numeric(m)
means_binom_5 <- numeric(m)
means_binom_10 <- numeric(m)
means_binom_50 <- numeric(m)
means_binom_100 <- numeric(m)

for(i in 1:m){
  means_binom_2[i] <- mean(rbinom(n = 2, size = 16, prob = 0.5))
}

for(i in 1:m){
  means_binom_5[i] <- mean(rbinom(n = 5, size = 16, prob = 0.5))
}

for(i in 1:m){
  means_binom_10[i] <- mean(rbinom(n = 10, size = 16, prob = 0.5))
}

for(i in 1:m){
  means_binom_50[i] <- mean(rbinom(n = 50, size = 16, prob = 0.5))
}

for(i in 1:m){
  means_binom_100[i] <- mean(rbinom(n = 100, size = 16, prob = 0.5))
}

means_binom <- data.frame(mean = c(means_binom_2, means_binom_5, means_binom_10,
                             means_binom_50, means_binom_100),
                          size = rep(c("n = 2", "n = 5", "n = 10", "n = 50", "n = 100"), each = m))

means_binom$size <- factor(means$size, levels = c("n = 2", "n = 5", "n = 10", "n = 50", "n = 100"))

library(ggplot2)

ggplot(data = subset(means_binom, subset = (size == "n = 2"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(4, 12)) +
  labs(title = "Sample size n = 2",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 8, sd = 2/sqrt(2)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means_binom, subset = (size == "n = 5"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(5, 11)) +
  labs(title = "Sample size n = 5",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 8, sd = 2/sqrt(5)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means_binom, subset = (size == "n = 10"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(6, 10)) +
  labs(title = "Sample size n = 10",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 8, sd = 2/sqrt(10)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means_binom, subset = (size == "n = 50"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(7, 9)) +
  labs(title = "Sample size n = 50",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 8, sd = 2/sqrt(50)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)

ggplot(data = subset(means_binom, subset = (size == "n = 100"))) +
  geom_density(aes(x = mean), fill = "orange2") +
  coord_cartesian(xlim = c(7.5, 8.5)) +
  labs(title = "Sample size n = 100",
       x = expression(bar(x)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 8, sd = 2/sqrt(100)), geom = "area",
                col = "black", linetype = 2, fill = "black", alpha = 0.2)



#\bar X-\bar Y
1-pnorm(2000, mean = 0, sd = sqrt((12000^2/40)+(20000^2/22)))

m <- 15000
nx <- 40
ny <- 22

meansX <- numeric(m)
meansY <- numeric(m)

for(i in 1:m){
  meansX[i] <- mean(rnorm(n = nx, mean = 35000, sd = 12000))
}

for(i in 1:m){
  meansY[i] <- mean(rnorm(n = ny, mean = 35000, sd = 20000))
}

XY <- data.frame(diff = meansX - meansY)

ggplot(data = XY) +
  geom_density(aes(x = diff), fill = "hotpink") +
  labs(x = expression(bar(X)-bar(Y)), y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt((12000^2/40)+(20000^2/22))),
                geom = "area", col = "black", linetype = 2, fill = "black", alpha = 0.2)

mean(XY$diff > 2000)























