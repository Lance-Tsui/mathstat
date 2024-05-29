# Lance Xu

# function of skewness
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}

# function of kurtosis
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}

#5.b

dataset20816215$tweet.gap.hour <- dataset20816215$tweet.gap / 3600

mean(dataset20816215$tweet.gap.hour)

mean(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])

sd(dataset20816215$tweet.gap.hour)

sd(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])

skewness(dataset20816215$tweet.gap.hour)

skewness(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])

summary(dataset20816215$tweet.gap.hour)

summary(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])

#5.c
library(MASS)

truehist(dataset20816215$tweet.gap.hour, xlab = "gap between tweets (in hour)", ylab = "Relative Frequency", main = "Relative frequency histogram of hours between tweets", las = 1, col = "dodgerblue3", nbins = 30)

curve(dexp(x, 1/mean(dataset20816215$tweet.gap.hour)), col = "red", add = TRUE, lwd = 2.5)

truehist(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0], xlab = "gap between tweets (in hour, excluding first of day)", ylab = "Relative Frequency", main = "Relative frequency histogram of hours between tweets, excluding first of day", las = 1, col = "dodgerblue3", nbins = 30)

curve(dexp(x, 1/mean(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])), col = "red", add = TRUE, lwd = 2.5)

#5.e

plot(ecdf(dataset20816215$tweet.gap.hour), xlab = "gap between tweets (in hour)", ylab = "Cumulative Probability", main = "ecdf of gap between tweets (in hour)", las = 1, col = "navy", lwd = 2, pch = NA, xlim = c(0, max(dataset20816215$tweet.gap.hour)))

curve(pexp(x, 1/mean(dataset20816215$tweet.gap.hour)), col = "red", add = TRUE, lwd = 2, lty = 2)

legend("bottomright", legend = c("ecdf", "CDF"), col = c("navy", "red"), lty = c(1, 2))

plot(ecdf(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0]), xlab = "gap between tweets (in hour, excluding first of day)", ylab = "Cumulative Probability", main = "ecdf of gap between tweets (in hour, excluding first of day)", las = 1, col = "navy", lwd = 2, pch = NA, xlim = c(0, max(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])))

curve(pexp(x, 1/mean(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])), col = "red", add = TRUE, lwd = 2, lty = 2)

legend("bottomright", legend = c("ecdf", "CDF"), col = c("navy", "red"), lty = c(1, 2))

#5.g
ExpRLF <- function(theta, n, thetahat) {exp(n*log(thetahat/theta) + n*(1-thetahat/theta))}

n <- length(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])

thetahat <- mean(dataset20816215$tweet.gap.hour[dataset20816215$first.tweet == 0])

theta <-seq(1.5, 3, 0.001)

plot(theta, ExpRLF(theta, n, thetahat) , xlab = expression(theta), ylab = expression(paste("R(",theta,")")), type = "l", lwd = 2, , main = "Exponential relative likelihood function", las = 1)

#6.b
plot(dataset20816215$retweets, dataset20816215$likes, xlab = "number of retweets", ylab = "number of likes", main = "comparison between number of retweets and likes", pch = 1, cex = 0.5, col = "navy")

#6.c
retweets.log <‐ log(dataset20816215$retweets + 1) # log is the natural log

likes.log <‐ log(dataset20816215$likes + 1)

#6.d
plot(retweets.log, likes.log, xlab = "number of log(retweets)", ylab = "number of log(likes)", main = "comparison between number of log(retweets) and log(likes)", pch = 1, cex = 0.5, col = "navy")

#6.e
cor(retweets.log,likes.log)