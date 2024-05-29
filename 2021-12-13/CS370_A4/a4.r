# Lance Xu 20816215

# data

data <- dataset20816215

# question 1

hashbin <- data$hashtags.binary

length(hashbin)

length(hashbin[hashbin == 1])

theta = length(hashbin[hashbin == 1]) / length(hashbin)

thetazero <- 0.11

thetahat <- theta

d <- abs(thetahat - thetazero) / sqrt(thetazero * (1-thetazero) /905)

1-pnorm(d)

n <- length(bashbin)

lambda<-(-2*log((theta0/thetahat)^(n*thetahat)*((1-theta0)/(1-thetahat))^(n-n*thetahat)))

pchisq(lambda, n - 1)

# question 2

hashtags <- data$hashtags

length(hashtags)

theta <- sum(hashtags) / length(hashtags)

library(MASS)

hist(hashtags)

thetahat <- theta

thetazero <- 2

d <- abs(thetahat - thetazero) / sqrt(thetazero / length(hashtags))

1-pnorm(d)

lambda<-(-2*log((thetazero/thetahat)^(n*thetahat)*exp(n*(thetahat-thetazero))))

# question 3

tweetgap <- data$tweet.gap[data$first.tweet == 0] / 3600

thetahat <- mean(tweetgap)

thetazero <- 2.5

d = abs(thetahat - thetazero) / (thetazero / sqrt(n))

2 * (1-pnorm(d))



lambda<-(-2*log((thetahat/thetazero)^n*exp(n*(1-thetahat/thetazero))))

2 * (1 - pnorm(sqrt(lambda)))

n <- length(tweetgap)

2 * n * thetahat / thetazero

pchisq(thetahat, 2 * n)

# question 4

tweetgap <- data$tweet.gap[data$first.tweet == 1 & data$tweet.gap < 86400
                          & (data$username == "@SimuLiu" | data$username == "jonnysun" | data$username == "@CPHO_Canada")]

tweetgap <- tweetgap / 3600

mean(tweetgap)

sd(tweetgap)

# sknewness function
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}

# kurtosis function
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}

skewness(tweetgap)

kurtosis(tweetgap)

x<-seq(1, length(tweetgap), 1)

qqplot(x, tweetgap, ylim=c(0,30))

u <- 7.12

n <- length(tweetgap)

T <- abs(mean(tweetgap) - u) / sd(tweetgap)

1-pt(T, n - 1)

2.7 * 2.7

U <- (n - 1) * sd(tweetgap) * sd(tweetgap) / 7.29

pchisq(U,n)