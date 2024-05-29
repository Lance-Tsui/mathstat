# Lance Xu 20816215

# plan
library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")

dataset <- midtermdataset20816215

nrow(dataset)

# we get 8 province accounts
unique(dataset$username)

# number of sample tweets from @sante_qc
length(dataset$username[dataset$username == "@sante_qc"])

# number of sample tweets from @ONThealth
length(dataset$username[dataset$username == "@ONThealth"])

# number of sample tweets from @GoAHealth
length(dataset$username[dataset$username == "@GoAHealth"])

# number of sample tweets from @PHSAofBC
length(dataset$username[dataset$username == "@PHSAofBC"])

# number of sample tweets from @SaskHealth
length(dataset$username[dataset$username == "@SaskHealth"])

# number of sample tweets from @HealthNS
length(dataset$username[dataset$username == "@HealthNS"])

# number of sample tweets from @Health_PEI
length(dataset$username[dataset$username == "@Health_PEI"])

# number of sample tweets from @HCS_GovNL
length(dataset$username[dataset$username == "@HCS_GovNL"])

rm(list = ls())

# analysis part 1

library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")

dataset <- midtermdataset20816215

nrow(dataset)

datasetbin <- dataset$urls.binary

# total number --> 939
n <- length(datasetbin)

# number of urls.binary == 1 --> 368
length(datasetbin[datasetbin == 1])

# theta
theta <- length(datasetbin[datasetbin == 1]) / length(datasetbin)

y<-rbinom(1,n,theta)    # observation from Binomial(n,theta) distribution

# thetahat
thetahat <- theta

# binomial relative likelihood function
# please correct BinomRLF
BinomRLF <- function(x)(((x/thetahat)^y) * ((1-x)/(1-thetahat))^(n-y))


xvalue<-seq(0.33,0.45,0.001)
plot(xvalue,BinomRLF(xvalue),xlab="theta",ylab="Rtheta",type="l",lwd=1.5)    # plot relative likelihood function

# draw a horizontal line at 0.15
abline(a = 0.15,b=0,col="red",lwd=2)

# smaller root
uniroot(function(x) BinomRLF(x)-0.15,lower=0.35,upper=0.36)

# larger root
uniroot(function(x) BinomRLF(x)-0.15,lower=0.42,upper=0.43)
        
thetazero <- 0.19

# test statistic
d <- (thetahat - thetazero) / sqrt(thetazero * (1 - thetazero) / n)
        
2 * (1-pnorm(d))
        

# clear temp variable
rm(list = ls())
        
# analysis part 2
library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")        
        
dataset <- midtermdataset20816215

dataset$time.of.day.hour <- dataset$time.of.day / 3600
        
ont <- dataset$time.of.day.hour[dataset$username == "@ONThealth"]

goa <- dataset$time.of.day.hour[dataset$username == "@GoAHealth"]
        
qqnorm(ont, main = "qqplot of time.of.day.hour in ONThealth", xlab = "x", ylab = "time.of.day.hour of ONThealth", pch = 1, col = "navy", cex = 0.5, las = 1)

qqline(ont, lty = 2, col = "red", lwd = 2)
        
qqnorm(goa, main = "qqplot of time.of.day.hour in GoAHealth", xlab = "x", ylab = "time.of.day.hour of GoAHealth", pch = 1, col = "navy", cex = 0.5, las = 1)

qqline(goa, lty = 2, col = "red", lwd = 2)

summary(ont)
        
summary(goa)
        
# sample size --> 195        
length(ont)
       
# sample size --> 150
length(goa)

# sknewness function
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}

# kurtosis function
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}

skewness(ont)
kurtosis(ont)
sd(ont)
skewness(goa)
kurtosis(goa)
sd(goa)
lont <- length(ont)
lgoa <- length(goa)
        
# sample variance
sont <- sd(ont) * sd(ont) * lont / (lont - 1)
sgoa <- sd(goa) * sd(goa) * lgoa / (lgoa - 1)
        
        
# 99% confidence interval
# degree of freedom --> n - 1
# 194 degree of freedom for ON
# 149 degree of freedom for AB
qchisq(0.005, lont - 1)
qchisq(0.995, lont - 1)
sqrt((lont - 1) * sont / qchisq(0.005, lont - 1))
sqrt((lont - 1) * sont / qchisq(0.995, lont - 1))
qchisq(0.005, lgoa - 1)
qchisq(0.005, lgoa - 1)
sqrt((lgoa - 1) * sgoa / qchisq(0.005, lgoa - 1))
sqrt((lgoa - 1) * sgoa / qchisq(0.995, lgoa - 1))

# point estimate difference
pointes <- mean(ont) - mean(goa)

t.test(ont,goa,mu=0,var.equal=TRUE, conf.level=0.95)
        
        
# clear temp variable
rm(list = ls())
       
        
        
        
        
        
        
        
# analysis part 3
library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")        
        
dataset <- midtermdataset20816215

# retweets.log
dataset$retweets.log <- log(dataset$retweets + 1)

# likes.log
dataset$likes.log <- log(dataset$likes + 1)
    
mod <- lm(likes.log ~ retweets.log, data = dataset)

# x axis --> retweets.log
# y axis --> likes.log
# a scatterplot of the data including the fitted line
plot(dataset$retweets.log, dataset$likes.log, xlab = "log(retweets + 1)", ylab = "log(likes + 1)", main = "Scatterplot of log(likes + 1) and log(retweets + 1)", pch = 1, cex = 0.5, col = "navy", las = 1, lwd = 1)
        
abline(coef(mod), lwd = 2, lty = 2, col = "red") # add fitted line


# standardized residuals
stdres <- rstandard(mod) # store standardized residuals
mean(stdres) # check mean
sd(stdres) # check standard deviation
# set up plot window to have 1 row of 3 plots
par(mfrow = c(1, 2))

# standardized residuals vs fitted values
plot(fitted(mod), stdres, main = "Std residuals vs. fitted values", xlab = "Fitted values", ylab = "Standardized Residuals", pch = 1, col = "navy", cex = 0.5, las = 1)
abline(h = 0, lty = 2, col = "red", lwd = 2)
# qqplot of standardized residuals
qqnorm(stdres, main = "qqplot of std residuals", xlab = "G(0, 1) Quantiles", ylab = "Standardized Residuals", pch = 1, col = "navy", cex = 0.5, las = 1)
qqline(stdres, lty = 2, col = "red", lwd = 2)
        
# 95% confidence interval for beta
confint(mod)

# prediction interval
predict(mod, data.frame("retweets.log" = log(30 + 1)), interval = "prediction", level = 0.9)

exp(3.844212) - 1
    
exp(2.558138) - 1

exp(5.130285) - 1


        
# clear temp variable
rm(list = ls())
        
        
# analysis part 4
library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")        
        
dataset <- midtermdataset20816215
        
longwds <- dataset$long.words
        
# number of tweets in the study population
n <- length(longwds)

# 95% confidence interval
a <- qnorm(0.975)

# thetahat
thetahat <- sum(longwds) / n

thetahat - a * sqrt(thetahat / n)

thetahat + a * sqrt(thetahat / n)



expected <- dpois(0:7, thetahat) * n

round(expected, 2)

e8 <- n - sum(expected)

expected <- append(expected, e8)
        
expected[8] <- expected[8] + expected[9]
        
expected <- expected[-length(expected)]

longwds[longwds == 7] <- "6+"

observed <- table(longwds)
        
observed[8] <- observed[8] + observed[9]
        
observed <- observed[-length(observed)]

cbind(observed, expected)
     
# test statistic
d <- 2 * (120 * log(120/76.38) + 190 * log(190/191.65) + 179 * log(179/240.43) + 179 * log(179/201.08)
         + 157 * log(157/126.13) + 65 * log(65/63.29) + 29 * log(29/26.47) + 20 * log(20/13.56))
        
# degree of freedom is 7

1 - pchisq(d, df = 7)
        
barplot(rbind(observed, expected), beside = T, col = c("dodgerblue3", "darkorchid"), density = 75, angle = c(45,135), ylim = c(0,250), las = 1, xlab = "Tweets", ylab = "Number of Long Words")

legend("topright", legend = c("Observed", "Expected"), fill = c("dodgerblue3", "darkorchid"), density = 75, angle = c(45,135))

# at least 2 long words
1 - dpois(0, thetahat) - dpois(1, thetahat)

longwds <- dataset$long.words


# clear temp variable
rm(list = ls())
        
# analysis part 5
library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")        
        
dataset <- midtermdataset20816215        

library(MASS)
        
# observed table of frequencies
tbl <- table(dataset$hashtags.binary, dataset$media.binary)
        

chisq.test(tbl)
 
# clear temp variable
rm(list = ls())
        
# analysis part 6
library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")        
        
dataset <- midtermdataset20816215        
        

        
# maximum likelihood estimate of theta1
# GoAHealth --> observed 133; total 150
sum(dataset$is.retweet[dataset$username == "@GoAHealth"])
length(dataset$is.retweet[dataset$username == "@GoAHealth"])
theta1 <- sum(dataset$is.retweet[dataset$username == "@GoAHealth"]) / length(dataset$is.retweet[dataset$username == "@GoAHealth"])

# maximum likelihood estimate of theta2
# HCS_GovNL --> observed 96; total 121
sum(dataset$is.retweet[dataset$username == "@HCS_GovNL"])
length(dataset$is.retweet[dataset$username == "@HCS_GovNL"])
theta2 <- sum(dataset$is.retweet[dataset$username == "@HCS_GovNL"]) / length(dataset$is.retweet[dataset$username == "@HCS_GovNL"])

# maximum likelihood estimate of theta3
# Health_PEI --> observed 2; total 41
sum(dataset$is.retweet[dataset$username == "@Health_PEI"])
length(dataset$is.retweet[dataset$username == "@Health_PEI"])
theta3 <- sum(dataset$is.retweet[dataset$username == "@Health_PEI"]) / length(dataset$is.retweet[dataset$username == "@Health_PEI"])

# maximum likelihood estimate of theta4
# HealthNS --> observed 43; total 146
sum(dataset$is.retweet[dataset$username == "@HealthNS"])
length(dataset$is.retweet[dataset$username == "@HealthNS"])
theta4 <- sum(dataset$is.retweet[dataset$username == "@HealthNS"]) / length(dataset$is.retweet[dataset$username == "@HealthNS"])

# maximum likelihood estimate of theta5
# ONThealth --> observed 159; total 195
sum(dataset$is.retweet[dataset$username == "@ONThealth"])
length(dataset$is.retweet[dataset$username == "@ONThealth"])
theta5 <- sum(dataset$is.retweet[dataset$username == "@ONThealth"]) / length(dataset$is.retweet[dataset$username == "@ONThealth"])

# maximum likelihood estimate of theta6
# PHSAofBC --> observed 57; total 127
sum(dataset$is.retweet[dataset$username == "@PHSAofBC"]) 
length(dataset$is.retweet[dataset$username == "@PHSAofBC"]) 
theta6 <- sum(dataset$is.retweet[dataset$username == "@PHSAofBC"]) / length(dataset$is.retweet[dataset$username == "@PHSAofBC"])

# maximum likelihood estimate of theta7
# sante_qc --> observed 1; total 36
sum(dataset$is.retweet[dataset$username == "@sante_qc"])
length(dataset$is.retweet[dataset$username == "@sante_qc"])
theta7 <- sum(dataset$is.retweet[dataset$username == "@sante_qc"]) / length(dataset$is.retweet[dataset$username == "@sante_qc"])

# maximum likelihood estimate of theta8
# SaskHealth --> observed 32; total 123
sum(dataset$is.retweet[dataset$username == "@SaskHealth"])
length(dataset$is.retweet[dataset$username == "@SaskHealth"])
theta8 <- sum(dataset$is.retweet[dataset$username == "@SaskHealth"]) / length(dataset$is.retweet[dataset$username == "@SaskHealth"])

        

# maximum likelihood estimate of theta
theta <- sum(dataset$is.retweet) / length(dataset$is.retweet)
        
        
# degree of freedom is 7


# square function    
square <- function(x) {x * x}
        
# test statistic
D <- square(133 - 150 * theta) / (150 * theta) + square(96 - 121 * theta) / (121 * theta) + square(2 - 41 * theta) / (41 * theta) + square(43 - 146 * theta) / (146 * theta) + square(159 - 195 * theta) / (195 * theta) + square(57 - 127 * theta) / (127 * theta) + square(1 - 36 * theta) / (36 * theta) + square(32 - 123 * theta) / (123 * theta)

1 - pchisq(D, df = 7)
        


# clear temp variable
rm(list = ls())
        
# conclusion
library(readr)

midtermdataset20816215 <- read_csv("C:/Users/12269/Downloads/midtermdataset20816215.csv")        
        
dataset <- midtermdataset20816215
        
        datasetbin <- dataset$urls.binary

# theta
theta <- length(datasetbin[datasetbin == 1]) / length(datasetbin)
        
1 - pbinom(4, size = 10, prob = theta)