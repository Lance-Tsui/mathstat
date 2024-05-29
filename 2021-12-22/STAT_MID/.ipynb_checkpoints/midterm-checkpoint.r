# Lance Xu 20816215

# read data
dataset20816215 <- midtermdataset20816215

# number of retweets
mean(dataset20816215$retweets)

# histogram
hist(dataset20816215$retweets, col="lightblue", ylim=c(0,200),breaks=500)

# (1)
# keywd vaccine
length(dataset20816215$vaccine[dataset20816215$vaccine == 1]) / length(dataset20816215$vaccine)

# keywd health
length(dataset20816215$health[dataset20816215$health == 1]) / length(dataset20816215$health)

# keywd covid
length(dataset20816215$covid[dataset20816215$covid == 1]) / length(dataset20816215$covid)

# mean number of number of retweets which contains keyword covid
mean(dataset20816215$retweets[dataset20816215$covid == 1])

# mean number of number of retweets which contains keyword health
mean(dataset20816215$retweets[dataset20816215$health == 1])

# mean number of number of retweets which contains keyword vaccine
mean(dataset20816215$retweets[dataset20816215$vaccine == 1])

# contains keyword vaccine
vacretweet <- dataset20816215$retweets[dataset20816215$vaccine == 1]

n <- length(vacretweet)

theta<-length(dataset20816215$vaccine[dataset20816215$vaccine == 1]) / length(dataset20816215$vaccine)


y<-rbinom(1,n,theta)    # observation from Binomial(n,theta) distribution

thetahat<-length(dataset20816215$vaccine[dataset20816215$vaccine == 1]) / length(dataset20816215$vaccine)

# binomial relative likelihood function
BinomRLF <- function(x) {exp(y*log(x/thetahat)+(n-y)*log((1-x)/(1-thetahat)))}

xvalue<-seq(0.25, 0.37,0.001)
plot(xvalue,BinomRLF(xvalue),xlab="theta",ylab="Rtheta",type="l",lwd=1.5)    # plot relative likelihood function

# draw a horizontal line at 0.15
abline(a=0.15,b=0,col="red",lwd=1)

# smaller root
uniroot(function(x) BinomRLF(x)-0.15,lower=0.24,upper=0.26)
        
# larger root
uniroot(function(x) BinomRLF(x)-0.15,lower=0.36,upper=0.38)

# get expectation and standard deviation using n, p, q
samplen <- 50
        
normexp <- 50 * thetahat
    
normsd <- 50 * thetahat * (1 - thetahat)

# probability that half of tweets contain 25 or more retweets
1 - pnorm(25, mean = normexp, sd = normsd)
        
        
# (2)
# number of media
length(dataset20816215$media)

# number of media == 0
length(dataset20816215$media[dataset20816215$media == 0])

# number of tweet that are first tweets
length(dataset20816215$first.tweet[dataset20816215$first.tweet == 1])

# first tweet and no media
length(dataset20816215$media[dataset20816215$media == 0 & dataset20816215$first.tweet == 1])

# first tweet and one media
length(dataset20816215$media[dataset20816215$media == 1 & dataset20816215$first.tweet == 1])

# first tweet and two media
length(dataset20816215$media[dataset20816215$media == 2 & dataset20816215$first.tweet == 1])

# first tweet and three media
length(dataset20816215$media[dataset20816215$media == 3 & dataset20816215$first.tweet == 1])

# first tweet and four media
length(dataset20816215$media[dataset20816215$media == 4 & dataset20816215$first.tweet == 1])

# not first tweet and no media
length(dataset20816215$media[dataset20816215$media == 0 & dataset20816215$first.tweet == 0])

# not first tweet and one media
length(dataset20816215$media[dataset20816215$media == 1 & dataset20816215$first.tweet == 0])

# not first tweet and two media
length(dataset20816215$media[dataset20816215$media == 2 & dataset20816215$first.tweet == 0])

# not first tweet and three media
length(dataset20816215$media[dataset20816215$media == 3 & dataset20816215$first.tweet == 0])

# not first tweet and four media
length(dataset20816215$media[dataset20816215$media == 4 & dataset20816215$first.tweet == 0])

# get mode
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(dataset20816215$media[dataset20816215$first.tweet == 1])

mean(dataset20816215$media[dataset20816215$first.tweet == 1])

getmode(dataset20816215$media[dataset20816215$first.tweet == 0])

mean(dataset20816215$media[dataset20816215$first.tweet == 0])

# maximum likelihood estimate
mean(dataset20816215$media)


# (3)
# retweets
retweets <- dataset20816215$retweets
retweets.log <- log(retweets + 1)

# summary
summary(retweets.log)
sd(retweets.log)

# import
library(MASS)

truehist(retweets.log, xlab = "Log of number of retweets", ylab = "Relative Frequency", main = "Relative frequency histogram of log of number of retweets", ylim = c(0, 0.8), xlim = c(0, 8), las = 1, col = "dodgerblue3", nbins = 25)
# add curve (note, if curve extends beyond plot area, adjust y-axis of plot using the ylim command when calling truehist())
curve(dexp(x, 1/mean(retweets.log)), col = "red", add = TRUE, lwd = 2)

# (4)
# with keyword vaccine
retweets.vaccine <- dataset20816215$retweets[dataset20816215$vaccine == 1]
retweets.log.vaccine <- log(retweets.vaccine + 1)

# summary
summary(retweets.log.vaccine)
sd(retweets.log.vaccine)
length(retweets.log.vaccine)

truehist(retweets.log.vaccine, xlab = "Log of number of retweets with keyword vaccine", ylab = "Relative Frequency", main = "Relative frequency histogram of log of number of retweets with keyword vaccine", ylim = c(0, 0.8), xlim = c(0, 8), las = 1, col = "dodgerblue3", nbins = 25)

curve(dexp(x, 1/mean(retweets.log.vaccine)), col = "red", add = TRUE, lwd = 2)

        
# chi square
qchisq(0.95, df=270, lower.tail=FALSE)
qchisq(0.95, df=669, lower.tail=FALSE)
        
# without keyword vaccine
retweets.notvaccine <- dataset20816215$retweets[dataset20816215$vaccine == 0]
retweets.log.notvaccine <- log(retweets.notvaccine + 1)

# summary
summary(retweets.log.notvaccine)
sd(retweets.log.notvaccine)
length(retweets.log.notvaccine)
        
truehist(retweets.log.notvaccine, xlab = "Log of number of retweets without keyword vaccine", ylab = "Relative Frequency", main = "Relative frequency histogram of log of number of retweets without keyword vaccine", ylim = c(0, 0.8), xlim = c(0, 8), las = 1, col = "dodgerblue3", nbins = 25)

curve(dexp(x, 1/mean(retweets.log.notvaccine)), col = "red", add = TRUE, lwd = 2)

# (5)
# time.of.day
dataset20816215$time.of.day.hour<-dataset20816215$time.of.day/3600

# summary between 9 to 12
summary(dataset20816215$likes[dataset20816215$time.of.day.hour >= 9 & dataset20816215$time.of.day.hour<=12])

sd(dataset20816215$likes[dataset20816215$time.of.day.hour >= 9 & dataset20816215$time.of.day.hour<=12])

# summary between 12 to 15
summary(dataset20816215$likes[dataset20816215$time.of.day.hour >= 12 & dataset20816215$time.of.day.hour<=15])

sd(dataset20816215$likes[dataset20816215$time.of.day.hour >= 12 & dataset20816215$time.of.day.hour<=15])

# get mean and standard deviation
meanam <- mean(dataset20816215$likes[dataset20816215$time.of.day.hour >= 9 & dataset20816215$time.of.day.hour<=12])
sdam <- sd(dataset20816215$likes[dataset20816215$time.of.day.hour >= 9 & dataset20816215$time.of.day.hour<=12])
lengtham <- length(dataset20816215$likes[dataset20816215$time.of.day.hour >= 9 & dataset20816215$time.of.day.hour<=12])
        
meanpm <- mean(dataset20816215$likes[dataset20816215$time.of.day.hour >= 12 & dataset20816215$time.of.day.hour<=15])
sdpm <- sd(dataset20816215$likes[dataset20816215$time.of.day.hour >= 12 & dataset20816215$time.of.day.hour<=15])
lengthpm <- sd(dataset20816215$likes[dataset20816215$time.of.day.hour >= 12 & dataset20816215$time.of.day.hour<=15])

# 95% confidence interval
erroram <- qnorm(0.975)*sdam/sqrt(lengtham)
errorpm <- qnorm(0.975)*sdpm/sqrt(lengthpm)
    
meanam + erroram
meanam - erroram
        
meanpm + errorpm
meanpm - errorpm
        
# 90% confidence interval
erroram <- qnorm(0.95)*sdam/sqrt(lengtham)
errorpm <- qnorm(0.95)*sdpm/sqrt(lengthpm)
    
meanam + erroram
meanam - erroram
        
meanpm + errorpm
meanpm - errorpm

# (6)
# we get 8 province accounts
unique(dataset20816215$username)

# number of sample tweets from @sante_qc
length(dataset20816215$username[dataset20816215$username == "@sante_qc"])

# number of sample tweets from @sante_qc and retweeted
length(dataset20816215$username[dataset20816215$username == "@sante_qc" & dataset20816215$is.retweet == 1])

# number of sample tweets from @ONThealth
length(dataset20816215$username[dataset20816215$username == "@ONThealth"])

# number of sample tweets from @ONThealth and retweeted
length(dataset20816215$username[dataset20816215$username == "@ONThealth" & dataset20816215$is.retweet == 1])

# number of sample tweets from @GoAHealth
length(dataset20816215$username[dataset20816215$username == "@GoAHealth"])

# number of sample tweets from @GoAHealth and retweeted
length(dataset20816215$username[dataset20816215$username == "@GoAHealth" & dataset20816215$is.retweet == 1])

# number of sample tweets from @PHSAofBC
length(dataset20816215$username[dataset20816215$username == "@PHSAofBC"])

# number of sample tweets from @PHSAofBC and retweeted
length(dataset20816215$username[dataset20816215$username == "@PHSAofBC" & dataset20816215$is.retweet == 1])

# number of sample tweets from @SaskHealth
length(dataset20816215$username[dataset20816215$username == "@SaskHealth"])

# number of sample tweets from @SaskHealth and retweeted
length(dataset20816215$username[dataset20816215$username == "@SaskHealth" & dataset20816215$is.retweet == 1])

# number of sample tweets from @HealthNS
length(dataset20816215$username[dataset20816215$username == "@HealthNS"])

# number of sample tweets from @HealthNS and retweeted
length(dataset20816215$username[dataset20816215$username == "@HealthNS" & dataset20816215$is.retweet == 1])

# number of sample tweets from @Health_PEI
length(dataset20816215$username[dataset20816215$username == "@Health_PEI"])

# number of sample tweets from @Health_PEI and retweeted
length(dataset20816215$username[dataset20816215$username == "@Health_PEI" & dataset20816215$is.retweet == 1])

# number of sample tweets from @HCS_GovNL
length(dataset20816215$username[dataset20816215$username == "@HCS_GovNL"])

# number of sample tweets from @HCS_GovNL and retweeted
length(dataset20816215$username[dataset20816215$username == "@HCS_GovNL" & dataset20816215$is.retweet == 1])


