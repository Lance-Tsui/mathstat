# q4 (a)
typeof(dataset20816215$day.of.week)

# q4 (b_i)

# Simu Liu's dist
manchu<-subset(dataset20816215,dataset20816215$username=="@SimuLiu")
table(factor(manchu$day.of.week, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))

# Simu's Frequency
barplot(table(factor(manchu$day.of.week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))),xlab="Day of Week",ylab="Number of Tweets",las=1,main="Frequency of Liu's Tweets",col = c("orange", "blue", "purple", "#114514","yellow","red","green"), ylim = c(0, 50), density = 25, angle = c(45, 90, 135, 180))

# Tam's dist
cpho<-subset(dataset20816215,dataset20816215$username=="@CPHO_Canada")
table(factor(cpho$day.of.week, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))

# Tam's Frequency
barplot(table(factor(cpho$day.of.week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))),xlab="Day of Week",ylab="Number of Tweets",las=1,main="Frequency of Tam's Tweets",col = c("orange", "blue", "purple", "#114514","yellow","red","green"), ylim = c(0, 50), density = 25, angle = c(45, 90, 135, 180))

# q4 (b_ii)
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# mode of Simu
getmode(manchu$day.of.week)

# mode of CPHO
getmode(cpho$day.of.week)

# a1 q5

# type of time.of.day
typeof(dataset20816215$time.of.day)

# sknewness function
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}

# kurtosis function
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}


rom<-subset(dataset20816215,dataset20816215$username=="@ROMtoronto")
# sample mean
mean(rom$time.of.day)
# sample median
median(rom$time.of.day)
# sample standard deviation
sd(rom$time.of.day)
# sample skewness
skewness(rom$time.of.day)
# sample kurtosis
kurtosis(rom$time.of.day)

zoo<-subset(dataset20816215,dataset20816215$username=="@TheTorontoZoo")
# sample mean
mean(zoo$time.of.day)
# sample median
median(zoo$time.of.day)
# sample standard deviation
sd(zoo$time.of.day)
# sample skewness
skewness(zoo$time.of.day)
# sample kurtosis
kurtosis(zoo$time.of.day)

library(MASS)

## adding Gaussian pdf:

# store numerical summaries needed for dnorm:
rom.mean <- mean(rom$time.of.day)
rom.sd <- sd(rom$time.of.day)

# draw histogram
truehist(rom$time.of.day, xlab = "Seconds", ylab = "Relative Frequency", main = "Relative frequency histogram of time.of.day in Ontario Museum's twitter", ylim = c(0, 0.00005), las = 1, col = "dodgerblue3", density = 25, angle = 45)
# add curve (note, if curve extends beyond plot area, adjust y-axis of plot using the ylim command when calling truehist())
curve(dnorm(x, rom.mean, rom.sd), col = "red", add = TRUE, lwd = 1.5)

## adding Gaussian pdf:

# store numerical summaries needed for dnorm:
zoo.mean <- mean(zoo$time.of.day)
zoo.sd <- sd(zoo$time.of.day)

# draw histogram
truehist(zoo$time.of.day, xlab = "Seconds", ylab = "Relative Frequency", main = "Relative frequency histogram of time.of.day in Toronto Zoo's twitter", ylim = c(0, 0.0001), las = 1, col = "dodgerblue3", density = 25, angle = 45)
# add curve (note, if curve extends beyond plot area, adjust y-axis of plot using the ylim command when calling truehist())
curve(dnorm(x, zoo.mean, zoo.sd), col = "red", add = TRUE, lwd = 1.5)