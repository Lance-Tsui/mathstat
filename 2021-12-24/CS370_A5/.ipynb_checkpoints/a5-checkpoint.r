# Lance Xu
# q1
dataset <- dataset20816215
dataset$retweets.log <- log(dataset$retweets + 1)
dataset$likes.log <- log(dataset$likes + 1)
dataset <- subset(dataset, subset = (username %in% c("@ROMtoronto", "@TheTorontoZoo")))
nrow(dataset)
mod <- lm(likes.log ~ retweets.log, data = dataset)

mean(dataset$retweets.log)
mean(dataset$likes.log)
cor(dataset$retweets.log, dataset$likes.log)
# summarize the model
summary(mod)

# extract elements of the model
coef(mod) # parameter estimates
coef(summary(mod)) # table of results
coef(summary(mod))[, 4] # p-values
sigma(mod) # residual standard error

plot(dataset$retweets.log, dataset$likes.log, xlab = "retweets.log", ylab = "likes.log", main = "Scatterplot of likes.log and retweets.log", pch = 1, cex = 0.5, col = "navy", las = 1, lwd = 1)
abline(coef(mod), lwd = 2, lty = 2, col = "red") # add fitted line

# standardized residuals
stdres <- rstandard(mod) # store standardized residuals
mean(stdres) # check mean
sd(stdres) # check standard deviation
# set up plot window to have 1 row of 3 plots
par(mfrow = c(1, 3))
# standardized residuals vs explanatory variate
plot(dataset$retweets.log, stdres, xlab = "retweets.log", ylab = "Standardized Residuals", main = "Std residuals vs. retweets.log", pch = 1, col = "navy", cex = 0.5, las = 1)
abline(h = 0, lty = 2, col = "red", lwd = 2)
# standardized residuals vs fitted values
plot(fitted(mod), stdres, main = "Std residuals vs. fitted values", xlab = "Fitted values", ylab = "Standardized Residuals", pch = 1, col = "navy", cex = 0.5, las = 1)
abline(h = 0, lty = 2, col = "red", lwd = 2)
# qqplot of standardized residuals
qqnorm(stdres, main = "qqplot of std residuals", xlab = "G(0, 1) Quantiles", ylab = "Standardized Residuals", pch = 1, col = "navy", cex = 0.5, las = 1)
qqline(stdres, lty = 2, col = "red", lwd = 2)

#the degree of freedom we got previously is 360

# 99% confidence interval for beta (lower bound)
coef(summary(mod))[2, 1] - qt(0.995, 360)*coef(summary(mod))[2, 2]
# 99% confidence interval for beta (upper bound)
coef(summary(mod))[2, 1] + qt(0.995, 360)*coef(summary(mod))[2, 2]


# direct computation
confint(mod, level = 0.95)

## estimating a response

coef(mod)[1] + coef(mod)[2]*log(50 + 1) # equation of fitted line

# prediction interval
predict(mod, data.frame("retweets.log" = log(50 + 1)), interval = "prediction", level = 0.95)

est.log <- coef(mod)[1] + coef(mod)[2]*log(50 + 1) # store in est.log
est <- exp(est.log) - 1 # transform back

# prediction interval
predict(mod, data.frame("retweets.log" = log(50 + 1)), interval = "prediction", level = 0.95)

mod <- lm(retweets.log ~ likes.log, data = dataset)

# q3
dataset <- dataset20816215
dataset$likes.log <- log(dataset$likes + 1)
dataset <- subset(dataset, subset = (username %in% c("@SimuLiu", "@CPHO_Canada","jonnysun")))
nrow(dataset)

# sknewness function
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)}

# kurtosis function
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/(sum((x - mean(x))^2)/length(x))^2}

# the mean of the variate likes.log if the tweet included no media 
data1 <- dataset$likes.log[dataset$media.binary == 0]


skewness(data1)

kurtosis(data1)

summary(data1)

qqnorm(data1, main = "qqplot: likes.log of selected accounts with media.binary 0", cex.main = 0.8, las = 1, col="blue")

qqline(data1, las=2, col="red")


# the mean of the variate likes.log if the tweet included at least one media
data2 <- dataset$likes.log[dataset$media.binary != 0]

skewness(data2)

kurtosis(data2)

summary(data2)

qqnorm(data2, main = "qqplot: likes.log of selected accounts with media.binary larger than 0", cex.main = 0.8, las = 1, col="blue")

qqline(data2, las=2, col="red")

# boxplot

par(mfrow = c(1, 2))

boxplot(data1,
main = "boxplot of likes log with no media",
xlab = "x",
ylab = "likes.log",
col = "orange",
border = "brown"
)

boxplot(data2,
main = "boxplot of likes log with media",
xlab = "x",
ylab = "likes.log",
col = "blue",
border = "red"
)

length(data1)

length(data2)

# 95% CI for standard deviation of likes.log with no media
c(sd(data1)*sqrt(180/qchisq(0.975, 180)), sd(data1)*sqrt(180/qchisq(0.025, 180)))
# 95% CI for standard deviation of likes.log with media
c(sd(data2)*sqrt(180/qchisq(0.975, 180)), sd(data2)*sqrt(180/qchisq(0.025, 180)))

# pooled standard deviation
pooled.sd <- sqrt((180*sd(data1)^2 + 180*sd(data2)^2)/(181 + 181 - 2))

# calculate test statistic
d <- abs(mean(data1) - mean(data2))/(pooled.sd*sqrt(1/181 + 1/181))

# calculate p-value
2*(1 - pt(d, 181 + 181 - 2))

# 99% confidence interval
t.test(data1, data2, var.equal = TRUE, conf.level = 0.99)

