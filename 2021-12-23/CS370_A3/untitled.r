# Lance Xu 20816215

# stat 231

# b i

summary(dataset20816215$long.words)

var(dataset20816215$long.words)

# b iii

# Y ~ Poisson(θ)
# P(Y>=7) = 1-P(Y<=6)
thetahat<-mean(dataset20816215$long.words)
1-ppois(6, thetahat)

# b iv
table(dataset20816215$long.words)

# b v
n<-dim(dataset20816215[0])[1]
dpois(0, thetahat)*n # expected count of games with 0 home goals
dpois(1, thetahat)*n # expected count of games with 1 home goal
expected<-dpois(0:9,thetahat)*n
expected<-round(expected, 2) # display rounded to 2 decimal places
e9<-n - sum(expected) # expected count for 9 or more home goals
dataset20816215$long.words[dataset20816215$long.words == 9] <- "9+" # relabel as 9 or more
observed<-table(dataset20816215$long.words) # get observed counts
cbind(observed, expected) # combine into a single data frame

barplot(rbind(observed, expected), beside = T, col=c("dodgerblue3", "darkorchid"), density = 75, angle = c(45, 135), ylim = c(0, 400), las = 1, xlab = "Goals", ylab = "Number of long word Tweets")

legend('topright', legend = c("Observed", "Expected"), fill = c("dodgerblue3", "darkorchid"), density = 75, angle = c(45, 135))
PoisRLF <- function(theta, n, thetahat) {exp(n*thetahat*log(theta/thetahat) + n*(thetahat - theta))}

# b vii
theta <-seq(1.25, 1.48, 0.001)
plot(theta, PoisRLF(theta, n, thetahat) , xlab = expression(theta), ylab = expression(paste("R(",theta,")")), type = "l", lwd = 2, , main = "Poisson relative likelihood function", las = 1, col = "navy")
abline(h = 0.1, col = "red", lwd = 2)

# find interval
uniroot(function(x) PoisRLF(x, n, thetahat) - 0.1, lower = 1.25, upper = 1.3)$root
uniroot(function(x) PoisRLF(x, n, thetahat) - 0.1, lower = 1.4, upper = 1.45)$root
        
# b viii
LogRLF <- function(theta, n, thetahat) {n*thetahat*log(theta/thetahat) + n*(thetahat - theta)}
theta <-seq(1.25, 1.47, 0.001)
plot(theta, LogRLF(theta, n, thetahat) , xlab = expression(theta), ylab = expression(paste("R(",theta,")")), type = "l", lwd = 2, , main = "Log relative likelihood function", las = 1, col = "navy")
        
#ln(0.15)
abline(h = -1.897, col = "red", lwd = 2)
uniroot(function(x) LogRLF(x, n, thetahat) + 1.897, lower = 1.25, upper = 1.3)$root
uniroot(function(x) LogRLF(x, n, thetahat) + 1.897, lower = 1.4, upper = 1.45)$root
        

#d ii
var(dataset20816215$long.words[dataset20816215$username == '@SimuLiu'])
var(dataset20816215$long.words[dataset20816215$username == '@CPHO_Canada'])
summary(dataset20816215$long.words[dataset20816215$username == '@SimuLiu'])
summary(dataset20816215$long.words[dataset20816215$username == '@CPHO_Canada'])