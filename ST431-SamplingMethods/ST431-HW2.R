
# Create vector of faculty publications 
x <- replicate(28, 0)
x <- c(x, replicate(4, 1), replicate(3, 2), replicate(4, 3), replicate(4, 4), replicate(2, 5),
       replicate(1, 6), replicate(2, 8), replicate(1, 9), replicate(1, 10))

hist(x, 10, xlab = 'Number of Refereed Publications', main = 'Histogram Showing The Number Of Refereed Publications By Faculty Members')

# Sum and mean of vector
sum(x)
mu <- mean(x)
var <- var(x)
n <- length(x)
# Standard error estimate
sqrt((var/n)*(1-(n/807)))

# Minimum sample size
s2 <- 0
for (i in 1:50) {
  s2 <- s2 + (x[i] - mu)^3
}
n_min <- 28 + 25 * ((s2 / (807 * var ^ 3)) ^ 2)
n_min

# Problem 15. Estimate and CI from the number of farms
agsrs <- read.csv('C:\\Users\\Jeremy\\OneDrive\\Fall 2016\\ST 431\\agsrs.csv')
# Subset data to remove missing data
farms92 <- subset(agsrs, farms92 >= 0)
# Mean of sample
mu <- mean(farms92$farms92)
# Plot of data
hist(farms92$farms92, breaks = 20, main = 'Number of farms per county in 1992', xlab = 'Number of farms')
abline(v = mu, col = 'red')
# Confidence Interval
s.var <- var(farms92$farms92)
se <- sqrt(s.var/300)
mu - 1.96*se
mu + 1.96*se
