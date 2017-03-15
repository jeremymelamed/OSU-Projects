# Problem 4.3 - Regression Estimation for tree age
x <- c(12, 11.4, 7.9, 9, 10.5, 7.9, 7.3, 10.2, 11.7, 11.3, 5.7, 8, 10.3, 12, 9.2, 8.5, 7, 10.7, 9.3, 8.2)
y <- c(125, 119, 83, 85, 99, 117, 69, 133, 154, 168, 61, 80, 114, 147, 122, 106, 82, 88, 97, 99)
n <- 20
N <- 1132
# Regression method coefficients
B1.hat <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
B1.hat
B0.hat <- mean(y) - B1.hat * mean(x)
B0.hat
# Estimate of mean age of trees using regression method
y.reg <- B0.hat+B1.hat*10.3
y.reg
# Sample variance of residuals
e.i <- y - (B0.hat + B1.hat * x)
s <- (1 / (n - 1)) * sum((e.i) ^ 2)
s
# Estamate of standard error
SE.reg <- sqrt((1 - n/N) * (s/n))
SE.reg

# Problem 4.7
data_4.7 <- read.csv('C:\\Users\\Jeremy\\OneDrive\\Fall 2016\\ST 431\\golfsrs.csv')
N <- 14938
n <- nrow(data_4.7)
# Add indicator variable for golf pro
data_4.7$x <- ifelse(data_4.7$pro == 'y', 1, 0)
# Add auxiliary variable of weekday 9-hole green fee at courses with pro
data_4.7$u <- data_4.7$x * data_4.7$wkday9
# Estimate of mean weekday green fee for 9 holes with pro available
yd <- mean(data_4.7$u)/mean(data_4.7$x)

# Sample variance
s.yd <- sum((data_4.7$wkday9 - mean(yd)) ^ 2) / (n - 1)
V.yd <- (1 - n/N)*(n/(n*mean(data_4.7$x))^2)*(n*mean(data_4.7$x)-1)*s.yd/(n-1)
SE <- sqrt(V.yd)

# Part b - golf courses without pros
# Indicator variable for no golf pro
data_4.7$x <- ifelse(data_4.7$pro == 'n', 1, 0)
# Auxiliary variable of weekday 9-hole green fee at courses with pro
data_4.7$u <- data_4.7$x * data_4.7$wkday9
# Estimate of mean weekday green fee for 9 holes without pro available
yd <- mean(data_4.7$u)/mean(data_4.7$x)

# Sample variance
s.yd <- sum((data_4.7$wkday9 - mean(yd)) ^ 2) / (n - 1)
s.yd
V.yd <- (1 - n/N)*(n/(n*mean(data_4.7$x))^2)*(n*mean(data_4.7$x)-1)*s.yd/(n-1)
V.yd
SE <- sqrt(V.yd)

sqrt(s.yd/(n*mean(data_4.7$x)))
   





# Problem 4.8
agsrs <- read.csv('C:\\Users\\Jeremy\\OneDrive\\Fall 2016\\ST 431\\agsrs.csv')
x <- agsrs$farms87
y <- agsrs$acres92
N <- 3078
n <- 300
tx <- 2087759

# Part a - Plot of data
B.hat <- sum(y)/sum(x)
B.hat
plot(x, y, xlab = 'Farms By County in 1987', ylab = 'Acres By County in 1992', main = 'Farms By County in 1987 vs Acres By County in 1992')
abline(a=0, b=B.hat, col='red')

# Correlation between auxiliary and desired variable
cor(x, y)

# Part b - Ratio estimation of total number of acres in '92
B.hat * tx
# Variance of residuals
ei <- y - B.hat*x
s <- sum(ei^2)/(n-1)
# Standard Error of Ratio Estimate
var <- (1-n/N) * (tx/mean(x))^2 * (s/n)
sqrt(var)        

# Part c - Regression estimation of total number of acres in '92
B1.hat <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
B1.hat
B0.hat <- mean(y) - B1.hat * mean(x)
B0.hat
#Estimate of total acres in '92
N * (B0.hat + B1.hat * tx/N)
# sample variance of the residuals
ei <- y - (B0.hat + B1.hat * x)
s <- sum((ei)^2) / (n-1)
s
# Standard Error of Regression Estimate
sqrt((1 - n/N) * (s/n)) * N  
        
        