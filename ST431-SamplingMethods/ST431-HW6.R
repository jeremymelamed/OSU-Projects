### Preprocessing R code for Lohr 5.8.5
setwd('C:\\Users\\Jeremy\\OneDrive\\Fall 2016\\ST 431')
spanish <- read.csv("spanish.csv")
spanish$class <- as.factor(spanish$class)

ti_trip <- aggregate(spanish$trip, by=list(spanish$class), FUN='sum')[,2]
ti_score <- aggregate(spanish$score, by=list(spanish$class), FUN='sum')[,2]
Mi <- aggregate(spanish$score, by=list(spanish$class), FUN='length')[,2]
N <- 72
n <- 10

# Estimate of total number of students planning a trip
t_hat <- (N/n) * sum(ti_trip)
t_hat

# Variance estimate of total
st.2 <- (1/(n-1)) * sum((ti_trip - t_hat/N)^2)
V_trip <- N^2 * (1 - n/N) * (st.2/n)
V_trip
SE_trip <- sqrt(V_trip)
SE_trip
# Confidence interval
t_hat - 1.96 * SE_trip
t_hat + 1.96 * SE_trip

# Estimate of mean vocabulary score
ybar <- sum(ti_score) / sum(Mi)
ybar

# Variance estimate of mean vocab score
V_score <- (1-n/N) * (1/(n*mean(Mi)^2)) * sum((ti_score - ybar * Mi)^2) / (n-1)
V_score
SE_score <- sqrt(V_score)
SE_score
# Confidence interval
ybar - 1.96 * SE_score
ybar + 1.96 * SE_score


### Problem 5.8.6 data entry
dat <- c(1,4,0,3,4,0,5,3,7,3,4,0,5,2,1,6,9,7,5,0,3,1,7,0,7,4,2,6,8,3,1,2,5,4,9,0)
worms <- matrix(dat, nrow = 3, ncol = 12, byrow = T)
N <- 580
n <- 12
Mi <- 24
mi <- 3

# Estimate of total worm fragments
ti_hat <- Mi/mi * apply(worms, 2, sum)
t_hat <- (N/n) * sum(ti_hat)
t_hat        
        
# Standard error estimate
st.2 <- (1/(n-1)) * sum((ti_hat - t_hat/N)^2)
st.2
si.2 <- numeric(1)
for (i in 1:ncol(worms)) {
        for (j in 1:nrow(worms)) {
                si.2 <- si.2 + (worms[j,i] - apply(worms, 2, mean)[i])^2
        }
}
si.2 <- si.2 / (mi-1)
si.2
V_worms <- N^2*(1 - n/N)*(st.2/n) + (N/n)*sum((1-mi/Mi)*(Mi^2/mi)*si.2)     
V_worms    
SE_worms <- sqrt(V_worms)
SE_worms

# Check using approximation
V_check <- N^2 * (st.2/n)
V_check



### Problem 5.8.12
coots <- read.csv('coots.csv')
# Data visualization
par(mfrow=c(1,2))
plot(coots$clutch, coots$length, xlab = 'Clutch', ylab = 'Length', main = 'Egg Length vs Clutch Scatterplot')
hist(coots$length, breaks = 12, xlab = 'Length', main = 'Histogram of Egg Lengths')
par(mfrow=c(1,1))

n <- length(unique(coots$clutch))
Mi <- aggregate(coots$csize, by=list(coots$clutch), FUN='mean')[,2]
yi_clutch <- aggregate(coots$length, by=list(coots$clutch), FUN='mean')[,2]

# Mean length estimate
ybar <- sum(yi_clutch * Mi) / sum(Mi)
ybar

# Estimate of variance. 
# N unknown and assumed large so second term of variance assumed to be negligible
sr.2 <- (1/(n-1)) * sum((Mi*yi_clutch - Mi*ybar)^2)
sr.2
V_length <- (1/mean(Mi)^2) * (sr.2/n)
V_length
SE_length <- sqrt(V_length)
SE_length


### Problem 5
birds <- read.csv('shorebirds.csv')
N <- 2130
n <- 12
