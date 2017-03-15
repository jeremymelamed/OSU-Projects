# ST 431 HW #7
setwd('C:\\Users\\Jeremy\\OneDrive\\Fall 2016\\ST 431')

### Lohr 5.8.8
books <- read.csv('books.csv')

# Boxplots for each shelf
boxplot(books$replace ~ books$shelf, xlab = 'Shelf Number', ylab = 'Replacement Cost',
        main = 'Boxplots of the eplacement cost for books on each shelf')

# Initializing values 
N <- 44
n <- 12
Mi <- aggregate(books$Mi, by=list(books$shelf), FUN='mean')[,2]
mi <- aggregate(books$shelf, by=list(books$shelf), FUN='length')[,2]
yi_bar <- aggregate(books$replace, by=list(books$shelf), FUN='mean')[,2]
ti <- Mi*yi_bar

# Estimate of the total value of the library
t_hat <- (N/n) * sum(Mi * yi_bar)
t_hat

# Variance and standard error estimates for thetotal replacement cost
st.2 <- sum((ti - t_hat/N)^2) / (n-1)
st.2
si.2 <- sum(aggregate(books$replace, by=list(books$shelf), FUN='var')[,2] / (mean(mi) - 1))
si.2
V_hat <- N^2 * (1-n/N) * (st.2/n) + (N/n) * sum((1-mi/Mi) * Mi^2 * (si.2/mi))
V_hat
SE_books <- sqrt(V_hat)
SE_books

# Check that variance seems reasonable
sqrt(N^2 * (st.2 / n))


### Problem 2
R <- .41
c1 <- 10
c2 <- 4
M <- 30
N <- 44

m_opt <- sqrt((c1*M*(N-1)*(1-R))/(c2*(N*M-1)*R))
m_opt


### Problem 3
N <- 4380 
k = 20
ti <- 30
M <- N / k

# Estimate of proportion
p_hat <- ti / M
p_hat

# Standard error estimate
V_fines <- (1-M/N) * (p_hat * (1 - p_hat)) / (M - 1)
V_fines

SE_fines <- sqrt(V_fines)
SE_fines

### Problem 4
N <- 10
x <- c(44,55,4,16)
x2 <- c(44, 4)
y <- c(56, 6)

# Ratio Estimator
B <- sum(y) / sum(x[1], x[3])
B
# Estimate of total
t1x <- N * mean(x)
t1x
t2y <- B * t1x
t2y

# Standard Error Estimate
sy.2 <- sum((y - mean(y))^2) / (length(y) - 1)
sy.2 
se.2 <- sum((y-B*x2)^2) / (length(y) - 1)
se.2

V_tyr <- N^2*(1-length(x)/N)*(sy.2/length(x)) + N^2*(1-length(y)/length(x))*(se.2/length(y))
V_tyr
SE_tyr <- sqrt(V_tyr)
SE_tyr


### Problem 4
birds <- read.csv('shorebirds.csv')
birds2 <- subset(birds, intense > 0)
N <- 2130
n <- 12

# Ratio Estimator
B <- mean(birds2$intense) / mean(birds2$rapid)
B

# Estimate of total birds
tx <- N*mean(birds$rapid)
tx
ty <- B*tx
ty

# Standard Error estimate
sy.2 <- sum((birds2$intense - mean(birds2$intense))^2) / (nrow(birds2)-1)
sy.2
se.2 <- sum((birds2$intense - B*birds2$rapid)^2) / (nrow(birds2)-1)
se.2

V_birds <- N^2*(1-nrow(birds)/N)*(sy.2/nrow(birds)) + N^2*(1-nrow(birds2)/nrow(birds))*(se.2/nrow(birds2))
V_birds
SE_birds <- sqrt(V_birds)
SE_birds



