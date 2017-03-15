# ST 431 - HW #9
library(ggplot2)
library(cowplot)
syc <- read.csv('C:\\Users\\Jeremy\\OneDrive\\Fall 2016\\ST 431\\syc.csv')

# Convert variables to factors
for (i in 1:ncol(syc)){
        syc[ ,i] <- as.factor(syc[ ,i])
}

# Problem 1
summary(syc$livewith)

which(syc$livewith == 99)

livwith_blanks <- subset(syc, livewith == 99) # famtime, numarr, probtn, corrinst, evertime have missing values for  
                                              # corresponding missing livewith values

# Visualizations for imputation variables
p1<- ggplot(syc, aes(race)) + 
        geom_bar(aes(fill = livewith)) + 
        ggtitle('Living Situation Based on Race')

p2 <- ggplot(syc, aes(educ)) + 
        geom_bar(aes(fill = livewith)) + 
        ggtitle('Living Situation Based on Education Level')

plot_grid(p1, p2)

# Impute missing values for living situation
impute_vals <- character(nrow(livwith_blanks))
set.seed(1)

for (i in 1:length(impute_vals)) {
        # Subset original data based on imputation values
        temp <- subset(syc, race == livwith_blanks$race[i] & educ == livwith_blanks$educ[i] & stratum == livwith_blanks$stratum[i])
        
        index <- sample(1:nrow(temp), 1)
        impute_vals[i] <- as.character(temp$livewith[index])
}

impute_vals

# Part b. Sequential hot-deck imputation of criminal offense type based on stratum
ggplot(syc, aes(stratum)) + 
        geom_bar(aes(fill = crimtype)) +
        ggtitle('Most Serious Criminal Offense By Stratum')

# Sort data by stratum
syc_sort <- syc[order(syc$stratum), ]

# Impute pervious value from sorted data
impute_vals <- character(length(which(syc_sort$crimtype == 9)))
for (i in 1:length(which(syc_sort$crimtype == 9))) {
        index <- which(syc_sort$crimtype == 9)[i] - 1
        impute_vals[i] <- as.character(syc_sort$crimtype[index])
}
which(syc$crimtype == 9)
impute_vals


############## Problem 2 ######################
setwd('C:\\Users\\Jeremy\\OneDrive\\Fall 2016\\ST 431')
source("cap_recap.R")

n1 <- 48
n2 <- 45
m <- 33

# Lincoln-Petersen
N.hat <- n1*n2/m
N.hat

V.N.hat <- (n1*n2/m)^2*(n2-m)/(m*(n2-1))
V.N.hat

# Chapman
N.til <- (n1+1)*(n2+1)/(m+1) - 1
N.til

V.N.til <- (n1+1)*(n2+1)*(n1-m)*(n2-m)/((m+1)^2*(m+2))
V.N.til

# Normal Approx CIs
N.hat - 1.96*sqrt(V.N.hat)
N.hat + 1.96*sqrt(V.N.hat)

# Formal data for R functions
xmat <- cbind(c(1,1,0),c(1,0,1))
y <- c(33,15,12)

# LRT
captout <- captureci(xmat, y)
captout$`Lower CL for N`
captout$`Upper CL for N`

# Bootstrap 
bootout <- captureboot(converty(y, xmat[,1], xmat[,2]), 
                       crossprod(xmat[,1],y), nboot=999, nfunc=nmle)
bootout$lcl
bootout$ucl











