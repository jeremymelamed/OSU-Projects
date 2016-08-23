#'Part a) Construct scatter plot of yield vs rainfall
library("Sleuth3")
library(ggplot2)
head(ex0915) # To see the data structure
ex0915 # To read the full data set
attach(ex0915)


data <- data.frame(ex0915)
plot(Rainfall, Yield, xlab = "Rainfall (in/year)", ylab = "Yield (bu/acre)", main = "Corn Production Yield vs Rainfall")
abline(lm(Yield~Rainfall))

#Part b) Fit Multiple Linear Regression
Rainfall2 <- Rainfall^2
mod.sep <- lm(Yield ~ Rainfall + Rainfall2)
summary(mod.sep)

#Part c) 95% CI for all parameters
confint(mod.sep)

#Part d
fit <- residuals(mod.sep)
plot(mod.sep, which=1)
qplot(Year, fit, xlab = "Year", ylab = "Residuals", main = "Scatter Plot of Residuals vs Year") + stat_hline(yintercept = 0)



#Part e
mod2 <- lm(Yield ~ Rainfall + cornProduction$Rainfall2 + Year)
summary(mod2)

#Part f
plot(Rainfall, Year)
abline(lm(Year ~ Rainfall)) 'Variability is increasing which is concerning'


