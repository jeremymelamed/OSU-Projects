'ST412 HW 2'
library("Sleuth3")
library(ggplot2)
library(grid)

'Part a) Construct "side-by-side" scatter-plots (including the regression line) for the number
of flowers vs. light intensity, with and without distinguishing for the the time
effect. Do the plots suggest a linear relationship between these variables? Do the
plots suggests an effect due to timing?'
myData <- read.csv('C:\\Users\\Jeremy\\Documents\\WInter 2016\\Case0901.csv')
flowers <- myData[,1]
time <- myData[,2]
intensity <- myData[,3]

# Scatterplot using different symbols for groups
par(mfrow=c(1,2))
'Plot 1'
plot(intensity,flowers, type = "p",col="blue",xlim = c(min(intensity),max(intensity)), ylim=c(min(flowers),max(flowers)), 
            main = "Flowers vs Intensity", xlab = "Intensity", ylab = "Flowers") 
abline(lm(flowers~intensity))
'Plot 2'
plot(intensity[1:12],flowers[1:12],type = "p", col="red", xlim = c(min(intensity),max(intensity)), ylim=c(min(flowers),max(flowers)), 
     main="Flowers vs Intensity ", xlab = "Intensity", ylab = "Flowers")
abline(lm(flowers[1:12]~intensity[1:12]), col="red")

points(intensity[13:24],flowers[13:24],type = "p", col="darkgreen", main="Flowers vs Intensity", xlab = "Intensity", ylab = "Flowers")
abline(lm(flowers[13:24]~intensity[13:24]), col="darkgreen")


'Part b) Regression model with indicator variable'
early = as.numeric(myData$Time == "2")

fit <- lm(Flowers~Intensity + early, data = myData)
summary(fit)
'All coefficients in the model are significant. The Intercept is effected by time'

'Part c)'
'Model: mu = 71.3 - 0.04Light + 12.15Timing - use hats'
'Fitted equation is estimation of the model for the given sample data'
'Estimation is data dependent'

'Part d)'
2*pt(4.624,21,lower.tail = FALSE)

'Part e)'
confint(fit, level = 0.95)

'Part f)'
'mu(flowers|light=600,early=1) - mu(flowers|light=300,early=1)'
'Do by hand -- equals 12.15'
coef(summary(fit["Intensity","early"]))




ggplot(myData, aes(x=Intensity, y=Flowers)) + 
  geom_point(shape=1) +
  geom_abline(aes(intercept = mod.sep$coefficients[[1]],slope = mod.sep$coefficients[[2]]))
  ggtitle("Flowers vs Light Intensity*Time")

