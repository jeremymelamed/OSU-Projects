#'ST 411 Data Analysis 3
install.packages("faraway")
library(faraway)
library(ggplot2)
source(url("http://stat511.cwick.co.nz/code/stat_qqline.r"))

'Initialize dataframe'
fat <- fat
mean(fat$age)
sd(fat$age)

#'Is there evidence that the mean bodyfat depends on age
fit <- lm(brozek ~ age + density + weight, data = fat)
summary(fit)
qplot(age, data = fat, main = "Histogram for the Age of Men in Sample", xlab = 'Age', ylab = 'Count')
qplot(brozek, data = fat)
anova(fit)

'There is significant evidence that bodyfat depends on age (p-value = 3.04e-06)'

#'Interpret estimates and confident intervals for slop and intercept parameters of the above linear regression
confint(fit)

'With 95% confidence, the slope parameter for the regression model is between 0.104 and 0.251'
'and the intercept is between 7.537 and 14.374'

#'What is dangerous about interpreting the intercept
'--An unborn person age 0 could have very different amounts of body fat during the pregnancy period'

#'Is the linear regression model appropriate'
qplot(age, brozek, data = fat, main = "Linear Regression Model with Confidence Interval", xlab = 'Age', ylab = 'Body Fat Percent') + geom_smooth(method = "lm")
qplot(age, .resid, data = fit, main = 'Residuals Plot', xlab = 'Age', ylab = 'Residual') + geom_hline(yintercept = 0) + geom_smooth() 
'Linearity assumption ok'
qplot(.fitted, .resid, data = fit, main = 'Residual vs Fitted Plot', xlab = 'Fitted', ylab = 'Residual') + stat_hline(yintercept = 10) + stat_hline(yintercept = -10)
'Assumption of constant spread ok'
qplot(sample = .resid, data = fit, main = 'Normal Probability Plot for Fitted Model', xlab = 'Theoretical',ylab = 'Sample') + stat_qqline() 
'Assumption of normality ok'
'Independence depends upon how data was collected'
'Was random sampling used??'

#'Provide and interpret CI for the mean body fat % and PI for body fat % of man who is 28
predict(fit, newdata = data.frame(age=c(28)), interval = 'confidence')
predict(fit, newdata = data.frame(age=c(28)), interval = 'predict')

#'Explain why the prediction interval obtained may not be accurate to predict the weight of a randomly sampled American who is 28.
'Because the prediction interval is so wide from variation in the data, using it to predict an individuals weight is not very useful'


