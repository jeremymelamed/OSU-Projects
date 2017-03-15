library(ggplot2)
chernobyl <- read.csv('C:\\Users\\Jeremy\\OneDrive\\Documents\\Winter 2016-Jeremy\\ST 412\\HW\\ex1111.csv')

# Part a
attach(chernobyl)
par(mfrow=c(1,1))
ggplot(chernobyl, aes(Soil, Mushroom)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
plot(Soil, Mushroom, main = "Soil vs Mushroom")
abline(lm(Mushroom~Soil))
identify(Soil,Mushroom)
'Outlier observation at row 17'

# Part b
chernobyl.fit <- lm(Mushroom~Soil)
summary(chernobyl.fit)

# Part c
chernobyl$resid <- residuals(chernobyl.fit)
chernobyl$fitted <- fitted(chernobyl.fit)

ggplot(chernobyl, aes(fitted,resid)) + geom_point() + geom_hline(yintercept = 0) +
  ggtitle("Residual vs Fitted Plot") +
  xlab("Fitted Values") +
  ylab("Residuals")
plot(chernobyl$fitted, chernobyl$resid)

# Part d
par(mfrow=c(1,2))
plot(chernobyl.fit) 

rstud <- rstudent(chernobyl.fit)
plot(1:17, rstud, main = "Studentized Residual Plot", xlab = "Observations", ylab = "Studentized Residuals",cex=0);text(1:17,rstud);abline(0,0,lty=2);abline(2,0);abline(-2,0)

cooks <- cooks.distance(chernobyl.fit)
plot(1:17, cooks, main = "Cooks Distance Plot", xlab = "Observations", ylab = "Cooks Distance",cex=0);text(1:17,cooks)

# Part e
data.new <- chernobyl[-c(17),]

new.fit <- lm(Mushroom~Soil, data = data.new)
summary(new.fit)

## Log Transform Analysis
log.chernobyl <- log(chernobyl)

# Part a
attach(log.chernobyl)
ggplot(log.chernobyl, aes(Soil, Mushroom)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
plot(Soil, Mushroom, main = "log(Soil) vs log(Mushroom)", xlab = "log(Soil)", ylab = "log(Mushroom)")
abline(lm(Mushroom~Soil))
identify(Soil,Mushroom)

# Part b
log.chernobyl.fit <- lm(Mushroom~Soil)
summary(log.chernobyl.fit)

# Part c
log.chernobyl$resid <- residuals(log.chernobyl.fit)
log.chernobyl$fitted <- fitted(log.chernobyl.fit)

ggplot(log.chernobyl, aes(fitted,resid)) + geom_point() + geom_hline(yintercept = 0) +
  ggtitle("Residual vs Fitted Plot") +
  xlab("Fitted Values") +
  ylab("Residuals")
plot(chernobyl$fitted, chernobyl$resid)

# Part d
par(mfrow=c(1,2))
plot(log.chernobyl.fit) 

rstud <- rstudent(log.chernobyl.fit)
plot(1:17, rstud, main = "Studentized Residual Plot", xlab = "Observations", ylab = "Studentized Residuals",cex=0);text(1:17,rstud);abline(0,0,lty=2);abline(2,0);abline(-2,0)

cooks <- cooks.distance(log.chernobyl.fit)
plot(1:17, cooks, main = "Cooks Distance Plot", xlab = "Observations", ylab = "Cooks Distance",cex=0);text(1:17,cooks)

# Part e
data.new <- log.chernobyl[-c(1),]

new.fit <- lm(Mushroom~Soil, data = data.new)
summary(new.fit)
