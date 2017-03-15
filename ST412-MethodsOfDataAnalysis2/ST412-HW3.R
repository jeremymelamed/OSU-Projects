# ST 412 HW 3
library(ggplot2)
algae <- read.csv("C:\\Users\\Jeremy\\OneDrive\\Documents\\Winter 2016-Jeremy\\ST 412\\HW\\density.csv")

# Part (a)
ggplot(algae, aes(day, log_density)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  xlab("day") +
  ylab("log-density") +
  ggtitle("Plot of log-density vs day for algae density")

# Part (b)
cor(algae$day, algae$log_density, method = "pearson")

# Part (c)
algae$day2 <- (algae$day)^2
algae$day3 <- (algae$day)^3
algae$day4 <- (algae$day)^4

mod1 <- lm(log_density ~ day, data = algae)
mod2 <- lm(log_density ~ day + day2, data = algae)
mod3 <- lm(log_density ~ day + day2 + day3, data = algae)
mod4 <- lm(log_density ~ day + day2 + day3 + day4, data = algae)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

# Part (d)
algae$day5 <- (algae$day)^5
algae$day6 <- (algae$day)^6
algae$day7 <- (algae$day)^7
algae$day8 <- (algae$day)^8
algae$day9 <- (algae$day)^9
algae$day10 <- (algae$day)^10
algae$day11 <- (algae$day)^11
algae$day12 <- (algae$day)^12
algae$day13 <- (algae$day)^13
algae$day14 <- (algae$day)^14
algae$day15 <- (algae$day)^15
mod13 <- lm(log_density ~ day + day2 + day3 + day4 + day5 + day6 + day7 + day8 + day9 + day10 + day11 + day12, data = algae)
summary(mod13)

# Part e
require(gridExtra)
algae$resid1 <- residuals(mod1)
algae$resid2 <- residuals(mod2)
algae$resid3 <- residuals(mod3)
algae$resid4 <- residuals(mod4)
algae$fit1 <- fitted(mod1)
algae$fit2 <- fitted(mod2)
algae$fit3 <- fitted(mod3)
algae$fit4 <- fitted(mod4)

p1 <- ggplot(algae, aes(day, resid1)) + geom_point() + geom_hline(yintercept = 0) + ylab("residual") + ggtitle("Residual vs Day plot for Model 1")
p2 <- ggplot(algae, aes(day, resid2)) + geom_point() + geom_hline(yintercept = 0) + ylab("residual") + ggtitle("Residual vs Day plot for Model 2")
p3 <- ggplot(algae, aes(day, resid3)) + geom_point() + geom_hline(yintercept = 0) + ylab("residual") + ggtitle("Residual vs Day plot for Model 3")
p4 <- ggplot(algae, aes(day, resid4)) + geom_point() + geom_hline(yintercept = 0) + ylab("residual") + ggtitle("Residual vs Day plot for Model 4")
grid.arrange(p1,p2,p3,p4, ncol=2)

p1 <- ggplot(algae, aes(fit1, resid1)) + geom_point() + geom_hline(yintercept = 0) + xlab("fit") + ylab("residual") + ggtitle("Residual vs Fitted plot for Model 1")
p2 <- ggplot(algae, aes(fit2, resid2)) + geom_point() + geom_hline(yintercept = 0) + xlab("fit") + ylab("residual") + ggtitle("Residual vs Fitted plot for Model 2")
p3 <- ggplot(algae, aes(fit3, resid3)) + geom_point() + geom_hline(yintercept = 0) + xlab("fit") + ylab("residual") + ggtitle("Residual vs Fitted plot for Model 3")
p4 <- ggplot(algae, aes(fit4, resid4)) + geom_point() + geom_hline(yintercept = 0) + xlab("fit") + ylab("residual") + ggtitle("Residual vs Fitted plot for Model 4")
grid.arrange(p1,p2,p3,p4, ncol=2)

p1 <- ggplot(algae, aes(sample = resid1)) + stat_qq() + ggtitle("Normal Probability Plot for Model 1")
p2 <- ggplot(algae, aes(sample = resid2)) + stat_qq() + ggtitle("Normal Probability Plot for Model 2")
p3 <- ggplot(algae, aes(sample = resid3)) + stat_qq() + ggtitle("Normal Probability Plot for Model 3")
p4 <- ggplot(algae, aes(sample = resid4)) + stat_qq() + ggtitle("Normal Probability Plot for Model 4")
grid.arrange(p1,p2,p3,p4, ncol=2)

# Part f
anova(mod1,mod2)
anova(mod2,mod3)
anova(mod3,mod4)

