library(ggplot2)
library(Sleuth3)

act <- data.frame(ex1320)
?ex1320
attach(act)

# Part a 
par(mfrow=c(1,2))
plot(Sex, Score, main = "Sex vs Score Boxplot")
plot(Background, Score, main = "Background vs Score Boxplot")

# Part b
nonAdditive <- lm(Score~Sex*Background)
summary(nonAdditive)
anova(nonAdditive)

# Part c
par(mfrow=c(1,1))
interaction.plot(Sex, Background, Score, main="Interaction Plot", ylab = "Mean ACT Score")

# Part d
additive <- lm(Score~Sex + Background)
anova(additive)

# Part e
anova(nonAdditive, additive)

# Part f
confint(additive, 2)
''
''

