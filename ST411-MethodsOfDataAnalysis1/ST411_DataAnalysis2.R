load(url("http://stat511.cwick.co.nz/homeworks/housework.rda"))
library(ggplot2)
library(coin)
source(url("http://stat511.cwick.co.nz/code/stat_qqline.r"))

#'Question 1 - Is there evidence the mean difference in the number of hours of time spent 
#'on household work between the husband and wife, depends on the wife's education level?

'Fitted model of data'
fit <- lm(diff_hours ~ wife_degree, data = housework)

'Creating vectors for hours spent on household chores for the different wife levels of education'
avgs <- with(housework, tapply(diff_hours, wife_degree, mean))
sds <- with(housework, tapply(diff_hours, wife_degree, sd))
ns <- with(housework, tapply(diff_hours, wife_degree, length))
avgs
sds
ns

'Histogram plots show exponential distribution'
qplot(diff_hours, data = housework, main = "Difference in Hours Spent Doing Household Work Between Wife Education Levels", xlab = 'Diffence in Hours', ylab = 'Count') + facet_wrap(~wife_degree, ncol = 2, scale = "free_y")

'Assumption of normality'
qplot(sample = diff_hours, data = housework, 
      main = "Difference in Hours Doing Household Work Between Wife Education Levels", 
      xlab = 'Diffence in Hours', ylab = 'Count') + facet_wrap(~wife_degree, ncol = 1, scales = 'free_y') + stat_qqline()
ns

'Boxplot test for equal std deviations'
qplot(factor(wife_degree), diff_hours, data = housework, geom = 'boxplot', 
      main = 'Boxplots for Wife Degree vs Difference in Hours of Housework', xlab = 'Wife Degree', ylab = 'Difference in Hours')
qplot(.fitted, .resid, data = fit, geom = 'boxplot', group = wife_degree, 
      main = 'Fitted vs Residual Group Means', xlab = 'Fitted', ylab = 'Residuals') 
sds

'There appears to be an outlier point in the High School and Graduate groups. Check One Way ANOVA with and without outliers'

'One Way ANOVA with all data points'
oneway.test(diff_hours ~ wife_degree, data = housework, var.equal = TRUE)
full_model <- lm(diff_hours ~ wife_degree, data = housework)
anova(full_model)

Fu'One Way ANOVA without the outlier points'
noOutliers <- subset(housework, diff_hours > -60 & diff_hours < 60)
qplot(factor(wife_degree), diff_hours, data = noOutliers)

full_model2 <- lm(diff_hours ~ wife_degree, data = noOutliers)
anova(full_model2)

#'The p-value for the ANOVA the raw data is 0.07, while the ANOVA after
#'taking out the two outlier points results in a p-value of 0.15.
#'This shows that the ANOVA test is no resistant to outliers.

kruskal_test(diff_hours ~ wife_degree, data = housework)

#'The Kruskal Wallis test shows much better resilience to the outliers in the data, 
#'but tests the hypothesis that the medians rather than means are equal. 



#'Question 2 - For each of the wife education levels, by how many hours does the mean 
#'difference in time spent between husband and wife, differ from that of the next lowest education category?

'Testing sample distribution normality'
hs <- subset(housework, wife_degree == "HIGH SCHOOL")
jc <- subset(housework, wife_degree == "JUNIOR COLLEGE")
ba <- subset(housework, wife_degree == "BACHELOR")
gr <- subset(housework, wife_degree == "GRADUATE")

qplot(sample = hs$diff_hours) + stat_qqline()
qplot(sample = jc$diff_hours) + stat_qqline()
qplot(sample = ba$diff_hours) + stat_qqline()
qplot(sample = gr$diff_hours) + stat_qqline()

'Displaying results'
avgs
sds
ns

#'Calculating t-test with pooled standard deviation
sp <- sqrt(sum((ns - 1)*sds^2)/sum(ns - 1))

'HS vs JC'
tstat1 <- (avgs[2] - avgs[1])/(sp*sqrt(1/ns[2] + 1/ns[1]))
tstat1 
2*pt(tstat1, sum(ns) - length(ns))

'JC vs BA'
tstat2 <- (avgs[2] - avgs[3])/(sp*sqrt(1/ns[3] + 1/ns[2]))
tstat2
2*pt(tstat2, sum(ns) - length(ns))

'BA vs GR'
tstat3 <- (avgs[4] - avgs[3])/(sp*sqrt(1/ns[4] + 1/ns[3]))
tstat3 
2*pt(tstat3, sum(ns) - length(ns))

'Set up pairwise comparison model'
library(multcomp)

full_model <- lm(diff_hours ~ wife_degree, data = housework)
comparisons <- glht(full_model, linfct = mcp(wife_degree = "Tukey"))

'LSD tests for two group comparisons'
summary(comparisons, test = adjusted("none"))

'LSD confidence intervals'
confint(comparisons, calpha = univariate_calpha())




