#'Data analysis 1 
#'
#'Question 1*********************
library(ggplot2)
source(url("http://stat511.cwick.co.nz/code/stat_qqline.r"))
acs <- read.csv(url("http://stat511.cwick.co.nz/homeworks/acs_or.csv"))
acs2 <- read.csv(url("http://stat511.cwick.co.nz/homeworks/acs_or2.csv"))

#'Adding column of age difference to acs
acs$ageDiff <- with(acs, age_husband - age_wife)

#'Creating subset without childing, and adding column of sample numbers and 
noChildren <- subset(acs, number_children == 0)
noChildren$sampleNum <- with(noChildren, rank(household))

mean(noChildren$age_husband)
sd(noChildren$age_husband)
sd(noChildren$age_husband)/mean(noChildren$age_husband)
length(noChildren$age_husband)
mean(noChildren$age_wife)
sd(noChildren$age_wife)
sd(noChildren$age_wife)/mean(noChildren$age_wife)
length(noChildren$age_wife)

#'Plot of Husband and Wife ages
qplot(ageDiff, data = acs,
      main = 'Age Difference Between Husbands and Wives Without Children', 
      xlab = 'Age Differencev (yrs)', ylab = 'Count of Age Difference')
mean(acs$ageDiff)
sd(acs$ageDiff)

qplot(age, data = acs2, main = 'Equal Variance Analysis for the Age of Husbands and Wives') + facet_wrap(~person, ncol = 1)

#'Normal probability plots 

qplot(sample = noChildren$ageDiff, 
       main = "Normal Probability Plot for Age Difference", xlab = 'Theoretical', ylab = 'Samples') + stat_qqline()

qqnorm(noChildren$ageDiff, main = "Normal Probability Plot for Age Difference", xlab = 'Theoretical', ylab = 'Samples') 
qqline(noChildren$ageDiff)

qplot(sample = noChildren$age_husband, main = "Normal Probability Plot for Age of Husbands", 
       xlab = 'Theoretical', ylab = 'Samples') 
#'
#'qqnorm(noChildren$age_wife, main = "Normal Probability Plot for Age of Wives",
#'       xlab = 'Theoretical', ylab = 'Samples') + qqline(noChildren$age_wife)


#'qplot(sampleNum, age_husband, data = noChildren, xlim = c(0,5200), ylim = c(0,125), main = 'Age of Husbands Without Children' ,xlab = 'Household Number', ylab = 'Husband Age (yrs)')
#'qplot(sampleNum, age_wife, data = noChildren, xlim = c(0,5200), ylim = c(0,125), main = 'Age of Wives without Children', xlab = 'Household Number', ylab = 'Wife Age (yrs)')

t.test(noChildren$age_husband, noChildren$age_wife, paired = TRUE) 


'Question 2***************'
acs$old_house <- ifelse(acs$decade_built >= 1970, "1970's or later", "1960's or earlier")

#'Subsetting old and new household data, and creating column of sample numbers
houseOld <- subset(acs, old_house == "1960's or earlier")
houseOld$sampleNum <- with(houseOld, rank(household))
houseNew <- subset(acs, old_house == "1970's or later")
houseNew$sampleNum <- with(houseNew, rank(household))

'Adding column of log values for electricity expenditure'
acs$logElectricity <- with(acs, log(electricity))

mean(houseOld$electricity)
length(houseOld$electricity)
mean(houseNew$electricity)
length(houseNew$electricity)

'Jitter Plots for summary'
qplot(decade_built, electricity, data = houseOld, geom = 'jitter', ylim = c(0,600), 
      main = 'Electricity Expenditure of Households Built in 1960s or Earlier', xlab = 'Decade Built', ylab = 'Electricity Expenditure ($)') + stat_qqline()
qplot(decade_built, electricity, data = houseNew, geom = 'jitter', ylim = c(0,600), 
      main = 'Electricity Expenditure of Households Built in 1970s or Later', xlab = 'Decade Built', ylab = 'Electricity Expenditure ($)')

'Histograms for equal standard deviations'
qplot(electricity, data = acs,  
      main = 'Electricity Usage of Houses Built Before Before and After 1970', 
      xlab = 'Electricity Usage ($/mo)', ylab = 'Count') + facet_wrap(~old_house, ncol = 1)
sd(houseOld$electricity)
sd(houseNew$electricity)

qplot(sample = electricity, data = acs) + facet_wrap(~old_house) + stat_qqline()

'Log transform of data'
qplot(log(electricity), data = acs,  
      main = 'Log of Electricity Usage of Houses Built Before Before and After 1970', 
      xlab = 'Electricity Usage ($/mo)', ylab = 'Count') + facet_wrap(~old_house, ncol = 1)
sd(houseOld$logElectricity)
sd(houseNew$logElectricity)

qplot(sample = logElectricity, data = acs, 
      main = 'Normal Probability Plot for Log of Electricity Expenditure', 
      xlab = 'log(theoretical)', ylab = 'sample') + facet_wrap(~old_house) + stat_qqline()


t.test(houseNew$logElectricity, houseOld$logElectricity)
exp(4.728033 - 4.711319)
exp(-0.00961147)
exp(0.04303978)

