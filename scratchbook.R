#Project Scratch Book

library(magrittr)
library(ggplot2)
library(tidyverse)
library(class)
library(caret)
library(e1071)
library(lsmeans)

df_raw = read.csv(file = 'C:\\Users\\amada\\OneDrive\\Desktop\\RStudioFiles\\Scripts\\SMU\\Term1\\DS6306\\CaseStudy2\\CaseStudy2_data.csv')
str(df_raw)

no = df_raw %>% filter(Attrition == "No")
yes = df_raw %>% filter(Attrition == "Yes")

##P1 CaseStudy 2

#Age
t.test(no$Age, yes$Age)

#MonthlyIncome
t.test(no$MonthlyIncome, yes$MonthlyIncome)

#TotalWorkingYears
t.test(no$TotalWorkingYears, yes$TotalWorkingYears)

#OverTime
table(df_raw$Attrition, df_raw$OverTime)
chi = chisq.test(df_raw$Attrition, df_raw$OverTime)
chi

##P2 Job Role

#JR v Age
df_raw %>% ggplot(aes(y = reorder(JobRole, Age), x = Age)) + geom_boxplot(fill = "Cornflower blue") + 
  ggtitle("Job Role v Age Boxplots") + ylab("Job Role")

#JR v Education
df_raw %>% group_by(JobRole) %>%
  summarise_at(vars(Education), list(EducationMean = mean)) %>%
  ggplot(aes(y = reorder(JobRole, EducationMean), x = EducationMean)) + geom_col(fill = "Cornflower blue") + 
  ggtitle("Job Role v Mean Education Score Column Plot") + xlab("Education Score Mean") + ylab("Job Role")

#JR v Education Field
df_raw %>% ggplot(aes(y = JobRole, fill = as.factor(EducationField))) + geom_bar(position="fill") + 
  ggtitle("Job Role v Education Field Barplot") + xlab("Count") + ylab("Job Role")

#MonthlyIncome
df_raw %>% ggplot(aes(y = reorder(JobRole, MonthlyIncome), x = MonthlyIncome)) + 
  geom_boxplot(fill = "Cornflower blue") + ggtitle("Job Level v Monthly Income Boxplots") + 
  ylab("Job Role")







#Look into a bit further

#JR v DistanceFRomHome
df_raw %>% ggplot(aes(y = JobRole, x = DistanceFromHome)) + geom_boxplot()
#Doesnt seem too different

#JobLevel
df_raw %>% ggplot(aes(y = JobRole, fill = as.factor(JobLevel))) + geom_bar(position="fill")
#This ones cool

#TotalWorkingYears
df_raw %>% ggplot(aes(y = reorder(JobRole, TotalWorkingYears), x = TotalWorkingYears)) + geom_boxplot()
df_raw %>% ggplot(aes(x = TotalWorkingYears, fill = JobRole)) + geom_density() + facet_wrap(~JobRole)

## P3 Personal Analysis

#Dep v MonthlyIncome
df_raw %>% ggplot(aes(y = Department, x = MonthlyIncome)) + geom_boxplot() + 
  ggtitle("Department v MonthlyIncome Boxplots")
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = Department)) + geom_density() + 
  facet_wrap(~Department) + ggtitle("Department v MonthlyIncome Density Plots")

#Years at company v Income
df_raw %>% ggplot(aes(y = MonthlyIncome, x = YearsAtCompany)) + geom_point() + geom_smooth(method = lm) + 
  ggtitle("MonthlyIncome v YearsAtCompany")
fit2 = lm(df_raw$MonthlyIncome~ df_raw$YearsAtCompany)
summary(fit2)
confint(fit2)

#Total Working Years v Income
df_raw %>% ggplot(aes(y = MonthlyIncome, x = TotalWorkingYears)) + geom_point()  + geom_smooth(method = lm)
  + ggtitle("MonthlyIncome v TotalWorkingYears")
fit1 = lm(df_raw$MonthlyIncome ~ df_raw$TotalWorkingYears)
summary(fit1)
confint(fit1)






#Cts Cts Relationships
df_raw %>% ggplot(aes(y = MonthlyIncome, x = Age)) + geom_point()

#Educ v Income
df_raw %>% ggplot(aes(x = MonthlyIncome, y = as.factor(Education))) + geom_boxplot()

