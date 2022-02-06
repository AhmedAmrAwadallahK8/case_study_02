#EDA for MonthlyIncome Model
library(magrittr)
library(ggplot2)
library(tidyverse)

df_raw = read.csv(file = 'C:\\Users\\amada\\OneDrive\\Desktop\\RStudioFiles\\Scripts\\SMU\\Term1\\DS6306\\CaseStudy2\\CaseStudy2_data.csv')
str(df_raw)

#Summary of possibly useful variables for a model
##Strong Relationship Present
#Age: 
#Education: 
#JobLevel: 
#JobRole: 
#TotalWorkingYears: 


##Moderate
#Department: 
#DistanceFromHome: 
#EducationField: 

##Unique
#StockOptionLevel: Stock option level 2 is a strong indicator for wealth for some reason


##Slight to Moderate
#NumCompaniesWorked
#PercentSalaryHike


##Slight
#Gender: 
#MaritalStatus: 
#PerformanceRating: 
#RelationshipSatisfaction: 


#MI v Age
df_raw %>% ggplot(aes(y=MonthlyIncome, x=Age)) + geom_point()
#Relationship Present

#MI v BusinessTravel
df_raw %>% ggplot(aes(y=MonthlyIncome, x=BusinessTravel)) + geom_boxplot()
#Not Much

#MI v Department
df_raw %>% ggplot(aes(y=MonthlyIncome, x=Department)) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = Department)) + geom_density(alpha=0.5)
#Some relationship present

#MI v DistanceFromHome
df_raw %>% ggplot(aes(y=MonthlyIncome, x=DistanceFromHome)) + geom_point()
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(DistanceFromHome))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(DistanceFromHome))) + geom_density(alpha=0.5)
#Moderate Relationship Present

#MI v Education
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(Education))) + geom_boxplot()
#Strong Relationship present

#MI v EducationField
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(EducationField))) + geom_boxplot()
#Relationship present

#MI v EnvironmentSatisfaction
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(EnvironmentSatisfaction))) + geom_boxplot()
#Not Much

#MI v Gender
df_raw %>% ggplot(aes(y=MonthlyIncome, x=Gender)) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = Gender)) + geom_density(alpha=0.5)
#Slight

#MI v JobInvolvement
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(JobInvolvement))) + geom_boxplot()
#Not Much

#MI v JobLevel
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(JobLevel))) + geom_boxplot()
#Strong Relationship Present

#MI v JobRole
df_raw %>% ggplot(aes(x=MonthlyIncome, y=as.factor(JobRole))) + geom_boxplot()
#Strong Relationship Present

#MI v JobSatisfaction
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(JobSatisfaction))) + geom_boxplot()
#Not Much

#MI v MaritalStatus
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(MaritalStatus))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = MaritalStatus)) + geom_density(alpha=0.5)
#Slight

#MI v NumCompaniesWorked
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(NumCompaniesWorked))) + geom_boxplot()

#Slight to Moderate

#MI v OverTime
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(OverTime))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = OverTime)) + geom_density(alpha=0.5)
#Not Much

#MI v PercentSalaryHike
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(PercentSalaryHike))) + geom_boxplot()
#Slight to Moderate

#MI v PerformanceRating
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(PerformanceRating))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(PerformanceRating))) + geom_density(alpha=0.5)
#Slight

#MI v RelationshipSatisfaction
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(RelationshipSatisfaction))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(RelationshipSatisfaction))) + geom_density(alpha=0.5)
#Slight

#MI v StockOptionLevel
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(StockOptionLevel))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(StockOptionLevel))) + geom_density(alpha=0.5)
#Slight but with a twist
#Stock option level 2 adds quite a bit

#MI v TotalWorkingYears xxxx
df_raw %>% ggplot(aes(y=MonthlyIncome, x=TotalWorkingYears)) + geom_point()
#Strong Relationship

#MI v WorkLifeBalance
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(WorkLifeBalance))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(WorkLifeBalance))) + geom_density(alpha=0.5)
#Not Much

#MI v YearsAtCompany
df_raw %>% ggplot(aes(y=MonthlyIncome, x=YearsAtCompany)) + geom_point()
#Relationship Present

#MI v YearsInCurrentRole
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(YearsInCurrentRole))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(YearsInCurrentRole))) + geom_density(alpha=0.5)
#Relationship Present

#MI v YearsSinceLastPromotion
df_raw %>% ggplot(aes(y=MonthlyIncome, x=YearsSinceLastPromotion)) + geom_point()
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(YearsSinceLastPromotion))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(YearsSinceLastPromotion))) + geom_density(alpha=0.5)
#Relationship Present

#MI v YearsWithCurrManager
df_raw %>% ggplot(aes(y=MonthlyIncome, x=YearsWithCurrManager)) + geom_point()
df_raw %>% ggplot(aes(y=MonthlyIncome, x=as.factor(YearsWithCurrManager))) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = as.factor(YearsWithCurrManager))) + geom_density(alpha=0.5)
#Relationship Present