#Case Study 2 R Scratch
library(magrittr)
library(ggplot2)
library(tidyverse)

df_raw = read.csv(file = 'C:\\Users\\amada\\OneDrive\\Desktop\\RStudioFiles\\Scripts\\SMU\\Term1\\DS6306\\CaseStudy2\\CaseStudy2_data.csv')
str(df_raw)
#Need to Predict MonthlyIncome(Numeric) and Attrition(Categorical)

#Objectives

#Identify Top 3 Factors that contribute to attrition
#Job Specific Trends
#One other interesting thing i Find
#Build an attrition prediction model (Categorical Prediction)
#Build a MonthlyIncome prediction model (Numeric Prediction)

#Explore Data in relaton to Attrition for Fun
colnames(df_raw)
str(df_raw)
#All Variables and what they represent
#ID - (Int) ID Of Person
#Age - (Int) Age of Person
#Attrition - (Chr) Yes if they left No if they stayed
#BusinessTravel - (Chr) Multiple categories describing frequency of travel
#DailyRate - (Int) UNSURE
#Department - (Chr) Section of business this person works for
#DistanceFromHome - (Int) Unit is not specified but can assume larger means further
#Education - (Int) Not clear what the numbers mean UNSURE
#EducationField - (Chr) Type of field education is in
#EmployeeCount - (Int) How many employees this person is in charge of? UNSURE
#EmployeeNumber - (Int) Another Identification Metric? UNSURE
#EnvironmentSatisfaction - (Int) Assume its a rating system on the business environment. Maybe 5 is excellent and 1 is terrible. UNSURE
#Gender - (Chr) Male/Female
#HouryRate - (Int) UNSURE
#JobInvolvement - (Int) UNSURE
#JobLevel - (Int) Higher level the more important the job
#JobRole - (Chr) Descriptive Job Data
#JobSatisfaction - (Int) Rating system. Higher prob means better
#MaritalStatus - (Chr) Marriage info
#MonthlyIncome - (Int) Income Per Month
#MonthlyRate - (Int) UNSURE
#NumCompaniesWorked - (Int) Previous Numer of Companies
#Over18 - (Chr) Binary Category
#OverTime - (Chr) Category
#PercentSalaryHike - (Int) Percent Increase in Salary (When and why)
#PerformanceRating - (Int) Assuming that 5 is best and 1 is worst UNSURE
#RelationshipSatisfaction - (Int) Relationship with manager
#StandardHours - (Int) Hours per week
#StockOptionLevel - (Int) Not sure what this signifies
#TotalWorkingYears - (Int) Years in workforce I assume
#TrainTimesLastYear - (Int) Numberof Trainings Attended
#WorkLifeBalance - (Int) Assuming that 5 means good work life balance a 1 means bad
#YearsATCompany - (Int) Yeah
#YearsInCurrentRole - (Int) Yeah
#YearsSinceLastPromotion - (Int) Yeah
#YearsWithCurrManager - (Int) Yeah

#Attrition Relationship with every variable
str(df_raw)
#Age
df_raw %>% ggplot(aes(x = Attrition, y = Age)) + geom_boxplot()
df_raw %>% ggplot(aes(x = Age, fill = Attrition)) + geom_density(alpha=0.5)
#Looks like the people leaving are younger
#Check if this difference is statistically significant

#Business Travel
df_raw %>% ggplot(aes(x = Attrition, fill = BusinessTravel)) + geom_bar(position="fill")
#Doesn't seem like there is a large difference in attrition based on travel

#Daily Rate
df_raw %>% ggplot(aes(x = Attrition, y = DailyRate)) + geom_boxplot()
#Possible Difference Here but it is small
#Stat check later

#Department
df_raw %>% ggplot(aes(x = Attrition, fill = Department)) + geom_bar(position="fill")
#Larger portion of people who leave are from the sales department

#DistanceFromHome
df_raw %>% ggplot(aes(x = Attrition, y = DistanceFromHome)) + geom_boxplot()
df_raw %>% ggplot(aes(x = DistanceFromHome, fill = Attrition)) + geom_density(alpha=0.5)
#DistanceFromHome may be a small reason

#Education
df_raw %>% ggplot(aes(x = Attrition, y = Education)) + geom_boxplot()
df_raw %>% ggplot(aes(x = Education, fill = Attrition)) + geom_density(alpha=0.5)
#Education doesnt appear to be a reason

#Education Field
df_raw %>% ggplot(aes(x = Attrition, fill = EducationField)) + geom_bar(position="fill")
#Slight Evidence that people who leave are more from Marketing, Technical Degree or HR

#EmployeeCount
df_raw %>% ggplot(aes(x = Attrition, y = EmployeeCount)) + geom_boxplot()
#Variable doesnt seem useful

#EmployeeNumber
df_raw %>% ggplot(aes(x = Attrition, y = EmployeeNumber)) + geom_boxplot()
#Not sure what this valeu represents
#Likely no difference regardless

#EnvironmentSatisfaction
df_raw %>% ggplot(aes(x = Attrition, y = EnvironmentSatisfaction)) + geom_boxplot()
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(EnvironmentSatisfaction))) + geom_bar(position="fill")
df_raw %>% ggplot(aes(x = EnvironmentSatisfaction, fill = Attrition)) + geom_density(alpha=0.5)

#Median appears the same but there is a large skew for people who do end up leaving, interesting
#More give a lower score if they did end up leaving

#Gender
df_raw %>% ggplot(aes(x = Attrition, fill = Gender)) + geom_bar(position="fill")
#Doesn't seem to be a deciding factor

#HourlyRate
df_raw %>% ggplot(aes(x = Attrition, y = HourlyRate)) + geom_boxplot()
#slight Difference

#JobInvolvement
df_raw %>% ggplot(aes(x = Attrition, y = JobInvolvement)) + geom_boxplot()
df_raw %>% ggplot(aes(x = JobInvolvement, fill = Attrition)) + geom_density(alpha=0.5)
#No visible difference

#JObLevel
df_raw %>% ggplot(aes(x = Attrition, y = JobLevel)) + geom_boxplot()
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(JobLevel))) + geom_bar(position="fill")
df_raw %>% ggplot(aes(x = JobLevel, fill = Attrition)) + geom_density(alpha = 0.5)
#Moderate. People who leave tend to have a lower job level

#JobRole
df_raw %>% ggplot(aes(x = Attrition, fill = JobRole)) + geom_bar(position="fill")
#People who leave tend moreso to be
#Lab Tech
#HealthCare Rep
#Sales Representative - This one has the biggest diff

#JabSatisfaction
df_raw %>% ggplot(aes(x = Attrition, y = JobSatisfaction)) + geom_boxplot()
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(JobSatisfaction))) + geom_bar(position="fill")
df_raw %>% ggplot(aes(x = JobSatisfaction, fill = Attrition)) + geom_density(alpha=0.5)
#Moderate difference
#Less 4s for ppl who left

#MaritalStatus
df_raw %>% ggplot(aes(x = Attrition, fill = MaritalStatus)) + geom_bar(position="fill")
#Larger percentage of people who leave are single

#MonthlyIncome
df_raw %>% ggplot(aes(x = Attrition, y = MonthlyIncome)) + geom_boxplot()
df_raw %>% ggplot(aes(x = MonthlyIncome, fill = Attrition)) + geom_density(alpha=0.5)
#Big Diff. Those who leave tend to have a lower income
#Check statistical Significance

#MonthlyRate
df_raw %>% ggplot(aes(x = Attrition, y = MonthlyRate)) + geom_boxplot()
#Slight Difference
#Unsure what this variable means

#NumCompaniesWorked
df_raw %>% ggplot(aes(x = Attrition, y = NumCompaniesWorked)) + geom_boxplot()
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(NumCompaniesWorked))) + geom_bar(position="fill")
df_raw %>% ggplot(aes(x = NumCompaniesWorked, fill = Attrition)) + geom_density(alpha=0.5)
#Might be slight evidence the more companies you work out the less likely you are to leave
#Can stat check later

#Over18
df_raw %>% ggplot(aes(x = Attrition, fill = Over18)) + geom_bar(position="fill")
#Everyone is over18 lol

#Overtime
df_raw %>% ggplot(aes(x = Attrition, fill = OverTime)) + geom_bar(position="fill")
#Larger percentage of people who quit are working overtime

#PercentSalaryHike
df_raw %>% ggplot(aes(x = Attrition, y = PercentSalaryHike)) + geom_boxplot()
#No Difference

#PerformanceRating
df_raw %>% ggplot(aes(x = PerformanceRating)) + geom_histogram() + facet_wrap(~Attrition)
#Doenst seem useful

#RelationshipSatisfaction
df_raw %>% ggplot(aes(x = Attrition, y = RelationshipSatisfaction)) + geom_boxplot()
df_raw %>% ggplot(aes(x = RelationshipSatisfaction)) + geom_histogram() + facet_wrap(~Attrition)
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(RelationshipSatisfaction))) + geom_bar(position="fill")
#More 1s and 2s for ppl who leave

#StandardHours
df_raw %>% ggplot(aes(x = Attrition, y = StandardHours)) + geom_boxplot()
df_raw %>% ggplot(aes(x = StandardHours)) + geom_histogram() + facet_wrap(~Attrition)
#Everyone has 80 for this. Not useful

#StockOptionLevel
df_raw %>% ggplot(aes(x = Attrition, y = StockOptionLevel)) + geom_boxplot()
df_raw %>% ggplot(aes(x = StockOptionLevel)) + geom_histogram() + facet_wrap(~Attrition)
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(StockOptionLevel))) + geom_bar(position="fill")
df_raw %>% ggplot(aes(x = StockOptionLevel, fill = Attrition)) + geom_density(alpha=0.5)
#Pppl who leave tend to have lower stock option levels

#TotalWorkingYears
df_raw %>% ggplot(aes(x = Attrition, y = TotalWorkingYears)) + geom_boxplot()
df_raw %>% ggplot(aes(x = TotalWorkingYears, fill = Attrition)) + geom_density(alpha=0.5)
#Large Difference
#Check Stats l8r

#TRainingTimesLastYear
df_raw %>% ggplot(aes(x = Attrition, y = TrainingTimesLastYear)) + geom_boxplot()
df_raw %>% ggplot(aes(x = StockOptionLevel, fill = Attrition)) + geom_density()
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(TrainingTimesLastYear))) + geom_bar(position="fill")

#People who leave have more extreme distributions

#WorkLifeBalance
df_raw %>% ggplot(aes(x = Attrition, y = WorkLifeBalance)) + geom_boxplot()
df_raw %>% ggplot(aes(x = Attrition, fill = as.factor(WorkLifeBalance))) + geom_bar(position="fill")
#Slight DIfference
#Ppl who leave tended to have a little bit more 1s reported

#YearsAtCompany
df_raw %>% ggplot(aes(x = Attrition, y = YearsAtCompany)) + geom_boxplot()
#Difference Present
#People who leave tended to work at the company less
#Bit of an odd variable

#YearsInCurrentRole
df_raw %>% ggplot(aes(x = Attrition, y = YearsInCurrentRole)) + geom_boxplot()
df_raw %>% ggplot(aes(x = YearsInCurrentRole, fill = Attrition)) + geom_density(alpha=0.5)
#People who leave tend to not have many years in current role
#This paired with yearsatcompany may show a behavior of loyalty

#YearsSinceLastPromotion
df_raw %>% ggplot(aes(x = Attrition, y = YearsSinceLastPromotion)) + geom_boxplot()
df_raw %>% ggplot(aes(x = YearsSinceLastPromotion, fill = Attrition)) + geom_density()

#Not much of a difference

#YearsWithCurrManager
df_raw %>% ggplot(aes(x = Attrition, y = YearsWithCurrManager)) + geom_boxplot()
df_raw %>% ggplot(aes(x = YearsWithCurrManager, fill = Attrition)) + geom_density()
#Not much of a difference

