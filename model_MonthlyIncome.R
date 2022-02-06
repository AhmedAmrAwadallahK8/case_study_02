#Monthly Income Model
library(magrittr)
library(ggplot2)
library(tidyverse)
library(class)
library(caret)
library(e1071)
library(lsmeans)

#Load
df_raw = read.csv(file = 'C:\\Users\\amada\\OneDrive\\Desktop\\RStudioFiles\\Scripts\\SMU\\Term1\\DS6306\\CaseStudy2\\CaseStudy2_data.csv')
str(df_raw)

#Select Variables that have a relationship with MonthlyIncome

MI_model_df_raw = df_raw %>% select("Age", "Education", "JobLevel", "JobRole", "TotalWorkingYears",
                             "Department", "DistanceFromHome", "EducationField", "StockOptionLevel",
                             "MonthlyIncome")



#Normalize
MI_model_df_raw$Age_Norm = (MI_model_df_raw$Age - mean(MI_model_df_raw$Age))/sd(MI_model_df_raw$Age)
MI_model_df_raw$Education_Norm = (MI_model_df_raw$Education - mean(MI_model_df_raw$Education))/sd(MI_model_df_raw$Education)
MI_model_df_raw$JobLevel_Norm = (MI_model_df_raw$JobLevel - mean(MI_model_df_raw$JobLevel))/sd(MI_model_df_raw$JobLevel)
MI_model_df_raw$TotalWorkingYears_Norm = (MI_model_df_raw$TotalWorkingYears - mean(MI_model_df_raw$TotalWorkingYears))/sd(MI_model_df_raw$TotalWorkingYears)
MI_model_df_raw$DistanceFromHome_Norm = (MI_model_df_raw$DistanceFromHome - mean(MI_model_df_raw$DistanceFromHome))/sd(MI_model_df_raw$DistanceFromHome)
MI_model_df_raw$StockOptionLevel_Norm = (MI_model_df_raw$StockOptionLevel - mean(MI_model_df_raw$StockOptionLevel))/sd(MI_model_df_raw$StockOptionLevel)

#Dummy Variables
#JobRole Department EducationField
#Department
MI_model_df_raw$HumanResDep =ifelse(MI_model_df_raw$Department == "Human Resources", 1 , 0)
MI_model_df_raw$ResearchDevelopment =ifelse(MI_model_df_raw$Department == "Research & Development", 1 , 0)
MI_model_df_raw$Sales =ifelse(MI_model_df_raw$Department == "Sales", 1 , 0)

#JobRole
MI_model_df_raw$SalesRep =ifelse(MI_model_df_raw$JobRole == "Sales Representative", 1 , 0)
MI_model_df_raw$SalesExec =ifelse(MI_model_df_raw$JobRole == "Sales Executive", 1 , 0)
MI_model_df_raw$ResearchSci =ifelse(MI_model_df_raw$JobRole == "Research Scientist", 1 , 0)
MI_model_df_raw$ResearchDirec =ifelse(MI_model_df_raw$JobRole == "Research Director", 1 , 0)
MI_model_df_raw$ManuDirec =ifelse(MI_model_df_raw$JobRole == "Manufacturing Director", 1 , 0)
MI_model_df_raw$Manager =ifelse(MI_model_df_raw$JobRole == "Manager", 1 , 0)
MI_model_df_raw$LabTech =ifelse(MI_model_df_raw$JobRole == "Laboratory Technician", 1 , 0)
MI_model_df_raw$HumanRes =ifelse(MI_model_df_raw$JobRole == "Human Resources", 1 , 0)
MI_model_df_raw$HealthRep =ifelse(MI_model_df_raw$JobRole == "Healthcare Representative", 1 , 0)

#EducationField
MI_model_df_raw$HumanResEduc =ifelse(MI_model_df_raw$EducationField == "Human Resources", 1 , 0)
MI_model_df_raw$LifeSci =ifelse(MI_model_df_raw$EducationField == "Life Sciences", 1 , 0)
MI_model_df_raw$Marketing =ifelse(MI_model_df_raw$EducationField == "Marketing", 1 , 0)
MI_model_df_raw$Med =ifelse(MI_model_df_raw$EducationField == "Medical", 1 , 0)
MI_model_df_raw$Other =ifelse(MI_model_df_raw$EducationField == "Other", 1 , 0)
MI_model_df_raw$TechDegree =ifelse(MI_model_df_raw$EducationField == "Technical Degree", 1 , 0)

#Consolidate into final DF
MI_model_df = MI_model_df_raw %>% select("Age_Norm", "Education_Norm", "JobLevel_Norm", "TotalWorkingYears_Norm",
                                          "DistanceFromHome_Norm", "StockOptionLevel_Norm",
                                         "HumanResDep", "ResearchDevelopment", "Sales",
                                         "SalesRep", "SalesExec", "ResearchSci", "ResearchDirec",
                                         "ManuDirec", "Manager", "LabTech", "HumanRes", "HealthRep",
                                         "HumanResEduc", "LifeSci", "Marketing", "Med",
                                         "Other", "TechDegree", "MonthlyIncome")



#Split into Train/Validation set and test set 60 20 20 split
tt_l = train_test_split(MI_model_df, splitPerc = 0.9)

train_data = tt_l[[1]]
test_data = tt_l[[2]]

#Specify Features and Target
fea = c("Age_Norm", "JobLevel_Norm", "TotalWorkingYears_Norm")
#Feature Notes
#Education_Norm Did not provide Statistically significant relationship





tar = c("MonthlyIncome")



#Setup train and test data
train = train_data %>% select(contains(fea), contains(tar))
train_fea = train_data %>% select(contains(fea))
train_tar = train_data %>% select(contains(tar))
test_fea = test_data %>% select(contains(fea))
test_tar = test_data %>% select(contains(tar))

fit = lm(MonthlyIncome~., data=train)
summary(fit)
test_tar$pred_lm = predict(fit, test_fea)
test_tar$resid = test_tar$MonthlyIncome - test_tar$pred_lm

#RMSE
sqrt(mean(test_tar$resid^2))

#Fine tune parameters for model, current parameters are sufficient
