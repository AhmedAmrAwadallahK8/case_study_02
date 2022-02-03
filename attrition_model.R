#Case Study 2 Attrition Model
library(magrittr)
library(ggplot2)
library(tidyverse)
library(class)
library(caret)
library(e1071)
library(lsmeans)

df_raw = read.csv(file = 'C:\\Users\\amada\\OneDrive\\Desktop\\RStudioFiles\\Scripts\\SMU\\Term1\\DS6306\\CaseStudy2\\CaseStudy2_data.csv')

#Check NAs
df_raw %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  gather(Column, NA_Count) %>%
  ggplot(aes(x=NA_Count, y=Column, fill = Column)) + geom_col() + ylab("Feature")

#No NAs

#Create Model with our previous reported three features
#Age
#MonthlyIncome
#OverTime
#TotalWorkingYears* 
#JobLevel*

df_model = df_raw %>% select("Age", "MonthlyIncome", "OverTime", "TotalWorkingYears", "JobLevel", "Attrition")

#Adjust OverTime such that its a dummy
str(df_model)
df_model$OverTime_Yes = ifelse(df_model$OverTime == "Yes", 1, 0)
df_model$OverTime_No = ifelse(df_model$OverTime == "No", 1, 0)

#Normalize the other variables
df_model$Age_Norm = (df_model$Age  - mean(df_model$Age))/sd(df_model$Age)
df_model$MonthlyIncome_Norm = (df_model$MonthlyIncome  - mean(df_model$MonthlyIncome))/sd(df_model$MonthlyIncome)
df_model$TotalWorkingYears_Norm = (df_model$TotalWorkingYears  - mean(df_model$TotalWorkingYears))/sd(df_model$TotalWorkingYears)
df_model$JobLevel_Norm = (df_model$JobLevel  - mean(df_model$JobLevel))/sd(df_model$JobLevel)

#Setup Final Data State
df_model_final = df_model %>% select("Age_Norm", "MonthlyIncome_Norm", "TotalWorkingYears_Norm", 
                                     "JobLevel_Norm", "OverTime_Yes", "OverTime_No", "Attrition")

#Randomly Shuffle df
df_model_final_shuffled = df_model_final[sample(1:nrow(df_model_final)), ]


#Split into Train/Validation set and test set 60 20 20 split
tt_l = train_test_split(df_model_final_shuffled, splitPerc = 0.9)

train_data = tt_l[[1]]
test_data = tt_l[[2]]
train_data %>% ggplot(aes(x=Attrition)) + geom_bar()
test_data %>% ggplot(aes(x=Attrition)) + geom_bar()

#SPecify Features and Target
fea = c("Age_Norm", "MonthlyIncome_Norm", "TotalWorkingYears_Norm", 
             "JobLevel_Norm", "OverTime_Yes", "OverTime_No")
tar = c("Attrition")

#Setup train and test data
train_fea = train_data %>% select(contains(fea))
train_tar = train_data %>% select(contains(tar))
test_fea = test_data %>% select(contains(fea))
test_tar = test_data %>% select(contains(tar))


#KNN Hyperparameter Tuning
optimal_k = find_optimal_k2(train_data, fea, tar, 
               5, 80, split = 0.888,  s_num=10, avg_loop=5, rep_upsample = 4, perc_downsample = 0.6,
                          title = "K versus Accuracy for KNN Model")

#Final KNN Model
clsf_rep = knn(train_fea[,], test_fea[,], train_tar[,], k=optimal_k, prob = T)
CM_rep = confusionMatrix(table(clsf_rep, test_tar[,]))
CM_rep

df_model_final %>% ggplot(aes(x=MonthlyIncome_Norm, y=MonthlyIncome_Norm, col = Attrition)) + geom_point()


CM_rep$byClass[11]
#Try down sampling the popular label
#Try Decision tree / forest after
#possible change optimizing metric to F1 score instead of accuracy

