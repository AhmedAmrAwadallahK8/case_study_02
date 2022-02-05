#Helper Functions for Case Study 02

#Train test splitting function
train_test_split = function(df, splitPerc){
  #Function that splits dataframe into a training set and test set based on specified split proportion
  #Params:
  #df: (data.frame) Raw dataframe that will be split
  #splitPerc: (int) Percentage of raw data to be placed into training set
  #Output
  # (list) Returns a list of two dataframe objects. First one being the training set and second test set
  trainIndices = sample(1:dim(df)[1], round(splitPerc * dim(df)[1]))
  train = df[trainIndices,]
  test = df[-trainIndices,]
  return(list(train, test))
}

#Finds K within specified range that maximizes accuracy for given Dataset
find_optimal_k2 = function(df, fea, tar, k_min, k_max, split = 0.8,  
                           s_num=10, avg_loop=5, rep_upsample = 3, perc_downsample = 0.7, 
                           title = "K versus Accuracy for KNN Model"){
  #Params:
  #df: (Data.Frame) Full Data set to be trained on
  #fea: (Vector of Strings) String of selected columns for features
  #tar: (Vector of Strings) String of selected columns for targets
  #Split: (Numeric) Sampling Split
  #k_min: (Numeric) Minimum K value to test
  #k_max: (Numeric) Maximum K value to test
  #s_num: (Numeric) How many samples to loop through
  #avg_loop: (Numeric) How many times to average per k
  #title: Title for plot
  #Output
  #(int) Returns the K value with the highest accuracy
  
  #Setup Data Structures for loop logic and output
  k_loop = k_max-k_min+1
  acc_overall = numeric(k_loop)
  k_vals = c(k_min:k_max)
  
  #Loop For Every sample
  for(i in 1:s_num){
    #Print progress through loop as it can take awhile
    print(i)
    
    #Split the data into train and test splits
    tt_list = train_test_split(df, split)
    train = tt_list[[1]]
    test = tt_list[[2]]
    #Upsample Training Set
    minority_target = train %>% filter(Attrition == "Yes")
    majority_target = train %>% filter(Attrition == "No")
    trainIndices = sample(1:dim(majority_target)[1], round(perc_downsample * dim(majority_target)[1]))
    train = majority_target[trainIndices,]
    for(l in 1:rep_upsample){
      train = rbind(train, minority_target)
    }
    
    
    train_fea = train %>% select(contains(fea))
    train_tar = train %>% select(contains(tar))
    test_fea = test %>% select(contains(fea))
    test_tar = test %>% select(contains(tar))
    
    #Loop for every K in range specified by parameters kmax-kmin
    for(j in 1:k_loop){
      k_current = k_vals[j]
      acc_vals_sample = c()
      acc = 0
      
      #Loop multiple times for specified k and take average
      for(k in 1:avg_loop){
        clsf_rep = knn(train_fea[,], test_fea[,], train_tar[,], k=k_current, prob = T)
        CM_rep = confusionMatrix(table(clsf_rep, test_tar[,]))
        acc = acc + CM_rep$byClass[11]
      }
      #Average across multiple trains
      acc_avg = acc/avg_loop
      #Add the vector to the overall accruacy vector for all Ks
      acc_overall[j] = acc_overall[j] + acc_avg
    }
  }
  #Average all Ks by number of samples taken
  acc_overall_avg = acc_overall/s_num
  
  #Find Optimal K
  optimal_k = k_min + which.max(acc_overall_avg) - 1
  
  #Plot 
  plot(k_vals, acc_overall_avg, main = title, type = 'l', col = "#DD1731", 
       xlab = "K", ylab = "F1", lwd=3)
  abline(v = optimal_k, col="green", lwd=2)
  
  
  return(optimal_k)
}

#Finds K within specified range that maximizes accuracy for given Dataset
find_optimal_depth = function(df, fea, tar, k_min, k_max, split = 0.8,  
                           s_num=10, avg_loop=5, rep_upsample = 3, perc_downsample = 0.7, 
                           title = "K versus Accuracy for KNN Model"){
  #Params:
  #df: (Data.Frame) Full Data set to be trained on
  #fea: (Vector of Strings) String of selected columns for features
  #tar: (Vector of Strings) String of selected columns for targets
  #Split: (Numeric) Sampling Split
  #k_min: (Numeric) Minimum K value to test
  #k_max: (Numeric) Maximum K value to test
  #s_num: (Numeric) How many samples to loop through
  #avg_loop: (Numeric) How many times to average per k
  #title: Title for plot
  #Output
  #(int) Returns the K value with the highest accuracy
  
  #Setup Data Structures for loop logic and output
  k_loop = k_max-k_min+1
  acc_overall = numeric(k_loop)
  k_vals = c(k_min:k_max)
  
  #Loop For Every sample
  for(i in 1:s_num){
    #Print progress through loop as it can take awhile
    print(i)
    
    #Split the data into train and test splits
    tt_list = train_test_split(df, split)
    train = tt_list[[1]]
    test = tt_list[[2]]
    #Upsample Training Set
    minority_target = train %>% filter(Attrition == "Yes")
    majority_target = train %>% filter(Attrition == "No")
    trainIndices = sample(1:dim(majority_target)[1], round(perc_downsample * dim(majority_target)[1]))
    train = majority_target[trainIndices,]
    for(l in 1:rep_upsample){
      train = rbind(train, minority_target)
      
    }
    
    
    train_fea = train %>% select(contains(fea))
    train_tar = train %>% select(contains(tar))
    test_fea = test %>% select(contains(fea))
    test_tar = test %>% select(contains(tar))
    
    #Loop for every K in range specified by parameters kmax-kmin
    for(j in 1:k_loop){
      k_current = k_vals[j]
      acc_vals_sample = c()
      acc = 0
      
      #Loop multiple times for specified k and take average
      for(k in 1:avg_loop){
        forest = randomForest(x=train_fea, y=as.factor(train_tar$Attrition),
                              ntree = 1000, maxnodes = k_current)
        pred_forest = predict(forest, test_fea)
        CM_rep = confusionMatrix(table(pred_forest, test_tar[,]))
        acc = acc + CM_rep$byClass[11]
      }
      #Average across multiple trains
      acc_avg = acc/avg_loop
      #Add the vector to the overall accruacy vector for all Ks
      acc_overall[j] = acc_overall[j] + acc_avg
    }
  }
  #Average all Ks by number of samples taken
  acc_overall_avg = acc_overall/s_num
  
  #Find Optimal K
  optimal_k = k_min + which.max(acc_overall_avg) - 1
  
  #Plot 
  plot(k_vals, acc_overall_avg, main = title, type = 'l', col = "#DD1731", 
       xlab = "K", ylab = "Accuracy", lwd=3)
  abline(v = optimal_k, col="green", lwd=2)
  
  
  return(optimal_k)
}
