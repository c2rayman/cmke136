#Christopher Rayman
#CMKE 136 Capstone Project
#Logistic Regession Model 3
#HIV Data Preperation for Weka Decision Tree

#Functions for Script
#Main function that will execute all other functions
model3_main <- function(){
  #library calls
  library(RWeka)
  library(ROCR)
  library(caret)
  library(e1071)
  
  print("Starting")
  
  #Path Needs to be Updated to Match File Location
  setwd("youpath")
  
  #Read in Dataset File
  fileData <- read.table(file = "./training_data.csv",
                         sep = ",",
                         header=TRUE)
  
  #forumla to use
  formula_text <- 'Resp~VL.t0 + CD4.t0'
  print(formula <- as.formula(formula_text))
  
  #build test and training data with 80% training
  rn_train <- sample(nrow(fileData),floor(nrow(fileData)*.80))
  train <- fileData[rn_train,]
  test <- fileData[-rn_train,]
  fit <- glm(formula, data = train, family = binomial())
  print(fit)
  
  test$scores <-predict(fit, type="response", newdata=test)
  pred <- prediction(test$scores, test$Resp)
  c<-confusionMatrix(as.integer(test$score >0.5),test$Resp)
  
  print(c$table)
  print("Ending")
}

#Run Main Function to Execute Script
model3_main()
