#Christopher Rayman
#CKME 136 Capstone Project
#Classifcation Model 2 - Text Classification

#library calls

#Functions for Script
#Main function that will execute all other functions
model2_main <- function(){
  print("Starting")
  library(tm)
  library(RWeka)
  
  #set ngram length
  gramLength <- 3
  #Path Needs to be Updated to Match File Location
  setwd("youpath")
  
  #Read in Dataset File
  fileData <- read.table(file = "./training_data.csv",
                         sep = ",",
                         header=TRUE)
  newData <- createNewDataFrame(fileData,gramLength)
  
  #call to perform logistic regression
  #text mining data frame 6
  newData2 <- createTermMartix(newData,fileData)
  formula = as.formula('out_put_class ~ .')
  weka_fit <- Logistic(formula, data = newData2)
  print(evaluate_Weka_classifier(weka_fit, numFolds = 10))
  print("Ending")
}

#Function to create new data frame with reverse transcriptase
createNewDataFrame <- function(dataIn,gramLength){
  #create new data frame with patient id and response code
  dataOut <- data.frame(dataIn[c(2)])
  
  #create new attribute for reverse transcriptase 
  dataOut[["rt_setence"]] <- ""
  
  #get number of rows in data in
  numRows <- nrow(dataIn)
  
  #populate new attribute for reverse transcriptase broken into length n
  #representated as one long sentence
  for(i in 1:numRows){
    
    #split the reverse transcriptase into ngrams
    n<- nchar(as.character(dataIn[i,4])) - gramLength + 1
    ngrams <- substring(as.character(dataIn[i,4]), 1:n, 1:n + gramLength - 1)
    
    #loop through ngram to create one long sentence
    for(j in 1:length(ngrams)){
      dataOut[i,2] <- paste0(dataOut[i,2],ngrams[j],sep=" ") 
    }
  }
  
  #return new data frame
  return(dataOut)
}

#function to create a term martix 4 frequency + cd4 and viral load
createTermMartix <- function(dataIn,fData){
  
  #read in data in and start creating a corpus
  myCorpus <- Corpus(VectorSource(dataIn$rt_setence))
  
  #clean up Corpus remove white space
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  
  #convert into corpus into term martix
  dtm_tfidf <- DocumentTermMatrix(myCorpus, control = list(weighting = weightTfIdf, minWordLength=3, minDocFreq=10))
  
  #convert into data frame
  df1 <- as.data.frame(as.matrix(dtm_tfidf))
  dataOut <- cbind(fData$VL.t0,fData$CD4.t0,df1,out_put_class=as.factor(dataIn$Resp))
  
  for(i in 1:nrow(dataOut)){
    if(dataOut[i,2]== 0){
      dataOut[i,2]= mean(fData$CD4.t0)
    }
  }
  return(dataOut)
}

#Call the main function
model2_main()