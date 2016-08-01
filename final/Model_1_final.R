#Christopher Rayman
#CMKE 136 Capstone Project
#Classification Model Version 3 max ngram length and average cd4
#for missing values
#HIV Data Preperation for Weka Decision Tree

#Functions for Script
#Main function that will execute all other functions
model1_main <- function(){
  print("Starting")
  
  #Length of Grams that Reverse Transcriptase is splitting up
  gramLength <- 3
  
  #Path Needs to be Updated to Match File Location
  setwd("youpath")
  
  #Read in Dataset File
  fileData <- read.table(file = "./training_data.csv",
                         sep = ",",
                         header=TRUE)
  
  #find the number of ngrams
  numGrams <-findMaxGrams(fileData,gramLength)
  print(numGrams)
  #create new dataframe with ngrams
  hivData <- createNGram(fileData,numGrams,gramLength)
  
  #write new file that will be used for Weka
  write.table(hivData,
              file = "hivdata.csv",
              sep=",",
              quote = FALSE,
              append = FALSE,
              na= "NA")
  
  print("Ending")
}

#function to return number of max n-grams im dataset
findMaxGrams <-function(dataIn,k){
  #get number of rows in data frame
  numRows <-nrow(dataIn)
  maxGram <- 0
  
  #loop through get number of rows and get max number of n-grams
  for(i in 1:numRows){
    n <- nchar(as.character(dataIn[i,4])) - k + 1
    ngrams <- substring(as.character(dataIn[i,4]), 1:n, 1:n + k - 1)
    
    #check if length of ngrams is larger than the previous
    if(length(ngrams) > maxGram){maxGram <-length(ngrams)}
  }
  return(maxGram)
}

#function to input fileData return dataframe with N-Grams
createNGram <-function(listData,numgrams,gramLngth){
  #Create new Dataframe with patient id, viral load, and cd4 count
  newDataFrame <- data.frame(listData[c(1,5,6)])
  
  #get number of rows in data frame
  numRows <-nrow(listData)
  
  #create new column names for each ngram
  for(j in 1:numgrams){
    newDataFrame[[paste("rtPos",as.character(j),sep="")]] <- ""
  }
  
  #loop through dataset and create columns of ngrams
  for(i in 1:numRows){
    n<- nchar(as.character(listData[i,4])) - gramLngth + 1
    ngrams <- substring(as.character(listData[i,4]), 1:n, 1:n + gramLngth - 1)
    
    for(j in 1:numgrams){
      newDataFrame[i,j+3] <-ngrams[j]
    }
    
    #make cd4 the mean value if 0
    if(newDataFrame[i,3] == 0){
      newDataFrame[i,3] <- mean(newDataFrame[,3])
    }
    
    #Set Response Code to Yes and No for Weka Nominal
    newDataFrame[["response"]] <- 'no'
    newDataFrame$response[listData$Resp ==1] <- 'yes'
  }
  return(newDataFrame)
}

#Run Main Function to Execute Script
model1_main()
