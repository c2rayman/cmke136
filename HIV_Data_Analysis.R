#Christopher Rayman
#CMKE 136 Capstone Project
#HIV Data Analysis

#Change Working Directory
#Path Needs to be Updated to Match File Location
setwd("yourpath")

#Load Library for Plotting
library(corrplot)

#Read in Dataset File
fileData <- read.table(file = "./training_data.csv",
                       sep = ",",
                       header=TRUE)

#Get Number of Rows
numRows <- nrow(fileData)

#Build Bar Plot Frequency for Response Code
responseCounts <- table(fileData$Resp)
barplot(responseCounts, ylim = c(0,800), main="Response Codes", 
        xlab="Number of Response Codes")
#Save Copy of Plot
dev.copy(jpeg,'Reponse Code')
dev.off()

#Loop Through Dataset to get Lengths of Protease
proteaseLengths <-''
for(i in 1:numRows){
  proteaseLengths <- c(proteaseLengths,nchar(as.character(fileData[i,3])))
}
proteaseLengths <- proteaseLengths[proteaseLengths!=""]
proteaseLengths <- type.convert(proteaseLengths)
#Build Bar Plot Frequency for Protease Lengths
proteaseFreq <- table(proteaseLengths)
barplot(proteaseFreq,ylim = c(0,1000), main="Protease Lengths", 
        xlab="Length of the Protease Attribute")
#Save a Copy of Plot
dev.copy(jpeg,'Protease')
dev.off()
avgProtease <- mean(proteaseLengths)
medianProtease <- median(proteaseLengths)

#Get Lenths of Reverse Transcriptase
reverseLengths <- ''
for(j in 1:numRows){
  reverseLengths <- c(reverseLengths,nchar(as.character(fileData[j,4])))
}
reverseLengths <- reverseLengths[reverseLengths!=""]
reverseLengths <- type.convert(reverseLengths)
#Build Bar Plot Frequency for Reverse Transcriptase Lengths
reverseFreq <- table(reverseLengths)
barplot(reverseFreq,ylim = c(0,200), main="Reverse Transcriptase Lengths", 
        xlab="Length of the Reverse Transcriptase Attribute")
#Save a Copy of Plot
dev.copy(jpeg,'Reverse Transcriptase')
dev.off()
avgReverse <- mean(reverseLengths)
medianReverse <- median(reverseLengths)

#Box Plot of Viral Load
boxplot(fileData$VL.t0)
#Save a Copy of Plot
dev.copy(jpeg,'Viral Load')
dev.off()
avgViral <- mean(fileData$VL.t0)
medianViral <- median(fileData$VL.t0)

#Box Plot of CD4 Counts
boxplot(fileData$CD4.t0)
#Save a Copy of Plot
dev.copy(jpeg,'CD4')
dev.off()
avgCD4 <- mean(fileData$CD4.t0)
medianCD4 <- median(fileData$CD4.t0)