

library(partykit)
library(leaps)
library(quantregForest)
library(caret)
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(stargazer)

library(inTrees); library(randomForest) 
library("PMCMR")
library("kSamples")
library(ggplot2)
library(ggpubr)
library(zoom)
library(reshape2)
require(gridExtra)
library(gtable)
library(grid)


# Import 2015 data from overall dataset
years = unique(allEV$year)
# this is because our previous experiment has shown that the relevance of the data decays over time, so for predicting future emissions, the latest campaign data (2015) should be used

years <- years[-4]

# 8 input features are selected
cpm <- c("ppmNO","vAgeBin2","manYear", "cGroup1", "odoReading","fType", "eBin","vehicleType", "vspBin" )
# rename them with abbreviation
colnames(allEVTemp) = c("ppmNO","VA","YM", "IO", "OR","FT", "EG","VT", "VSP" )

allEVTemp = allEV[which(allEV$year %in% x[4]),cpm]

allEVTemp$VT = factor(allEVTemp$VT)

# load useful customised functions
source("popular functions.R")
#clean the na records
allEVTemp = cleandata(allEVTemp)

counter = 1
iData <- data.frame(matrix(nrow = 10*8, ncol = 2)) 

# ran the loop 10 times to decide if the tree is pruned to the right level, which is at level 5
# this is to ensure the cTree model is not overfit
# the cTree model is used to extract rules that are used to generalise QRF (Quantile regression forest) model
for(i in 1:10){

  #split the data into training and test set
  trainIndex <- createDataPartition(allEVTemp$ppmNO, p = .7,
                                  list = FALSE,
                                  times = 1)  

  allEVtraining = allEVTemp[trainIndex,]
  allEVtest = allEVTemp[-trainIndex,]
  #test the tree level from 1 to 8, and error rates stablize after level 5
  for (j in 1:8){
    ct <- partykit::ctree(ppmNO ~. , data= allEVtraining,control = ctree_control(maxdepth = 5))

    #fit<-rpart(ppmNO~.,data=allEVtraining, control = ctrl)
    
    a = predict(ct, allEVtest[,-1])
    iData[counter, 1] = j
    iData[counter, 2] = (mean(abs(a-allEVtest[,1])))
    
    counter = counter + 1
  }
  
  #build a QRF model using the same training data, with 50 trees and mtry = 2, both parameters were confirmed from previous experiment
qrf <- quantregForest(x=allEVtraining[, -1], y=allEVtraining[,1], mtry = 2,  ntree = 50)

}
write.csv(iData, file = "ctree level.csv")


# raw rules extracted from ctree model "ct" 

a <- partykit:::.list.rules.party(ct)


# Extract rule conditions from a 
b <- ctExtractionB(a)
c = ctExtractionC(a)
iData <- data.frame(matrix(nrow = length(b), ncol = 15)) 

# Evaluate rule qualities using MAE and MAD to median, also calculate the generalised prediction values using QRF model for each rule

for(i in 1:length(b)){

  tempTest = allEVtest[which(eval(parse(text = b[i]))),]
  tempTrain = allEVtraining[which(eval(parse(text = c[i]))),]
  predictionTest = predict(qrf, tempTest[,-1])
  predictionTrain = predict(qrf, tempTrain[,-1])
  treePrediction = predict(ct2, tempTest[,-1])
  
  iData[i,1] = paste("Rule", i)
  iData[i,2] = nrow(tempTest)
  iData[i,3] = nrow(tempTrain)
  iData[i,4] = mean(tempTest$ppmNO)
  iData[i,5] = mean(tempTrain$ppmNO)
  iData[i,6] = mean(abs(predictionTest[,2]-tempTest$ppmNO))
  iData[i,7] = mean(abs(predictionTrain[,2]-tempTrain$ppmNO))
  iData[i,8] = sd(predictionTrain[,2])
  iData[i,9] = mean(abs(predictionTrain[,3]-predictionTrain[,1]))
  iData[i,10] = mean(abs(mean(tempTest$ppmNO)-tempTest$ppmNO))
  iData[i,11] = mean(abs(mean(predictionTrain[,2])-tempTest$ppmNO))
  iData[i,12] = mean(abs(predictionTrain[,2]-median(predictionTrain[,2])))
  iData[i,13] = mean(abs(mean(treePrediction)-tempTest$ppmNO))
  iData[i,14] = mean(treePrediction)
  iData[i,15] = mean(predictionTrain[,2])
  
}


colnames(iData) = c("RuleID","nrowTest","nrowTrain", "meanTest", "meanTrain", "MAETest", "MAETrain", "SDTrain","meanUQLQ", "MAEmeanpredictor", "MAEExpectedValueTrain", "MADtrain", "MAEtreeprediction", "TreePrediction", "GeneralizedQRF")

iData[,c(3:15)] = round(iData[,c(3:15)],2)
predictionTrain = predict(qrf, allEVtraining[,-1])
stargazer(iData[,c("RuleID","MADtrain" ,"MAETest")], summary=FALSE, rownames=FALSE)
stargazer(iData[,c("RuleID","TreePrediction","GeneralizedQRF" ,"MAEtreeprediction","MAEExpectedValueTrain", "MAETest")], summary=FALSE, rownames=FALSE)

# Save the rules and evaluation results

write.csv(iData, file = "ruleEvaluation.csv")

