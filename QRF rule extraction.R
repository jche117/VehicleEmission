

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



years = unique(allEV$year)

years <- years[-4]

cpm <- c("ppmNO","vAgeBin2","manYear", "cGroup1", "odoReading","fType", "eBin","vehicleType", "vspBin" )
cpm2 <- c("ppmNO","vAge","manYear", "cGroup1", "odoReading","fType", "engineCC","vehicleType", "vsp" )

colnames(allEVTemp) = c("ppmNO","VA","YM", "IO", "OR","FT", "EG","VT", "VSP" )
allEVTemp = allEV[which(allEV$year %in% x[4]),cpm]
allEVTemp = allEV[which(allEV$year %in% x[4]),cpm2]

allEVTemp$VT = factor(allEVTemp$VT)
#colnames(allEVTemp) <- c("ppmNO","vAgeBin2","manYearBin2", "cGroup1", "odoBin","fType", "engine","vehicleType", "vspBin" )
allEVTemp = cleandata(allEVTemp)

# if want to use man Year
setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland")
source("popular functions.R")

counter = 1
iData <- data.frame(matrix(nrow = 10*8, ncol = 2)) 

# ran the loop 10 times to decide if the tree is pruned to the right level, which is at level 5
for(i in 1:10){

  trainIndex <- createDataPartition(allEVTemp$ppmNO, p = .7,
                                  list = FALSE,
                                  times = 1)  

  allEVtraining = allEVTemp[trainIndex,]
  allEVtest = allEVTemp[-trainIndex,]
  
  for (j in 1:8){
    ct <- partykit::ctree(ppmNO ~. , data= allEVtraining,control = ctree_control(maxdepth = 5))
    ct2 <- partykit::ctree(ppmNO ~. , data= allEVtraining,control = ctree_control(maxdepth = 2))
    
    #fit<-rpart(ppmNO~.,data=allEVtraining, control = ctrl)
    
    a = predict(ct, allEVtest[,-1])
    iData[counter, 1] = j
    iData[counter, 2] = (mean(abs(a-allEVtest[,1])))
    
    counter = counter + 1
  }
qrf <- quantregForest(x=allEVtraining[, -1], y=allEVtraining[,1], mtry = 2,  ntree = 50)
#a = predict(qrf, allEVtest[,-1])

#print(mean(abs(a[,2]-allEVtest[,1])))

}
write.csv(iData, file = "ctree level.csv")



a <- partykit:::.list.rules.party(ct2)



b <- ctExtractionB(a)
c = ctExtractionC(a)
iData <- data.frame(matrix(nrow = length(b), ncol = 15)) 

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
predictionTraining = predict(qrf, allEVtraining[,-1])
mean(abs(predictionTest[,2]-allEVtest$ppmNO))
sd(predictionTraining[,2])

colnames(iData) = c("RuleID","nrowTest","nrowTrain", "meanTest", "meanTrain", "MAETest", "MAETrain", "SDTrain","meanUQLQ", "MAEmeanpredictor", "MAEExpectedValueTrain", "MADtrain", "MAEtreeprediction", "TreePrediction", "GeneralizedQRF")
iData$retained = "Retained"
iData[which(iData$MADtrain > 293),"retained"] = "Not retained"
iData[,c(3:15)] = round(iData[,c(3:15)],2)
predictionTrain = predict(qrf, allEVtraining[,-1])
stargazer(iData[,c("RuleID","MADtrain" ,"MAETest")], summary=FALSE, rownames=FALSE)
stargazer(iData[,c("RuleID","TreePrediction","GeneralizedQRF" ,"MAEtreeprediction","MAEExpectedValueTrain", "MAETest")], summary=FALSE, rownames=FALSE)

d = sData$ruleID
d[order(nchar(d),d)]

mean(abs(predictionTrain[,2]-median(predictionTrain[,2])))
plot(iData$MADtrain, iData$MAETest)

sData <- data.frame(matrix(nrow = length(b), ncol = 9)) 


source("popular functions.R")
split = "&"

for(i in 1:length(a)){
  t = data.frame(strsplit(a[i], split, fixed = FALSE, perl = FALSE, useBytes = FALSE)) 
  sData[i,1] = paste("Rule",i)
  for(j in 1:nrow(t)){
    tempString = as.character(t[j,1]) 
    tempString = transformRules(tempString)
    if(grepl("YM", tempString, fixed=TRUE)){
      sData[i,2] = gsub("YM\\ ", "", tempString)
    }
    if(grepl("IO", tempString, fixed=TRUE)){
      sData[i,3] = gsub("\\ IO\\ ", "", tempString)
    }    
    if(grepl("VA", tempString, fixed=TRUE)){
      sData[i,4] = gsub("\\ VA\\ ", "", tempString)
    }    
    if(grepl("OR", tempString, fixed=TRUE)){
      sData[i,5] = gsub("\\ OR\\ ", "", tempString)
    }
    if(grepl("EG", tempString, fixed=TRUE)){
      sData[i,6] = gsub("\\ EG\\ ", "", tempString)
    }
    if(grepl("VSP", tempString, fixed=TRUE)){
      sData[i,7] = gsub("\\ VSP\\ ", "", tempString)
    }
    if(grepl("FT", tempString, fixed=TRUE)){
      sData[i,8] = gsub("\\ FT\\ ", "", tempString)
    }
    if(grepl("VT", tempString, fixed=TRUE)){
      sData[i,9] = gsub("\\ VT\\ ", "", tempString)
    }
  }
}
colnames(sData) = c("ruleID","YM", "IO", "VA","OR", "EG", "VSP" ,"FT","VT")
stargazer(sData, summary=FALSE, rownames=FALSE)



sum(iData$X2)
sum(iData$X3)




write.csv(iData, file = "ruleEvaluation.csv")
ggplot(m, aes(x=MADtrain, y=MAETest, color=retained)) +
  geom_point()
