library(quantregForest)
library(randomForest)
library(leaps)
library(caret)

library(dummies)
library(party)
library(partykit)


# test 

# new change 
cpm <- c("ppmNO","vAgeBin2","manYearBin2", "cGroup2", "odoBin","fType", "eBin","vehicleType", "vspBin" )
source("popular functions.R")

setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland")

allEVcont = allEV[,c(total,"year")]
allEVcont = cleandata(allEVcont)
# Define the number of runs
runs = 20
mtry = seq(1:8)
# Define the data matrix
iData <- data.frame(matrix(nrow = 320, ncol = 8)) 
# Define the outlier cook distance parameter, 4 means 4 times higher than mean cook distance

count = 1
mtry[8]
for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  
  trainIndex <- createDataPartition(allEVcont$ppmNO, p = .2,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = allEVcont[trainIndex,]
  singleTest = allEVcont[-trainIndex,]
  
  
  
  
  
  
  
  for (i in 1:length(mtry)){
    message("*********** predicting ", count ," level ", " \n",appendLF = FALSE)
    
    qrf <- quantregForest(x=singleTraining[, 2:9], y=singleTraining[,1], mtry = mtry[i],  ntree = 200)
    rf <- randomForest(x=singleTraining[, 2:9], y=singleTraining[,1], mtry = mtry[i],  ntree = 200)
    
    
    
    predictions = predict(qrf, singleTest[,2:9])
    iData[count, 1] = z
    iData[count, 2] = "qrf"
    iData[count, 3] = i
    iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
    iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
    
    count = count + 1
    
    predictions = predict(rf, singleTest[,2:9])
    iData[count, 1] = z
    iData[count, 2] = "rf"
    iData[count, 3] = i
    iData[count, 4] = mean(abs(predictions - singleTest$ppmNO))
    iData[count, 5] = sqrt(mean((predictions - singleTest$ppmNO)^2))
    
    
    
    
    
    
    count = count+1
  }
  
  
}
iData = iData[,1:5]
colnames(iData) = c("run", "model","mtry","MAE", "RMSE") 
write.csv(iData, file = "rfvsqrf.csv")


14*20*2


runs = 20

iData <- data.frame(matrix(nrow = 14*20*2, ncol = 8)) 
# Define the outlier cook distance parameter, 4 means 4 times higher than mean cook distance

count = 1
nTr = seq(10, 100, by = 10)
nTr = c(seq(2,8, by =2), nTr)

for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  
  trainIndex <- createDataPartition(allEVcont$ppmNO, p = .3,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = allEVcont[trainIndex,]
  singleTest = allEVcont[-trainIndex,]
  
  
  
  
  
  
  
  for (i in 1:length(nTr)){
    message("*********** predicting ntree", nTr[i] ," level ", " \n",appendLF = FALSE)
    
    qrf <- quantregForest(x=singleTraining[, 2:9], y=singleTraining[,1], mtry = 2,  ntree = nTr[i])
    rf <- randomForest(x=singleTraining[, 2:9], y=singleTraining[,1], mtry = 2,  ntree = nTr[i])
    
    
    
    predictions = predict(qrf, singleTest[,2:9])
    iData[count, 1] = z
    iData[count, 2] = "QRF"
    iData[count, 3] = nTr[i]
    iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
    iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
    
    count = count + 1
    
    predictions = predict(rf, singleTest[,2:9])
    iData[count, 1] = z
    iData[count, 2] = "RF"
    iData[count, 3] = nTr[i]
    iData[count, 4] = mean(abs(predictions - singleTest$ppmNO))
    iData[count, 5] = sqrt(mean((predictions - singleTest$ppmNO)^2))
    
    
    
    
    
    
    count = count+1
  }
  
  
}

iData = iData[,1:5]
colnames(iData) = c("run", "model","ntree","MAE", "RMSE") 
write.csv(iData, file = "rfvsqrf ntree.csv")


z= 20
count = 1
featureContinuous <- c("ppmNO","manYear", "cGroup1", "odoReading","fType", "engineCC","vehicleType", "vAge", "vsp" )
cpm <- c("ppmNO","vAgeBin2","manYearBin2", "cGroup1", "odoBin","fType", "eBin","vehicleType", "vspBin" )
total = c("ppmNO","manYear", "cGroup1", "odoReading","fType", "engineCC","vehicleType", "vAge", "vsp" , "vAgeBin2","manYearBin2","cGroup1", "odoBin", "eBin", "vspBin")
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/")
source("popular functions.R")
allEVcont = cleandata(allEV[,total])
iData <- data.frame(matrix(nrow = 20*4, ncol = 8)) 

for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
  
  trainIndex <- createDataPartition(allEVcont$ppmNO, p = .3,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = allEVcont[trainIndex, featureContinuous]
  singleTraining = dummy.data.frame(singleTraining, sep = ".")

  singleTest = allEVcont[-trainIndex,cpm]
  
  
  
  
  
  ctrl<-trainControl(method="repeatedcv", number=10, repeats=3)
  lmCVFit<-train(ppmNO ~ ., data = singleTraining, method = "lm", trControl = ctrl, metric="Rsquared")
  
  tempTraining = cookOutlierClean(allEVcont[trainIndex, cpm], lmCVFit$finalModel )

  
  singleTraining = allEVcont[trainIndex, cpm]
  
  qrf <- quantregForest(x=singleTraining[, 2:9], y=singleTraining[,1], mtry = 2,  ntree = 100)
  rf <- randomForest(x=singleTraining[, 2:9], y=singleTraining[,1], mtry = 2,  ntree = 100)
  
  predictions = Predict(qrf, singleTest[,2:9])
  iData[count, 1] = z
  iData[count, 2] = "QRF"
  iData[count, 3] = "Outliers Included"
  iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
  
  count = count + 1
  
  predictions = predict(rf, singleTest[,2:9])
  iData[count, 1] = z
  iData[count, 2] = "RF"
  iData[count, 3] = "Outliers Included"
  iData[count, 4] = mean(abs(predictions - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions - singleTest$ppmNO)^2))
  
  count = count+1
  
  
  #plot(ppmNO ~ vAge, data = singleTraining)
  #plot(ppmNO ~ vAgeBin2, data = tempTraining)
  qrf <- quantregForest(x=tempTraining[, 2:9], y=tempTraining[,1], mtry = 2,  ntree = 100)
  rf <- randomForest(x=tempTraining[, 2:9], y=tempTraining[,1], mtry = 2,  ntree = 100)
  
  
  predictions = Predict(qrf, singleTest[,2:9])
  iData[count, 1] = z
  iData[count, 2] = "QRF"
  iData[count, 3] = "Outliers Excluded"
  iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
  
  count = count + 1
  
  predictions = predict(rf, singleTest[,2:9])
  iData[count, 1] = z
  iData[count, 2] = "RF"
  iData[count, 3] = "Outliers Excluded"
  iData[count, 4] = mean(abs(predictions - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions - singleTest$ppmNO)^2))
  
  count = count+1
  
  
}
iData = iData[,1:5]
colnames(iData) = c("run", "model","outlier","MAE", "RMSE") 
write.csv(iData, file = "rfvsqrf outlier.csv")


z= 20
count = 1
featureContinuous <- c("ppmNO","manYear", "cGroup1", "odoReading","fType", "engineCC","vehicleType", "vAge", "vsp" )
cpm <- c("ppmNO","vAgeBin2","manYearBin2", "cGroup1", "odoBin","fType", "eBin","vehicleType", "vspBin" )
total = c("ppmNO","manYear", "cGroup1", "odoReading","fType", "engineCC","vehicleType", "vAge", "vsp" , "vAgeBin2","manYearBin2","cGroup1", "odoBin", "eBin", "vspBin")
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/")
source("popular functions.R")
allEVcont = cleandata(allEV[,total])
iData <- data.frame(matrix(nrow = 20*4, ncol = 8)) 

for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
  
  trainIndex <- createDataPartition(allEVcont$ppmNO, p = .3,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = allEVcont[trainIndex, total]
  singleTest = allEVcont[-trainIndex,total]
  
  
  
  
  
 


  f = as.formula("ppmNO ~ manYear + odoReading + engineCC + vAge + vsp | fType + cGroup1 + vehicleType")
  
  hmodel2 <- party::mob(f, data = singleTraining[, featureContinuous], model  = linearModel)
  
  predictions = predict(hmodel2, singleTest[,featureContinuous][,-1])
  iData[count, 1] = z
  iData[count, 2] = "MOB"
  iData[count, 3] = "Outliers Included"
  iData[count, 4] = mean(abs(predictions - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions - singleTest$ppmNO)^2))
  
  count = count+1
  
  
  

  qrf <- quantregForest(x=singleTraining[, cpm][,-1], y=singleTraining[,1], mtry = 2,  ntree = 50)

  predictions = predict(qrf, singleTest[,cpm][,-1])
  iData[count, 1] = z
  iData[count, 2] = "QRF"
  iData[count, 3] = "Outliers Included"
  iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
  
  count = count + 1
  
  
  singleTraining = allEVcont[trainIndex, featureContinuous]
  singleTraining = dummy.data.frame(singleTraining, sep = ".")
  ctrl<-trainControl(method="repeatedcv", number=10, repeats=3)
  lmCVFit<-train(ppmNO ~ ., data = singleTraining, method = "lm", trControl = ctrl, metric="Rsquared")
  
  
  tempTraining = cookOutlierClean(allEVcont[trainIndex, total], lmCVFit$finalModel )
  
  #plot(ppmNO ~ vAge, data = singleTraining)
  #plot(ppmNO ~ vAgeBin2, data = tempTraining)
  qrf <- quantregForest(x=tempTraining[, cpm][,-1], y=tempTraining[,1], mtry = 2,  ntree = 50)

  predictions = predict(qrf, singleTest[,cpm][,-1])
  iData[count, 1] = z
  iData[count, 2] = "QRF"
  iData[count, 3] = "Outliers Excluded"
  iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
  
  count = count + 1
  
  
  hmodel2 <- party::mob(f, data = tempTraining[, featureContinuous], model  = linearModel)
  
  predictions = predict(hmodel2, singleTest[,featureContinuous][,-1])
  iData[count, 1] = z
  iData[count, 2] = "MOB"
  iData[count, 3] = "Outliers Excluded"
  iData[count, 4] = mean(abs(predictions - singleTest$ppmNO))
  iData[count, 5] = sqrt(mean((predictions - singleTest$ppmNO)^2))
  
  count = count+1
  
  
}
iData = iData[81:160,1:5]
colnames(iData) = c("run", "model","outlier","MAE", "RMSE") 
write.csv(iData, file = "rfvsmob outlier.csv")






z = 20


finalRanking = data.frame(qrf$importance)
finalRanking$featurename = rownames(finalRanking)
finalRanking$mtry = 2
finalRanking$run = 1

m = c(2,4,6,8)
count = 1

for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
  
  trainIndex <- createDataPartition(allEVcont$ppmNO, p = .3,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = allEVcont[trainIndex, total]
  singleTest = allEVcont[-trainIndex,total]
  
  
  
  
  
  
  
  for(j in 1:4){
  
  
  
  qrf <- quantregForest(x=singleTraining[, cpm][,-1], y=singleTraining[,1], mtry = m[j],  ntree = 50)
  

  tempRanking = data.frame(qrf$importance)
  tempRanking$featurename = rownames(tempRanking)
  tempRanking$mtry = m[j]
  tempRanking$run = z
  
  finalRanking = rbind(finalRanking,tempRanking)

  
  
  }
  
  
  
}
#figure 4.17 
write.csv(finalRanking, file = "feature ranking_mtry.csv")

#Next experiment is to find the most performing feature pair

iData <- data.frame(matrix(nrow = 20*7, ncol = 8)) 
cpm <- c("ppmNO","vAgeBin2","manYearBin2", "cGroup1", "odoBin","fType", "eBin","vehicleType", "vspBin" )
twopairList <- c("cGroup1","vAgeBin2", "odoBin","eBin",  "vspBin","fType" ,"vehicleType")

count = 1
runs = 20

for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
  
  trainIndex <- createDataPartition(allEVcont$ppmNO, p = .3,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = allEVcont[trainIndex, total]
  singleTest = allEVcont[-trainIndex,total]
  
  for (i in 1:length(twopairList)){

    message("*********** predicting ", twopairList[1:i] ," level ", " \n",appendLF = FALSE)

    qrf <- quantregForest(x=singleTraining[, cpm][,c("manYearBin2", twopairList[1:i])], y=singleTraining[,1], mtry = 2,  ntree = 50)
    
    
    predictions = predict(qrf, singleTest[,cpm][,c("manYearBin2", twopairList[1:i])])
    iData[count, 1] = z
    iData[count, 2] = "QRF"
    iData[count, 3] = twopairList[i]
    iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
    iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
    
    count = count + 1
    
  }
    
    
}
iData = iData[,1:5]
colnames(iData) = c("run", "model", "feature", "MAE", "RMSE")

write.csv(iData, file = "twopair_feature_validation.csv")
write.csv(iData, file = "stepwise_evaluation.csv")

# Figure 4.20 var imp by campaign years

finalRanking = data.frame(qrf$importance)
finalRanking$featurename = rownames(finalRanking)
finalRanking$run = 1
finalRanking$year = 2005
count = 1
for(j in 1:4){
  message("*********** predicting ", years[j] ," level ", " \n",appendLF = FALSE)
  
  tempCont = allEVcont[which(allEVcont$year == years[j]),]
  for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
    
    
  trainIndex <- createDataPartition(tempCont$ppmNO, p = .6,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = tempCont[trainIndex, total]
  singleTest = tempCont[-trainIndex,total]
  
   
    qrf <- quantregForest(x=singleTraining[, cpm][,-1], y=singleTraining[,1], mtry = 4,  ntree = 50)
    
    
    tempRanking = data.frame(qrf$importance)
    tempRanking$featurename = rownames(tempRanking)
    tempRanking$run = z
    tempRanking$year = years[j]
    
    finalRanking = rbind(finalRanking,tempRanking)
    
    
    
  }
  
  
  
}
write.csv(finalRanking, file = "feature_ranking_byyear.csv")


# Figure 4.21 stepwise feature induction by year

iData <- data.frame(matrix(nrow = 20*28, ncol = 5)) 
cpm <- c("ppmNO","vAgeBin2","manYearBin2", "cGroup2", "odoBin","fType", "eBin","vehicleType", "vspBin" )
twopairList <- c("cGroup1","vAgeBin2", "odoBin","eBin",  "vspBin","fType" ,"vehicleType")

count = 1
runs = 20

for(j in 1:4){
  message("*********** predicting ", years[j] ," level ", " \n",appendLF = FALSE)
  
  tempCont = allEVcont[which(allEVcont$year == years[j]),]
for (z in 1:runs){
  # Split the data randomly, half as training, half as test
  message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
  
  trainIndex <- createDataPartition(tempCont$ppmNO, p = .6,
                                    list = FALSE,
                                    times = 1)  
  
  singleTraining = tempCont[trainIndex, total]
  singleTest = tempCont[-trainIndex,total]
  a = singleTraining[, ][,c("manYearBin2", twopairList[1:7])]
  for (i in 1:length(twopairList)){
    
    message("*********** predicting ", twopairList[1:i] ," level ", " \n",appendLF = FALSE)
    
    qrf <- quantregForest(x=singleTraining[, ][,c("manYearBin2", twopairList[1:i])], y=singleTraining[,1], mtry = 2,  ntree = 50)
    
    
    predictions = predict(qrf, singleTest[,][,c("manYearBin2", twopairList[1:i])])
    iData[count, 1] = z
    iData[count, 2] = years[j]
    iData[count, 3] = twopairList[i]
    iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
    iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
    
    count = count + 1
    
  }
}
  
}

colnames(iData) = c("run", "year", "feature", "MAE", "RMSE")

write.csv(iData, file = "stepwise_byyear.csv")


#Figure 4.22 Single-year model comparison


iData <- data.frame(matrix(nrow = 20*28, ncol = 5)) 
cpm <- c("ppmNO","vAgeBin2","manYearBin2", "cGroup2", "odoBin","fType", "eBin","vehicleType", "vspBin" )
twopairList <- c("cGroup1","vAgeBin2", "odoBin","eBin",  "vspBin","fType" ,"vehicleType")

count = 1
runs = 20

for(j in 1:4){
  message("*********** predicting ", years[j] ," level ", " \n",appendLF = FALSE)
  
  tempCont = allEVcont[which(allEVcont$year == years[j]),]
  for (z in 1:runs){
    # Split the data randomly, half as training, half as test
    message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
    
    trainIndex <- createDataPartition(tempCont$ppmNO, p = .6,
                                      list = FALSE,
                                      times = 1)  
    
    singleTraining = tempCont[trainIndex, total]
    singleTest = tempCont[-trainIndex,total]
    sample(nrow(singleTest), 5000)
    a = singleTraining[, ][,c("manYearBin2", twopairList[1:7])]
    for (i in 1:length(twopairList)){
      
      message("*********** predicting ", twopairList[1:i] ," level ", " \n",appendLF = FALSE)
      
      qrf <- quantregForest(x=singleTraining[, ][,c("manYearBin2", twopairList[1:i])], y=singleTraining[,1], mtry = 2,  ntree = 50)
      
      
      predictions = predict(qrf, singleTest[,][,c("manYearBin2", twopairList[1:i])])
      iData[count, 1] = z
      iData[count, 2] = years[j]
      iData[count, 3] = twopairList[i]
      iData[count, 4] = mean(abs(predictions[,2] - singleTest$ppmNO))
      iData[count, 5] = sqrt(mean((predictions[,2] - singleTest$ppmNO)^2))
      
      count = count + 1
      
    }
  }
  
}
