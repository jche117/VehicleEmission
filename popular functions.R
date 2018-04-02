#setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland")
#setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland")

library(reshape2)

#newDF<-melt(newDF,id.vars="Year")
#pdf("ct.pdf",width=7,height=5)
#dev.off

cleandata <- function(myDF) {
  myDF <- myDF[complete.cases(myDF), ]
  #qnt <- quantile(myDF$ppmNO, probs=c(.05, .95))
  #myDF<- myDF[which(myDF$ppmNO<=qnt[2]&myDF$ppmNO>=qnt[1]),]
  
}

transformRules <- function(tempString) {
  
tempString <- gsub("\\\"NA\\\",","",tempString)
tempString <- gsub("\\\"NA\\\"","",tempString)
tempString <- gsub("\\\"","'",tempString)
tempString <- gsub("%in%","",tempString)
tempString <- gsub("c\\(","\\{",tempString)
tempString <- gsub("\\,\\ \\ \\ \\ \\)","\\}",tempString)
tempString <- gsub("\\,\\ \\)","\\}",tempString)
tempString <- gsub("\\)","\\}",tempString)

return(tempString)

}
ctExtractionB <- function (x) {
  rData <- x
  for (i in 1: length(x)){
    tempString <- x[i]
    tempString <- gsub("\\\"","'",tempString)
    tempString <- gsub("YM","allEVtest[['YM']]",tempString)
    tempString <- gsub("IO","allEVtest[['IO']]",tempString)
    tempString <- gsub("OR","allEVtest[['OR']]",tempString)
    tempString <- gsub("FT","allEVtest[['FT']]",tempString)
    tempString <- gsub("VA","allEVtest[['VA']]",tempString)
    tempString <- gsub("EG","allEVtest[['EG']]",tempString)
    tempString <- gsub("VT","allEVtest[['VT']]",tempString)
    tempString <- gsub("VSP","allEVtest[['VSP']]",tempString)
    rData[i] <- tempString
  }
  
  return(rData)
  
}

ctExtractionC <- function (x) {
  rData <- x
  for (i in 1: length(x)){
    tempString <- x[i]
    tempString <- gsub("\\\"","'",tempString)
    tempString <- gsub("YM","allEVtraining[['YM']]",tempString)
    tempString <- gsub("IO","allEVtraining[['IO']]",tempString)
    tempString <- gsub("OR","allEVtraining[['OR']]",tempString)
    tempString <- gsub("FT","allEVtraining[['FT']]",tempString)
    tempString <- gsub("VA","allEVtraining[['VA']]",tempString)
    tempString <- gsub("EG","allEVtraining[['EG']]",tempString)
    tempString <- gsub("VT","allEVtraining[['VT']]",tempString)
    tempString <- gsub("VSP","allEVtraining[['VSP']]",tempString)
    rData[i] <- tempString
  }
  
  return(rData)
  
}

cleandataOutliers <- function(myDF, myPo, lq, hq) {
  myDF <- myDF[complete.cases(myDF), ]
  qnt <- quantile(myDF[[myPo]], probs=c(lq, hq))
  myDF<- myDF[which(myDF[[myPo]]<=qnt[2]&myDF[[myPo]]>=qnt[1]),]
  
  return(myDF)
}

#selectTrainingData <- function(myDF, percentage){
#  sample <- sample.int(n = nrow(myDF), size = floor(percentage*nrow(myDF)), replace = F)
 # return(sample)
#}

selectTrainingData <- function(myDF, size){
  sample <- sample.int(n = nrow(myDF), size = floor(size), replace = F)
  return(sample)
}

ctExtraction <- function (x) {
  rData <- x
  for (i in 1: length(x)){
    tempString <- x[i]
    tempString <- gsub("\\\"","'",tempString)
    tempString <- gsub("manYearBin2","allEV[['manYearBin2']]",tempString)
    tempString <- gsub("cGroup1","allEV[['cGroup2']]",tempString)
    tempString <- gsub("odoBin","allEV[['odoBin']]",tempString)
    tempString <- gsub("fType","allEV[['fType']]",tempString)
    tempString <- gsub("vAgeBin2","allEV[['vAgeBin2']]",tempString)
    tempString <- gsub("eBin","allEV[['eBin']]",tempString)
    tempString <- gsub("vehicleType","allEV[['vehicleType']]",tempString)
    tempString <- gsub("vspBin","allEV[['vspBin']]",tempString)
    tempString <- gsub("siteID","allEV[['siteID']]",tempString)
    
    
    rData[i] <- tempString
  }
  
  return(rData)
  
}
wrap_strings <- function(vector_of_strings,width){sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")})}


generateTraining <- function(myDF,percentage, features, myPo, lq, hq){
  #set.seed(101)
  myDF = cleandata(myDF[,features])
  subsetTemp = myDF[which(myDF[["year"]]==x[1]),]
  subsetTemp = cleandataOutliers(subsetTemp, myPo, lq, hq)
  sample <- selectTrainingData(subsetTemp, percentage)
  trainingData <- subsetTemp[sample,]
  testingData <- subsetTemp[-sample,]
  
  for (i in 2:length(x)){
    
    subsetTemp <- myDF[which(myDF$year==x[i]),]
    subsetTemp = cleandataOutliers(subsetTemp, myPo, lq, hq)
    sample <- selectTrainingData(subsetTemp, percentage)
    
    trainingData <- rbind(trainingData,subsetTemp[sample,])
    testingData <- rbind(testingData,subsetTemp[-sample,])
    
  }
  dlist = list()
  dlist[[1]] = trainingData
  dlist[[2]] = testingData
  
  return(dlist)
}

generateFixSizeData <- function(myDF){
  minSize = min(table(myDF[["year"]]))
  subsetTemp = myDF[which(myDF[["year"]]==x[1]),]
  sample <- sample(nrow(subsetTemp), minSize, replace = F)
  finalData = subsetTemp[sample,]
  
  for (i in 2:length(x)){
    subsetTemp <- myDF[which(myDF$year==x[i]),]
    sample <- sample(nrow(subsetTemp), minSize, replace = F)
    
    finalData <- rbind(finalData,subsetTemp[sample,])
    
    }
  return(finalData)
}
  
MADmedian <- function(Input, sampleMedian){
  return(mean(abs(Input-sampleMedian)))
}

MADmean <- function(Input, sampleMean){
  return(mean(abs(Input-sampleMean)))
  
}

cookOutlierClean <- function(inputData, linearmodel){
  cooksd = cooks.distance(linearmodel)
  #influential <- names(cooksd)[(cooksd > cookOutlier*mean(cooksd, na.rm=T))]
  influential <- names(cooksd)[(cooksd > (4/(length(cooksd))))]
  
  influential = as.numeric(gsub("X", "", influential))
  influentialData = inputData[-which(rownames(inputData) %in% influential) ,]
  return(influentialData)
  #plot(ppmNO~ vsp, data = influentialData)
}

cookOutlier <- function(inputData, cookOutlier, linearmodel){
  cooksd = cooks.distance(linearmodel)
  #influential <- names(cooksd)[(cooksd > cookOutlier*mean(cooksd, na.rm=T))]
  influential <- names(cooksd)[(cooksd > (4/(length(cooksd))))]
  
  influential = as.numeric(gsub("X", "", influential))
  influentialData = inputData[which(rownames(inputData) %in% influential) ,]
  return(influentialData)
  #plot(ppmNO~ vsp, data = influentialData)
}
cookOutlierLabel <- function(inputData, linearmodel){
  cooksd = cooks.distance(linearmodel)
  #influential <- names(cooksd)[(cooksd > cookOutlier*mean(cooksd, na.rm=T))]
  influential <- names(cooksd)[(cooksd > (4/(length(cooksd))))]
  
  influential = as.numeric(gsub("X", "", influential))
  inputData$outlier = "Not Outlier"
  inputData[which(rownames(inputData) %in% influential) ,"outlier"] = "Outlier"
  return(inputData)
  #plot(ppmNO~ vsp, data = influentialData)
}


replaceAcro <- function(inputData){
  
inputData <- data.frame(lapply(inputData, function(x) {gsub("vAgeBin2", "VA", x)}))
  
inputData <- data.frame(lapply(inputData, function(x) {gsub("cGroup1", "IO", x)}))
inputData <- data.frame(lapply(inputData, function(x) {gsub("eBin", "EG", x)}))
inputData <- data.frame(lapply(inputData, function(x) {gsub("manYearBin2", "YM", x)}))
inputData <- data.frame(lapply(inputData, function(x) {gsub("fType", "FT", x)}))
inputData <- data.frame(lapply(inputData, function(x) {gsub("odoBin", "OR", x)}))
inputData <- data.frame(lapply(inputData, function(x) {gsub("vehicleType", "VT", x)}))
inputData <- data.frame(lapply(inputData, function(x) {gsub("vspBin", "VSP", x)}))

return(inputData)

}
