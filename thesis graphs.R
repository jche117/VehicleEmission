library(datasets)
library(ggplot2)
library(readr)
library(stargazer)
library(lsr)
library(reshape2)
library(varhandle)
library(gridExtra)
library("png")
library(grid)
library(moments)
library(stats)
library(ggpmisc)
library(polynom)

# Initial data and function load

x <- unique(allEV$year)
x <- x[-4]
allEV = allEV[which(allEV$year %in% x),]
allEV$vAgeBin2 <- factor(allEV$vAgeBin2, levels = xlevels[c(1,7,11,12,13,2,3,4,5,6,8,9,10)])


setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland")
source("popular functions.R")
source("addNewData.R")

fill <- "#4271AE"
line <- "#1F3552"

# Calculate Variance, MAD, MeanAbsolute around median/mean per each collection day 


dates = unique((allEV$date))

iData <- data.frame(matrix(0, nrow = length(dates), ncol = 7))
allTemp = cleandata(allEV[, c("ppmNO", "date", "year")]) 



for (i in 1:nrow(iData)){
  inputEmissions = allTemp[which(allTemp$date==dates[i]),"ppmNO"]
  iData[i,1] = as.character(dates[i]) 
  iData[i,2] = as.numeric(sqrt(var(inputEmissions) ) )
  iData[i,3] = allTemp[which(allTemp$date==dates[i]),"year"][1]
  iData[i,4] = as.numeric(sqrt(mad(inputEmissions )) ) 
  iData[i,5] = as.numeric(MADmean(inputEmissions, mean(inputEmissions) ) ) 
  
  iData[i,6] = as.numeric(MADmedian(inputEmissions, median(inputEmissions) ) ) 
  iData[i,7] = length(inputEmissions)
}
iData$X1 = factor(iData$X1)
iData$X3 = factor(iData$X3)
colnames(iData) =  (c("Dates","SD", "Year", "MAD", "MADMean", "MADMedian", "Size"))
iData$campaignID = c(1,2,2,3,3,4,5,6,3,7,8,8,8,9,9,9,10,10,10, 11,11,11,12, 13,13,13,13,13, 14,14,14,15,16,16,16,17,18,18,18,18,18,19)
iData$campaignID = factor(iData$campaignID)


df2<-melt(iData[,c(1,2,3,5,6)],id.vars=c("Dates", "Year"))

pdf("MADmean.pdf",width=7,height=3)

ggplot(df2, aes(x = Year, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Mean Absolute Deviation") +
  scale_x_discrete(name = "Campaign Year") +
  theme_bw() +
  
  facet_grid(. ~ variable)
dev.off()



pdf("campaignDiff.pdf",width=14,height=6)

  ggplot(iData, aes(x=campaignID, y=SD,  color=campaignID)) +
    geom_point()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_y_continuous(name = "Mean Absolute Deviation to Median") +
    scale_x_discrete(name = "Group ID") +
  geom_text(aes(label=Dates))
  dev.off()
  
  
setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland/Thesis v2")
resultTable <- data.frame(matrix(0, nrow = 4, ncol = 10))


for (i in 1:length(x)){
  inputData = iData[which(iData$Year==x[i]),]
  resultTable[i,1] = as.character(x[i]) 
  resultTable[i,2] = as.numeric(mean(inputData$SD) )
  resultTable[i,3] = as.numeric(sd(inputData$SD) )
  resultTable[i,4] = as.numeric(mean(inputData$MADMedian) )
  resultTable[i,5] = as.numeric(sd(inputData$MADMedian) )
  resultTable[i,6] = as.numeric(mean(inputData$MADMean) )
  resultTable[i,7] = as.numeric(sd(inputData$MADMean) )
}

colnames(resultTable) = c("Campaign Year", "SD", "+/-", "MAD to Median", "+/-", "MAD to Mean", "+/-")
stargazer(resultTable[,c(1:7)], summary=FALSE, rownames=FALSE)

siteTable = as.data.frame.matrix(table( allEV$siteID, allEV$year))
tempTable = as.data.frame.matrix(table( allEV$siteID, allEV$date))

siteTable$SiteName = c("Lagoon DR", "West End Rd", "Lambie Dr (S)", "Upper Harbour Highway", "Elliot St", "Whangaparaoa Rd", "Universal Dr")
stargazer(siteTable[,c(5,6,1,2,3,4)], summary=FALSE, rownames=FALSE)
siteTable$SiteID = rownames(siteTable)

setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland/Thesis v2")

pdf("siteEmission.pdf",width=10,height=5)

ggplot(allEV, aes(x = siteID, y = ppmNO)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "NO Emissions (in ppmNO)") +
  scale_x_discrete(name = "Site ID") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ year)
dev.off()




iData <- matrix(nrow = 4, ncol = 6)


for (i in 1:4){
  subset1 <- cleandata(allEV[which(allEV$year == x[i]), c("ppmNO", "vAge","date")]) 
  iData[i,1] = paste("Year ",x[i])
  iData[i,2] = nrow(subset1)
  iData[i,3] = skewness((subset1$ppmNO) )
  iData[i,4] = mean(subset1$ppmNO)
  iData[i,5] = median(subset1$ppmNO)
  
}

mytest = data.frame(iData)
colnames(mytest) = c("Campaign", "Sample Size", "Skewness","Mean NO emission", "Median NO emission" )
stargazer(mytest[,1:5], summary=FALSE, rownames=FALSE)

iData <- matrix(nrow = 4, ncol = 3)


for (i in 1:4){
  subset1 <- cleandata(allEV[which(allEV$year == x[i]), c("ppmNO", "vAge","date")]) 
  iData[i,1] = paste("Year ",x[i])
  iData[i,2] = length(unique(subset1$date))
  
  
}
mytest = data.frame(iData)
colnames(mytest) = c("Campaign Year","Number of Collections", "Number of Campaigns"  )
mytest$`Number of Campaigns` = c(5,4,3,4)
stargazer(mytest, summary=FALSE, rownames=FALSE)

xlevels = levels(allEV$vAgeBin2)
levels(allEV$vAgeBin2) = c(1,7, 12,13,14,2,3,4,5,6,8,9,10,11)
allEV$vAgeBin2 <- factor(allEV$vAgeBin2, levels = xlevels[c(1,7,11,12,13,2,3,4,5,6,8,9,10)])

varImpTable$year = factor(varImpTable$year)
varImpTable$cookOutlier = factor(varImpTable$cookOutlier)

ggplot(varImpTable, aes(x = cookOutlier, y = Overall)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "NO Emissions (in ppmNO)") +
  scale_x_discrete(name = "Site ID") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ feature)

iData$X3 = factor(iData$X3)
ggplot(iData, aes(x = X3, y = X4)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "NO Emissions (in ppmNO)") +
  scale_x_discrete(name = "Site ID") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

allEV$vw = "not VW"
allEV[which(allEV$make == "VOLKSWAGEN"), "not VW"] = "VW"
allEV[which(allEV$make == "VOLKSWAGEN"), "vw"] = "vw"
allEV$euro = "not Euro"


allEV$vw = factor(allEV$vw)

pollutant = c("uvSmoke", "perCO", "ppmNO", "ppmHC", "perCO2")
fueltype = c("Diesel", "Petrol")


setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/VW scandal/")
for (i in 1:length(pollutant)){
  for (j in 1:length(fueltype)){

  

    
      ggplot(allEV[which(allEV$fType == fueltype[j]),], aes_string(x = "vw", y = pollutant[i])) +
        geom_boxplot(fill = fill, colour = line,
                     alpha = 0.7) +
        scale_y_continuous(name = pollutant[i]) +
        scale_x_discrete(name = fueltype[j]) +
        theme_bw() +
        theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, hjust = 1))+
        facet_grid(. ~ year)
      ggsave(paste(pollutant[i],"_",fueltype[j],"plot.png"), width = 16, height = 9, dpi = 100)
      

  }
}

i = 3
j = 1

allEV$manYearBin = factor(allEV$manYearBin)
temp = allEV[which(allEV$fType == fueltype[1] & allEV$manYear>2000),]
ggplot(temp, aes(x =vw, y = ppmNO)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = pollutant[i]) +
  scale_x_discrete(name = fueltype[j]) +
  theme_bw() +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ manYearBin)
ggsave(paste(pollutant[i],"_",fueltype[j]," engplot.png"), width = 16, height = 9, dpi = 100)


my.formula <- y ~ x

ggplot(allEV, aes(x=vAge, y=ppmNO)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, formula = my.formula)+    # Don't add shaded confidence region
stat_poly_eq(formula = my.formula, color = "blue",
             eq.with.lhs = "italic(hat(y))~`=`~",
             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
             parse = TRUE) +  scale_x_continuous(name = "Vehicle Age")

setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland/Thesis v2")
ggsave(paste("LR.png"), width = 10, height = 7, dpi = 100)

ggsave("LR.pdf",width=7,height=4)


m <- data.frame(matrix(0, ncol = 2, nrow = 3))

colnames(m) = c("Diesel" , "Others")
rownames(m) = c("Observation 1 (Diesel)", "Observation 2 (Others)", "Observation 3 (Petrol)")
m[1,] = c(1,0)
m[2,] = c(0,1)
m[3,] = c(0,0)
m$Observations = rownames(m)
stargazer(m[,c(3,1,2)], summary=FALSE, rownames=FALSE)

m <- data.frame(matrix(0, ncol = 4, nrow = 20))


unique(allEV$cGroup1)

for (z in 1:20){
  message("*********** predicting ", z ," level ", " \n",appendLF = FALSE)
  
trainIndex <- createDataPartition(allEVcont$ppmNO, p = .6,
                                  list = FALSE,
                                  times = 1)  

allEVtraining = allEVcont[trainIndex,]
allEVtest = allEVcont[-trainIndex,]
lmCVFit<-train(ppmNO ~ ., data = allEVtraining[,-25], method = "lm", trControl = ctrl, metric="Rsquared")
predictions = predict(lmCVFit, allEVtraining[,-c(1,25)])
m[z,1] = sqrt(mean(((predictions - allEVtraining$ppmNO)^2)))
m[z,2] = mean(abs(predictions - allEVtraining$ppmNO))

predictions = predict(lmCVFit, allEVtest[,-c(1,25)])
m[z,3] = sqrt(mean(((predictions - allEVtest$ppmNO)^2)))
m[z,4] = mean(abs(predictions - allEVtest$ppmNO))

}
colnames(m) = c("RMSE Training", "MAE Training", "RMSE Test", "MAE Test")
m$id = seq(1:20)
m<-melt(m,id.vars="id")
allEV$fType

ggplot(data = m, aes(x = datatype, y = value)) +
  geom_boxplot(fill = fill,colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "Error Measure Type") +
  theme_bw() +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ measuretype)
ggsave("CV.pdf",width=7,height=4)

m$measuretype = ""
m$datatype = ""
m[grep("RMSE", m$variable), "measuretype"] = "RMSE"
m[grep("MAE", m$variable), "measuretype"] = "MAE"
m[grep("Training", m$variable), "datatype"] = "Training"
m[grep("Test", m$variable), "datatype"] = "Test"

cooks.distance(lmCVFit)

ggsave(paste(pollutant[i],"_",fueltype[j]," engplot.png"), width = 16, height = 9, dpi = 100)






my.formula <- y ~ x

ggplot(allEVtraining, aes(x=vAge, y=ppmNO, color = outlier)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, formula = my.formula) +  scale_x_continuous(name = "Vehicle Age")



trainIndex <- createDataPartition(allEV$ppmNO, p = .6,
                                  list = FALSE,
                                  times = 1)  

allEVtraining = allEV[trainIndex,]
allEVtest = allEVcont[-trainIndex,]
ctrl<-trainControl(method="repeatedcv", number=10, repeats=3)
lmCVFit<-train(ppmNO ~ ., data = allEVtraining[,-15], method = "lm", trControl = ctrl, metric="Rsquared")

allEVtraining = cookOutlierLabel(allEVtraining, lmCVFit$finalModel)
smoother <- geom_smooth(color = "red", alpha = 0.5, method = "lm", se = F)

a = ggplot(data = allEVtraining, mapping = aes(x = vAge, y = ppmNO, color = outlier)) + geom_point(shape=1) + smoother+
  scale_x_continuous(name = "Vehicle Age", limits = c(-0, 80)) +
  scale_y_continuous(name = "NO Emissions (in ppmNO)", limits = c(-1500, 7500)) 
  
b = ggplot(data = allEVtraining[which(allEVtraining$outlier == "Not Outlier"),], mapping = aes(x = vAge, y = ppmNO, color = outlier)) + geom_point(shape=1) + smoother+
  scale_x_continuous(name = "Vehicle Age", limits = c(-0, 80)) +
  scale_y_continuous(name = "NO Emissions (in ppmNO)", limits = c(-1500, 7500)) 
pdf("outlier.pdf",width=8,height=3)

grid.arrange(a, b, ncol=2)
dev.off()
setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland/Thesis v2")

m<-melt(iData,id.vars= c("run", "outlier"))

ggplot(data = m, aes(x = outlier, y = value)) +
  geom_boxplot(fill = fill,colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "Model Type") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ variable)
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")
ggsave("outlierv2.pdf",width=7,height=4)

mean(iData[which(iData$outlier == "Include Outliers"), "RMSE"])/mean(iData[which(iData$outlier == "Exclude Outliers"), "RMSE"])
mean(iData[which(iData$outlier == "Include Outliers"), "MAE"])/mean(iData[which(iData$outlier == "Exclude Outliers"), "MAE"])

m<-melt(iData,id.vars= c("run", "Algorithm"))


trainIndex <- createDataPartition(allEVcont$ppmNO, p = .2,
                                  list = FALSE,
                                  times = 1)  

allEVtraining = allEV[trainIndex,]
allEVtest = allEV[-trainIndex,]

ggplot(allEVtraining, aes(x=vAge, y=ppmNO, color = fType)) +
  geom_point(shape=1) +  scale_x_continuous(name = "Vehicle Age")


setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")

m[which(m$model == "qrf"),"model"] = "QRF"
m[which(m$model == "rf"),"model"] = "RF"

iData = read_csv("rfvsqrf.csv")
iData = iData[,2:6]


m<-melt(iData,id.vars= c("run", "model", "mtry"))
m$mtry = factor(m$mtry)
m$model = factor(m$model)
ggplot(data = m[which(m$variable == "RMSE"),], aes(x = mtry, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "mTry") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ model)
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")
ggsave("mtryRMSE.pdf",width=7,height=4)

ggplot(data = m[which(m$variable == "MAE"),], aes(x = mtry, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "mTry") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ model)
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")
ggsave("mtryMAE.pdf",width=7,height=4)


iData = read_csv("rfvsqrf ntree.csv")
iData = iData[,2:6]


m<-melt(iData,id.vars= c("run", "model", "ntree"))
m$ntree = factor(m$ntree)
m$model = factor(m$model)

ggplot(data = m[which(m$variable == "RMSE"),], aes(x = ntree, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "nTree") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ model)
setwd("/Users/jiazhenchen/Google Drive/Vehicle Emission Auckland/Thesis v2")

setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")
ggsave("ntreeRMSE.pdf",width=7,height=4)

ggplot(data = m[which(m$variable == "MAE"),], aes(x = ntree, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "nTree") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ model)
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/")
ggsave("ntreeMAE.pdf",width=7,height=4)


iData = read_csv("rfvsqrf outlier.csv")
iData = iData[,2:6]
iData$newField = ""
iData[which(iData$outlier == "Outliers Included"), "newField"] = "Outliers Excluded"
iData[which(iData$outlier == "Outliers Excluded"), "newField"] = "Outliers Included"
iData = iData[,c(1,2,4,5,6)]

m<-melt(iData,id.vars= c("run", "newField", "model"))
m$outlier = factor(m$newField)
m$model = factor(m$model)

ggplot(data = m[which(m$variable == "RMSE"),], aes(x = model, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "Model Type") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ outlier)

setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")
ggsave("outlierRMSE.pdf",width=7,height=4)

ggplot(data = m[which(m$variable == "MAE"),], aes(x = model, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "Model Type") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ outlier)
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/")
ggsave("outlierMAE.pdf",width=7,height=4)



iData = read_csv("rfvsmob outlier.csv")
iData = iData[,2:6]


m<-melt(iData,id.vars= c("run", "model", "outlier"))
m$outlier = factor(m$outlier)
m$model = factor(m$model)

ggplot(data = m[which(m$variable == "RMSE"),], aes(x = model, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "Model Type") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ outlier)

setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")
ggsave("mobqrf_outlierRMSE.pdf",width=7,height=4)

ggplot(data = m[which(m$variable == "MAE"),], aes(x = model, y = value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Prediction Error (ppmNO)") +
  scale_x_discrete(name = "Model Type") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ outlier)
setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/")
ggsave("mobqrf_outlierMAE.pdf",width=7,height=4)

setwd("C:/Users/jche117.UOA.000/Google Drive/Vehicle Emission Auckland/Thesis v2")

# Figure 4.17 feature ranking and mtry 
class(m$IncNodePurity)
m = read_csv("feature ranking_mtry.csv")
m = m[,-1]
a= m$IncNodePurity
m = replaceAcro(m)
m$IncNodePurity = a
ggplot(data = m , aes(x = featurename, y = IncNodePurity)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Feature Importance") +
  scale_x_discrete(name = "Feature") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ mtry)
ggsave("mtry feature ranking.pdf",width=7,height=4)

#Figure 4.18

m$mtry = factor(m$mtry)
ggplot(data = m , aes(x = mtry, y = IncNodePurity)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Feature Importance") +
  scale_x_discrete(name = "mTry") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ featurename)
ggsave("featureranking2.pdf",width=7,height=4)

levels(m$featurename) = c("YM", "IO", "VA", "OR", "EG", "VSP", "FT", "VT")
m$featurename <- factor(m$featurename, c("YM", "IO", "VA", "OR", "EG", "VSP", "FT", "VT"))



#twopair feature analysis 
m = read_csv("twopair_feature_validation.csv")
m = m[,2:6]

ggplot(data = m , aes(x = feature, y = MAE)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Model Performance (MAE)") +
  scale_x_discrete(name = "Feature") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))

#Figure 4.19 stepwise feature induction evaluation
m = read_csv("stepwise_evaluation.csv")
m = m[,2:6]
m$featurename <- factor(m$featurename, c("cGroup1","vAgeBin2", "odoBin","eBin",  "vspBin","fType" ,"vehicleType"))
a= m$MAE
m = replaceAcro(m)
m$MAE = a


levels(m$featuren) = c("YM", "IO", "VA", "OR", "EG", "VSP", "FT", "VT")
m$feature <- factor(m$feature, c("IO", "VA", "OR", "EG", "VSP", "FT", "VT"))


ggplot(data = m , aes(x = feature, y = MAE)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Model Performance (MAE)") +
  scale_x_discrete(name = "Feature") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("stepwise.pdf",width=7,height=4)

#Figure 4.20 

m = read_csv("feature_ranking_byyear.csv")
m = m[,-1]
a= m$IncNodePurity
m = replaceAcro(m)
m$IncNodePurity = a
m$featurename <- factor(m$featurename, c("YM", "IO", "VA", "OR", "EG", "VSP", "FT", "VT"))

ggplot(data = m , aes(x = featurename, y = IncNodePurity)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Feature Importance") +
  scale_x_discrete(name = "Feature") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ year)
ggsave("varImpYear.pdf",width=7,height=4)


#Figure 4.21

m = read_csv("stepwise_byyear.csv")
m = m[,-1]
a= m$MAE
m = replaceAcro(m)
m$MAE = a
m$feature <- factor(m$feature, c( "IO", "VA", "OR", "EG", "VSP", "FT", "VT"))

ggplot(data = m , aes(x = feature, y = MAE)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Model Performance (MAE)") +
  scale_x_discrete(name = "Feature") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ year)
ggsave("stepwiseyear.pdf",width=7,height=4)

#Figure 5.3
 allEV$vAgeBin = factor(allEV$vAgeBin)
a = ggplot(data = allEV[which(allEV$manYearBin <= "1989"),] , aes(x = vAgeBin, y = ppmNO)) +
geom_boxplot(fill = fill, colour = line,
             alpha = 0.7) +
  scale_y_continuous(name = "Nitric Oxides (ppm)", limits = c(0, 8000)) +
  scale_x_discrete(name = "Vehicle Age (Manufactured prior 1989)") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, hjust = 1))

b = ggplot(data = allEV[which(allEV$manYearBin <= "1999"&allEV$manYearBin > "1989"),] , aes(x = vAgeBin, y = ppmNO)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Nitric Oxides (ppm)", limits = c(0, 8000)) +
  scale_x_discrete(name = "Vehicle Age (Manufactured 1989 - 1999)") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, hjust = 1))
c = ggplot(data = allEV[which(allEV$manYearBin <= "2009"&allEV$manYearBin > "1999"),] , aes(x = vAgeBin, y = ppmNO)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Nitric Oxides (ppm)", limits = c(0, 8000)) +
  scale_x_discrete(name = "Vehicle Age (Manufactured 1999 - 2009)") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, hjust = 1))
d = ggplot(data = allEV[which(allEV$manYearBin > "2009"),] , aes(x = vAgeBin, y = ppmNO)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Nitric Oxides (ppm)", limits = c(0, 8000)) +
  scale_x_discrete(name = "Vehicle Age (Manufactured after 2009)") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, hjust = 1))

pdf("corrYMVA2.pdf",width=12,height=5)

grid.arrange(a,b,c,d, ncol = 2)

dev.off()
ggsave("corrYMVA.pdf",width=7,height=4)

#Figure 5.1 
m = read_csv("ctree level.csv")
m = m[,-1]
m$TreeDepth = factor(m$TreeDepth)
colnames(m) = c("TreeDepth", "MAE")
ggplot(data = m , aes(x = TreeDepth, y = MAE)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "MAE") +
  scale_x_discrete(name = "Tree Depth") +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("treeDepth.pdf",width=7,height=4)


png("ctextraction.png", res=80, height=800, width=2400) 
plot(ct2) 
dev.off()

# Figure 5.6

m = read_csv("ruleEvaluation.csv")
m$RuleID = paste("Rule", m$X1)
ggplot(m, aes(x=MADtrain, y=MAETest)) +
  scale_y_continuous(name = "MAE") +
  scale_x_continuous(name = "MAD to median") +
  geom_text(label=m$RuleID)

ggsave("ruleMAD.pdf",width=7,height=4)

m = m[,c("RuleID","MAEtreeprediction","MAEExpectedValueTrain", "MAETest")]
m$RuleID =paste("Rule",rownames(m))  
colnames(m) = c("RuleID","MAEcTree","MAEGeneralizedQRF", "MAEQRF")
m<-melt(m,id.vars= "RuleID")
colnames(m) = c("RuleID","PredictorType","value")
m$RuleID = as.character(m$RuleID)
m$RuleID = factor(m$RuleID)
ggplot(m, aes(x=RuleID, y=value, fill=PredictorType)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+  
  scale_y_continuous(name = "MAE") +
theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("predictor.pdf",width=12,height=5)
m$RuleID = factor(m$RuleID, levels = levels(m$RuleID)[c(1,12,21:27,2:11, 13:20)])
m[order(nchar(m$RuleID),m$RuleID),"RuleID"]

m = read_csv("surveyML.csv")
stargazer(m, summary=FALSE, rownames=FALSE)
m$AbilityModel = c("Moderate", "High", "High", "High", "High", "High", "Low")

ggplot(data = allEV[which(allEV$fuelType %in% c("Diesel","Petrol"))] , aes(x = vAgeBin2, y = ppmNO)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_y_continuous(name = "Feature Importance") +
  scale_x_continuous(name = "Feature") +
  theme_bw() +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ fuelType)
ggsave("mtry feature ranking.pdf",width=7,height=4)


temp = allEVTemp[which(allEVTemp$fType %in% c("Diesel","Petrol")),]
temp$vAgeBin2 = factor(temp$vAgeBin2, levels(temp$vAgeBin2)[c(1,7,11:13,2:6,8:10)])
ggplot(data = temp, mapping = aes(x = vAgeBin2, y = ppmNO)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7) +
  scale_x_discrete(name = "Vehicle Age") +
  scale_y_continuous(name = "NO Emissions (in ppm)", limits = c(-1500, 7500)) +
  facet_grid(. ~ fType)+
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("dieselpetrol.pdf",width=7,height=4)



dfMultiYear = read.csv("yearly model on yearly data.csv") 
dfMultiYear = dfMultiYear[,-1]
colnames(dfMultiYear) = c("Model", "DataYear", "Trial", "Value")
dfMultiYear = dfMultiYear[which(!dfMultiYear$Model %in% "Model Multi-Year"),]

dfMultiYear$Model = factor(dfMultiYear$Model)
dfMultiYear$DataYear = factor(dfMultiYear$DataYear)
class(dfMultiYear$Value)


ggplot(dfMultiYear, aes(x = Model, y = Value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7)  +
  scale_y_continuous(name = "Mean Absolute Error") +
  scale_x_discrete(name = "Model with Top N Ranked Features") +
  theme_bw() +
  facet_grid(. ~ DataYear)

ggplot(dfMultiYear, aes(x = DataYear, y = Value)) +
  geom_boxplot(fill = fill, colour = line,
               alpha = 0.7)  +
  scale_y_continuous(name = "Mean Absolute Error") +
  scale_x_discrete(name = "Model with Top N Ranked Features") +
  theme_bw()
facet_grid(. ~ DataYear)
