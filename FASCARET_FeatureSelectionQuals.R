# warning: could take over an hour to install all models the first time you install the fscaret package
# install.packages("fscaret", dependencies = c("Depends", "Suggests"))
library(tidyverse)
library(lubridate)
library(caret)
library(stringi)
library(stringr)
library(nnet)
library(xgboost)
library(randomForest)
library(earth)
library(kernlab)
library(e1071)
library(elasticnet)
library(Cubist)
library(fscaret)

# list of models fscaret supports:
data(funcRegPred)
funcRegPred

#list of models caret supports:
names(getModelInfo())

quals <- read_csv("All_System_Qualifiers_Yr_2018_Cleaned.csv", col_names = T)

quals <- quals %>%
  filter(Month != 11)

colSums(is.na(quals))

quals$Dist_Range[quals$Dist_Range == "NH"] <- NA

table(quals$Dist_Range, quals$RaceType)

quals <- quals %>%
  select(-Dist_Range)

colSums(is.na(quals))

quals <- na.omit(quals)

str(quals)

summary(quals)



quals$Handicap <- as.factor(quals$Handicap)
quals$Ratings_Range <- as.factor(quals$Ratings_Range)
quals$Going_Range <- as.factor(quals$Going_Range)
quals$RaceType <- as.factor(quals$RaceType)
#quals$Dist_Range <- as.factor(quals$Dist_Range)
quals$System_Name <- as.factor(quals$System_Name)


qualsData <- quals %>%
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Rev_Weight_Rank, NumberOfResults, Age, BF_Placed_SP_PL)


# split data set into train and test portion
set.seed(1234)
splitIndex <- createDataPartition(qualsData$BF_Placed_SP_PL, p = .50, list = FALSE, times = 1)
trainDF <- qualsData[ splitIndex,]
testDF  <- qualsData[-splitIndex,]

# limit models to use in ensemble and run fscaret
fsModels <- c("pls", "avNNet", "cubist", "rf", "gbm", "earth")
myFS<-fscaret(trainDF, testDF, preprocessData=TRUE, regPred = T, classPred = F, installReqPckg = T,
              Used.funcRegPred = fsModels, with.labels=TRUE, #myTimeLimit = 4000000,
              impCalcMet = "RMSE", supress.output=FALSE, no.cores=4)

# analyze results
print(myFS$VarImp)
print(myFS$PPlabels)

