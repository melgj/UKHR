library(tidyverse)
library(lubridate)
library(caret)
library(stringi)
library(stringr)
library(nnet)


quals <- read_csv("All_System_Qualifiers_to_2018_10.csv", col_names = T)

colSums(is.na(quals))

quals$Dist_Range[is.na(quals$Dist_Range)] <- "NH"

quals <- quals %>% 
  drop_na(Betfair.Placed)

quals <- quals %>% 
  select(-c(Trainer, Jockey, Sire, Alarms))

colSums(is.na(quals))

quals <- na.omit(quals)

str(quals)

quals$Handicap <- as.factor(quals$Handicap)
quals$Ratings_Range <- as.factor(quals$Ratings_Range)
quals$Going_Range <- as.factor(quals$Going_Range)
quals$RaceType <- as.factor(quals$RaceType)
quals$Dist_Range <- as.factor(quals$Dist_Range)
quals$System_Name <- as.factor(quals$System_Name)


qualsData <- quals %>% 
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, System_Name, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, BFSP_PL)

colSums(is.na(qualsData))

##############################################

library(earth)

library(doMC)

registerDoMC(4)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows,]

ukTestSet <- qualsData[-ukTrainRows,]

summary(ukTrainSet)
str(ukTrainSet)

marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:25)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 6,
                              #summaryFunction = RMSE,
                              verboseIter = T)

set.seed(100)                              

bfPLMod <- train(BFSP_PL ~ ., 
                   data = qualsData,
                   method = "earth",
                   tuneGrid = marsGrid,
                   metric = "RMSE",
                   trControl = train.control)

print(bfPLMod)

varImp(bfPLMod)

saveRDS(bfPLMod, "Systems_MARS_BFSPPL_Model.RDS")

#readRDS("Systems_MARS_BFSPPL_Model.RDS")

predBFPL <- predict(bfPLMod, newdata = ukTestSet, type = "raw")

head(predBFPL)
head(ukTestSet$BFSP_PL)

R2(predBFPL, ukTestSet$BFSP_PL)
RMSE(predBFPL, ukTestSet$BFSP_PL)
cor(predBFPL, ukTestSet$BFSP_PL)

ukTestSet$PredPL <- predBFPL

ukPos <- filter(ukTestSet, PredPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, PredPL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(PredPL > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))

ukPos %>% 
  group_by(Handicap) %>% 
  filter(PredPL > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))

###############################################################

predBFPL <- predict(bfPLMod, newdata = qualsData, type = "raw")

head(predBFPL)
head(qualsData$BFSP_PL)

R2(predBFPL, qualsData$BFSP_PL)
RMSE(predBFPL, qualsData$BFSP_PL)
cor(predBFPL, qualsData$BFSP_PL)

qualsData$PredPL <- predBFPL

ukPos <- filter(qualsData, PredPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(qualsData, PredPL <= 0)
mean(ukNeg$BFSP_PL)

mean(qualsData$BFSP_PL)

qualsData %>% 
  group_by(Handicap) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))

ukPos %>% 
  group_by(Handicap) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))





#####################################################################
qualsData <- quals %>% 
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, System_Name, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, BFSP_PL)

library(doMC)

registerDoMC(4)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows,]

ukTestSet <- qualsData[-ukTrainRows,]

colnames(ukTrainSet)

colSums(is.na(ukTrainSet))


numUKTrain <- ukTrainSet %>% 
  select_if(is.numeric)

highCorrs <- findCorrelation(cor(numUKTrain), cutoff = 0.7)

colnames(numUKTrain[,highCorrs])

library(corrplot)

correlations <- cor(numUKTrain)

correlations

corrplot(correlations, order = "hclust")

ukTrainSet <- ukTrainSet[,-highCorrs]
ukTestSet <- ukTestSet[, -highCorrs]

colnames(ukTrainSet)

#############################################################
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

nnetGrid <- expand.grid(.decay = c(0.01, 0.05, 0.10),
                        .size = c(1:10),
                        .bag = F)
                        

set.seed(100)

ukNN <- train(BFSP_PL ~ .,
              data = ukTrainSet,
              method = "avNNet",
              tuneGrid = nnetGrid,
              trControl = train.control,
              preProc = c("center", "scale"),
              linout = T,
              trace = F,
              MaxNWts = 10 * (ncol(ukTrainSet) + 1) +10 +1,
              maxit = 1000)

print(ukNN)

predBFPL <- predict(ukNN, newdata = ukTestSet, type = "raw")

head(predBFPL)
head(ukTestSet$BFSP_PL)

R2(predBFPL, ukTestSet$BFSP_PL)
RMSE(predBFPL, ukTestSet$BFSP_PL)
cor(predBFPL, ukTestSet$BFSP_PL)

ukTestSet$PredPL <- predBFPL


ukPos <- filter(ukTestSet, PredPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, PredPL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(PredPL >= 0.00) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))

#saveRDS(ukNN, "Systems_NN_BFSPPL_Model.RDS")

#########################

#################################################################

# XGB Tree Algorithm to predict BFSP Profit/Loss

library(xgboost)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows,]

ukTestSet <- qualsData[-ukTrainRows,]


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)
                              #classProbs = TRUE, 
                              #summaryFunction = RMSE)


tune.grid <- expand.grid(eta = c(0.01),
                         nrounds = c(150, 300),
                         lambda = c(0.05),
                         alpha = c(1.0))
                         
                         
View(tune.grid)

set.seed(100)

xgbLinModUK <- train(BFSP_PL ~ .,
                      data = ukTrainSet,
                      method = "xgbLinear",
                      preProc = c("center", "scale"),
                      metric = "RMSE",
                      tuneGrid = tune.grid,
                      trControl = train.control)

print(xgbLinModUK)

varImp(xgbLinModUK)





predXGBbfpl <- predict(xgbLinModUK, newdata = ukTestSet, type = "raw")

head(predXGBbfpl)


summary(predXGBbfpl)


R2(predXGBbfpl, ukTestSet$BFSP_PL)
RMSE(predXGBbfpl, ukTestSet$BFSP_PL)
cor(predXGBbfpl, ukTestSet$BFSP_PL)

ukTestSet$XGBPL <- predXGBbfpl


ukPos <- filter(ukTestSet, XGBPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, XGBPL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(XGBPL <= 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


saveRDS(xgbLinModUK, "XGB_Linear_Systems_BFPL_Model.RDS")

#xgbLinModUK <- readRDS("XGB_Linear_Systems_BFPL_Model.RDS")


print(xgbLinModUK)


###############################################################



library(randomForest)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows,]

ukTestSet <- qualsData[-ukTrainRows,]

colSums(is.na(ukTrainSet))

set.seed(100)

rfMod <- train(BFSP_PL ~ .,
               data = ukTrainSet,
               method = "rf",
               ntrees = 1500,
               PreProc = c("center", "scale"),
               importance = T)

print(rfMod)

varImp(rfMod)

predRF <- predict(rfMod, newdata = ukTestSet, type = "raw")

head(predRF)

ukTestSet$RF_PL <- predRF


ukPos <- filter(ukTestSet, RF_PL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, RF_PL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(RF_PL > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


saveRDS(rfMod, "RF_BFPL_Model.RDS")



################################################################



# XGB Tree Algorithm to predict Win Probability


quals <- quals %>% 
  mutate(Result = if_else(Actual == 1, 
                          "WON",
                          "LOST"))

qualsData <- quals %>% 
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, System_Name, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, Actual)

qualsData$Result <- as.factor(qualsData$Result)


set.seed(100)

ukTrainRows <- createDataPartition(qualsData$Result, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows,]

ukTestSet <- qualsData[-ukTrainRows,]


library(xgboost)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T,
                              classProbs = TRUE, 
                              summaryFunction = mnLogLoss)


tune.grid <- expand.grid(eta = c(0.01, 0.05, 0.10),
                         nrounds = c(150, 200, 250),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.5, 3.0),
                         colsample_bytree = c(0.50, 0.75),
                         gamma = 1,
                         subsample = c(0.5,1.0))
View(tune.grid)

xgbTreeModUK <- train(Result ~ .,
                      data = ukTrainSet,
                      method = "xgbTree",
                      metric = "logLoss",
                      tuneGrid = tune.grid,
                      trControl = train.control)

print(xgbTreeModUK)

varImp(xgbTreeModUK)

xgbTreeModUK <- readRDS("XGB_Systems_Model_Prob")

predOutcomeXGB <- predict(xgbTreeModUK, newdata = ukTestSet, type = "raw")

head(predOutcomeXGB)

table(ukTestSet$Result, predOutcomeXGB)


confusionMatrix(ukTestSet$Result, predOutcomeXGB)


