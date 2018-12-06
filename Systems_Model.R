#library(MASS)
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



quals <- read_csv("All_System_Qualifiers_Yr_2018.csv", col_names = T)

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
  select(BetFairSPForecastWinPrice, System_Name, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, BFSP_PL)

colSums(is.na(qualsData))

##############################################

#library(earth)

# library(doMC)
# 
# registerDoMC(8)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows,]

ukTestSet <- qualsData[-ukTrainRows,]

summary(ukTrainSet)
str(ukTrainSet)

#########################################################################################

marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:50)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

set.seed(100)                              

bfPLMod <- train(BFSP_PL ~ ., 
                   data = ukTrainSet,
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

# Build NN Model


library(doMC)

registerDoMC(8)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

colnames(qualsData)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]

colnames(ukTrainSet)

colSums(is.na(ukTrainSet))


numUKTrain <- ukTrainSet %>% 
  select_if(is.numeric)

highCorrs <- findCorrelation(cor(numUKTrain), cutoff = 0.75)

colnames(numUKTrain[,highCorrs])

library(corrplot)

correlations <- cor(numUKTrain)

correlations

library(cor)

corrplot(correlations, order = "hclust")

ukTrainSet <- ukTrainSet[,-highCorrs]
ukTestSet <- ukTestSet[, -highCorrs]

colnames(ukTrainSet)

############################################################

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

nnetGrid <- expand.grid(.decay = c(0.01, 0.025, 0.05, 0.075, 0.10),
                        .size = c(1:5),
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
              maxit = 500)

print(ukNN)

#ukNN <- readRDS("Systems_NN_BFSPPL_Model.RDS")

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
  filter(PredPL >= 0.20) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))

saveRDS(ukNN, "Systems_NN_BFSPPL_Model_v50.RDS")

#########################

#################################################################

# XGB Tree Algorithm to predict BFSP Profit/Loss

# library(xgboost)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)
                              #classProbs = TRUE, 
                              #summaryFunction = RMSE)


tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05, 0.075, 0.1),
                         nrounds = c(100, 150, 200),
                         lambda = c(0.01, 0.025, 0.05, 0.075, 0.1),
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
  filter(XGBPL >= 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


saveRDS(xgbLinModUK, "XGB_Linear_Systems_BFPL_Model_v50.RDS")

#xgbLinModUK <- readRDS("XGB_Linear_Systems_BFPL_Model.RDS")


################################################################

# Build RF Model

library(doMC)

registerDoMC(8)

library(randomForest)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]

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


saveRDS(rfMod, "RF_BFPL_Model_v50.RDS")

##############################################################

# numUKTrain <- ukTrainSet %>% 
#   select_if(is.numeric)
# 
# highCorrs <- findCorrelation(cor(numUKTrain), cutoff = 0.75)
# 
# colnames(numUKTrain[,highCorrs])
# 
# library(corrplot)
# 
# correlations <- cor(numUKTrain)
# 
# correlations
# 
# corrplot(correlations, order = "hclust")
# 
# ukTrainSet <- ukTrainSet[,-highCorrs]
# ukTestSet <- ukTestSet[, -highCorrs]
# 
# colnames(ukTrainSet)
# 
# 
# knnMod <- train(BFSP_PL ~ .,
#                 data = ukTrainSet,
#                 method = "knn",
#                 preProc = c("center", "scale"),
#                 tune.grid = data.frame(.k = 1:30),
#                 trControl = trainControl(method = "repeatedcv",
#                                          number = 10,
#                                          repeats = 3,
#                                          verboseIter = T))
# 
# print(knnMod)
# 
# varImp(knnMod)
# 
# predKNN <- predict(knnMod, newdata = ukTestSet, type = "raw")
# 
# head(predKNN)
# 
# ukTestSet$KNN_PL <- predKNN
# 
# ukPos <- filter(ukTestSet, KNN_PL > 0)
# mean(ukPos$BFSP_PL)
# 
# ukNeg <- filter(ukTestSet, KNN_PL <= 0)
# mean(ukNeg$BFSP_PL)
# 
# mean(ukTestSet$BFSP_PL)
# 
# ukTestSet %>% 
#   group_by(Handicap) %>% 
#   filter(KNN_PL > 0.0) %>% 
#   summarise(Runs = n(),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>% 
#   arrange(desc(Avg_PL))

############################################################

# Build SVM Model

library(doMC)

registerDoMC(8)

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.3, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]

colnames(ukTestSet)

train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              verboseIter = T)

set.seed(100)

svmLinModUK <- train(BFSP_PL ~ .,
                     data = ukTrainSet,
                     method = "svmRadial",
                     preProc = c("center", "scale"),
                     metric = "RMSE",
                     tuneLength = 10,
                     trControl = train.control)

print(svmLinModUK)

varImp(svmLinModUK)

predSVM <- predict(svmLinModUK, newdata = ukTestSet, type = "raw")

head(predSVM)

ukTestSet$SVM_PL <- predSVM

ukPos <- filter(ukTestSet, SVM_PL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, SVM_PL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(SVM_PL > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

#saveRDS(svmLinModUK, "SVM_Systems_Model.RDS")

#########################################################################

# Build Ridge Regression Model

library(doMC)
registerDoMC(4)


set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]

colnames(ukTestSet)


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

ridgeGrid <- data.frame(.lambda = seq(0, .2, length = 30))

ridgeGrid

set.seed(100)                              

ridgeMod <- train(BFSP_PL ~ ., 
                     data = ukTrainSet,
                     method = "ridge",
                     preProc = c("center", "scale"),
                     tuneLength = 20,
                     metric = "RMSE",
                     trControl = train.control)

ridgeMod

predRIDGE <- predict(ridgeMod, newdata = ukTestSet, type = "raw")

head(predRIDGE)

ukTestSet$Ridge_PL <- predRIDGE

ukPos <- filter(ukTestSet, Ridge_PL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, Ridge_PL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(Ridge_PL > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

saveRDS(ridgeMod, "Ridge_BFPL_Model.RDS")

#############################################################
# Build PLS Model

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]

colnames(ukTestSet)


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

set.seed(100)                              

plsMod <- train(BFSP_PL ~ ., 
                data = ukTrainSet,
                method = "simpls",
                preProc = c("center", "scale"),
                tuneLength = 20,
                metric = "RMSE",
                trControl = train.control)

plsMod

predPLS <- predict(plsMod, newdata = ukTestSet, type = "raw")

head(predPLS)

ukTestSet$PLS_PL <- predPLS

ukPos <- filter(ukTestSet, PLS_PL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, PLS_PL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(PLS_PL > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

saveRDS(plsMod, "PLS_BFPL_Model.RDS")


#############################################################

# XGB Tree Algorithm to predict Win Probability


quals3 <- quals %>% 
  mutate(Result = if_else(Actual == 1, 
                          "WON",
                          "LOST"))

qualsData3 <- quals3 %>% 
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, Result)

qualsData3$Result <- as.factor(qualsData3$Result)


set.seed(100)

ukTrainRows <- createDataPartition(qualsData3$Result, p = 0.6, list = FALSE)

ukTrainSet <- qualsData3[ukTrainRows,]

ukTestSet <- qualsData3[-ukTrainRows,]


library(xgboost)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T,
                              classProbs = TRUE, 
                              summaryFunction = mnLogLoss)


tune.grid <- expand.grid(eta = c(0.01, 0.05, 0.10),
                         nrounds = c(100, 150, 200),
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

xgbTreeModUK

predOutcomeXGB <- predict(xgbTreeModUK, newdata = ukTestSet, type = "prob")

head(predOutcomeXGB)

predOutcomeXGB <- predict(xgbTreeModUK, newdata = ukTestSet, type = "raw")

table(ukTestSet$Result, predOutcomeXGB)

confusionMatrix(ukTestSet$Result, predOutcomeXGB)

saveRDS(xgbTreeModUK, "XGB_Systems_Model_Prob_V50.RDS")

#xgbTreeModUK <- readRDS("XGB_Systems_Model_Prob")


##############################################################

# Build Final Model using XGBoost Linear

nn <- readRDS("Systems_NN_BFSPPL_Model_v20.RDS")
#xgb <- readRDS("XGB_Linear_Systems_BFPL_Model.RDS")
rf <- readRDS("RF_BFPL_Model_v20.RDS")
#mars <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

nnPred <- predict(nn, newdata = qualsData, type = "raw")
xgbProb <- predict(xgbTreeModUK, newdata = qualsData, type = "prob")
#xgbPred <- predict(xgb, newdata = qualsData, type = "raw")
rfPred <- predict(rf, newdata = qualsData, type = "raw")
#marsPred <- predict(mars, newdata = qualsData, type = "raw")

qualsData2 <- qualsData %>% 
  mutate(NN_Pred = nnPred,
         RF_Pred = rfPred,
         Win_Prob = xgbProb[,2]) %>% 
  select(-c(System_Name))

head(qualsData2[,14:26])

#View(qualsData2)

colnames(qualsData2)


set.seed(200)

ukTrainRows <- createDataPartition(qualsData2$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData2[ukTrainRows,]

ukTestSet <- qualsData2[-ukTrainRows,]

colnames(ukTrainSet)

# ukTrainSet2 <- ukTrainSet %>% 
#   select(-c(XGB_Pred, System_Name))
# 
# ukTestSet2 <- ukTestSet %>% 
#   select(-c(XGB_Pred, System_Name))
  


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              verboseIter = T)
#classProbs = TRUE, 
#summaryFunction = RMSE)


tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05, 0.075, 0.1),
                         nrounds = c(100, 150, 200, 250),
                         lambda = c(0.01, 0.025, 0.05, 0.075, 0.1),
                         alpha = c(1.0))


set.seed(200)

xgbPredModUK <- train(BFSP_PL ~ .,
                     data = ukTrainSet,
                     method = "xgbLinear",
                     preProc = c("center", "scale"),
                     metric = "RMSE",
                     tuneGrid = tune.grid,
                     trControl = train.control)

print(xgbPredModUK)

varImp(xgbPredModUK)





predMod <- predict(xgbPredModUK, newdata = ukTestSet, type = "raw")

head(predMod)


summary(predMod)


R2(predMod, ukTestSet$BFSP_PL)
RMSE(predMod, ukTestSet$BFSP_PL)
cor(predMod, ukTestSet$BFSP_PL)

ukTestSet$ModelPL <- predMod


ukPos <- filter(ukTestSet, ModelPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, ModelPL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(ModelPL > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(ModelPL <= 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


#saveRDS(xgbPredModUK, "Final_BFPL_Model_V30.RDS")

# Test XGBoost Final Preds against all qualsData2

#xgbPredModUK <- readRDS("Final_BFPL_Model_V10.RDS")

predModAll <- predict(xgbPredModUK, newdata = qualsData2, type = "raw")

head(predModAll)

qualsData2$ModelPL <- predModAll

ukPos <- filter(qualsData2, ModelPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(qualsData2, ModelPL <= 0)
mean(ukNeg$BFSP_PL)

mean(qualsData2$BFSP_PL)

qualsData2 %>% 
  group_by(Handicap) %>% 
  filter(ModelPL > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

qualsData2 %>% 
  group_by(Handicap) %>% 
  filter(ModelPL <= 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

###################################################################

# Build Final Model using SVM

library(doMC)

registerDoMC(8)

nn <- readRDS("Systems_NN_BFSPPL_Model_v20.RDS")
xgb <- readRDS("XGB_Linear_Systems_BFPL_Model.RDS")
rf <- readRDS("RF_BFPL_Model_v20.RDS")
xgbProb <- readRDS("XGB_Systems_Model_Prob")
#mars <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

nnPred <- predict(nn, newdata = qualsData, type = "raw")
xgbProb <- predict(xgbProb, newdata = qualsData, type = "prob")
xgbPred <- predict(xgb, newdata = qualsData, type = "raw")
rfPred <- predict(rf, newdata = qualsData, type = "raw")
#marsPred <- predict(mars, newdata = qualsData, type = "raw")

qualsData2 <- qualsData %>% 
  mutate(NN_Pred = nnPred,
         RF_Pred = rfPred,
         XGB_Pred = xgbPred,
         Win_Prob = xgbProb[,2]) %>% 
  select(-c(System_Name))

head(qualsData2[,14:27])

#View(qualsData2)

colnames(qualsData2)


set.seed(200)

ukTrainRows <- createDataPartition(qualsData2$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData2[ukTrainRows, ]

ukTestSet <- qualsData2[-ukTrainRows, ]

colnames(ukTrainSet)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(200)

svmFinalMod <- train(BFSP_PL ~ ((XGB_Pred + RF_Pred + NN_Pred + Win_Prob)^2),
                     data = ukTrainSet,
                     method = "svmRadial",
                     preProc = c("center", "scale"),
                     metric = "RMSE",
                     tuneLength = 10,
                     trControl = train.control)

print(svmFinalMod)

svmFinalMod

predSVM <- predict(svmFinalMod, newdata = ukTestSet, type = "raw")

head(predSVM)

ukTestSet$SVM_Pred <- predSVM

summary(ukTestSet$SVM_Pred)

ukPos <- filter(ukTestSet, SVM_Pred > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, SVM_Pred <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(SVM_Pred >= 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(SVM_Pred <= 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

#saveRDS(svmFinalMod, "SVM_Final_Model_v10.RDS")





################################################################

# Build Final Model using MARS

library(doMC)

registerDoMC(8)

nn <- readRDS("Systems_NN_BFSPPL_Model_v20.RDS")
xgb <- readRDS("XGB_Linear_Systems_BFPL_Model.RDS")
rf <- readRDS("RF_BFPL_Model_v20.RDS")
xgbProb <- readRDS("XGB_Systems_Model_Prob")
#mars <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

nnPred <- predict(nn, newdata = qualsData, type = "raw")
xgbProb <- predict(xgbProb, newdata = qualsData, type = "prob")
xgbPred <- predict(xgb, newdata = qualsData, type = "raw")
rfPred <- predict(rf, newdata = qualsData, type = "raw")
#marsPred <- predict(mars, newdata = qualsData, type = "raw")

qualsData2 <- qualsData %>% 
  mutate(NN_Pred = nnPred,
         RF_Pred = rfPred,
         XGB_Pred = xgbPred,
         Win_Prob = xgbProb[,2]) %>% 
  select(-c(System_Name))

head(qualsData2[,14:27])

#View(qualsData2)

colnames(qualsData2)


set.seed(200)

ukTrainRows <- createDataPartition(qualsData2$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData2[ukTrainRows, ]

ukTestSet <- qualsData2[-ukTrainRows, ]


marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:50)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

set.seed(200)                              

marsFinalMod <- train(BFSP_PL ~ ((XGB_Pred + RF_Pred + NN_Pred + Win_Prob)^2), 
                 data = ukTrainSet,
                 method = "earth",
                 preProc = c("center", "scale"),
                 tuneGrid = marsGrid,
                 metric = "RMSE",
                 trControl = train.control)

marsFinalMod

print(marsFinalMod)

varImp(marsFinalMod)

predMars <- predict(marsFinalMod, newdata = ukTestSet, type = "raw")

ukTestSet$MARS_Pred <- predMars[,1]

summary(ukTestSet$MARS_Pred)

ukPos <- filter(ukTestSet, MARS_Pred > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, MARS_Pred <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(MARS_Pred >= 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(MARS_Pred <= 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


#saveRDS(marsFinalMod, "MARS_Final_Model_v10.RDS")

#marsFinalMod <- readRDS("MARS_Final_Model_v10.RDS")

########################################################################


# Build Final Model using PLS simpls kernel

library(doMC)

registerDoMC(4)

nn <- readRDS("Systems_NN_BFSPPL_Model_v20.RDS")
xgb <- readRDS("XGB_Linear_Systems_BFPL_Model.RDS")
rf <- readRDS("RF_BFPL_Model_v20.RDS")
xgbProb <- readRDS("XGB_Systems_Model_Prob")
#mars <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

nnPred <- predict(nn, newdata = qualsData, type = "raw")
xgbProb <- predict(xgbProb, newdata = qualsData, type = "prob")
xgbPred <- predict(xgb, newdata = qualsData, type = "raw")
rfPred <- predict(rf, newdata = qualsData, type = "raw")
#marsPred <- predict(mars, newdata = qualsData, type = "raw")

qualsData2 <- qualsData %>% 
  mutate(NN_Pred = nnPred,
         RF_Pred = rfPred,
         XGB_Pred = xgbPred,
         Win_Prob = xgbProb[,2]) %>% 
  select(-c(System_Name))

head(qualsData2[,14:27])

#View(qualsData2)

colnames(qualsData2)


set.seed(200)

ukTrainRows <- createDataPartition(qualsData2$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData2[ukTrainRows, ]

ukTestSet <- qualsData2[-ukTrainRows, ]


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

set.seed(100)                              

plsFinalMod <- train(BFSP_PL ~ ((XGB_Pred + RF_Pred + NN_Pred + Win_Prob)^2), 
                      data = ukTrainSet,
                      method = "simpls",
                      preProc = c("center", "scale"),
                      tuneLength = 20,
                      metric = "RMSE",
                      trControl = train.control)

plsFinalMod

print(plsFinalMod)

#varImp(plsFinalMod)

predPLS <- predict(plsFinalMod, newdata = ukTestSet, type = "raw")

ukTestSet$PLS_Pred <- predPLS

summary(ukTestSet$PLS_Pred)

ukPos <- filter(ukTestSet, PLS_Pred > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, PLS_Pred <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(PLS_Pred >= 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(PLS_Pred <= 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

saveRDS(plsFinalMod, "PLS_Final_Model.RDS")


plsFinalMod <- readRDS("PLS_Final_Model.RDS")


###############################################################################
# 
# # XGB Tree Algorithm to predict Win Probability
# 
# 
# quals <- quals %>% 
#   mutate(Result = if_else(Actual == 1, 
#                           "WON",
#                           "LOST"))
# 
# qualsData <- quals %>% 
#   select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
#          Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
#          Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, Actual)
# 
# qualsData$Result <- as.factor(qualsData$Result)
# 
# 
# set.seed(100)
# 
# ukTrainRows <- createDataPartition(qualsData$Result, p = 0.6, list = FALSE)
# 
# ukTrainSet <- qualsData[ukTrainRows,]
# 
# ukTestSet <- qualsData[-ukTrainRows,]
# 
# 
# library(xgboost)
# 
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               verboseIter = T,
#                               classProbs = TRUE, 
#                               summaryFunction = mnLogLoss)
# 
# 
# tune.grid <- expand.grid(eta = c(0.01, 0.05, 0.10),
#                          nrounds = c(150, 200, 250),
#                          max_depth = 6:8,
#                          min_child_weight = c(2.0, 2.5, 3.0),
#                          colsample_bytree = c(0.50, 0.75),
#                          gamma = 1,
#                          subsample = c(0.5,1.0))
# View(tune.grid)
# 
# xgbTreeModUK <- train(Result ~ .,
#                       data = ukTrainSet,
#                       method = "xgbTree",
#                       metric = "logLoss",
#                       tuneGrid = tune.grid,
#                       trControl = train.control)
# 
# print(xgbTreeModUK)
# 
# varImp(xgbTreeModUK)
# 
# xgbTreeModUK <- readRDS("XGB_Systems_Model_Prob")
# 
# predOutcomeXGB <- predict(xgbTreeModUK, newdata = ukTestSet, type = "raw")
# 
# head(predOutcomeXGB)
# 
# table(ukTestSet$Result, predOutcomeXGB)
# 
# 
# confusionMatrix(ukTestSet$Result, predOutcomeXGB)


####################################################################
# 
# # SVM Model using e1071
# 
# library(doMC)
# # 
# registerDoMC(4)
# 
# 
# 
# set.seed(100)
# 
# ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)
# 
# ukTrainSet <- qualsData[ukTrainRows, ]
# #ukTrainSetY <- qualsData[ukTrainRows, ]
# 
# ukTestSet <- qualsData[-ukTrainRows, ]
# # ukTestSet <- qualsData[-ukTrainRows, 24]
# 
# # Tune Parameters
# 
# 
# svmTune <- tune.svm(BFSP_PL ~.,
#                     data = ukTrainSet,
#                     gamma = 2^(-1:1), cost = 2^(-2:6),
#                     tunecontrol = tune.control(nrepeat = 3,
#                                                repeat.aggregate = mean,
#                                                sampling = "cross",
#                                                best.model = T))
#                                                
# set.seed(100)
# 
# svmModel <- svm(BFSP_PL ~.,
#                 data = ukTrainSet,
#                 scale = 1,
#                 kernel = "radial",
#                 type = "eps-regression",
#                 cost = 4,
#                 gamma = 0.1,
#                 epsilon = 0.1,
#                 cross = 10,
#                 verbose = T)
# 
# 
# 
# 
# colnames(ukTrainSet)
# 
