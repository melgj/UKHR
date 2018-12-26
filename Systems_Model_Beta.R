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
library(Cubist)



quals <- read_csv("All_System_Qualifiers_Yr_2018_v2.csv", col_names = T)

flat <- c("AW", "FLAT")

quals$RaceCode <- if_else(quals$RaceType %in% flat, "FLAT", "NH")

table(quals$RaceCode, quals$RaceType)

colnames(quals)

colSums(is.na(quals))

summary(quals)



quals$Handicap <- as.factor(quals$Handicap)
quals$Ratings_Range <- as.factor(quals$Ratings_Range)
quals$Going_Range <- as.factor(quals$Going_Range)
quals$RaceType <- as.factor(quals$RaceType)
quals$RaceCode <- as.factor(quals$RaceCode)
#quals$Dist_Range <- as.factor(quals$Dist_Range)
quals$System_Name <- as.factor(quals$System_Name)


qualsData <- quals %>%
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceCode, Handicap, Going_Range,
         Ratings_Range, Rev_Weight_Rank, NumberOfResults, Age, Spd_Rank, ClassDiffTotal, FCPAdvantage, RAdj.Advantage,
         Class_Rank, DaysSinceLastRun, ClassWeightDiffRuns1Year, ClsAdvantage, FrmAdvantage, HCPAdvantage,
         DifferentialRankingClassWeight5Years, BF_Placed_SP_PL)


colSums(is.na(qualsData))

qualsData <- qualsData %>%
  drop_na(BF_Placed_SP_PL)

# set.seed(100)
# #
# dataSplit <- createDataPartition(qualsData$BF_Placed_SP_PL, p = 0.60, list = FALSE)
#
# dataSplit

#qualsData <- qualsData[sample(nrow(qualsData)),]

splitA <- createDataPartition(qualsData$BF_Placed_SP_PL, p = 0.6, list = F)

trainingQualsData <- qualsData[splitA,]

splitB <- createDataPartition(trainingQualsData$BF_Placed_SP_PL, p = 0.5, list = F)

ensembleData <- trainingQualsData[splitB,]
blenderData <- trainingQualsData[-splitB,]
testingData <- qualsData[-splitA,]

targetName <- 'BF_Placed_SP_PL'
predVars <- names(qualsData)[names(qualsData) != targetName]

# Benchmark all BF_Placed_SP_PL

mean(testingData$BF_Placed_SP_PL) # -0.03

#######################################################################

# Train Control for all models


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

###########################################################################
# XGB Tree Algorithm to predict BF_Placed_SP_PL

# library(xgboost)

set.seed(100)


tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.025, 0.05),
                         alpha = c(1.0))


#View(tune.grid)

set.seed(100)

xgbMod <- train(BF_Placed_SP_PL ~ .,
                data = ensembleData,
                method = "xgbLinear",
                preProc = c("center", "scale"),
                metric = "RMSE",
                tuneGrid = tune.grid,
                trControl = train.control)

print(xgbMod)

varImp(xgbMod)





saveRDS(xgbMod, "XGB_Linear_Systems_BFPL_Model_p1.RDS")

#xgbMod <- readRDS("XGB_Linear_Systems_BFPL_Model_p1.RDS")



########################################################################################
library(doParallel)
cores <- detectCores()
cl <- makePSOCKcluster(cores, outfile = "") # number of cores to use
registerDoParallel(cl)

# Build NN Model


# library(doMC)
#
# registerDoMC(8)

numUKTrain <- ensembleData %>%
  select_if(is.numeric)

highCorrs <- findCorrelation(cor(numUKTrain), cutoff = 0.75)

colnames(numUKTrain[,highCorrs])

nnTrainSet <- numUKTrain[,-highCorrs]

colnames(nnTrainSet)


nnetGrid <- expand.grid(.decay = c(0.01, 0.025),
                        .size = c(1:5),
                        .bag = F)


set.seed(100)

nnMod <- train(BF_Placed_SP_PL ~ .,
              data = nnTrainSet,
              method = "avNNet",
              tuneGrid = nnetGrid,
              trControl = train.control,
              preProc = c("center", "scale"),
              linout = T,
              trace = F,
              MaxNWts = 10 * (ncol(nnTrainSet) + 1) +10 +1,
              maxit = 500)

print(nnMod)


saveRDS(nnMod, "Systems_NN_BFSPPL_Model_p1.RDS")

#nnMod <- readRDS("Systems_NN_BFSPPL_Model_p1.RDS")

################################################################

# Build RF Model

library(doMC)

registerDoMC(4)

mtry <- c(5, 11)

mtryGrid <- expand.grid(mtry = mtry)

print(mtryGrid)

set.seed(100)

rfMod <- train(BF_Placed_SP_PL ~ .,
               data = ensembleData,
               method = "rf",
               ntrees = 1000,
               trControl = train.control,
               tuneGrid = mtryGrid,
               PreProc = c("center", "scale"),
               importance = F)

print(rfMod)

varImp(rfMod)

saveRDS(rfMod, "RF_BFPL_Model_p1.RDS")

#rfMod <- readRDS("RF_BFPL_Model_p1.RDS")


##############################################################

set.seed(100)

svmMod <- train(BF_Placed_SP_PL ~ .,
                  data = blenderData,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  metric = "RMSE",
                  tuneLength = 5,
                  trControl = train.control)

print(svmMod)

#varImp(svmMod)

saveRDS(svmMod, "SVM_BFPL_Model_p1.RDS")

#svmMod <- readRDS("SVM_BFPL_Model_p1.RDS")



#############################################################
# Build PLS Model

set.seed(100)

plsMod <- train(BF_Placed_SP_PL ~ .,
                data = ensembleData,
                method = "simpls",
                preProc = c("center", "scale"),
                tuneLength = 20,
                metric = "RMSE",
                trControl = train.control)

plsMod



saveRDS(plsMod, "PLS_BFPL_Model_p1.RDS")


#############################################################

# Cubist Tree Algorithm to predict BF_Placed_SP_PL

#colnames(qualsData)

cTrain <- ensembleData %>%
  select_if(is.numeric)

cTrainVars <- names(cTrain)[names(cTrain) != targetName]

cubistGrid <- expand.grid(committees = seq(1, 51, 10),
                          neighbors = seq(0, 3, 1))


#View(cubistGrid)


set.seed(100)

cubistMod <- train(x = cTrain[,cTrainVars], y = cTrain$BF_Placed_SP_PL,
                   method = "cubist",
                   tuneGrid = cubistGrid,
                   preProc = c("center", "scale"),
                   trControl = train.control)


cubistMod

print(cubistMod)

saveRDS(cubistMod, "Cubist_BFPL_Model_p1.RDS")

#stopCluster(cl)

##############################################################

blenderData$predNN <- predict(nnMod, newdata = blenderData, type = "raw")
blenderData$predRF <- predict(rfMod, newdata = blenderData, type = "raw")
blenderData$predPLS <- predict(plsMod, newdata = blenderData, type = "raw")
blenderData$predCUB <- predict(cubistMod, newdata = blenderData, type = "raw")
blenderData$predXGB <- predict(xgbMod, newdata = blenderData, type = "raw")
blenderData$predSVM <- predict(svmMod, newdata = blenderData, type = "raw")

testingData$predNN <- predict(nnMod, newdata = testingData, type = "raw")
testingData$predRF <- predict(rfMod, newdata = testingData, type = "raw")
testingData$predPLS <- predict(plsMod, newdata = testingData, type = "raw")
testingData$predCUB <- predict(cubistMod, newdata = testingData, type = "raw")
testingData$predXGB <- predict(xgbMod, newdata = testingData, type = "raw")
testingData$predSVM <- predict(svmMod, newdata = testingData, type = "raw")

write_csv(blenderData, paste0("ukhr_blender_data_", targetName, ".csv"))
write_csv(testingData, paste0("ukhr_testing_data_", targetName, ".csv"))

predDF <- testingData %>%
  select(predNN, predRF, predPLS, predCUB, predXGB, predSVM, BF_Placed_SP_PL)

cor(predDF)

# RMSE(testingData$predNN, testingData$BF_Placed_SP_PL)
# RMSE(testingData$predRF, testingData$BF_Placed_SP_PL)
# RMSE(testingData$predPLS, testingData$BF_Placed_SP_PL)
# RMSE(testingData$predCUB, testingData$BF_Placed_SP_PL)
# RMSE(testingData$predXGB, testingData$BF_Placed_SP_PL)
# RMSE(testingData$predSVM, testingData$BF_Placed_SP_PL)

testingData %>%
  group_by() %>%
  filter(predXGB > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))

##############################################################
targetName <- 'BF_Placed_SP_PL'

blenderData <- read_csv(paste0("ukhr_blender_data_", targetName, ".csv"), col_names = T)
testingData <- read_csv(paste0("ukhr_testing_data_", targetName, ".csv"), col_names = T)

summary(blenderData)

str(blenderData)

predVars <- names(blenderData)[names(blenderData) != targetName]

predVars

# Final Ensemble XGB Model

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)
set.seed(100)

tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.025, 0.05),
                         alpha = c(1.0))


#View(tune.grid)

set.seed(100)

xgbFinal <- train(BF_Placed_SP_PL ~ .,
                data = blenderData,
                method = "xgbLinear",
                preProc = c("center", "scale"),
                metric = "RMSE",
                tuneGrid = tune.grid,
                trControl = train.control)

print(xgbFinal)

varImp(xgbFinal)

testingData$FinalPredsXGB <- predict(xgbFinal, newdata = testingData, type = "raw")

RMSE(testingData$FinalPredsXGB, testingData$BF_Placed_SP_PL)

testingData %>%
  group_by() %>%
  filter(predRF > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))



mean(testingData$BF_Placed_SP_PL)


# Save Final XGB Ensemble Model

saveRDS(xgbFinal, "XGB_Final_Ensemble_p1.RDS")

colnames(blenderData)

#############################
# library(doParallel)

# cores <- detectCores()
# cl <- makePSOCKcluster(cores) # number of cores to use
# registerDoParallel(cl)

#########################################################################

# SVM Final Model

library(doParallel)
cores <- detectCores()
cl <- makePSOCKcluster(cores) # number of cores to use
registerDoParallel(cl)


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(100)

svmFinal <- train(BF_Placed_SP_PL ~ .,
                     data = blenderData,
                     method = "svmRadial",
                     preProc = c("center", "scale"),
                     metric = "RMSE",
                     tuneLength = 10,
                     trControl = train.control)

print(svmFinal)

#varImp(svmFinal)

testingData$FinalPredsSVM <- predict(svmFinal, newdata = testingData, type = "raw")

RMSE(testingData$FinalPredsSVM, testingData$BF_Placed_SP_PL)

testingData %>%
  group_by() %>%
  filter(FinalPredsSVM > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))



mean(testingData$BF_Placed_SP_PL)


##########################################################################

# Final Cubist Model

library(doParallel)
cores <- detectCores()
cl <- makePSOCKcluster(cores) # number of cores to use
registerDoParallel(cl)

targetName <- "BF_Placed_SP_PL"

cTrain <- blenderData %>%
  select_if(is.numeric)

cTrainVars <- names(cTrain)[names(cTrain) != targetName]

cTrainVars

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

cubistGrid <- expand.grid(committees = seq(1, 51, 5),
                          neighbors = seq(0, 9, 3))

set.seed(100)

finalCUB <- train(x = cTrain[,cTrainVars], y = cTrain$BF_Placed_SP_PL,
                   method = "cubist",
                   tuneGrid = cubistGrid,
                   preProc = c("center", "scale"),
                   trControl = train.control)


finalCUB

testingData$FinalPredsCUBIST <- predict(finalCUB, newdata = testingData, type = "raw")

RMSE(testingData$FinalPredsCUBIST, testingData$BF_Placed_SP_PL)

testingData %>%
  group_by() %>%
  filter(FinalPredsCUBIST < 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))



mean(testingData$BF_Placed_SP_PL)


stopCluster(cl)





##########################################################################
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(100)

ridgeMod <- train(BF_Placed_SP_PL ~ .,
                  data = blenderData,
                  method = "ridge",
                  preProc = c("center", "scale"),
                  tuneLength = 20,
                  metric = "RMSE",
                  trControl = train.control)

ridgeMod

testingData$FinalPredsRIDGE <- predict(ridgeMod, newdata = testingData, type = "raw")


RMSE(testingData$FinalPredsRIDGE, testingData$BF_Placed_SP_PL)

testingData %>%
  group_by(Handicap) %>%
  filter(predNN > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))



mean(testingData$BF_Placed_SP_PL)
