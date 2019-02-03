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
library(mgcv)



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

quals <- quals %>%
  drop_na(BF_Placed_SP_PL)

numQuals <- quals %>%
  select_if(is.numeric) %>%
  select(-c(Year:Month))

colnames(numQuals)

highCorrs <- findCorrelation(cor(numQuals), cutoff = 0.8)

(highCorNames <- names(numQuals[,highCorrs]))

highCorNames2 <- highCorNames[1:11]

highCorNames2

quals <- quals %>%
  select(-highCorNames2)

colnames(quals)

# numrescor <- apply(numQuals, MARGIN = 2, FUN = function(x, y) cor(x, y),
#                    y = numQuals$NumberOfResults)
#
# numrescor > abs(0.75)


qualsData <- quals %>%
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, BF_Place_ROI, RaceCode, Handicap, Going_Range,
         Ratings_Range, Rev_Weight_Rank, NumberOfResults, Age, Spd_Rank, ClassDiffTotal, FCPAdvantage, RAdj.Advantage,
         Class_Rank, DaysSinceLastRun, ClassWeightDiffRuns1Year, ClsAdvantage, FrmAdvantage, HCPAdvantage,
         DifferentialRankingClassWeight5Years, BF_Placed_SP_PL, BFSP_PL)


colSums(is.na(qualsData))

set.seed(123)

splitA <- createDataPartition(qualsData$BF_Placed_SP_PL, p = 0.6, list = F)

trainingQualsData <- qualsData[splitA,]

testingQualsData <- qualsData[-splitA,]

colnames(trainingQualsData)

set.seed(123)

splitB <- createDataPartition(trainingQualsData$BF_Placed_SP_PL, p = 0.5, list = F)

ensembleData <- trainingQualsData[splitB, -32]
blenderData <- trainingQualsData[-splitB, -32]
testingData <- qualsData[-splitA, -32]

targetName <- 'BF_Placed_SP_PL'
predVars <- names(qualsData)[names(qualsData) != targetName]

# Benchmark all BF_Placed_SP_PL

mean(testingData$BF_Placed_SP_PL) # -0.0285

#######################################################################

# Train Control for all models


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

###########################################################################
# XGB Tree Algorithm to predict BF_Placed_SP_PL

# library(xgboost)

set.seed(123)


tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.025, 0.05),
                         alpha = c(1.0))


#View(tune.grid)

set.seed(123)

xgbMod <- train(BF_Placed_SP_PL ~ .,
                data = ensembleData,
                method = "xgbLinear",
                preProc = c("center", "scale"),
                metric = "RMSE",
                tuneGrid = tune.grid,
                trControl = train.control)

print(xgbMod)

varImp(xgbMod)




saveRDS(xgbMod, "XGB_Linear_Systems_BFPL_Model_m3.RDS")

xgbMod <- readRDS("XGB_Linear_Systems_BFPL_Model_m3.RDS")



########################################################################################
# library(doParallel)
# cores <- detectCores()
# cl <- makePSOCKcluster(cores, outfile = "") # number of cores to use
# registerDoParallel(cl)

# Build NN Model


library(doMC)
registerDoMC(4)

numUKTrain <- ensembleData %>%
  select_if(is.numeric)

highCorrs <- findCorrelation(cor(numUKTrain), cutoff = 0.75)

colnames(numUKTrain[,highCorrs])

nnTrainSet <- numUKTrain[,-highCorrs]

colnames(nnTrainSet)


nnetGrid <- expand.grid(.decay = c(0.01, 0.025),
                        .size = c(1:5),
                        .bag = F)


set.seed(123)

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


saveRDS(nnMod, "Systems_NN_BFSPPL_Model_m3.RDS")

nnMod <- readRDS("Systems_NN_BFSPPL_Model_m3.RDS")

################################################################

# Build RF Model

# library(doMC)
#
# registerDoMC(4)

mtry <- c(5, 11)

mtryGrid <- expand.grid(mtry = mtry)

print(mtryGrid)

set.seed(123)

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

saveRDS(rfMod, "RF_BFPL_Model_m3.RDS")

rfMod <- readRDS("RF_BFPL_Model_m3.RDS")


##############################################################

set.seed(123)

svmMod <- train(BF_Placed_SP_PL ~ .,
                  data = ensembleData,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  metric = "RMSE",
                  tuneLength = 6,
                  trControl = train.control)

print(svmMod)

#varImp(svmMod)

saveRDS(svmMod, "SVM_BFPL_Model_m3.RDS")

svmMod <- readRDS("SVM_BFPL_Model_m3.RDS")



#############################################################
# Build PLS Model

set.seed(123)

plsMod <- train(BF_Placed_SP_PL ~ .,
                data = ensembleData,
                method = "simpls",
                preProc = c("center", "scale"),
                tuneLength = 20,
                metric = "RMSE",
                trControl = train.control)

plsMod



saveRDS(plsMod, "PLS_BFPL_Model_m3.RDS")

plsMod <- readRDS("PLS_BFPL_Model_m3.RDS")


#############################################################

# Cubist Tree Algorithm to predict BF_Placed_SP_PL

#colnames(qualsData)

cTrain <- ensembleData %>%
  select_if(is.numeric)

cTrainVars <- names(cTrain)[names(cTrain) != targetName]

cubistGrid <- expand.grid(committees = seq(1, 51, 10),
                          neighbors = seq(0, 3, 1))


#View(cubistGrid)


set.seed(123)

cubistMod <- train(x = cTrain[,cTrainVars], y = cTrain$BF_Placed_SP_PL,
                   method = "cubist",
                   tuneGrid = cubistGrid,
                   preProc = c("center", "scale"),
                   trControl = train.control)


cubistMod

print(cubistMod)

saveRDS(cubistMod, "Cubist_BFPL_Model_m3.RDS")

cubistMod <- readRDS("Cubist_BFPL_Model_m3.RDS")

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
  #group_by(Handicap) %>%
  filter(predSVM > 0.0) %>%
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

# Final Linear Ensemble Model

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(123)

finalLinMod <- train(BF_Placed_SP_PL ~ predNN + predRF + predPLS + predSVM + predCUB + predXGB,
                     method = "lm",
                     data = blenderData,
                     preProc = c("scale", "center"),
                     trControl = train.control,
                     metric = "RMSE")

print(finalLinMod)

summary(finalLinMod)

testingData$FinalPredLM <- predict(finalLinMod, newdata = testingData, type = "raw")

saveRDS(finalLinMod, "Final_Linear_Mod_m3.RDS")

finalLinMod <- readRDS("Final_Linear_Mod_m3.RDS")

###############################################################

# Final Interactive Linear Ensemble Model

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(123)

finalLinModInt <- train(BF_Placed_SP_PL ~ (predNN + predPLS + predRF + predCUB + predSVM + predXGB)^3,
                     method = "lm",
                     data = blenderData,
                     preProc = c("scale", "center"),
                     trControl = train.control,
                     metric = "RMSE")

print(finalLinModInt)

summary(finalLinModInt)

testingData$FinalPredLMI <- predict(finalLinModInt, newdata = testingData, type = "raw")

saveRDS(finalLinModInt, "Final_Linear_Mod_Int_m3.RDS")

finalLinModInt <- readRDS("Final_Linear_Mod_Int_m3.RDS")

testingData %>%
  #group_by(Handicap) %>%
  filter(FinalPredLM > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))

#########################################

# Final Interactive SVM Radial Model

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(123)

finalSVMModInt <- train(BF_Placed_SP_PL ~ (predNN + predRF + predPLS + predXGB + predCUB + predSVM)^3,
                        method = "svmRadial",
                        data = blenderData,
                        preProc = c("scale", "center"),
                        tuneLength = 6,
                        trControl = train.control,
                        metric = "RMSE")

print(finalSVMModInt)

summary(finalSVMModInt)

testingData$FinalPredSVMI <- predict(finalSVMModInt, newdata = testingData, type = "raw")

saveRDS(finalSVMModInt, "Final_SVM_Mod_Int_m3.RDS")

finalSVMModInt <- readRDS("Final_SVM_Mod_Int_m3.RDS")

testingData %>%
  #group_by(Handicap) %>%
  filter(FinalPredSVMI > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))

#############################################################

# Final GAM Ensemble Model

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

finalGamMod <- train(BF_Placed_SP_PL ~ predNN + predRF + predPLS + predXGB + predCUB + predSVM,
                        method = "gam",
                        data = blenderData,
                        preProc = c("scale", "center"),
                        trControl = train.control,
                        metric = "RMSE")

print(finalGamMod)

summary(finalGamMod)

testingData$FinalPredGAM <- predict(finalGamMod, newdata = testingData, type = "raw")

saveRDS(finalGamMod, "Final_GAM_Mod_m3.RDS")

finalGamMod <- readRDS("Final_GAM_Mod_m3.RDS")

colnames(testingData)

testingData %>%
  #group_by(Handicap) %>%
  filter(FinalPredGAM > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))






################################################################


# Final Ensemble XGB Model

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)


tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 125),
                         lambda = c(0.05, 0.075, 0.1),
                         alpha = c(1.0))


#View(tune.grid)

set.seed(123)

finalXGBModInt <- train(BF_Placed_SP_PL ~ (predNN + predRF + predPLS + predXGB + predCUB + predSVM)^3,
                data = blenderData,
                method = "xgbLinear",
                preProc = c("center", "scale"),
                metric = "RMSE",
                tuneGrid = tune.grid,
                trControl = train.control)

print(finalXGBModInt)

varImp(finalXGBModInt)

saveRDS(finalXGBModInt, "Final_XGB_Mod_Int_m3.RDS")

finalXGBModInt <- readRDS("Final_XGB_Mod_Int_m3.RDS")

testingData$FinalPredsXGBI <- predict(finalXGBModInt, newdata = testingData, type = "raw")

#RMSE(testingData$FinalPredsXGB, testingData$BF_Placed_SP_PL)

testingData <- testingData %>%
  mutate(FinalXGBPreds_Band = cut(FinalPredsXGBI, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                         labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                         ordered_result = T),
         RF_XGB_Avg = (predRF + predXGB)/2,
         RF_XGB_Avg_Band = cut(RF_XGB_Avg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                               labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                               ordered_result = T))

testingData %>%
  group_by(FinalXGBPreds_Band) %>%
  #filter(FinalPredsXGBI > 0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))

View(tail(testingData))

head(testingData)

write_csv(testingData, paste0("ukhr_testing_data_final_", targetName, ".csv"))

#####################################################################################
set.seed(123)

splitA2 <- createDataPartition(quals$BF_Placed_SP_PL, p = 0.6, list = F)

trainingQualsData2 <- quals[splitA2,]

testingQualsData2 <- quals[-splitA2,]

set.seed(123)

splitB2 <- createDataPartition(trainingQualsData2$BF_Placed_SP_PL, p = 0.5, list = F)

ensembleData2 <- trainingQualsData2[splitB2,]
blenderData2 <- trainingQualsData2[-splitB2,]
testingData2 <- testingQualsData2 #qualsData[-splitA,]





blenderData2$predNN <- predict(nnMod, newdata = blenderData2, type = "raw")
blenderData2$predRF <- predict(rfMod, newdata = blenderData2, type = "raw")
blenderData2$predPLS <- predict(plsMod, newdata = blenderData2, type = "raw")
blenderData2$predCUB <- predict(cubistMod, newdata = blenderData2, type = "raw")
blenderData2$predXGB <- predict(xgbMod, newdata = blenderData2, type = "raw")
blenderData2$predSVM <- predict(svmMod, newdata = blenderData2, type = "raw")

testingData2$predNN <- predict(nnMod, newdata = testingData2, type = "raw")
testingData2$predRF <- predict(rfMod, newdata = testingData2, type = "raw")
testingData2$predPLS <- predict(plsMod, newdata = testingData2, type = "raw")
testingData2$predCUB <- predict(cubistMod, newdata = testingData2, type = "raw")
testingData2$predXGB <- predict(xgbMod, newdata = testingData2, type = "raw")
testingData2$predSVM <- predict(svmMod, newdata = testingData2, type = "raw")

testingData2$FinalLinearModel <- predict(finalLinMod, newdata = testingData2, type = "raw")
testingData2$FinalLinearModelInt <- predict(finalLinModInt, newdata = testingData2, type = "raw")
testingData2$FinalSVMModelInt <- predict(finalSVMModInt, newdata = testingData2, type = "raw")
testingData2$FinalGamMod <- predict(finalGamMod, newdata = testingData2, type = "raw")
testingData2$FinalXGBModInt <- predict(finalXGBModInt, newdata = testingData2, type = "raw")



testingData2 <- testingData2 %>%
  mutate(Final_Linear_Int_Band = cut(FinalLinearModelInt, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                  labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                             ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                  ordered_result = T),
         Final_Linear_Band = cut(FinalLinearModel, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                     labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                                ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                     ordered_result = T),
         Final_SVM_Int_Band = cut(FinalSVMModelInt, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                     labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                                ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                  ordered_result = T),
         Final_XGB_Int_Band = cut(FinalXGBModInt, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                  labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                             ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                  ordered_result = T),
         Final_GAM_Band = cut(FinalGamMod, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                  labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                             ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                  ordered_result = T),
         RF_XGB_Avg = (predRF + predXGB)/2,
         RF_XGB_Avg_Band = cut(RF_XGB_Avg, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                               labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                          ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                               ordered_result = T),
         XGB_Pred_Band = cut(predXGB, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                             labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                        ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                             ordered_result = T),
         RF_Pred_Band = cut(predRF, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                            labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                       ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                            ordered_result = T),
         NN_Pred_Band = cut(predNN, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                            labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                       ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                            ordered_result = T),
         CUB_Pred_Band = cut(predCUB, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                            labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                       ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                            ordered_result = T),
         SVM_Pred_Band = cut(predSVM, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                            labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                       ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                            ordered_result = T),
         PLS_Pred_Band = cut(predPLS, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                             labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                        ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                             ordered_result = T))

write_csv(testingData2, "Final_Models_Testing_Set_Predictions.csv")

colnames(testingData2)

View(tail(testingData2))

testingData2 <- read_csv("Final_Models_Testing_Set_Predictions.csv")


testingData2 %>%
  group_by(Final_GAM_Band) %>%
  filter(FinalGamMod > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0),
         Placed = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won, na.rm = T),
            Places = sum(Placed, na.rm = T),
            WinPercent = mean(Won, na.rm = T),
            Place_Percent = mean(Placed, na.rm = T),
            Avg_W_PL = mean(BFSP_PL, na.rm = T),
            Total_W_PL = sum(BFSP_PL, na.rm = T),
            Avg_P_PL = mean(BF_Placed_SP_PL, na.rm = T),
            Total_P_PL = sum(BF_Placed_SP_PL, na.rm = T)) %>%
  arrange(desc(Avg_W_PL))


########################################################################################################################
########################################################################################################################
########################################################################################################################



colnames(blenderData2)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)


tune.grid <- expand.grid(eta = c(0.01, 0.025),
                         nrounds = c(100, 125),
                         lambda = c(0.05, 0.075, 0.1),
                         alpha = c(1.0))



set.seed(123)

xgbFinalWinMod <- train(BFSP_PL ~ .,
                  data = blenderData2,
                  method = "xgbLinear",
                  preProc = c("center", "scale"),
                  metric = "RMSE",
                  tuneGrid = tune.grid,
                  trControl = train.control)

print(xgbFinalWinMod)

varImp(xgbFinalWinMod)

saveRDS(xgbFinalWinMod, "Final_XGB_Win_Model.RDS")


testingData2$FinalWinPreds <- predict(xgbFinalWinMod, newdata = testingData2, type = "raw")
testingData2$FinalPlacePreds <- predict(xgbFinal, newdata = testingData2, type = "raw")

colnames(testingData2)



testingData2 <- testingData2 %>%
  mutate(FinalWinPreds_Band = cut(FinalWinPreds, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                               labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                               ordered_result = T),
         FinalPlacePreds_Band = cut(FinalPlacePreds, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                               labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                               ordered_result = T),
         RF_XGB_Avg = (predRF + predXGB)/2,
         RF_XGB_Avg_Band = cut(RF_XGB_Avg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                               labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                               ordered_result = T),
         XGB_Pred_Band = cut(predXGB, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                               labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                               ordered_result = T),
         RF_Pred_Band = cut(predRF, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                             labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                             ordered_result = T),
         NN_Pred_Band = cut(predNN, breaks = c(-10000, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 10000),
                            labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.15", ">0.15 to 0.20", ">0.20 to 0.25", ">0.25"),
                            ordered_result = T),
         PLS_Pred_Band = cut(predPLS, breaks = c(-10000, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 10000),
                            labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.15", ">0.15 to 0.20", ">0.20 to 0.25", ">0.25"),
                            ordered_result = T))

testingData2$FinalPredsAvg <- (testingData2$FinalWinPreds + testingData2$FinalPlacePreds)/2

testingData2 <- testingData2 %>%
  mutate(FinalPredsAvg_Band = cut(FinalPredsAvg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                                  labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                                  ordered_result = T))

write_csv(testingData2, "BF_Ensemble_WinPlace_Predictions.csv")

testingData2 %>%
  group_by() %>%
  filter(predRF >= 0.25, predXGB >= 0.25, FinalPlacePreds >= 0.25) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))





mean(testingData$BF_Placed_SP_PL)


tail(testingQualsData$BetFairSPForecastWinPrice, 50)
tail(testingData$BetFairSPForecastWinPrice, 50)


# Save Final XGB Ensemble Model

saveRDS(xgbFinal, "XGB_Final_Ensemble_m3.RDS")

colnames(blenderData)

#xgbFinal <- readRDS("XGB_Final_Ensemble_m3.RDS")

################################



testingQualsData$FinalPredsXGBPlace <- predict(xgbFinalPlace, newdata = testingData, type = "raw")

summary(testingQualsData$FinalPredsXGB)

summary(testingQualsData$FinalPredsXGBPlace)

testingQualsData <- testingQualsData %>%
  mutate(FinalPredsPlace_Band = cut(FinalPredsXGBPlace, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                               labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                               ordered_result = T))

testingQualsData %>%
  group_by() %>%
  filter(FinalPredsXGB > 0, FinalPredsXGBPlace > 0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))

testingQualsData <- testingQualsData %>%
  mutate(FinalMods_WinPlaceAvg = (FinalPredsXGB + FinalPredsXGBPlace)/2,
         FinalModsAvg_Band = cut(FinalMods_WinPlaceAvg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
                                    labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
                                    ordered_result = T))


testingQualsData %>%
  group_by(FinalModsAvg_Band) %>%
  filter(FinalPredsXGB > 0, FinalPredsXGBPlace > 0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))

colnames(testingQualsData)

testingQualsData <- testingQualsData %>%
  mutate(VOBF_Band = cut(ValueOdds_BetfairFormat, breaks = c(0, 2.00, 6.0, 11.0, 21, 51, 5000),
                                 labels = c("<=2.0", ">2.0 to 6.0",">6.0 to 11.0" ,">11.0 to 21.0",
                                            ">21 to 51", ">51.0"),
                                 ordered_result = T))


testingQualsData %>%
  group_by() %>%
  filter(FinalPredsXGB >= 1, FinalPredsXGBPlace >= 1) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))


#############################
# library(doParallel)

# cores <- detectCores()
# cl <- makePSOCKcluster(cores) # number of cores to use
# registerDoParallel(cl)

#########################################################################

# SVM Final Model
#
# library(doParallel)
# cores <- detectCores()
# cl <- makePSOCKcluster(cores) # number of cores to use
# registerDoParallel(cl)


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(123)

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

# library(doParallel)
# cores <- detectCores()
# cl <- makePSOCKcluster(cores) # number of cores to use
# registerDoParallel(cl)

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

set.seed(123)

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
  filter(FinalPredsCUBIST > 0.0) %>%
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

set.seed(123)

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
  group_by() %>%
  filter(predXGB > 0.25) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))



mean(testingData$BF_Placed_SP_PL)

######################################################

colnames(testingData)

########################################################

sysRes <- quals %>%
  group_by(System_Name) %>%
  #filter(predNN > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

sysRes

View(sysRes)

ukhrSys <- which(str_detect(quals$System_Name,"UKHR"))

ukhrSysQuals <- quals[ukhrSys,]

nonUKHR <- quals[-ukhrSys,]

ukhrPL <- ukhrSysQuals %>%
  group_by() %>%
  #filter(predNN > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

ukhrPL

nonUKHR_PL <- nonUKHR %>%
  group_by() %>%
  #filter(predNN > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

nonUKHR_PL

trSys <- which(str_detect(quals$System_Name,"Trainer"))

trSysQuals <- quals[trSys,]

nonTrQuals <- quals[-trSys,]

trPL <- trSysQuals %>%
  group_by() %>%
  #filter(predNN > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

trPL

nonTrQuals_PL <- nonTrQuals %>%
  group_by() %>%
  #filter(predNN > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

nonTrQuals_PL


