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
library(brnn)

ukhrData <- read_csv("UKHR_17_18_Cleaned.csv", col_names = T)

ukhrData$Handicap <- as.factor(ukhrData$Handicap)
ukhrData$Ratings_Range <- as.factor(ukhrData$Ratings_Range)
ukhrData$Going_Range <- as.factor(ukhrData$Going_Range)
ukhrData$RaceType <- as.factor(ukhrData$RaceType)

ukhrData2 <- ukhrData %>%
  select(Handicap, Runners, RatingAdvantage, Age, FCPAdvantage, DaysSinceLastRun, BetFairSPForecastPlacePrice, JockeyAdvantage,
         WinFAdvantage, Sire_Distance_ROI, BFSP_PL)

colSums(is.na(ukhrData2))

ukhrNumQuals <- ukhrData2 %>%
  select_if(is.numeric)

colnames(ukhrNumQuals)

highCorrs <- findCorrelation(cor(ukhrNumQuals), cutoff = 0.8, exact = T, names = T)

highCorrs

ukhrData2 <- ukhrData2 %>%
  select(-highCorrs)

colnames(ukhrData2)

nzv <- nearZeroVar(ukhrData2)

set.seed(101)

splitA <- createDataPartition(ukhrData2$BFSP_PL, p = 0.3, list = F)

trainingUKHRData <- ukhrData2[splitA,]

testingUKHRData <- ukhrData2[-splitA,]

colnames(trainingUKHRData)

set.seed(101)

splitB <- createDataPartition(trainingUKHRData$BFSP_PL, p = 0.5, list = F)

ensembleData <- trainingUKHRData[splitB,]
blenderData <- trainingUKHRData[-splitB,]
testingData <- ukhrData[-splitA,]

mean(ukhrData$BF_Placed_SP_PL) #-0.05084
mean(ukhrData$BFSP_PL) #-0.05996
mean(testingData$BF_Placed_SP_PL) #-0.05275
mean(testingData$BFSP_PL) #-0.06391
# Train Control for all models


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

###########################################################################
# XGB Tree Algorithm to predict BF_Placed_SP_PL

# library(xgboost)

tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.025, 0.05),
                         alpha = c(1.0))


#View(tune.grid)

set.seed(101)

xgbModUKHR <- train(BFSP_PL ~ .,
                data = ensembleData,
                method = "xgbLinear",
                preProc = c("center", "scale"),
                metric = "RMSE",
                tuneGrid = tune.grid,
                trControl = train.control)

print(xgbModUKHR)

varImp(xgbModUKHR)




saveRDS(xgbModUKHR, "XGB_UKHR_Model.RDS")

xgbModUKHR <- readRDS("XGB_UKHR_Model.RDS")

testingData$XGB_Place_Model <- predict(xgbModUKHR, newdata = testingData, type = "raw")
blenderData$XGB_Place_Model <- predict(xgbModUKHR, newdata = blenderData, type = "raw")

testingData <- testingData %>%
  mutate(XGB_Place_Model_Band = cut(XGB_Place_Model, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                 labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                            ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                 ordered_result = T))



testingData %>%
  #group_by(XGB_Place_Model_Band) %>%
  filter(XGB_Place_Model > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))





##########################################################################################

quals <- read_csv("All_System_Qualifiers_Yr_2018_v3.csv", col_names = T)

#flat <- c("AW", "FLAT")

#quals$RaceCode <- if_else(quals$RaceType %in% flat, "FLAT", "NH")

#table(quals$RaceCode, quals$RaceType)

colnames(quals)

colSums(is.na(quals))

summary(quals)



quals$Handicap <- as.factor(quals$Handicap)
quals$Ratings_Range <- as.factor(quals$Ratings_Range)
quals$Going_Range <- as.factor(quals$Going_Range)
quals$RaceType <- as.factor(quals$RaceType)
#quals$RaceCode <- as.factor(quals$RaceCode)
#quals$Dist_Range <- as.factor(quals$Dist_Range)
quals$System_Name <- as.factor(quals$System_Name)

quals <- quals %>%
  drop_na(BF_Placed_SP_PL)

#summary(quals$ValueOdds_Probability)

# quals %>%
#   group_by(Value_Odds_Range) %>%
#   summarise(Runs = n(),
#             VO_Exp = sum(ValueOdds_Probability),
#             VO_Act = sum(Actual),
#             VO_AER = round(VO_Act/VO_Exp, 2))
#
# set.seed(101)
#
# splitA <- createDataPartition(quals$BF_Placed_SP_PL, p = 0.7, list = F)
#
# trainingQualsData <- quals[splitA,]
#
# testingData <- quals[-splitA,]
#
# colnames(trainingQualsData)
#
# set.seed(101)
#
# splitB <- createDataPartition(trainingQualsData$BF_Placed_SP_PL, p = 0.5, list = F)
#
# ensembleData <- trainingQualsData[splitB,]
# blenderData <- trainingQualsData[-splitB,]
# #testingData <- qualsData[-splitA,]


mean(quals$VSP_PL) #-0.119
mean(quals$BF_Placed_SP_PL, na.rm = T) #-0.028


# summary(quals$BFSP_PL)

# head(quals)

# quals %>%
#   group_by(Trainer) %>%
#   summarise(Runs = n(),
#             Wins = sum(Actual),
#             WinPct = round(Wins/Runs,2),
#             ExpWins = round(sum(Expected,2)),
#             AER_Win = round(Wins/ExpWins, 2),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL),
#             Avg_Placed_PL = mean(BF_Placed_SP_PL),
#             Total_Placed_PL = sum(BF_Placed_SP_PL)) %>%
#   filter(ExpWins > 5) %>%
#   arrange(desc(AER_Win)) %>%
#   View()

# numQuals <- quals %>%
#   select_if(is.numeric)
#
# colnames(numQuals)
#
# highCorrs <- findCorrelation(cor(numQuals), cutoff = 0.8, exact = T, names = T)
#
#
# quals <- quals %>%
#   select(-highCorrs)
#
# colnames(quals)

# numrescor <- apply(numQuals, MARGIN = 2, FUN = function(x, y) cor(x, y),
#                    y = numQuals$NumberOfResults)
#
# numrescor > abs(0.75)

# ukVarsDF <- read_csv("UKHR_Training_Data_April_2019.csv", col_names = T)
#
# goodVars <- colnames(ukVarsDF)
#
# goodVars


qualsData <- quals %>%
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Archie, Placed_AE_Ratio, Placed_Archie, Btn_AE_Ratio,
         WinPercent, meanPL, totalPL, VSP_ROI, BF_Place_ROI, Handicap, Going_Range, Runners, Ratings_Range,
         RatingAdvantage, NumberOfResults, Age, FCPAdvantage, RAdj.Advantage, Class_Rank, DaysSinceLastRun,
         Raw.Advantage, Rating_Rank, BetFairSPForecastPlacePrice, SpeedAdvantage,TrainerAdvantage, ConnAdvantage,
         RawRanking, TrainerRanking, RAdjRanking, LastRaceRatingRank, ConnRanking, JockeyAdvantage,WinFAdvantage,
         FrmRanking, HCPRanking, DifferentialRankingClassWeight5YearsWins, FC_Fav_Rank, SpeedAdvantage, BF_Placed_SP_PL)


colSums(is.na(qualsData))

numQuals <- qualsData %>%
  select_if(is.numeric)

colnames(numQuals)

highCorrs <- findCorrelation(cor(numQuals), cutoff = 0.8, exact = T, names = T)

highCorrs


qualsData <- qualsData %>%
  select(-highCorrs)

colnames(qualsData)

set.seed(101)

splitA <- createDataPartition(qualsData$BF_Placed_SP_PL, p = 0.7, list = F)

trainingQualsData <- qualsData[splitA,]

testingQualsData <- quals[-splitA,]

colnames(trainingQualsData)

set.seed(101)

splitB <- createDataPartition(trainingQualsData$BF_Placed_SP_PL, p = 0.5, list = F)

ensembleData <- trainingQualsData[splitB,]
blenderData <- trainingQualsData[-splitB,]
testingData <- qualsData[-splitA,]

colnames(qualsData)

targetName <- 'BF_Placed_SP_PL'
predVars <- colnames(qualsData)[1:37]
predVars

# Benchmark all BF_Placed_SP_PL

mean(testingData$BF_Placed_SP_PL) # -0.025

#######################################################################

# Train Control for all models


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

###########################################################################
# XGB Tree Algorithm to predict BF_Placed_SP_PL

# library(xgboost)

tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.025, 0.05),
                         alpha = c(1.0))


#View(tune.grid)

set.seed(101)

xgbMod <- train(BF_Placed_SP_PL ~ .,
                data = ensembleData,
                method = "xgbLinear",
                preProc = c("center", "scale"),
                metric = "RMSE",
                tuneGrid = tune.grid,
                trControl = train.control)

print(xgbMod)

varImp(xgbMod)




saveRDS(xgbMod, "XGB_1159_Model.RDS")

xgbMod <- readRDS("XGB_1159_Model.RDS")



########################################################################################

# XGB Win Probability Model




#
# train.controlXGB <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               verboseIter = T,
#                               classProbs = TRUE,
#                               summaryFunction = twoClassSummary)
#
#
# tune.grid <- expand.grid(eta = c(0.01, 0.25, 0.05),
#                          nrounds = c(100, 150),
#                          max_depth = 5:7,
#                          min_child_weight = c(2.5, 3.0, 3.50),
#                          colsample_bytree = c(0.75),
#                          gamma = 1,
#                          subsample = 1)
# View(tune.grid)
#
# xgbWinProbModel <- train(Result ~ .,
#                       data = xgbEnsembleData,
#                       method = "xgbTree",
#                       metric = "ROC",
#                       tuneGrid = tune.grid,
#                       trControl = train.controlXGB)
#
# print(xgbWinProbModel)
#
# varImp(xgbWinProbModel)
#
# xgbWinProbModel
#
# saveRDS(xgbWinProbModel, "XGB_Systems_Win_Prob_m7.RDS")





########################################################################################
# library(doParallel)
# cores <- detectCores()
# cl <- makePSOCKcluster(cores, outfile = "") # number of cores to use
# registerDoParallel(cl)

# Build NN Model

#
# library(doMC)
# registerDoMC(4)
#
# numUKTrain <- ensembleData %>%
#   select_if(is.numeric)
#
# highCorrs <- findCorrelation(cor(numUKTrain), cutoff = 0.75)
#
# colnames(numUKTrain[,highCorrs])
#
# nnTrainSet <- numUKTrain[,-highCorrs]
#
# colnames(nnTrainSet)
#
#
# nnetGrid <- expand.grid(.decay = c(0.01, 0.025),
#                         .size = c(1:5),
#                         .bag = F)
#
#
# set.seed(101)
#
# nnMod <- train(BF_Placed_SP_PL ~ .,
#               data = nnTrainSet,
#               method = "avNNet",
#               tuneGrid = nnetGrid,
#               trControl = train.control,
#               preProc = c("center", "scale"),
#               linout = T,
#               trace = F,
#               MaxNWts = 10 * (ncol(nnTrainSet) + 1) +10 +1,
#               maxit = 500)
#
# print(nnMod)
#
#
# saveRDS(nnMod, "Systems_NN_BFSPPL_Model_m7.RDS")
#
# nnMod <- readRDS("Systems_NN_BFSPPL_Model_m7.RDS")

################################################################

# Build RF Model

#library(doMC)

#registerDoMC(4)


library(doMC)
registerDoMC(4)

mtry = c(2,5,10)

mtryGrid <- expand.grid(mtry = mtry)

print(mtryGrid)

set.seed(101)

rfMod <- train(BF_Placed_SP_PL ~ .,
               data = ensembleData,
               method = "rf",
               ntrees = 1000,
               trControl = train.control,
               #tuneGrid = mtryGrid,
               preProc = c("center", "scale"),
               importance = T)

print(rfMod)

varImp(rfMod)

saveRDS(rfMod, "RF_1159_Model.RDS")

#rfMod <- readRDS("RF_1159_Model.RDS")


##############################################################

set.seed(101)

svmMod <- train(BF_Placed_SP_PL ~ .,
                  data = ensembleData,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  metric = "RMSE",
                  tuneLength = 6,
                  trControl = train.control)

print(svmMod)

#varImp(svmMod)

saveRDS(svmMod, "SVM_1159_Model.RDS")

#svmMod <- readRDS("SVM_1159_Model.RDS")



#############################################################
# Build PLS Model

set.seed(101)

plsMod <- train(BF_Placed_SP_PL ~ .,
                data = ensembleData,
                method = "simpls",
                preProc = c("center", "scale"),
                tuneLength = 20,
                metric = "RMSE",
                trControl = train.control)

plsMod



saveRDS(plsMod, "PLS_1159_Model.RDS")

#plsMod <- readRDS("PLS_1159_Model.RDS")


#############################################################

# Cubist Tree Algorithm to predict BF_Placed_SP_PL

#colnames(qualsData)

targetName <- "BF_Placed_SP_PL"

cTrain <- ensembleData %>%
  select_if(is.numeric)

cTrainVars <- names(cTrain)[names(cTrain) != targetName]

cubistGrid <- expand.grid(committees = seq(1, 51, 10),
                          neighbors = seq(0, 3, 1))


#View(cubistGrid)


set.seed(101)

cubistMod <- train(x = cTrain[,cTrainVars], y = cTrain$BF_Placed_SP_PL,
                   method = "cubist",
                   tuneGrid = cubistGrid,
                   preProc = c("center", "scale"),
                   trControl = train.control)


cubistMod

print(cubistMod)

saveRDS(cubistMod, "Cubist_1159_Model.RDS")

#cubistMod <- readRDS("Cubist_1159_Model.RDS")

#############################################################

# Build Bayesian Additive Regression Tree Model

# bm.grid <- expand.grid(num_trees = c(100, 150),
#                          beta = c(1.5, 2),
#                          alpha = c(0.75, 0.95))
#
# set.seed(101)
#
#
# bmMod <- train(BF_Placed_SP_PL ~ .,
#                 data = ensembleData,
#                 method = "bartMachine",
#                 preProc = c("center", "scale"),
#                 metric = "RMSE",
#                 tuneGrid = bm.grid,
#                 trControl = train.control)
#
# print(bmMod)

######################################################

# Build bayesian regularised feed forward nn

set.seed(101)

brnnMod <- train(BF_Placed_SP_PL ~ .,
               data = ensembleData,
               method = "brnn",
               preProc = c("center", "scale"),
               trControl = train.control)


brnnMod

print(brnnMod)

saveRDS(brnnMod, "BRNN_1159_Model.RDS")

#brnnMod <- readRDS("BRNN_1159_Model.RDS")

##################################################

set.seed(101)

gamMod <- train(BF_Placed_SP_PL ~ .,
                     method = "gam",
                     data = ensembleData,
                     preProc = c("scale", "center"),
                     trControl = train.control,
                     metric = "RMSE")

summary(gamMod)

print(gamMod)

varImp(gamMod)

saveRDS(gamMod, "GAM_1159_Model.RDS")

#readRDS("GAM_1159_Model.RDS")

#stopCluster(cl)

##############################################################

xgbMod <- readRDS("XGB_1159_Model.RDS")
#nnMod <- readRDS("Systems_NN_BFSPPL_Model_m7.RDS")
rfMod <- readRDS("RF_1159_Model.RDS")
svmMod <- readRDS("SVM_1159_Model.RDS")
plsMod <- readRDS("PLS_1159_Model.RDS")
cubistMod <- readRDS("Cubist_1159_Model.RDS")
brnnMod <- readRDS("BRNN_1159_Model.RDS")
gamMod <- readRDS("GAM_1159_Model.RDS")



#blenderData$predNN <- predict(nnMod, newdata = blenderData, type = "raw")
blenderData$predRF <- predict(rfMod, newdata = blenderData, type = "raw")
blenderData$predPLS <- predict(plsMod, newdata = blenderData, type = "raw")
blenderData$predCUB <- predict(cubistMod, newdata = blenderData, type = "raw")
blenderData$predXGB <- predict(xgbMod, newdata = blenderData, type = "raw")
blenderData$predSVM <- predict(svmMod, newdata = blenderData, type = "raw")
blenderData$predBRNN <- predict(brnnMod, newdata = blenderData, type = "raw")
blenderData$predGAM <- predict(gamMod, newdata = blenderData, type = "raw")

#testingData$predNN <- predict(nnMod, newdata = testingData, type = "raw")
testingData$predRF <- predict(rfMod, newdata = testingData, type = "raw")
testingData$predPLS <- predict(plsMod, newdata = testingData, type = "raw")
testingData$predCUB <- predict(cubistMod, newdata = testingData, type = "raw")
testingData$predXGB <- predict(xgbMod, newdata = testingData, type = "raw")
testingData$predSVM <- predict(svmMod, newdata = testingData, type = "raw")
testingData$predBRNN <- predict(brnnMod, newdata = testingData, type = "raw")
testingData$predGAM <- predict(gamMod, newdata = testingData, type = "raw")

write_csv(blenderData, paste0("ukhr_blender_data_", targetName, ".csv"))
write_csv(testingData, paste0("ukhr_testing_data_", targetName, ".csv"))

predDF <- testingData %>%
  select(predRF, predPLS, predCUB, predXGB, predSVM, predBRNN, predGAM, BF_Placed_SP_PL)

summary(predDF$predPLS)

cor(predDF)

colSums(is.na(predDF))

testingData %>%
  #group_by(Handicap) %>%
  filter(predBRNN > 0.0) %>%
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

colSums(is.na(blenderData))

summary(blenderData$predPLS)

str(blenderData)

predVars <- names(blenderData)[names(blenderData) != targetName]

predVars

nearZeroVar(blenderData)

# Final Linear Ensemble Model

# library(doMC)
#
# registerDoMC(4)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(101)

finalLinMod <- train(BF_Placed_SP_PL ~ .,
                     method = "lm",
                     data = blenderData,
                     preProc = c("scale", "center"),
                     trControl = train.control,
                     metric = "RMSE")

print(finalLinMod)

summary(finalLinMod)

testingData$FinalPredLM <- predict(finalLinMod, newdata = testingData, type = "raw")

saveRDS(finalLinMod, "Final_Linear_Mod_1159.RDS")

#finalLinMod <- readRDS("Final_Linear_Mod_1159.RDS")

###############################################################

# Final Interactive Linear Ensemble Model
#
# library(doMC)
#
# registerDoMC(4)
#
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               verboseIter = T)
#
# set.seed(101)
#
# finalLinModInt <- train(BF_Placed_SP_PL ~ (predBRNN +
#                           predGAM +
#                           predRF +
#                           predCUB +
#                           predSVM +
#                           predXGB)^2,
#                         method = "lm",
#                         data = blenderData,
#                         preProc = c("scale", "center"),
#                         trControl = train.control,
#                         metric = "RMSE")
#
# print(finalLinModInt)
#
# summary(finalLinModInt)
#
# testingData$FinalPredLMI <- predict(finalLinModInt, newdata = testingData, type = "raw")
#
# saveRDS(finalLinModInt, "Final_Linear_Mod_Int_1159.RDS")
#
# finalLinModInt <- readRDS("Final_Linear_Mod_Int_1159.RDS")
#
# testingData %>%
#   #group_by(Handicap) %>%
#   filter(FinalPredLM > 0.0) %>%
#   mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BF_Placed_SP_PL),
#             Total_PL = sum(BF_Placed_SP_PL)) %>%
#   arrange(desc(Avg_PL))

#########################################

# Final SVM Radial Model
# library(doMC)
#
# registerDoMC(4)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

set.seed(101)

finalSVMMod <- train(BF_Placed_SP_PL ~ .,
                     method = "svmRadial",
                     data = blenderData,
                     preProc = c("scale", "center"),
                     tuneLength = 6,
                     trControl = train.control,
                     metric = "RMSE")

print(finalSVMMod)

summary(finalSVMMod)

testingData$FinalPredSVM <- predict(finalSVMMod, newdata = testingData, type = "raw")

saveRDS(finalSVMMod, "Final_SVM_Mod_1159.RDS")

#finalSVMMod <- readRDS("Final_SVM_Mod_1159.RDS")

testingData %>%
  #group_by(Handicap) %>%
  filter(FinalPredSVM > 0.0) %>%
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

set.seed(101)

finalGamMod <- train(BF_Placed_SP_PL ~ .,
                     method = "gam",
                     data = blenderData,
                     preProc = c("scale", "center"),
                     trControl = train.control,
                     metric = "RMSE")

print(finalGamMod)

summary(finalGamMod)

testingData$FinalPredGAM <- predict(finalGamMod, newdata = testingData, type = "raw")

saveRDS(finalGamMod, "Final_GAM_Mod_1159.RDS")

#finalGamMod <- readRDS("Final_GAM_Mod_1159.RDS")

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

###############################################################

set.seed(101)

finalRFMod <- train(BF_Placed_SP_PL ~ .,
               method = "rf",
               data = blenderData,
               preProc = c("scale", "center"),
               trControl = train.control,
               metric = "RMSE")

summary(finalRFMod)

testingData$FinalPredRF <- predict(finalRFMod, newdata = testingData, type = "raw")

saveRDS(finalRFMod, "Final_RF_Mod_1159.RDS")

#finalRFMod <- readRDS("Final_RF_Mod_1159.RDS")

print(finalRFMod)

varImp(finalRFMod)

testingData %>%
  #group_by(Handicap) %>%
  filter(FinalPredRF > 0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))


write_csv(testingData, paste0("ukhr_testing_data_final_1159.csv", targetName, ".csv"))


################################################################

read_csv("ukhr_testing_data_final_1159.csvBF_Placed_SP_PL.csv", col_names = T)


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

set.seed(101)

finalXGBMod <- train(BF_Placed_SP_PL ~ .,
                     data = blenderData,
                     method = "xgbLinear",
                     preProc = c("center", "scale"),
                     metric = "RMSE",
                     tuneGrid = tune.grid,
                     trControl = train.control)

print(finalXGBMod)

varImp(finalXGBMod)

saveRDS(finalXGBMod, "Final_XGB_Mod_1159.RDS")

finalXGBMod <- readRDS("Final_XGB_Mod_1159.RDS")

testingData$FinalPredsXGB <- predict(finalXGBMod, newdata = testingData, type = "raw")

#RMSE(testingData$FinalPredsXGB, testingData$BF_Placed_SP_PL)

# testingData <- testingData %>%
#   mutate(FinalXGBPreds_Band = cut(FinalPredsXGBI, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                          labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                          ordered_result = T),
#          RF_XGB_Avg = (predRF + predXGB)/2,
#          RF_XGB_Avg_Band = cut(RF_XGB_Avg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                ordered_result = T))

testingData %>%
  #group_by(Handicap) %>%
  filter(FinalPredsXGB > 0.0) %>%
  mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))

View(tail(testingData))

head(testingData)

colnames(testingData)

write_csv(testingData, paste0("ukhr_testing_data_final_1159.csv", targetName, ".csv"))

#####################################################################################
# set.seed(101)
#
# splitA2 <- createDataPartition(quals$BF_Placed_SP_PL, p = 0.6, list = F)
#
# trainingQualsData2 <- quals[splitA2,]
#
# testingQualsData2 <- quals[-splitA2,]
#
# set.seed(101)
#
# splitB2 <- createDataPartition(trainingQualsData2$BF_Placed_SP_PL, p = 0.5, list = F)
#
# ensembleData2 <- trainingQualsData2[splitB2,]
# blenderData2 <- trainingQualsData2[-splitB2,]
# testingQualsData <- testingQualsData2 #qualsData[-splitA,]
#
finalLinMod <- readRDS("Final_Linear_Mod_1159.RDS")
#finalLinModInt <- readRDS("Final_Linear_Mod_Int_m7.RDS")
finalSVMMod <- readRDS("Final_SVM_Mod_1159.RDS")
finalGamMod <- readRDS("Final_GAM_Mod_1159.RDS")
finalRFMod <- readRDS("Final_RF_Mod_1159.RDS")
finalXGBMod <- readRDS("Final_XGB_Mod_1159.RDS")

rfMod <- readRDS("RF_1159_Model.RDS")
plsMod <- readRDS("PLS_1159_Model.RDS")
cubistMod <- readRDS("Cubist_1159_Model.RDS")
xgbMod <- readRDS("XGB_1159_Model.RDS")
svmMod <- readRDS("SVM_1159_Model.RDS")
brnnMod <- readRDS("BRNN_1159_Model.RDS")
gamMod <- readRDS("GAM_1159_Model.RDS")
#
# blenderData2$predNN <- predict(nnMod, newdata = blenderData2, type = "raw")
# blenderData2$predRF <- predict(rfMod, newdata = blenderData2, type = "raw")
# blenderData2$predPLS <- predict(plsMod, newdata = blenderData2, type = "raw")
# blenderData2$predCUB <- predict(cubistMod, newdata = blenderData2, type = "raw")
# blenderData2$predXGB <- predict(xgbMod, newdata = blenderData2, type = "raw")
# blenderData2$predSVM <- predict(svmMod, newdata = blenderData2, type = "raw")
# blenderData2$predBRNN <- predict(brnnMod, newdata = blenderData2, type = "raw")
# blenderData2$predGAM <- predict(gamMod, newdata = blenderData2, type = "raw")
#
# testingQualsData$predNN <- predict(nnMod, newdata = testingQualsData, type = "raw")
testingQualsData$predRF <- predict(rfMod, newdata = testingQualsData, type = "raw")
testingQualsData$predPLS <- predict(plsMod, newdata = testingQualsData, type = "raw")
testingQualsData$predCUB <- predict(cubistMod, newdata = testingQualsData, type = "raw")
testingQualsData$predXGB <- predict(xgbMod, newdata = testingQualsData, type = "raw")
testingQualsData$predSVM <- predict(svmMod, newdata = testingQualsData, type = "raw")
testingQualsData$predBRNN <- predict(brnnMod, newdata = testingQualsData, type = "raw")
testingQualsData$predGAM <- predict(gamMod, newdata = testingQualsData, type = "raw")

testingQualsData$FinalLinearModel <- predict(finalLinMod, newdata = testingQualsData, type = "raw")
#testingQualsData$FinalLinearModelInt <- predict(finalLinModInt, newdata = testingQualsData, type = "raw")
testingQualsData$FinalSVMModel <- predict(finalSVMMod, newdata = testingQualsData, type = "raw")
testingQualsData$FinalGamMod <- predict(finalGamMod, newdata = testingQualsData, type = "raw")
testingQualsData$FinalXGBMod <- predict(finalXGBMod, newdata = testingQualsData, type = "raw")
testingQualsData$FinalRFMod <- predict(finalRFMod, newdata = testingQualsData, type = "raw")




testingQualsData <- testingQualsData %>%
  mutate(Final_Linear_Band = cut(FinalLinearModel, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                     labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                                ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                     ordered_result = T),
         Final_SVM_Band = cut(FinalSVMModel, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                     labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                                ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                  ordered_result = T),
         Final_XGB_Band = cut(FinalXGBMod, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                  labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                             ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                  ordered_result = T),
         Final_GAM_Band = cut(FinalGamMod, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                  labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                             ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                                  ordered_result = T),
         Final_RF_Band = cut(FinalRFMod, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
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
                             ordered_result = T),
         BRNN_Pred_Band = cut(predBRNN, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                             labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                        ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                             ordered_result = T),
         GAM_Pred_Band = cut(predGAM, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                             labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                        ">0.30 to 0.40", ">0.40 to 0.50", ">0.50"),
                             ordered_result = T))

write_csv(testingQualsData, "Final_1159_Models_Testing_Set_Predictions.csv")

colnames(testingQualsData)

View(head(testingQualsData))

fp <- testingQualsData[,546:550]

cor(fp)

###################################################################

library(tidyverse)
library(lubridate)
library(caret)
library(stringi)
library(stringr)


testingQualsData <- read_csv("Final_1159_Models_Testing_Set_Predictions.csv")

colnames(testingQualsData)

testingQualsData <- testingQualsData %>%
  mutate(Final_Models_Avg = (FinalLinearModel + FinalXGBMod + FinalRFMod + FinalSVMModel + FinalGamMod)/5,
         Final_Models_Avg_Band = cut(Final_Models_Avg, breaks = c(-10000, 0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 10000),
                                     labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.20" ,">0.20 to 0.30",
                                                ">0.30 to 0.40", ">0.40 to 0.50", ">0.50")),
         Runners_Band = cut(Runners, breaks = c(-1, 4, 7, 15, 100),
                            labels = c("<5", "5-7", "8-15", "16+")),
         Btn_AE_Band = cut(Btn_AE_Ratio, breaks = c(0, 0.9, 1.0, 1.1, 1.2, 1.3, 1000),
                           labels = c("<=0.9", ">0.9 to 1.0", ">1 to 1.1", ">1.1 to 1.2", ">1.2 to 1.3", ">1.3")))

testingQualsData <- testingQualsData %>%
  mutate(FLM_Score = if_else(FinalLinearModel > 0, 1, 0),
         FXGB_Score = if_else(FinalXGBMod > 0, 1, 0),
         FRF_Score = if_else(FinalRFMod > 0, 1, 0),
         FSVM_Score = if_else(FinalSVMModel > 0, 1, 0),
         FGAM_Score = if_else(FinalGamMod > 0, 1, 0),
         Final_Model_Score = FLM_Score + FXGB_Score + FRF_Score + FSVM_Score + FGAM_Score)

testingQualsData <- testingQualsData %>%
  group_by(Year, Month, DayOfMonth) %>%
  mutate(FMA_Rank = min_rank(desc(Final_Models_Avg))) %>%
  ungroup()

testingQualsData$FMA_Rank




# summary(testingQualsData$ClassDiffTotal)
#
testingQualsData <- testingQualsData %>%
  mutate(Exp_Win = 1/Betfair.Win.S.P.,
         Act_Win = if_else(BFSP_PL > 0, 1, 0))



testingQualsData %>%
  group_by(Final_Models_Avg_Band) %>%
  #filter(Final_Models_Avg > 0.05, Handicap != "NONHCP") %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0),
         Placed = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won, na.rm = T),
            WinPercent = mean(Won, na.rm = T),
            Avg_W_PL = mean(BFSP_PL, na.rm = T),
            Total_W_PL = sum(BFSP_PL, na.rm = T),
            AER_Win = sum(Act_Win, na.rm = T)/sum(Exp_Win, na.rm = T),
            Places = sum(Placed, na.rm = T),
            Place_Percent = mean(Placed, na.rm = T),
            Avg_P_PL = mean(BF_Placed_SP_PL, na.rm = T),
            Total_P_PL = sum(BF_Placed_SP_PL, na.rm = T)) %>%
  arrange(desc(AER_Win)) %>%
  View()







########################################################################################################################
########################################################################################################################
########################################################################################################################


#
# colnames(blenderData2)
#
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               verboseIter = T)
#
#
# tune.grid <- expand.grid(eta = c(0.01, 0.025),
#                          nrounds = c(100, 125),
#                          lambda = c(0.05, 0.075, 0.1),
#                          alpha = c(1.0))
#
#
#
# set.seed(101)
#
# xgbFinalWinMod <- train(BFSP_PL ~ .,
#                   data = blenderData2,
#                   method = "xgbLinear",
#                   preProc = c("center", "scale"),
#                   metric = "RMSE",
#                   tuneGrid = tune.grid,
#                   trControl = train.control)
#
# print(xgbFinalWinMod)
#
# varImp(xgbFinalWinMod)
#
# saveRDS(xgbFinalWinMod, "Final_XGB_Win_Model.RDS")
#
#
# testingQualsData$FinalWinPreds <- predict(xgbFinalWinMod, newdata = testingQualsData, type = "raw")
# testingQualsData$FinalPlacePreds <- predict(xgbFinal, newdata = testingQualsData, type = "raw")
#
# colnames(testingQualsData)
#
#
#
# testingQualsData <- testingQualsData %>%
#   mutate(FinalWinPreds_Band = cut(FinalWinPreds, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                ordered_result = T),
#          FinalPlacePreds_Band = cut(FinalPlacePreds, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                ordered_result = T),
#          RF_XGB_Avg = (predRF + predXGB)/2,
#          RF_XGB_Avg_Band = cut(RF_XGB_Avg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                ordered_result = T),
#          XGB_Pred_Band = cut(predXGB, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                ordered_result = T),
#          RF_Pred_Band = cut(predRF, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                              labels = c("<=0.0", ">0.0 to 0.25", ">0.25 to 0.5", ">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                              ordered_result = T),
#          NN_Pred_Band = cut(predNN, breaks = c(-10000, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 10000),
#                             labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.15", ">0.15 to 0.20", ">0.20 to 0.25", ">0.25"),
#                             ordered_result = T),
#          PLS_Pred_Band = cut(predPLS, breaks = c(-10000, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 10000),
#                             labels = c("<=0.0", ">0.0 to 0.05", ">0.05 to 0.10", ">0.10 to 0.15", ">0.15 to 0.20", ">0.20 to 0.25", ">0.25"),
#                             ordered_result = T))
#
# testingQualsData$FinalPredsAvg <- (testingQualsData$FinalWinPreds + testingQualsData$FinalPlacePreds)/2
#
# testingQualsData <- testingQualsData %>%
#   mutate(FinalPredsAvg_Band = cut(FinalPredsAvg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                   labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                   ordered_result = T))
#
# write_csv(testingQualsData, "BF_Ensemble_WinPlace_Predictions.csv")
#
# testingQualsData %>%
#   group_by() %>%
#   filter(predRF >= 0.25, predXGB >= 0.25, FinalPlacePreds >= 0.25) %>%
#   mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>%
#   arrange(desc(Avg_PL))
#
#
#
#
#
# mean(testingData$BF_Placed_SP_PL)
#
#
# tail(testingQualsData$BetFairSPForecastWinPrice, 50)
# tail(testingData$BetFairSPForecastWinPrice, 50)
#
#
# # Save Final XGB Ensemble Model
#
# saveRDS(xgbFinal, "XGB_Final_Ensemble_m7.RDS")
#
# colnames(blenderData)
#
# #xgbFinal <- readRDS("XGB_Final_Ensemble_m7.RDS")
#
# ################################
#
#
#
# testingQualsData$FinalPredsXGBPlace <- predict(xgbFinalPlace, newdata = testingData, type = "raw")
#
# summary(testingQualsData$FinalPredsXGB)
#
# summary(testingQualsData$FinalPredsXGBPlace)
#
# testingQualsData <- testingQualsData %>%
#   mutate(FinalPredsPlace_Band = cut(FinalPredsXGBPlace, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                ordered_result = T))
#
# testingQualsData %>%
#   group_by() %>%
#   filter(FinalPredsXGB > 0, FinalPredsXGBPlace > 0) %>%
#   mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BF_Placed_SP_PL),
#             Total_PL = sum(BF_Placed_SP_PL)) %>%
#   arrange(desc(Avg_PL))
#
# testingQualsData <- testingQualsData %>%
#   mutate(FinalMods_WinPlaceAvg = (FinalPredsXGB + FinalPredsXGBPlace)/2,
#          FinalModsAvg_Band = cut(FinalMods_WinPlaceAvg, breaks = c(-10000, 0, 0.25, 0.5, 1.0, 2.5, 5.0, 10000),
#                                     labels = c("<=0.0", ">0.0 to 0.25",">0.25 to 0.5" ,">0.5 to 1.0", ">1.0 to 2.5", ">2.5 to 5", ">5"),
#                                     ordered_result = T))
#
#
# testingQualsData %>%
#   group_by(FinalModsAvg_Band) %>%
#   filter(FinalPredsXGB > 0, FinalPredsXGBPlace > 0) %>%
#   mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BF_Placed_SP_PL),
#             Total_PL = sum(BF_Placed_SP_PL)) %>%
#   arrange(desc(Avg_PL))
#
# colnames(testingQualsData)
#
# testingQualsData <- testingQualsData %>%
#   mutate(VOBF_Band = cut(ValueOdds_BetfairFormat, breaks = c(0, 2.00, 6.0, 11.0, 21, 51, 5000),
#                                  labels = c("<=2.0", ">2.0 to 6.0",">6.0 to 11.0" ,">11.0 to 21.0",
#                                             ">21 to 51", ">51.0"),
#                                  ordered_result = T))
#
#
# testingQualsData %>%
#   group_by() %>%
#   filter(FinalPredsXGB >= 1, FinalPredsXGBPlace >= 1) %>%
#   mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BF_Placed_SP_PL),
#             Total_PL = sum(BF_Placed_SP_PL)) %>%
#   arrange(desc(Avg_PL))


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
#
#
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               verboseIter = T)
#
# set.seed(101)
#
# svmFinal <- train(BF_Placed_SP_PL ~ .,
#                      data = blenderData,
#                      method = "svmRadial",
#                      preProc = c("center", "scale"),
#                      metric = "RMSE",
#                      tuneLength = 10,
#                      trControl = train.control)
#
# print(svmFinal)
#
# #varImp(svmFinal)
#
# testingData$FinalPredsSVM <- predict(svmFinal, newdata = testingData, type = "raw")
#
# RMSE(testingData$FinalPredsSVM, testingData$BF_Placed_SP_PL)
#
# testingData %>%
#   group_by() %>%
#   filter(FinalPredsSVM > 0.0) %>%
#   mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BF_Placed_SP_PL),
#             Total_PL = sum(BF_Placed_SP_PL)) %>%
#   arrange(desc(Avg_PL))
#
#
#
# mean(testingData$BF_Placed_SP_PL)


##########################################################################

# Final Cubist Model

# library(doParallel)
# cores <- detectCores()
# cl <- makePSOCKcluster(cores) # number of cores to use
# registerDoParallel(cl)
#
# targetName <- "BF_Placed_SP_PL"
#
# cTrain <- blenderData %>%
#   select_if(is.numeric)
#
# cTrainVars <- names(cTrain)[names(cTrain) != targetName]
#
# cTrainVars
#
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               verboseIter = T)
#
# cubistGrid <- expand.grid(committees = seq(1, 51, 5),
#                           neighbors = seq(0, 9, 3))
#
# set.seed(101)
#
# finalCUB <- train(x = cTrain[,cTrainVars], y = cTrain$BF_Placed_SP_PL,
#                    method = "cubist",
#                    tuneGrid = cubistGrid,
#                    preProc = c("center", "scale"),
#                    trControl = train.control)
#
#
# finalCUB
#
# testingData$FinalPredsCUBIST <- predict(finalCUB, newdata = testingData, type = "raw")
#
# RMSE(testingData$FinalPredsCUBIST, testingData$BF_Placed_SP_PL)
#
# testingData %>%
#   group_by() %>%
#   filter(FinalPredsCUBIST > 0.0) %>%
#   mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BF_Placed_SP_PL),
#             Total_PL = sum(BF_Placed_SP_PL)) %>%
#   arrange(desc(Avg_PL))
#
#
#
# mean(testingData$BF_Placed_SP_PL)
#
#
# stopCluster(cl)
#
#
#
#
#
# ##########################################################################
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               verboseIter = T)
#
# set.seed(101)
#
# ridgeMod <- train(BF_Placed_SP_PL ~ .,
#                   data = blenderData,
#                   method = "ridge",
#                   preProc = c("center", "scale"),
#                   tuneLength = 20,
#                   metric = "RMSE",
#                   trControl = train.control)
#
# ridgeMod
#
# testingData$FinalPredsRIDGE <- predict(ridgeMod, newdata = testingData, type = "raw")
#
#
# RMSE(testingData$FinalPredsRIDGE, testingData$BF_Placed_SP_PL)
#
# testingData %>%
#   group_by() %>%
#   filter(predXGB > 0.25) %>%
#   mutate(Won = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BF_Placed_SP_PL),
#             Total_PL = sum(BF_Placed_SP_PL)) %>%
#   arrange(desc(Avg_PL))
#
#
#
# mean(testingData$BF_Placed_SP_PL)
#
# ######################################################
#
# colnames(testingData)
#
# ########################################################
#
# sysRes <- quals %>%
#   group_by(System_Name) %>%
#   #filter(predNN > 0.0) %>%
#   mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>%
#   arrange(desc(Avg_PL))
#
# sysRes
#
# View(sysRes)
#
# ukhrSys <- which(str_detect(quals$System_Name,"UKHR"))
#
# ukhrSysQuals <- quals[ukhrSys,]
#
# nonUKHR <- quals[-ukhrSys,]
#
# ukhrPL <- ukhrSysQuals %>%
#   group_by() %>%
#   #filter(predNN > 0.0) %>%
#   mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>%
#   arrange(desc(Avg_PL))
#
# ukhrPL
#
# nonUKHR_PL <- nonUKHR %>%
#   group_by() %>%
#   #filter(predNN > 0.0) %>%
#   mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>%
#   arrange(desc(Avg_PL))
#
# nonUKHR_PL
#
# trSys <- which(str_detect(quals$System_Name,"Trainer"))
#
# trSysQuals <- quals[trSys,]
#
# nonTrQuals <- quals[-trSys,]
#
# trPL <- trSysQuals %>%
#   group_by() %>%
#   #filter(predNN > 0.0) %>%
#   mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>%
#   arrange(desc(Avg_PL))
#
# trPL
#
# nonTrQuals_PL <- nonTrQuals %>%
#   group_by() %>%
#   #filter(predNN > 0.0) %>%
#   mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   summarise(Runs = n(),
#             Winners = sum(Won),
#             WinPercent = mean(Won),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>%
#   arrange(desc(Avg_PL))
#
# nonTrQuals_PL


