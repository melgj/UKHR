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



quals <- read_csv("All_System_Qualifiers_Yr_2018_Cleaned.csv", col_names = T)

colnames(quals)

colSums(is.na(quals))

quals <- quals %>%
  select(-Dist_Range)


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


colSums(is.na(qualsData))

# set.seed(100)
# #
# dataSplit <- createDataPartition(qualsData$BF_Placed_SP_PL, p = 0.60, list = FALSE)
#
# dataSplit

qualsData <- qualsData[sample(nrow(qualsData)),]

targetName <- 'BF_Placed_SP_PL'
predVars <- names(qualsData)[names(qualsData) != targetName]

split <- floor(nrow(qualsData)/5)
ensembleData <- qualsData[0:split,]
blenderData <- qualsData[(split+1):(split*2),]
testingData <- qualsData[(split*2+1):nrow(qualsData),]

#######################################################################

# Train Control for all models


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

###########################################################################
# XGB Tree Algorithm to predict BF_Place_SP_PL

# library(xgboost)

set.seed(100)


tune.grid <- expand.grid(eta = c(0.01, 0.05, 0.1),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.05, 0.1),
                         alpha = c(1.0))


View(tune.grid)

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

#xgbLinModUK <- readRDS("XGB_Linear_Systems_BFPL_Model.RDS")

########################################################################################

cores <- detectCores()
cl <- makePSOCKcluster(cores) # number of cores to use
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

#ukNN <- readRDS("Systems_NN_BFSPPL_Model.RDS")

saveRDS(nnMod, "Systems_NN_BFSPPL_Model_p1.RDS")

################################################################

# Build RF Model

# library(doMC)
#
# registerDoMC(8)

mtry <- c(2,4,6)

mtryGrid <- expand.grid(mtry = mtry)
#print(mtryGrid)

set.seed(100)

rfMod <- train(BF_Placed_SP_PL ~ .,
               data = ensembleData,
               method = "rf",
               ntrees = 1000,
               trControl = train.control,
               tuneGrid = mtryGrid,
               PreProc = c("center", "scale"),
               importance = T)

print(rfMod)

varImp(rfMod)

saveRDS(rfMod, "RF_BFPL_Model_p1.RDS")


##############################################################
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


View(cubistGrid)

# library(doMC)
# registerDoMC(4)

set.seed(100)

cubistMod <- train(x = cTrain[,cTrainVars], y = cTrain$BF_Placed_SP_PL,
                   method = "cubist",
                   tuneGrid = cubistGrid,
                   preProc = c("center", "scale"),
                   trControl = train.control)


cubistMod

print(cubistMod)

saveRDS(cubistMod, "Cubist_BFPL_Model_p1.RDS")

##############################################################

blenderData$predNN <- predict(nnMod, newdata = blenderData, type = "raw")
blenderData$predRF <- predict(rfMod, newdata = blenderData, type = "raw")
blenderData$predPLS <- predict(plsMod, newdata = blenderData, type = "raw")
blenderData$predCUB <- predict(cubistMod, newdata = blenderData, type = "raw")
blenderData$predXGB <- predict(xgbMod, newdata = blenderData, type = "raw")

testingData$predNN <- predict(nnMod, newdata = testingData, type = "raw")
testingData$predRF <- predict(rfMod, newdata = testingData, type = "raw")
testingData$predPLS <- predict(plsMod, newdata = testingData, type = "raw")
testingData$predCUB <- predict(cubistMod, newdata = testingData, type = "raw")
testingData$predXGB <- predict(xgbMod, newdata = testingData, type = "raw")

write_csv(blenderData, "ukhr_blender_data.csv")
write_csv(testingData, "ukhr_testing_data.csv")

predDF <- testingData %>%
  select(predNN, predRF, predPLS, predCUB, predXGB, BF_Placed_SP_PL)

cor(predDF)

RMSE(testingData$predNN, testingData$BF_Placed_SP_PL)
RMSE(testingData$predRF, testingData$BF_Placed_SP_PL)
RMSE(testingData$predPLS, testingData$BF_Placed_SP_PL)
RMSE(testingData$predCUB, testingData$BF_Placed_SP_PL)
RMSE(testingData$predXGB, testingData$BF_Placed_SP_PL)


##############################################################

blenderData <- read_csv("ukhr_blender_data.csv", col_names = T)
testingData <- read_csv("ukhr_testing_data.csv", col_names = T)

summary(blenderData)

str(blenderData)

targetName <- "BF_Placed_SP_PL"

predVars <- names(blenderData)[names(blenderData) != targetName]

predVars

# Final Ensemble XGB Model

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)
set.seed(100)

tune.grid <- expand.grid(eta = c(0.01, 0.05, 0.1),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.05, 0.1),
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
  filter(predRF > 0) %>%
  mutate(Placed = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Places = sum(Placed),
            PlacePercent = mean(Placed),
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
  group_by() %>%
  filter(FinalPredsRIDGE > 0) %>%
  mutate(Placed = if_else(BF_Placed_SP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Places = sum(Placed),
            PlacePercent = mean(Placed),
            Avg_PL = mean(BF_Placed_SP_PL),
            Total_PL = sum(BF_Placed_SP_PL)) %>%
  arrange(desc(Avg_PL))



mean(testingData$BF_Placed_SP_PL)
