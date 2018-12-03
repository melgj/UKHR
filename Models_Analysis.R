# saveRDS(ukNN, "Systems_NN_BFSPPL_Model_v20.RDS")
# saveRDS(xgbLinModUK, "XGB_Linear_Systems_BFPL_Model_v20.RDS")
# saveRDS(rfMod, "RF_BFPL_Model_v20.RDS")
# saveRDS(bfPLMod, "Systems_MARS_BFSPPL_Model.RDS")

library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(caret)
library(nnet)
library(xgboost)
library(randomForest)
library(earth)
library(kernlab)
library(e1071)


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
  select(BetFairSPForecastWinPrice, System_Name, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, BFSP_PL)

colSums(is.na(qualsData))

colnames(qualsData)

nn <- readRDS("Systems_NN_BFSPPL_Model_v20.RDS")
xgb <- readRDS("XGB_Linear_Systems_BFPL_Model_v20.RDS")
rf <- readRDS("RF_BFPL_Model_v20.RDS")
#mars <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]




predNN <- predict(nn, newdata = ukTestSet, type = "raw")
predXGB <- predict(xgb, newdata = ukTestSet, type = "raw")
predRF <- predict(rf, newdata = ukTestSet, type = "raw")
#predMars <- predict(mars, newdata = ukTestSet, type = "raw")

cor(predNN, predXGB)
cor(predNN, predRF)
cor(predXGB, predRF)
# cor(predMars, predXGB)
# cor(predMars, predRF)
# cor(predMars, predNN)

ukTestSet$NN_Pred <- predNN
ukTestSet$XGB_Pred <- predXGB
ukTestSet$RF_Pred <- predRF
#ukTestSet$Mars_Pred <- predMars

summary(ukTestSet)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(NN_Pred > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(XGB_Pred > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(RF_Pred > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(RF_Pred > 0.0, XGB_Pred > 0, NN_Pred > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet$Model_Avg = (ukTestSet$RF_Pred + ukTestSet$NN_Pred + ukTestSet$XGB_Pred)/3

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(Model_Avg > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(RF_Pred > 0,XGB_Pred > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

############################################################################

predNN <- predict(nn, newdata = qualsData, type = "raw")
predXGB <- predict(xgb, newdata = qualsData, type = "raw")
predRF <- predict(rf, newdata = qualsData, type = "raw")

#saveRDS(xgbTreeModUK, "XGB_Systems_Model_Prob")

xgbProb <- readRDS("XGB_Systems_Model_Prob")

predXGB_Prob <- predict(xgbProb, newdata = qualsData, type = "prob")

predXGB_Prob[,2]

qualsData2 <- qualsData %>% 
  mutate(NN_Pred = predNN,
         XGB_Pred = predXGB,
         RF_Pred = predRF)

qualsData2$Win_Prob <- predXGB_Prob[,2]

colnames(qualsData2)

summary(qualsData2$Win_Prob)

#saveRDS(xgbPredModUK, "Final_BFPL_Model_V30.RDS")

modV20 <- readRDS("Final_BFPL_Model_V20.RDS")
modV30 <- readRDS("Final_BFPL_Model_V30.RDS")

set.seed(200)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData2[ukTrainRows, -2]

ukTestSet <- qualsData2[-ukTrainRows, -2]



predV20 <- predict(modV20, newdata = ukTestSet, type = "raw")
predV30 <- predict(modV30, newdata = ukTestSet, type = "raw")

ukTestSet <- ukTestSet %>% 
  mutate(v20 = predV20,
         v30 = predV30)

ukTestSet <- ukTestSet %>% 
  mutate(Model_Avg = (RF_Pred + NN_Pred + XGB_Pred)/3,
         V_Model_Avg = (v20 + v30)/2)


ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(v20 > 0, v30 > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))















