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
ridge <- readRDS("Ridge_BFPL_Model.RDS")
pls <- readRDS("PLS_BFPL_Model.RDS")
#mars <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, -2]

ukTestSet <- qualsData[-ukTrainRows, -2]




predNN <- predict(nn, newdata = ukTestSet, type = "raw")
predXGB <- predict(xgb, newdata = ukTestSet, type = "raw")
predRF <- predict(rf, newdata = ukTestSet, type = "raw")
predRidge <- predict(ridge, newdata = ukTestSet, type = "raw")
predPLS <- predict(pls, newdata = ukTestSet, type = "raw")
#predMars <- predict(mars, newdata = ukTestSet, type = "raw")

predDF <- tibble(NN = predNN,XGB = predXGB, RF = predRF, RIDGE = predRidge, PLS = predPLS, 
                 BFPL = ukTestSet$BFSP_PL)

predDF$Result <- if_else(predDF$BFPL > 0, 1, 0)

head(predDF)

cor(predDF)

winPcnt <- predDF %>% 
  group_by(Result) %>% 
  summarise_all(.funs = (Win_Percent = mean))
  

winPcnt

# cor(predMars, predXGB)
# cor(predMars, predRF)
# cor(predMars, predNN)

ukTestSet$NN_Pred <- predNN
ukTestSet$XGB_Pred <- predXGB
ukTestSet$RF_Pred <- predRF
ukTestSet$RIDGE_Pred <- predRidge
ukTestSet$PLS_Pred <- predPLS
#ukTestSet$Mars_Pred <- predMars

summary(ukTestSet)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(NN_Pred >= 0.2) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(XGB_Pred > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(RF_Pred > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(PLS_Pred > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(RIDGE_Pred > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


ukTestSet$Model_Avg = (ukTestSet$RF_Pred + ukTestSet$XGB_Pred + ukTestSet$NN_Pred + ukTestSet$PLS_Pred)/4

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(Model_Avg >= 0.2) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))
############################################################################


############################################################################

predNN <- predict(nn, newdata = qualsData, type = "raw")
predXGB <- predict(xgb, newdata = qualsData, type = "raw")
predRF <- predict(rf, newdata = qualsData, type = "raw")
predPLS <- predict(pls, newdata = qualsData, type = "raw")

#saveRDS(xgbTreeModUK, "XGB_Systems_Model_Prob")

xgbProb <- readRDS("XGB_Systems_Model_Prob")

predXGB_Prob <- predict(xgbProb, newdata = qualsData, type = "prob")

predXGB_Prob[,2]

qualsData2 <- qualsData %>% 
  mutate(NN_Pred = predNN,
         XGB_Pred = predXGB,
         RF_Pred = predRF,
         PLS_Pred = predPLS)

qualsData2$Win_Prob <- predXGB_Prob[,2]

colnames(qualsData2)

summary(qualsData2$Win_Prob)

#saveRDS(xgbPredModUK, "Final_BFPL_Model_V30.RDS")

modV20 <- readRDS("Final_BFPL_Model_V20.RDS")
modV30 <- readRDS("Final_BFPL_Model_V30.RDS")
modMars <- readRDS("MARS_Final_Model_v10.RDS")

set.seed(100)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData2[ukTrainRows, -2]

ukTestSet <- qualsData2[-ukTrainRows, -2]



predV20 <- predict(modV20, newdata = ukTestSet, type = "raw")
predV30 <- predict(modV30, newdata = ukTestSet, type = "raw")
predMars <- predict(modMars, newdata = ukTestSet, type = "raw")

ukTestSet <- ukTestSet %>% 
  mutate(v20 = predV20,
         v30 = predV30,
         Mars = predMars[,1])

ukTestSet <- ukTestSet %>% 
  mutate(Model_Avg = (RF_Pred + NN_Pred + XGB_Pred)/3,
         V_Model_Avg = (v20 + v30)/2)

colnames(ukTestSet)

modelPreds <- ukTestSet[, 23:33] 

modelPreds$Result <- if_else(modelPreds$BFSP_PL > 0, 1, 0)

colnames(modelPreds)

cor(modelPreds)

winPcnt <- modelPreds %>% 
  group_by(Result) %>% 
  summarise_all(.funs = (Win_Percent = mean))

modelPreds %>% 
  filter(v30 > 0, Result == 1) %>%
  summarise(Avg_WBFSP = mean(BFSP_PL) + 1,
            Median_WBFSP = median(BFSP_PL),
            Max_WBFSP = max(BFSP_PL),
            Min_WBFSP = min(BFSP_PL))
  

winPcnt


ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(V_Model_Avg > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(V_Model_Avg > 0, v20 > 2.0, v30 < 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))



#################################################################################################

ukTest_v30 <- filter(ukTestSet, v30 > 0)

colnames(ukTest_v30)

baseBet = 2.0
bank = 100

ukTest_v30$Bank = bank

bbsq <- function(df, base, bank, fraction) {
  base <- base
  bank <- bank
  new_bank <- bank
  netProfit = 0
  df$Profit <- 0
  df$Bank <- bank
  df$Net_Profit <- 0
  
  
  for(runner in 1:nrow(df)){
    df$Profit[runner] <- if_else(netProfit > 0,
                                 (base + (netProfit ^ fraction)) * df$BFSP_PL[runner], base * df$BFSP_PL[runner])
    new_bank <- new_bank + df$Profit[runner]
    netProfit <- new_bank - bank
    
    df$Bank[runner] <- new_bank
    df$Net_Profit[runner] <- netProfit
    
  }
  return(df)
  
}

profitV30 <- bbsq(ukTest_v30, 10.0, 750.00, 0.25)

profitV30

View(profitV30)










