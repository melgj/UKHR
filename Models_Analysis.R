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


quals <- read_csv("Nov18_System_Quals_All_Cleaned.csv", col_names = T)

colnames(quals)

colSums(is.na(quals))

quals$Dist_Range[is.na(quals$Dist_Range)] <- "NH"

quals <- quals %>%
  drop_na(BFSP_PL)

#quals <- quals %>%
  #select(-c(Trainer, Jockey, Sire, Alarms))

#colSums(is.na(quals))

#quals <- na.omit(quals)

str(quals)

quals$Handicap <- as.factor(quals$Handicap)
quals$Ratings_Range <- as.factor(quals$Ratings_Range)
quals$Going_Range <- as.factor(quals$Going_Range)
quals$RaceType <- as.factor(quals$RaceType)
quals$Dist_Range <- as.factor(quals$Dist_Range)
quals$System_Name <- as.factor(quals$System_Name)

colnames(quals)


qualsData <- quals %>%
  select(Time24Hour, Meeting, Horse, BetFairSPForecastWinPrice, System_Name, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, Betfair.Win.S.P., BFSP_PL, everything())

colSums(is.na(qualsData))

colnames(qualsData)

nn <- readRDS("Systems_NN_BFSPPL_Model_v50.RDS")
xgb <- readRDS("XGB_Linear_Systems_BFPL_Model_v50.RDS")
rf <- readRDS("RF_BFPL_Model_v50.RDS")
ridge <- readRDS("Ridge_BFPL_Model.RDS")
pls <- readRDS("PLS_BFPL_Model.RDS")
#mars <- readRDS("Systems_MARS_BFSPPL_Model.RDS")



colnames(qualsData)



# set.seed(100)
#
# ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)
#
# ukTrainSet <- qualsData[ukTrainRows, -2]
#
# ukTestSet <- qualsData[-ukTrainRows, -2]
#
# mean(ukTrainSet$BFSP_PL)
# mean(ukTestSet$BFSP_PL)




predNN <- predict(nn, newdata = qualsData, type = "raw")
predXGB <- predict(xgb, newdata = qualsData, type = "raw")
predRF <- predict(rf, newdata = qualsData, type = "raw")
predRidge <- predict(ridge, newdata = qualsData, type = "raw")
predPLS <- predict(pls, newdata = qualsData, type = "raw")
#predMars <- predict(mars, newdata = ukTestSet, type = "raw")

predDF <- tibble(NN = predNN,XGB = predXGB, RF = predRF, RIDGE = predRidge, PLS = predPLS,
                 BFPL = qualsData$BFSP_PL)

predDF$Result <- if_else(predDF$BFPL > 0, 1, 0)

head(predDF)

cor(predDF)

# winPcnt <- predDF %>%
#   group_by(Result) %>%
#   summarise_all(.funs = (Win_Percent = mean))
#
#
# winPcnt

# cor(predMars, predXGB)
# cor(predMars, predRF)
# cor(predMars, predNN)

qualsData$NN_Pred <- predNN
qualsData$XGB_Pred <- predXGB
qualsData$RF_Pred <- predRF
qualsData$RIDGE_Pred <- predRidge
qualsData$PLS_Pred <- predPLS
#ukTestSet$Mars_Pred <- predMars

summary(qualsData)

BFSP_PL = BFSP_PL

qualsData %>%
  group_by() %>%
  filter(NN_Pred > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData %>%
  group_by() %>%
  filter(XGB_Pred > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData %>%
  group_by() %>%
  filter(RF_Pred > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData %>%
  group_by() %>%
  filter(PLS_Pred > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))


qualsData %>%
  group_by() %>%
  filter(RIDGE_Pred > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))


qualsData$Model_Avg = (qualsData$RF_Pred + qualsData$XGB_Pred + qualsData$NN_Pred)/3

qualsData %>%
  group_by() %>%
  filter(Model_Avg > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
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

xgbProb <- readRDS("XGB_Systems_Model_Prob_V50.RDS")

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
modV50 <- readRDS("Final_BFPL_Model_V50.RDS")
modMars <- readRDS("MARS_Final_Model_v10.RDS")
modSVM <- readRDS("SVM_Final_Model_v10.RDS")
modCUBIST <- readRDS("Cubist_Final_Mod_V1.RDS")


# set.seed(100)
#
# ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)
#
# ukTrainSet <- qualsData2[ukTrainRows, -2]
#
# ukTestSet <- qualsData2[-ukTrainRows, -2]



predV20 <- predict(modV20, newdata = qualsData2, type = "raw")
predV30 <- predict(modV30, newdata = qualsData2, type = "raw")
predV50 <- predict(modV50, newdata = qualsData2, type = "raw")
predMars <- predict(modMars, newdata = qualsData2, type = "raw")
predSVM <- predict(modSVM, newdata = qualsData2, type = "raw")
predCUBIST <- predict(modCUBIST, newdata = qualsData2, type = "raw")

qualsData2 <- qualsData2 %>%
  mutate(v20 = predV20,
         v30 = predV30,
         v50 = predV50,
         Mars = predMars[,1],
         SVM = predSVM,
         CUBIST = predCUBIST)

qualsData2 <- qualsData2 %>%
  mutate(Model_Avg = (RF_Pred + NN_Pred + XGB_Pred)/3,
         V_Model_Avg = (v20 + v30 + v50)/3)

colnames(qualsData2)

# modelPreds <- ukTestSet[, 23:33]
#
# modelPreds$Result <- if_else(modelPreds$BFSP_PL > 0, 1, 0)
#
# colnames(modelPreds)
#
# cor(modelPreds)
#
# winPcnt <- modelPreds %>%
#   group_by(Result) %>%
#   summarise_all(.funs = (Win_Percent = mean))
#
# modelPreds %>%
#   filter(v30 > 0, Result == 1) %>%
#   summarise(Avg_WBFSP = mean(BFSP_PL) + 1,
#             Median_WBFSP = median(BFSP_PL),
#             Max_WBFSP = max(BFSP_PL),
#             Min_WBFSP = min(BFSP_PL))
#
#
# winPcnt

######################################################################

qualsData2$Model_Odds <- 1/qualsData2$Win_Prob

qualsData2$Model_Val_Ratio <- qualsData2$BetFairSPForecastWinPrice / qualsData2$Model_Odds

# qualsData2 <- qualsData2 %>%
#   filter(Month == 11)

colnames(qualsData2)

# qualsData2 %>%
#   group_by() %>%
#   #filter() %>%
#   summarise(Runs = n(),
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>%
#   arrange(desc(AE_Ratio))
#
# table(qualsData2$Month)

qualsData2 %>%
  group_by() %>%
  filter(Model_Avg > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(V_Model_Avg > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(V_Model_Avg > 0, Model_Avg > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(XGB_Pred > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(NN_Pred > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(RF_Pred > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(v20 > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(v30 > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(v50 > 0.0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(SVM > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(Mars > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 <- qualsData2 %>%
  mutate(Final_Models_Avg = (SVM + Mars + v20 + v30 + v50)/5)

qualsData2 %>%
  group_by() %>%
  filter(Final_Models_Avg > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(Final_Models_Avg > 0, Model_Avg > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))

qualsData2 %>%
  group_by() %>%
  filter(CUBIST > 0) %>%
  mutate(Won = if_else(BFSP_PL > 0, 1, 0)) %>%
  summarise(Runs = n(),
            Winners = sum(Won),
            WinPercent = mean(Won),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>%
  arrange(desc(Avg_PL))




colnames(qualsData2)

write_csv(qualsData2, "Nov18_Models_PL_Results.csv")


#################################################################################################



ukTest_v50 <- filter(qualsData2, v50 > 0)

ukTest_v50 <- ukTest_v50 %>%
  arrange(Time24Hour, Meeting, Horse, desc(v50))

ukTest_v50 <- distinct(ukTest_v50, Time24Hour, Meeting, Horse, .keep_all = T)

colnames(ukTest_v50)

baseBet = 2.0
bank = 100

ukTest_v50$Bank = bank

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

profitV50 <- bbsq(ukTest_v50, 10.0, 750.00, 0.25)

profitV50

profitV50$Net_Profit[374]

View(profitV50)
#####################################################################

#November 2018

quals <- read_csv("All_System_Qualifiers_Yr_2018.csv", col_names = T)

table(quals$RaceType, quals$System_Name)

bugSystems <- c("Poly_Meeting_Sires", "Poly_Meeting_Trainers", "Polytrack_Sires", "Southwell_Trainers")
bugRT <- c("CHASE", "FLAT", "HURDLE")

bugQuals <- quals %>%
  filter(RaceType %in% bugRT, System_Name %in% bugSystems)

bugQuals %>%
  group_by(System_Name) %>%
  summarise(Runs = n(),
            Total_PL = sum(BFSP_PL),
            Avg_PL = mean(BFSP_PL))

table(bugQuals$RaceType, bugQuals$System_Name)

bugIndex <- which(quals$System_Name %in% bugSystems & quals$RaceType %in% bugRT)

qualsCleaned <- quals[-bugIndex,]

table(qualsCleaned$RaceType, qualsCleaned$System_Name)

quals <- qualsCleaned

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
quals$Month <- as.factor(quals$Month)

qualsData <- quals %>%
  select(Month, BetFairSPForecastWinPrice, System_Name, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, BFSP_PL)

qualsData %>%
  filter(Month == 11) %>%
  summarise(Runs = n(),
            BFPL_Avg = mean(BFSP_PL),
            BFPL_Total = sum(BFSP_PL))

qualsData <- qualsData %>%
  filter(Month == 11)

set.seed(300)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows, ]

ukTestSet <- qualsData[-ukTrainRows, ]

mean(ukTestSet$BFSP_PL)
mean(ukTrainSet$BFSP_PL)






