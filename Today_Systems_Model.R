library(tidyverse)
library(lubridate)
library(stringi)
library(stringr)
library(caret)
library(earth)
library(nnet)
library(xgboost)
library(randomForest)
library(kernlab)



#bfPLMod <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

bfNNPLMod <- readRDS("Systems_NN_BFSPPL_Model_v50.RDS")

bfRFMod <- readRDS("RF_BFPL_Model_v50.RDS")

bfXGB_PL_Mod <- readRDS("XGB_Linear_Systems_BFPL_Model_v50.RDS")

bfXGBMod <- readRDS("XGB_Systems_Model_Prob_V50.RDS")


todaySQ <- read_csv("Today_All_System_Qualifiers.csv", col_names = T)

summary(todaySQ)

colSums(is.na(todaySQ))

todaySQ$Dist_Range[is.na(todaySQ$Dist_Range)] <- "NH"

colnames(todaySQ)

todaySQ$Handicap <- as.factor(todaySQ$Handicap)
todaySQ$Ratings_Range <- as.factor(todaySQ$Ratings_Range)
todaySQ$Going_Range <- as.factor(todaySQ$Going_Range)
todaySQ$RaceType <- as.factor(todaySQ$RaceType)
todaySQ$Dist_Range <- as.factor(todaySQ$Dist_Range)

#predMARSBFPL <- predict(bfPLMod, newdata = todaySQ, type = "raw")
predNNBFPL <- predict(bfNNPLMod, newdata = todaySQ, type = "raw")
predXGBL_BFPL <- predict(bfXGB_PL_Mod, newdata = todaySQ, type = "raw")
predRF_BFPL <- predict(bfRFMod, newdata = todaySQ, type = "raw")
predXGBBFProb <- predict(bfXGBMod, newdata = todaySQ, type = "prob")

#View(predBFPL)


#todaySQ$Mars_Pred <- predMARSBFPL

todaySQ$NN_Pred <- predNNBFPL

todaySQ$XGB_Pred <- predXGBL_BFPL

todaySQ$RF_Pred <- predRF_BFPL

todaySQ2 <- cbind(todaySQ, predXGBBFProb)

todaySQ2 <-  todaySQ2 %>%
  rename(Win_Prob = WON,
         Lose_Prob = LOST)

todaySQ2


V20_Mod <- readRDS("Final_BFPL_Model_V20.RDS")

V20_Mod_Preds <- predict(V20_Mod, newdata = todaySQ2, type = "raw")

V20_Mod_Preds

V30_Mod <- readRDS("Final_BFPL_Model_V30.RDS")

V30_Mod_Preds <- predict(V30_Mod, newdata = todaySQ2, type = "raw")

V30_Mod_Preds

V50_Mod <- readRDS("Final_BFPL_Model_V50.RDS")

V50_Mod_Preds <- predict(V50_Mod, newdata = todaySQ2, type = "raw")

V50_Mod_Preds

SVM_Mod <- readRDS("SVM_Final_Model_v10.RDS")

SVM_Mod_Preds <- predict(SVM_Mod, newdata = todaySQ2, type = "raw")

MARS_Mod <- readRDS("MARS_Final_Model_v10.RDS")

MARS_Mod_Preds <- predict(MARS_Mod, newdata = todaySQ2, type = "raw")

CUBIST_Mod <- readRDS("Cubist_Final_Mod_V1.RDS")

CUBIST_Mod_Preds <- predict(CUBIST_Mod, newdata = todaySQ2, type = "raw")

todaySQ2$Model_Preds_V20 <- V20_Mod_Preds
todaySQ2$Model_Preds_V30 <- V30_Mod_Preds
todaySQ2$Model_Preds_V50 <- V50_Mod_Preds
todaySQ2$SVM_Model_Preds <- SVM_Mod_Preds
todaySQ2$MARS_Model_Preds <- MARS_Mod_Preds[,1]
todaySQ2$CUBIST_Model_Preds <- CUBIST_Mod_Preds


todaySQ2 <- todaySQ2 %>%
  rename(Model_Win_Prob = Win_Prob) %>%
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Base_Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3,
         V_Models_Avg = (Model_Preds_V20 + Model_Preds_V30 + Model_Preds_V50)/3,
         Final_Models_Avg = (Model_Preds_V20 + Model_Preds_V30 + Model_Preds_V50 + SVM_Model_Preds +
                               MARS_Model_Preds + CUBIST_Model_Preds)/6,
         All_Models_Avg = (Model_Preds_V20 + Model_Preds_V30 + Model_Preds_V50 + SVM_Model_Preds + MARS_Model_Preds +
                             CUBIST_Model_Preds + XGB_Pred + NN_Pred + RF_Pred)/9) %>%
  arrange(Time24Hour, Meeting, Horse)

posModels <- todaySQ2 %>%
  mutate(All_Model_Pos = if_else(All_Models_Avg > 0, 1, 0),
         Final_Model_Pos = if_else(Final_Models_Avg > 0, 1, 0),
         V_Model_Pos = if_else(V_Models_Avg > 0, 1, 0),
         Base_Model_Pos = if_else(Base_Models_Avg > 0, 1, 0),
         SVM_Model_Pos = if_else(SVM_Model_Preds > 0, 1, 0),
         MARS_Model_Pos = if_else(MARS_Model_Preds > 0, 1, 0),
         CUBIST_Model_Pos = if_else(CUBIST_Model_Preds > 0, 1, 0),
         V50_Model_Pos = if_else(Model_Preds_V50 > 0, 1, 0),
         V30_Model_Pos = if_else(Model_Preds_V30 > 0, 1, 0),
         V20_Model_Pos = if_else(Model_Preds_V20 > 0, 1, 0),
         XGB_Model_Pos = if_else(XGB_Pred > 0, 1, 0),
         NN_Model_Pos = if_else(NN_Pred > 0, 1, 0),
         RF_Model_Pos = if_else(RF_Pred > 0, 1, 0)) %>%
  select(All_Model_Pos, Final_Model_Pos, V_Model_Pos, Base_Model_Pos, SVM_Model_Pos, MARS_Model_Pos, CUBIST_Model_Pos,
         V50_Model_Pos, V30_Model_Pos, V20_Model_Pos, XGB_Model_Pos, NN_Model_Pos, RF_Model_Pos)


posModels$Model_Score = apply(posModels, MARGIN = 1, FUN = sum)

todaySQ2$Model_Score = posModels$Model_Score

todaySQ2 <- todaySQ2 %>%
  filter(Model_Score > 0)



todaySQ2ModelQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Model_Score, All_Models_Avg, Final_Models_Avg, V_Models_Avg, Base_Models_Avg, SVM_Model_Preds,
         MARS_Model_Preds, CUBIST_Model_Preds, Model_Preds_V50, Model_Preds_V30, Model_Preds_V20, XGB_Pred, NN_Pred, RF_Pred, Model_Value_Odds,
         Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>%
  filter(V_Models_Avg > 0) %>%
  arrange(Time24Hour, Meeting, Horse)

todaySQ2ModelQuals

View(todaySQ2ModelQuals)


write_csv(todaySQ2ModelQuals, paste0("Today_Model_Sys_Quals_", Sys.Date(), ".csv"))

todaySQ2All <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Model_Score, All_Models_Avg, Final_Models_Avg, V_Models_Avg, Base_Models_Avg, SVM_Model_Preds,
         MARS_Model_Preds, CUBIST_Model_Preds, Model_Preds_V50, Model_Preds_V30, Model_Preds_V20, XGB_Pred, NN_Pred, RF_Pred, Model_Value_Odds,
         Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>%
  arrange(Time24Hour, Meeting, Horse)


View(todaySQ2All)

write_csv(todaySQ2All, paste0("Today_Model_Sys_Ratings_", Sys.Date(), ".csv"))

#
# todaySQ2Hcp <- todaySQ2 %>%
#   rename(Model_Win_Prob = Win_Prob) %>%
#   mutate(Model_Value_Odds = 1/Model_Win_Prob,
#          Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
#          Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3,
#          V_Models_Avg = (Model_Preds_V20 + Model_Preds_V30)/2) %>%
#   select(Time24Hour, Meeting, Horse, System_Name, Model_Preds_V20, Model_Preds_V30, V_Models_Avg,XGB_Pred, NN_Pred, RF_Pred,
#          Models_Avg, Model_Value_Odds, Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>%
#   filter(Handicap != "NONHCP", (Model_Preds_V20 > 0 | Model_Preds_V30 > 0)) %>%
#   arrange(Time24Hour, Meeting, Horse)
#
#
# write_csv(todaySQ2Hcp, paste0("Today_Model_Hcp_Ratings_", today$Date[1], ".csv"))

todaySQ2Elite <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Model_Score, All_Models_Avg, Final_Models_Avg, V_Models_Avg, Base_Models_Avg, SVM_Model_Preds,
         MARS_Model_Preds, CUBIST_Model_Preds, Model_Preds_V50, Model_Preds_V30, Model_Preds_V20, XGB_Pred, NN_Pred, RF_Pred, Model_Value_Odds,
         Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>%
  filter(Base_Models_Avg > 0, Final_Models_Avg > 0) %>%
  arrange(Time24Hour, Meeting, Horse)

todaySQ2Elite

View(todaySQ2Elite)

write_csv(todaySQ2Elite, paste0("Today_Elite_Model_Quals_", Sys.Date(), ".csv"))


todaySQ2DualAvg <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Model_Score, All_Models_Avg, Final_Models_Avg, V_Models_Avg, Base_Models_Avg, SVM_Model_Preds,
         MARS_Model_Preds, CUBIST_Model_Preds, Model_Preds_V50, Model_Preds_V30, Model_Preds_V20, XGB_Pred, NN_Pred, RF_Pred, Model_Value_Odds,
         Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>%
  filter(V_Models_Avg > 0, Base_Models_Avg > 0) %>%
  arrange(Time24Hour, Meeting, Horse)

todaySQ2DualAvg

View(todaySQ2DualAvg)

write_csv(todaySQ2DualAvg, paste0("Today_DualAvg_Model_Quals_", Sys.Date(), ".csv"))


todayModVal <- todaySQ2 %>%
  mutate(UKVO_v_MVO = ValueOdds_BetfairFormat/Model_Value_Odds) %>%
  select(Time24Hour, Meeting, Horse, System_Name, UKVO_v_MVO, Model_Value_Odds, ValueOdds_BetfairFormat, BetFairSPForecastWinPrice,
         Model_Score, All_Models_Avg, Final_Models_Avg, V_Models_Avg, Base_Models_Avg, Handicap) %>%
  filter(UKVO_v_MVO >= 1.25) %>%
  arrange(Time24Hour, Meeting, Horse)

View(todayModVal)










