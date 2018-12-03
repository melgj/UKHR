# library(tidyverse)
# library(lubridate)
# library(stringi)
# library(stringr)
library(caret)
library(earth)
library(nnet)
library(xgboost)
library(randomForest)


#bfPLMod <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

bfNNPLMod <- readRDS("Systems_NN_BFSPPL_Model_v20.RDS")

bfRFMod <- readRDS("RF_BFPL_Model_v20.RDS")

bfXGB_PL_Mod <- readRDS("XGB_Linear_Systems_BFPL_Model_v20.RDS")

bfXGBMod <- readRDS("XGB_Systems_Model_Prob")


todaySQ <- read_csv(file.choose(), col_names = T)

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

SVM_Mod <- readRDS("SVM_Final_Model_v10.RDS")

SVM_Mod_Preds <- predict(SVM_Mod, newdata = todaySQ2, type = "raw")

todaySQ2$Model_Preds_V20 <- V20_Mod_Preds
todaySQ2$Model_Preds_V30 <- V30_Mod_Preds
todaySQ2$SVM_Model_Preds <- SVM_Mod_Preds

todaySQ2ModelQuals <- todaySQ2 %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3,
         V_Models_Avg = (Model_Preds_V20 + Model_Preds_V30)/2) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, SVM_Model_Preds, Model_Preds_V30, Model_Preds_V20, V_Models_Avg, XGB_Pred, NN_Pred, RF_Pred, 
         Models_Avg, Model_Value_Odds, Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>% 
  filter(SVM_Model_Preds > 0) %>% 
  arrange(Time24Hour, Meeting, Horse)

todaySQ2ModelQuals

View(todaySQ2ModelQuals)


write_csv(todaySQ2ModelQuals, paste0("Today_Model_Sys_Quals_", today$Date[1], ".csv"))

todaySQ2All <- todaySQ2 %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3,
         V_Models_Avg = (Model_Preds_V20 + Model_Preds_V30)/2) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, SVM_Model_Preds, Model_Preds_V20, Model_Preds_V30, V_Models_Avg,XGB_Pred, NN_Pred, RF_Pred, 
         Models_Avg, Model_Value_Odds, Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>% 
  arrange(Time24Hour, Meeting, Horse)
  

View(todaySQ2All)  

write_csv(todaySQ2All, paste0("Today_Model_Sys_Ratings_", today$Date[1], ".csv"))

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
  filter(SVM_Model_Preds > 0, XGB_Pred > 0, NN_Pred > 0, RF_Pred > 0, Model_Preds_V20 > 0, Model_Preds_V30 > 0) %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3,
         V_Models_Avg = (Model_Preds_V20 + Model_Preds_V30)/2) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, SVM_Model_Preds, Model_Preds_V20, Model_Preds_V30, V_Models_Avg,XGB_Pred, NN_Pred, RF_Pred, 
         Models_Avg, Model_Value_Odds, Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>% 
  arrange(Time24Hour, Meeting, Horse)

todaySQ2Elite

View(todaySQ2Elite)

write_csv(todaySQ2Elite, paste0("Today_Elite_Model_Quals_", today$Date[1], ".csv"))


todaySQ2Trips <- todaySQ2 %>% 
  filter(SVM_Model_Preds <= 0, Model_Preds_V20 <= 0, Model_Preds_V30 <= 0, XGB_Pred > 0, NN_Pred > 0, RF_Pred > 0) %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3,
         V_Models_Avg = (Model_Preds_V20 + Model_Preds_V30)/2) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, SVM_Model_Preds, Model_Preds_V20, Model_Preds_V30, V_Models_Avg,XGB_Pred, NN_Pred, RF_Pred, 
         Models_Avg, Model_Value_Odds, Model_Val_Ratio, Handicap, Ratings_Range, everything()) %>% 
  arrange(Time24Hour, Meeting, Horse)

todaySQ2Trips

View(todaySQ2Trips)

write_csv(todaySQ2Trips, paste0("Today_TriplePos_Model_Quals_", today$Date[1], ".csv"))



  
