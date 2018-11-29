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


Final_Mod <- readRDS("Final_BFPL_Model_V30.RDS")

Mod_Preds <- predict(Final_Mod, newdata = todaySQ2, type = "raw")

Mod_Preds

todaySQ2$Model_Preds <- Mod_Preds

todaySQ2ModelQuals <- todaySQ2 %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, Model_Preds, XGB_Pred, NN_Pred, RF_Pred, Models_Avg, Model_Value_Odds, Model_Val_Ratio, 
         Handicap, Ratings_Range, everything()) %>% 
  arrange(Time24Hour, Meeting, Horse)


write_csv(todaySQ2ModelQuals, paste0("Today_Model_Sys_Quals_", today$Date[1], ".csv"))

todaySQ2All <- todaySQ2 %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, Model_Preds, XGB_Pred, NN_Pred, RF_Pred, Models_Avg, Model_Value_Odds, Model_Val_Ratio, 
         Handicap, Ratings_Range, everything()) %>% 
  arrange(Time24Hour, Meeting, Horse)
  

write_csv(todaySQ2All, paste0("Today_Model_Sys_Ratings_", today$Date[1], ".csv"))

#write_csv(todaySQ2All, "Today_Model_Sys_Quals.csv")


todaySQ2Hcp <- todaySQ2 %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, Model_Preds, XGB_Pred, NN_Pred, RF_Pred, Models_Avg, Model_Value_Odds, Model_Val_Ratio,
         Handicap, Ratings_Range, everything()) %>% 
  filter(Handicap != "NONHCP", Model_Preds > 0) %>% 
  arrange(Time24Hour, Meeting, Horse)


write_csv(todaySQ2Hcp, paste0("Today_Model_Hcp_Ratings_", today$Date[1], ".csv"))

#write_csv(todaySQ2Hcp, "Today_Model_Hcp_Quals.csv")

todaySQ2Elite <- todaySQ2 %>% 
  filter(Model_Preds > 0, XGB_Pred > 0, NN_Pred > 0, RF_Pred > 0) %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, Model_Preds, XGB_Pred, NN_Pred, RF_Pred, Models_Avg, Model_Value_Odds, Model_Val_Ratio,
         Handicap, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio,
         Placed_Archie, Btn_AE_Ratio) %>% 
  arrange(Time24Hour, Meeting, Horse)

todaySQ2Elite

View(todaySQ2Elite)

write_csv(todaySQ2Elite, paste0("Today_Elite_Model_Quals_", today$Date[1], ".csv"))


todaySQ2Trips <- todaySQ2 %>% 
  filter(Model_Preds <= 0, XGB_Pred > 0, NN_Pred > 0, RF_Pred > 0) %>% 
  rename(Model_Win_Prob = Win_Prob) %>% 
  mutate(Model_Value_Odds = 1/Model_Win_Prob,
         Model_Val_Ratio = BetFairSPForecastWinPrice/Model_Value_Odds,
         Models_Avg = (XGB_Pred + NN_Pred + RF_Pred)/3) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, Model_Preds, XGB_Pred, NN_Pred, RF_Pred, Models_Avg, Model_Value_Odds, Model_Val_Ratio,
         Handicap, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio,
         Placed_Archie, Btn_AE_Ratio) %>% 
  arrange(Time24Hour, Meeting, Horse)

todaySQ2Trips

write_csv(todaySQ2Trips, paste0("Today_TriplePos_Model_Quals_", today$Date[1], ".csv"))


#write_csv(todaySQ2Trips, "Today_TriplePos_Model_Quals.csv")
  
