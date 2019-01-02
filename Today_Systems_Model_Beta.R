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

bfNNPLMod <- readRDS("Systems_NN_BFSPPL_Model_p1.RDS")

bfRFMod <- readRDS("RF_BFPL_Model_p1.RDS")

bfXGB_PL_Mod <- readRDS("XGB_Linear_Systems_BFPL_Model_p1.RDS")

bfPLS_PL_Mod <- readRDS("PLS_BFPL_Model_p1.RDS")

bfCUB_PL_Mod <- readRDS("Cubist_BFPL_Model_p1.RDS")

bfSVMMod <- readRDS("SVM_BFPL_Model_p1.RDS")




todaySQ <- read_csv("Today_All_System_Qualifiers.csv", col_names = T)

summary(todaySQ)

colSums(is.na(todaySQ))

todaySQ$Dist_Range[is.na(todaySQ$Dist_Range)] <- "NH"

colnames(todaySQ)

flat <- c("AW", "FLAT")

todaySQ$RaceCode <- if_else(todaySQ$RaceType %in% flat, "FLAT", "NH")

todaySQ$Handicap <- as.factor(todaySQ$Handicap)
todaySQ$Ratings_Range <- as.factor(todaySQ$Ratings_Range)
todaySQ$Going_Range <- as.factor(todaySQ$Going_Range)
todaySQ$RaceType <- as.factor(todaySQ$RaceType)
todaySQ$Dist_Range <- as.factor(todaySQ$Dist_Range)
todaySQ$RaceCode <- as.factor(todaySQ$RaceCode)

#predMARSBFPL <- predict(bfPLMod, newdata = todaySQ, type = "raw")
predNNBFPL <- predict(bfNNPLMod, newdata = todaySQ, type = "raw")
predXGBL_BFPL <- predict(bfXGB_PL_Mod, newdata = todaySQ, type = "raw")
predRF_BFPL <- predict(bfRFMod, newdata = todaySQ, type = "raw")
predSVM_BFPL <- predict(bfSVMMod, newdata = todaySQ, type = "raw")
predPLS_BFPL <- predict(bfPLS_PL_Mod, newdata = todaySQ, type = "raw")
predCUB_BFPL <- predict(bfCUB_PL_Mod, newdata = todaySQ, type = "raw")
#predXGBBFProb <- predict(bfXGBMod, newdata = todaySQ, type = "prob")

#
todaySQ$predNN <- predNNBFPL

todaySQ$predXGB <- predXGBL_BFPL

todaySQ$predRF <- predRF_BFPL

todaySQ$predSVM <- predSVM_BFPL

todaySQ$predPLS <- predPLS_BFPL

todaySQ$predCUB <- predCUB_BFPL

todaySQ2 <- todaySQ %>%
  mutate(Base_Models_Avg = (predXGB + predNN + predRF + predSVM + predPLS + predCUB)/6) %>%
  arrange(Time24Hour, Meeting, Horse)

XGB_Final_Model <- readRDS("XGB_Final_Ensemble_p1.RDS")

todaySQ2$Final_Place_Model_Preds <- predict(XGB_Final_Model, newdata = todaySQ2, type = "raw")

XGB_Final_Win_Model <- readRDS("Final_XGB_Win_Model.RDS")

todaySQ2$Final_Win_Model_Preds <- predict(XGB_Final_Win_Model, newdata = todaySQ2, type = "raw")

todaySQ2$Final_Models_Avg <- (todaySQ2$Final_Place_Model_Preds + todaySQ2$Final_Win_Model_Preds)/2

todaySQ2ModelQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Models_Avg, Final_Win_Model_Preds, Final_Place_Model_Preds,
         predXGB, predRF, predSVM, predPLS, predNN, Base_Models_Avg, Handicap, Ratings_Range, everything()) %>%
  filter(predRF >= 0.25 | predXGB >= 0.25 | Final_Place_Model_Preds >= 0.25) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Models_Avg))

todaySQ2ModelQuals

View(todaySQ2ModelQuals)


write_csv(todaySQ2ModelQuals, paste0("Today_Model_Sys_Quals_", Sys.Date(), ".csv"))


todaySQ2ModelEliteQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Models_Avg, Final_Win_Model_Preds, Final_Place_Model_Preds,
         predXGB, predRF, predSVM, predPLS, predNN, Base_Models_Avg, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Place_Model_Preds >= 0.25, predXGB >= 0.25, predRF >= 0.25) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Models_Avg))


todaySQ2ModelEliteQuals

View(todaySQ2ModelEliteQuals)

write_csv(todaySQ2ModelEliteQuals, paste0("Today_Model_Elite_Quals_", Sys.Date(), ".csv"))


todaySQ2DualQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Models_Avg, Final_Win_Model_Preds, Final_Place_Model_Preds,
         predXGB, predRF, predSVM, predPLS, predNN, Base_Models_Avg, Handicap, Ratings_Range, everything()) %>%
  filter(predXGB >= 0.25, predRF >= 0.25)

View(todaySQ2DualQuals)

write_csv(todaySQ2DualQuals, paste0("Today_Model_Dual_Quals_", Sys.Date(), ".csv"))

todaySQ2FinalModelsDQ <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Models_Avg, Final_Win_Model_Preds, Final_Place_Model_Preds,
         predXGB, predRF, predSVM, predPLS, predNN, Base_Models_Avg, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Win_Model_Preds > 0, Final_Place_Model_Preds > 0) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Models_Avg))


todaySQ2FinalModelsDQ

View(todaySQ2FinalModelsDQ)

write_csv(todaySQ2FinalModelsDQ, paste0("Today_Final_Models_DQ_", Sys.Date(), ".csv"))


todaySQ2ModelRatings <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Models_Avg, Final_Win_Model_Preds, Final_Place_Model_Preds,
         predXGB, predRF, predSVM, predPLS, predNN, Base_Models_Avg, Handicap, Ratings_Range, everything()) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Models_Avg))


todaySQ2ModelRatings

View(todaySQ2ModelRatings)

write_csv(todaySQ2ModelRatings, paste0("Today_Model_Ratings_", Sys.Date(), ".csv"))


