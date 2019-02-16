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

bfNNPLMod <- readRDS("Systems_NN_BFSPPL_Model_m3.RDS")

bfRFMod <- readRDS("RF_BFPL_Model_m3.RDS")

bfXGB_PL_Mod <- readRDS("XGB_Linear_Systems_BFPL_Model_m3.RDS")

bfPLS_PL_Mod <- readRDS("PLS_BFPL_Model_m3.RDS")

bfCUB_PL_Mod <- readRDS("Cubist_BFPL_Model_m3.RDS")

bfSVMMod <- readRDS("SVM_BFPL_Model_m3.RDS")

bfGAMMod <- readRDS("Gam_BFPL_Model_m3.RDS")




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
predGAM_BFPL <- predict(bfGAMMod, newdata = todaySQ, type = "raw")
#predXGBBFProb <- predict(bfXGBMod, newdata = todaySQ, type = "prob")

#
todaySQ$predNN <- predNNBFPL

todaySQ$predXGB <- predXGBL_BFPL

todaySQ$predRF <- predRF_BFPL

todaySQ$predSVM <- predSVM_BFPL

todaySQ$predPLS <- predPLS_BFPL

todaySQ$predCUB <- predCUB_BFPL

todaySQ$predGAM <- predGAM_BFPL

todaySQ2 <- todaySQ %>%
  mutate(Base_Models_Avg = (predXGB + predNN + predRF + predSVM + predPLS + predCUB + predGAM)/7) %>%
  arrange(Time24Hour, Meeting, Horse)

XGB_Final_Model <- readRDS("Final_XGB_Mod_Int_m3.RDS")

todaySQ2$Final_Place_Model_Preds <- predict(XGB_Final_Model, newdata = todaySQ2, type = "raw")

XGB_Final_Win_Model <- readRDS("Final_XGB_Win_Model.RDS")

todaySQ2$Final_Win_Model_Preds <- predict(XGB_Final_Win_Model, newdata = todaySQ2, type = "raw")

Final_Linear_Model <- readRDS("Final_Linear_Mod_m5.RDS")

todaySQ2$Final_Linear_Model <- predict(Final_Linear_Model, newdata = todaySQ2, type = "raw")

Final_SVM_Model <- readRDS("Final_SVM_Mod_Int_m5.RDS")

todaySQ2$Final_SVM_Model <- predict(Final_SVM_Model, newdata = todaySQ2, type = "raw")

Final_GAM_Model <- readRDS("Final_GAM_Mod_m5.RDS")

todaySQ2$Final_GAM_Model <- predict(Final_GAM_Model, newdata = todaySQ2, type = "raw")


todaySQ2ModelQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Linear_Model > 0.0 | Final_GAM_Model >= 0.10 | Final_SVM_Model >= 0.10 | predXGB >= 0.05 | predRF >= 0.10 | predCUB >= 0.10) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))

todaySQ2ModelQuals

View(todaySQ2ModelQuals)


write_csv(todaySQ2ModelQuals, paste0("Today_Model_Sys_Quals_", Sys.Date(), ".csv"))


todaySQ2ModelEliteQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Linear_Model > 0.0, Final_GAM_Model >= 0.10, predXGB >= 0.05, predRF >= 0.10, predCUB >= 0.10) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))


todaySQ2ModelEliteQuals

View(todaySQ2ModelEliteQuals)

write_csv(todaySQ2ModelEliteQuals, paste0("Today_Model_Elite_Quals_", Sys.Date(), ".csv"))


todaySQ2DualQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(predXGB >= 0.05, predRF >= 0.10)

View(todaySQ2DualQuals)

write_csv(todaySQ2DualQuals, paste0("Today_Model_Dual_Quals_", Sys.Date(), ".csv"))

todaySQ2FinalModelsDQ <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Linear_Model > 0.0, Final_GAM_Model >= 0.10) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))


todaySQ2FinalModelsDQ

View(todaySQ2FinalModelsDQ)

write_csv(todaySQ2FinalModelsDQ, paste0("Today_Final_Models_DQ_", Sys.Date(), ".csv"))


todaySQ2ModelRatings <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
          predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))


todaySQ2ModelRatings

#View(todaySQ2ModelRatings)

write_csv(todaySQ2ModelRatings, paste0("Today_Model_Ratings_", Sys.Date(), ".csv"))


