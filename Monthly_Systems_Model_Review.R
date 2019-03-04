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




monthAllQ <- read_csv("Today_All_System_Qualifiers.csv", col_names = T)

ukhr <- read_csv("UKHR_Master_BF_2018_12_31.csv")

dec18 <- ukhr %>%
  filter(Month == 12, Year == 2018)

colnames(monthAllQ)

monthSQ <- dec18 %>%
  full_join(monthAllQ, by = c("Time24Hour", "Meeting", "Horse", "Trainer", "Jockey", "NumberOfResults", "Furlongs")) %>%
  filter(!is.na(System_Name))

colnames(monthSQ) <- str_replace_all(colnames(monthSQ), ".x", "")

summary(monthSQ)

colSums(is.na(monthSQ))

monthSQ$Dist_Range[is.na(monthSQ$Dist_Range)] <- "NH"

colnames(monthSQ)

flat <- c("AW", "FLAT")

monthSQ$RaceCode <- if_else(monthSQ$RaceType %in% flat, "FLAT", "NH")

monthSQ$Handicap <- as.factor(monthSQ$Handicap)
monthSQ$Ratings_Range <- as.factor(monthSQ$Ratings_Range)
monthSQ$Going_Range <- as.factor(monthSQ$Going_Range)
monthSQ$RaceType <- as.factor(monthSQ$RaceType)
monthSQ$Dist_Range <- as.factor(monthSQ$Dist_Range)
monthSQ$RaceCode <- as.factor(monthSQ$RaceCode)

colSums(is.na(monthSQ))

monthSQ <- monthSQ %>%
  drop_na(Betfair.Win.S.P.)


#predMARSBFPL <- predict(bfPLMod, newdata = monthSQ, type = "raw")
predNNBFPL <- predict(bfNNPLMod, newdata = monthSQ, type = "raw")
predXGBL_BFPL <- predict(bfXGB_PL_Mod, newdata = monthSQ, type = "raw")
predRF_BFPL <- predict(bfRFMod, newdata = monthSQ, type = "raw")
predSVM_BFPL <- predict(bfSVMMod, newdata = monthSQ, type = "raw")
predPLS_BFPL <- predict(bfPLS_PL_Mod, newdata = monthSQ, type = "raw")
predCUB_BFPL <- predict(bfCUB_PL_Mod, newdata = monthSQ, type = "raw")
#predXGBBFProb <- predict(bfXGBMod, newdata = monthSQ, type = "prob")

#
monthSQ$predNN <- predNNBFPL

monthSQ$predXGB <- predXGBL_BFPL

monthSQ$predRF <- predRF_BFPL

monthSQ$predSVM <- predSVM_BFPL

monthSQ$predPLS <- predPLS_BFPL

monthSQ$predCUB <- predCUB_BFPL

monthSQ2 <- monthSQ %>%
  mutate(Base_Models_Avg = (predXGB + predNN + predRF + predSVM + predPLS + predCUB)/6) %>%
  arrange(Time24Hour, Meeting, Horse)

XGB_Final_Model <- readRDS("Final_XGB_Mod_Int_m3.RDS")

monthSQ2$Final_Place_Model_Preds <- predict(XGB_Final_Model, newdata = monthSQ2, type = "raw")

XGB_Final_Win_Model <- readRDS("Final_XGB_Win_Model.RDS")

monthSQ2$Final_Win_Model_Preds <- predict(XGB_Final_Win_Model, newdata = monthSQ2, type = "raw")

Final_Linear_Model <- readRDS("Final_Linear_Mod_m3.RDS")

monthSQ2$Final_Linear_Model <- predict(Final_Linear_Model, newdata = monthSQ2, type = "raw")

Final_SVM_Model <- readRDS("Final_SVM_Mod_Int_m3.RDS")

monthSQ2$Final_SVM_Model <- predict(Final_SVM_Model, newdata = monthSQ2, type = "raw")

Final_GAM_Model <- readRDS("Final_GAM_Mod_m3.RDS")

monthSQ2$Final_GAM_Model <- predict(Final_GAM_Model, newdata = monthSQ2, type = "raw")


monthSQ2ModelQuals <- monthSQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Linear_Model > 0 | Final_GAM_Model > 0 | predXGB >= 0.10 | predRF >= 0.10 | predCUB >= 0.10) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))

monthSQ2ModelQuals

#View(monthSQ2ModelQuals)


write_csv(monthSQ2ModelQuals, paste0("Dec18_Model_Sys_Quals_", Sys.Date(), ".csv"))


monthSQ2ModelEliteQuals <- monthSQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Linear_Model > 0, Final_GAM_Model > 0, predXGB > 0.10, predRF > 0.30, predCUB > 0.10) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))


monthSQ2ModelEliteQuals

#View(monthSQ2ModelEliteQuals)

write_csv(monthSQ2ModelEliteQuals, paste0("Dec18_Model_Elite_Quals_", Sys.Date(), ".csv"))


monthSQ2DualQuals <- monthSQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(predXGB >= 0.20, predRF >= 0.30)

#View(monthSQ2DualQuals)

write_csv(monthSQ2DualQuals, paste0("Dec18_Model_Dual_Quals_", Sys.Date(), ".csv"))

monthSQ2FinalModelsDQ <- monthSQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
         predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Linear_Model > 0, Final_GAM_Model > 0) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))


monthSQ2FinalModelsDQ

#View(monthSQ2FinalModelsDQ)

write_csv(monthSQ2FinalModelsDQ, paste0("Dec18_Final_Models_DQ_", Sys.Date(), ".csv"))


monthSQ2ModelRatings <- monthSQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Linear_Model, Final_GAM_Model, Final_SVM_Model, Final_Place_Model_Preds,
          predXGB, predRF, predCUB, Handicap, Ratings_Range, everything()) %>%
  arrange(Time24Hour, Meeting, Horse, desc(Final_Linear_Model))


monthSQ2ModelRatings

#View(monthSQ2ModelRatings)

write_csv(monthSQ2ModelRatings, paste0("Dec18_Model_Ratings_", Sys.Date(), ".csv"))


