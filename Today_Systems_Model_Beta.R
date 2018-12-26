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

todaySQ$RaceCode <- if_else(quals$RaceType %in% flat, "FLAT", "NH")

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

todaySQ2$Final_Model_Preds <- predict(XGB_Final_Model, newdata = todaySQ2, type = "raw")



todaySQ2ModelQuals <- todaySQ2 %>%
  select(Time24Hour, Meeting, Horse, System_Name, Final_Model_Preds, Base_Models_Avg, predXGB, predRF, predSVM, predCUB,
         predPLS, predNN, Handicap, Ratings_Range, everything()) %>%
  filter(Final_Model_Preds > 0) %>%
  arrange(Time24Hour, Meeting, Horse)

todaySQ2ModelQuals

View(todaySQ2ModelQuals)


write_csv(todaySQ2ModelQuals, paste0("Today_Model_Sys_Quals_", Sys.Date(), ".csv"))











