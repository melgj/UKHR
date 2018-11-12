# library(tidyverse)
# library(lubridate)
library(caret)
library(earth)
library(nnet)
# library(stringi)
# library(stringr)

bfPLMod <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

bfNNPLMod <- readRDS("Systems_NN_BFSPPL_Model.RDS")

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

predMARSBFPL <- predict(bfPLMod, newdata = todaySQ, type = "raw")
predNNBFPL <- predict(bfNNPLMod, newdata = todaySQ, type = "raw")
predXGBBFPL <- predict(bfXGBMod, newdata = todaySQ, type = "prob")

#View(predBFPL)


todaySQ$PredMARSPL <- predMARSBFPL

todaySQ$PredNNPL <- predNNBFPL

todaySQ2 <- cbind(todaySQ, predXGBBFPL)

#head(todaySQ$PredMARSPL)
#head(todaySQ$PredNNPL)

summary(todaySQ2)

write_csv(todaySQ2, paste0("Today_Sys_Quals_", today$Date[1], ".csv"))
