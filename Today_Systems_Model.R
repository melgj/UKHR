library(tidyverse)
library(lubridate)
library(caret)
library(earth)
library(stringi)
library(stringr)

bfPLMod <- readRDS("Systems_MARS_BFSPPL_Model.RDS")

todaySQ <- read_csv(file.choose(), col_names = T)

summary(todaySQ)

colSums(is.na(todaySQ))

todaySQ$Dist_Range[is.na(todaySQ$Dist_Range)] <- "NH"

todaySQ$Handicap <- as.factor(todaySQ$Handicap)
todaySQ$Ratings_Range <- as.factor(todaySQ$Ratings_Range)
todaySQ$Going_Range <- as.factor(todaySQ$Going_Range)
todaySQ$RaceType <- as.factor(todaySQ$RaceType)
todaySQ$Dist_Range <- as.factor(todaySQ$Dist_Range)

predBFPL <- predict(bfPLMod, newdata = todaySQ, type = "raw")

#View(predBFPL)

head(predBFPL)

todaySQ$PredPL <- predBFPL

write_csv(todaySQ, paste0("Today_Sys_Quals_", "2018_10_24", ".csv"))
