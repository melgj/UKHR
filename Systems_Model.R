library(tidyverse)
library(lubridate)
library(caret)
library(stringi)
library(stringr)


quals <- read_csv("All_System_Qualifiers_to_2018_09.csv", col_names = T)

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


qualsData <- quals %>% 
  select(BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Placed_AE_Ratio, Placed_Archie,
         Btn_AE_Ratio, WinPercent, meanPL, totalPL, VSP_ROI, Place_Percent, BF_Place_ROI, RaceType, Handicap, Going_Range,
         Ratings_Range, Dist_Range, Rev_Weight_Rank, NumberOfResults, Age, BFSP_PL)

##############################################

library(earth)

library(doMC)

registerDoMC(4)

set.seed(200)

ukTrainRows <- createDataPartition(qualsData$BFSP_PL, p = 0.7, list = FALSE)

ukTrainSet <- qualsData[ukTrainRows,]

ukTestSet <- qualsData[-ukTrainRows,]

marsGrid <- expand.grid(.degree = 1:2, .nprune = 10:20)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

set.seed(100)                              

bfPLMod <- train(BFSP_PL ~ ., 
                   data = qualsData,
                   method = "earth",
                   tuneGrid = marsGrid,
                   metric = "RMSE",
                   trControl = train.control)

print(bfPLMod)

varImp(bfPLMod)

saveRDS(bfPLMod, "Systems_MARS_BFSPPL_Model.RDS")

readRDS("Systems_MARS_BFSPPL_Model.RDS")

predBFPL <- predict(bfPLMod, newdata = ukTestSet, type = "raw")

head(predBFPL)
head(ukTestSet$BFSP_PL)

R2(predBFPL, ukTestSet$BFSP_PL)
RMSE(predBFPL, ukTestSet$BFSP_PL)
cor(predBFPL, ukTestSet$BFSP_PL)

ukTestSet$PredPL <- predBFPL

ukPos <- filter(ukTestSet, PredPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, PredPL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(PredPL > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))

###############################################################

predBFPL <- predict(bfPLMod, newdata = qualsData, type = "raw")

head(predBFPL)
head(qualsData$BFSP_PL)

R2(predBFPL, qualsData$BFSP_PL)
RMSE(predBFPL, qualsData$BFSP_PL)
cor(predBFPL, qualsData$BFSP_PL)

qualsData$PredPL <- predBFPL

ukPos <- filter(qualsData, PredPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(qualsData, PredPL <= 0)
mean(ukNeg$BFSP_PL)

mean(qualsData$BFSP_PL)

qualsData %>% 
  group_by(Handicap) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL))




#####################################################################



