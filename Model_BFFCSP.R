#setwd("~/git_projects/UKHR_Project")

library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(doMC)
library(earth)
library(caret)

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_09_30.csv", col_names = T)

ukhr_master_BF$Handicap <- as.factor(ukhr_master_BF$Handicap)
ukhr_master_BF$RaceType <- as.factor(ukhr_master_BF$RaceType)

colnames(ukhr_master_BF)

summary(ukhr_master_BF$Month)

BfFcSP <- ukhr_master_BF %>% 
  filter(Year >= 2018, Month >= 4) %>% 
  select(UKHR_RaceID, RaceType, BetFairSPForecastWinPrice, ValueOdds_Probability, Runners, Handicap, ConnAdvantage, RatingAdvantage, 
         RatingsPosition, ConnRanking, Betfair.Win.S.P.) %>% 
  drop_na(Betfair.Win.S.P.)

#remove(ukhr_master_BF)



########################################################
registerDoMC(4)

set.seed(100)

ukTrainRows <- createDataPartition(BfFcSP$Betfair.Win.S.P., p = 0.6, list = FALSE)

ukTrainSet <- BfFcSP[ukTrainRows,]

ukTestSet <- BfFcSP[-ukTrainRows,]

marsGrid <- expand.grid(.degree = 1:2, .nprune = 5:10)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)

#marsGrid


set.seed(100)                              

marsModUK <- train(Betfair.Win.S.P. ~ (ValueOdds_Probability + Runners + RatingAdvantage + Handicap +
                                         ConnAdvantage + ConnRanking + RatingsPosition)^2, 
                    data = ukTrainSet,
                    method = "earth",
                    tuneGrid = marsGrid,
                    metric = "RMSE",
                    trControl = train.control)

print(marsModUK)

varImp(marsModUK)

predBFSPMARS <- predict(marsModUK, newdata = ukTestSet, type = "raw")

head(predBFSPMARS)
head(ukTestSet$Betfair.Win.S.P.)

R2(predBFSPMARS, ukTestSet$Betfair.Win.S.P.)
RMSE(predBFSPMARS, ukTestSet$Betfair.Win.S.P.)
cor(predBFSPMARS, ukTestSet$Betfair.Win.S.P.)

###############################################################

ukhr_master_BF2 <- ukhr_master_BF %>% 
  select(BetFairSPForecastWinPrice, Betfair.Win.S.P.) %>% 
  drop_na(BetFairSPForecastWinPrice, Betfair.Win.S.P.)

cor(ukhr_master_BF2$BetFairSPForecastWinPrice, ukhr_master_BF2$Betfair.Win.S.P.)
RMSE(ukhr_master_BF2$BetFairSPForecastWinPrice, ukhr_master_BF2$Betfair.Win.S.P.)
R2(ukhr_master_BF2$BetFairSPForecastWinPrice, ukhr_master_BF2$Betfair.Win.S.P.)


predBFSPMARS <- predict(marsModUK, newdata = today, type = "raw")

# today$Adj_BFSPFC <- predBFSPMARS
# 
# today_BFSPFC <- select(today, Time24Hour, Meeting, Horse, BetFairSPForecastWinPrice, Adj_BFSPFC, ValueOdds_BetfairFormat)
# 
# write_csv(today_BFSPFC, "Adjusted_BFSP.csv")


  
numUK <- ukhr_master_BF %>% 
  select_if(is.numeric) %>% 
  select(matches("Rank|Adv|Rating|Betf|BetF|Last|Year|UK")) %>% 
  filter(Year >= 2017)

corVals <- apply(numUK,
                 MARGIN = 2,
                 FUN = function(x,y) cor(x,y),
                 y = numUK$Betfair.Win.S.P.)

sort(abs(corVals))

sort(corVals[corVals >= abs(0.1)])

df <- tibble(names(corVals), corVals)
df <- df %>% 
  arrange(desc(corVals)) %>% 
  filter(corVals >= 0.1)

ukdf <- df[3:20,]
ukdf
