#setwd("~/git_projects/UKHR_Project")

library(earth)
library(caret)

BfFcSp <- ukhr_master_BF %>% 
  select(Year, UKHR_RaceID, RaceType, Handicap, ValueOdds_Probability, Value_Odds_Ratio, Betfair.Win.S.P.) %>% 
  filter(Year >= 2017) %>% 
  drop_na(Value_Odds_Ratio, Value_Odds_Ratio, Betfair.Win.S.P.)

BfFcSp$Handicap <- as.factor(BfFcSp$Handicap)

levels(BfFcSp$Handicap)

colSums(is.na(BfFcSp))


########################################################


set.seed(100)

ukTrainRows <- createDataPartition(BfFcSp$Betfair.Win.S.P., p = 0.5, list = FALSE)

ukTrainSet <- BfFcSp[ukTrainRows,]

ukTestSet <- BfFcSp[-ukTrainRows,]

marsGrid <- expand.grid(.degree = 1:2, .nprune = 10:20)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              #summaryFunction = RMSE,
                              verboseIter = T)


set.seed(100)                              

marsModUK <- train(Betfair.Win.S.P. ~ ((Handicap + ValueOdds_Probability + Value_Odds_Ratio)^2), 
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
  drop_na(BetFairSPForecastWinPrice, Betfair.Win.S.P.)

cor(ukhr_master_BF2$BetFairSPForecastWinPrice, ukhr_master_BF2$Betfair.Win.S.P.)
RMSE(ukhr_master_BF2$BetFairSPForecastWinPrice, ukhr_master_BF2$Betfair.Win.S.P.)
R2(ukhr_master_BF2$BetFairSPForecastWinPrice, ukhr_master_BF2$Betfair.Win.S.P.)


predBFSPMARS <- predict(marsModUK, newdata = today, type = "raw")

today$Adj_BFSPFC <- predBFSPMARS

today_BFSPFC <- select(today, Time24Hour, Meeting, Horse, BetFairSPForecastWinPrice, Adj_BFSPFC, ValueOdds_BetfairFormat)

write_csv(today_BFSPFC, "Adjusted_BFSP.csv")
                       
                       