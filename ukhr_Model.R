
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(caret)
#library(doMC)
library(earth)
library(randomForest)
library(xgboost)

library(doParallel)
cl <- makePSOCKcluster(4) # number of cores to use
registerDoParallel(cl)

## computation




setwd("~/git_projects/ukhr/UKHR")

#registerDoMC(4)
#library(kernlab)
ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_07_31.csv",col_names = T)

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Rating_Rank = min_rank(RatingsPosition))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Ratings_Range = cut(Rating_Rank, 2,
                             labels = c("Top_Half", "Bottom_Half"), 
                             ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(LTO_Days_Range = cut(DaysSinceLastRun, breaks = c(0, 4, 7, 35, 91, 182, 365, 1000),
                              labels = c("<=4", "<=7", "<=35", "<=91", "<=182", "<=365", "<=1000"), 
                              ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(BFSPFC_Odds_Range = cut(BetFairSPForecastWinPrice, breaks = c(0, 6, 11, 21, 51, 1000),
                                 labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                                 ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 2, 6, 11, 21, 51, 1000),
                               labels = c("<=2","<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                               ordered_result = T))


ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Value_Odds_Range = cut(ValueOdds_BetfairFormat, breaks = c(0, 6, 11, 21, 51, 1000),
                                labels = c("<=6", ">6 to 11", ">11 to 21",">21 to 51", ">51"),
                                ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Value_Odds_Ratio = BetFairSPForecastWinPrice / ValueOdds_BetfairFormat,
         VOR_Range = cut(Value_Odds_Ratio, breaks = c(0, 0.5, 1.0, 2.5, 5.0, 10, 100),
                         labels = c("<=0.5", ">0.5 to 1.0", ">1 to 2.50", ">2.50 to 5.0", ">5 to 10", ">10"),
                         ordered_result = T)) 

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(BFSP_ValOdds_Ratio = Betfair.Win.S.P. / ValueOdds_BetfairFormat,
         BFSP_VOR_Range = cut(BFSP_ValOdds_Ratio, breaks = c(0, 0.5, 1.0, 2.5, 5.0, 10, 100),
                              labels = c("<=0.5", ">0.5 to 1.0", ">1 to 2.50", ">2.50 to 5.0", ">5 to 10", ">10"),
                              ordered_result = T)) 

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Runners_Range = cut(Actual.Runners, breaks = c(0, 8, 16, 100),
                             labels = c("<=8", "9-16", "17+"),
                             ordered_result = T)) 




ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Place_Expected = 1/Betfair.Place.S.P.)

slowGround <- c("SOFT","SFT-HVY","HEAVY", "GD-SFT", "YIELD", "GD-YLD", "YLD-SFT") 

fastGround <- c("GOOD", "GD-FM", "FIRM", "HARD")

syntheticGround <- c("STAND", "STD-SLOW", "STANDARD", "STD-FAST", "SLOW")

softGround <- c("SOFT", "SFT-HVY", "HEAVY")

unique(ukhr_master_BF$Going)

ukhr_master_BF$Going_Range <- ifelse(ukhr_master_BF$Going %in% slowGround, "SLOW", 
                                     ifelse(ukhr_master_BF$Going %in% fastGround,"FAST", "SYNTHETIC"))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(Weight_Rank = min_rank(desc(Weight_Pounds)))

#head(ukhr_master_BF$Weight_Rank, 30)   
#head(ukhr_master_BF$Weight_Pounds, 30)

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(Rev_Weight_Rank = min_rank(Weight_Pounds))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(Fav_Rank = min_rank(Betfair.Win.S.P.))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(FC_Fav_Rank = min_rank(BetFairSPForecastWinPrice))



ukhr <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Median_Rating = median(Rating),
         Avg_Rating = mean(Rating),
         Rtng_Norm = Rating/sum(Rating),
         Rtng_Diff_Median = Rating - Median_Rating,
         Rtng_Rank = min_rank(RatingsPosition),
         AE = Actual - (1/Betfair.Win.S.P.),
         BFSP = Betfair.Win.S.P.,
         Total_PL = BFSP_PL + BF_Placed_SP_PL) %>% 
  select(UKHR_RaceID, Rating, Meeting, RaceType, Handicap, RatingAdvantage, Median_Rating, Avg_Rating, Rtng_Norm, Rtng_Diff_Median,
         RatingAdvantage, Rtng_Rank, ValueOdds_Probability, Value_Odds_Ratio, ValueOdds_BetfairFormat,
         BetFairSPForecastWinPrice, Betfair.Win.S.P., AE, Total_PL) %>% 
  na.omit()

summary(ukhr)

# ukhr$RaceType <- as.factor(ukhr$RaceType)
# 
# table(ukhr$RaceType)
# 
# ukhrSampleRows <- createDataPartition(ukhr$Handicap, p=0.2, list = F)
# 
# ukhrTemp <- ukhr[ukhrSampleRows,]
# 
# table(ukhrTemp$RaceType, ukhrTemp$Handicap)




###############################################################################################
# 
# ukhrHcpAW <- filter(ukhr, RaceType == "AW", Handicap == "HANDICAP" | RaceType == "NURSERY")
# 
# library(car)
# 
# scatterplot(AE ~ Value_Odds_Ratio, data = ukhrHcpAW,
#             spread = F, smoother.args = list(lty=2), pch=19,
#             main = "AE v Value Odds Ratio + Value Odds Ratio",
#             xlab = "Value Odds Ratio", 
#             ylab = "AE",
#             xlim = c(0,5))
# 
# 
# myVars <- c("Value_Odds_Ratio", "ValueOdds_Probability", "BetFairSPForecastWinPrice", "AE", "Rtng_Diff_Median", "Total_PL")
# 
# skewCalc <- function(x, na.omit =T) {
#   m = mean(x)
#   n = length(x)
#   s = sd(x)
#   skew = sum((x-m)^3/s^3)/n
#   kurt = sum((x-m)^4/s^4)/n-3
#   return (c(n=n, mean = round(m, 4), sd = round(s, 4), skew = round(skew,4), kurtosis = round(kurt, 4)))
# }
# 
# sapply(ukhrHcpAW[myVars], skewCalc)
# 
# hist(ukhrHcpAW$Total_PL, breaks = 1000, xlim = c(0, 30), ylim = c(0, 5000))

#############################################################################################

rm(ukhr_master_BF)

ukhr$RaceType <- as.factor(ukhr$RaceType)
ukhr$Handicap <- as.factor(ukhr$Handicap)

head(ukhr)

neg <- which(ukhr$Avg_Rating < 0)

#View(ukhr[neg,])

ukhrTrain <- ukhr[-neg,]

str(ukhrTrain)

length(which(ukhrTrain$Total_PL > 0))

ukhrTrain <- na.omit(ukhrTrain)


ukhrTrain$UKHR_RaceID <- as.character(ukhrTrain$UKHR_RaceID)

colnames(ukhrTrain)

summary(ukhrTrain)

numVars <- ukhrTrain[,-c(1,3,4,5)]

select_if(numVars, is.numeric)

str(numVars)
correlations <- cor(numVars)

correlations

highCorr <- findCorrelation(correlations, cutoff = .75)

highCorr

colnames(numVars[,highCorr])

ukhrTrain <- select(ukhrTrain, -c(ValueOdds_BetfairFormat, Rating, BetFairSPForecastWinPrice, Avg_Rating))

summary(ukhrTrain)

ukhrTrain <- ukhrTrain[, -c(13:14)]

summary(ukhrTrain)

#library(corrplot)

#corrplot(correlations, order = "hclust")



# Check Zero Vars

#zv <- nearZeroVar(ukhrTrain)

#zv



########################################################

set.seed(100)

ukhrTrainRows <- createDataPartition(ukhrTrain$Betfair.Win.S.P., p = 0.05, list = FALSE)

ukhrTrainSet <- ukhrTrain[ukhrTrainRows,]

ukhrTestSet <- ukhrTrain[-ukhrTrainRows,]

#ukhrTrainSetReduced <- select(ukhrTrainSet, -highCorr)

summary(ukhrTrainSet)

set.seed(100)

tune.grid <- expand.grid(eta = c(0.1, 0.2),
                         nrounds = c(100,150),
                         lambda = c(0.1,0.2),
                         alpha = c(0.5,1.0))
                         



xgbModUKHR <- train(Betfair.Win.S.P. ~ ., 
                  data = ukhrTrainSet[,-c(1:4)],
                  method = "xgbLinear",
                  metric = "RMSE",
                  tuneGrid = tune.grid,
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           verboseIter = T))
                                           #repeats = 3))

#stopCluster(cl)
#registerDoSEQ()
                                           

print(xgbModUKHR)

saveRDS(xgbModUKHR, "UKHR_XGB_Model.RDS")

summary(xgbModUKHR)

varImp(xgbModUKHR)

predBFSP_xgb <- predict(xgbModUKHR, newdata = ukhrTestSet, type = "raw")

head(predBFSP_xgb)

summary(predBFSP_xgb)

ukhrTestSet$Preds <- predBFSP_xgb

head(ukhrTestSet$Preds)
head(ukhrTestSet$Betfair.Win.S.P.)

pRank <- ukhrTestSet %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(BFSP_Pred_Rank = min_rank(Preds),
         BetfairSPFCWinPrice = (1/ValueOdds_Probability)*Value_Odds_Ratio)

View(head(pRank))

colnames(pRank)

p <- pRank %>% 
  group_by(BFSP_Pred_Rank) %>% 
  summarise(Runs = n(), Avg_BFSP_Pred = mean(Preds), Avg_BFSP = mean(Betfair.Win.S.P.), Avg_UKHR_BFSPFC = mean(BetfairSPFCWinPrice)) %>% 
  arrange(BFSP_Pred_Rank) %>% 
  select(BFSP_Pred_Rank, Avg_BFSP_Pred, Avg_BFSP, Avg_UKHR_BFSPFC)
  

residualValues <- ukhrTestSet$Betfair.Win.S.P. - ukhrTestSet$Preds

summary(residualValues)

observed <- ukhrTestSet$Betfair.Win.S.P.
predicted <- ukhrTestSet$Preds

axisRange <- extendrange(c(observed, predicted))

plot(observed, predicted,
     ylim = c(1.01, 100),
     xlim = c(1.01, 100))

abline(0,1, col = "darkgrey", lty = 2)

plot(predicted, residualValues, ylab = "Residual")

abline(h = 0, col = "darkgrey", lty = 2)

R2(predicted, observed)

RMSE(predicted, observed)

cor(predicted, observed)

cor(pRank$BetfairSPFCWinPrice,pRank$Betfair.Win.S.P.)

R2(pRank$BetfairSPFCWinPrice,pRank$Betfair.Win.S.P.)

RMSE(pRank$BetfairSPFCWinPrice,pRank$Betfair.Win.S.P.)



# table(ukhrTestSet$Total_PL, predTotal_PLrf)
# 
# confusionMatrix(ukhrTestSet$Total_PL, predTotal_PLrf)
# 
# predTotal_PLProbNN <- predict(xgbModUKHR, newdata = ukhrTestSet, type = "prob")
# 
# head(predTotal_PLProbNN)

