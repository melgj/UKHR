# library(tidyverse)
# library(stringr)
# library(stringi)
# library(lubridate)
#
# uk18 <- read_csv("UKHR_Master_BF_2018_10_31.csv", col_names = T) %>%
#   filter(Year == 2018)
#
# colnames(uk18)
#
#
#
# uk18 <- uk18 %>%
#   rename(Declared = Runners) %>%
#   group_by(UKHR_RaceID) %>%
#   mutate(Runners = length(unique(Horse)),
#          Runners_Range = cut(Runners, breaks = c(0, 8, 16, 100),
#                              labels = c("<=8", "9-16", "17+"),
#                              ordered_result = T))
#
#
#
#
#
# t5Q <- uk18 %>%
#   drop_na(Rating_Rank, ValueOdds_BetfairFormat, BetFairSPForecastWinPrice, VOR_Range, Value_Odds_Range,
#           Speed_Rank_Range) %>%
#   filter(Rating_Rank <= 5, ValueOdds_BetfairFormat <= 21, Runners >= 5, Value_Odds_Ratio > 1.0) %>%
#   group_by(UKHR_RaceID) %>%
#   mutate(HiRnkValBet = if_else(Value_Odds_Ratio > 1.0, 1, 0)) %>%
#   filter(HiRnkValBet == 1) %>%
#   mutate(Min_Rnk_Val_Bet = min_rank(Rating_Rank)) %>%
#   filter(Min_Rnk_Val_Bet == 1) %>%
#   arrange(Time24Hour, Meeting, Horse)
#
# t5Q
#
# t5n <- t5Q %>%
#   select_if(is.numeric)
#
# t5c <- t5Q %>%
#   select_if(is.character)
#
# t5f <- t5Q %>%
#   select_if(is.factor)
#
# colnames(t5c)
# colnames(t5f)
# colnames(t5n)
#
# summary(t5n$Runners)
#
# t5n2 <- t5n %>%
#   select(matches("Rating|Rank|Adv|Weight|Class|Number|DaysSince|Age|Value|Odds|Betf|BetF|BF|Form|Runner"))
#
# colnames(t5n2)
#
# t5n3 <- t5n2 %>%
#   select(-c(Betfair.Place.S.P., Betfair.Win.S.P., Betfair.Placed, Actual.Runners, BF_Placed_SP_PL, BFSP_ValOdds_Ratio, CardNumber,
#             MeanWeight, StallNumber, StallPercentage, Race5RunsAgoRaceClass, Race5RunsAgoRaceClassType, Race4RunsAgoRaceClass,
#             Race4RunsAgoRaceClassType, Race3RunsAgoRaceClass, Race3RunsAgoRaceClassType, Race2RunsAgoRaceClass, Race2RunsAgoRaceClassType))
#
# t5c2 <- t5c %>%
#   select(Going_Range, RaceType, Gender, Handicap, FcFav_Odds_Range)
#
# t5c2 <- t5c2 %>%
#   ungroup()
#
# t5n3 <- t5n3 %>%
#   ungroup()
#
# t5f <- t5f %>%
#   ungroup()
#
# t5char <- cbind(t5c2, t5f)
#
# t5Data <- cbind(t5char, t5n3)
#
# colnames(t5Data)
#
# t5Data <- t5Data[2:198]
#

# typeof(t5Data)
#
# t5df <- as_tibble(t5Data)
#
# str(t5df)
#
# t5df$Going_Range <- as.factor(t5df$Going_Range)
# t5df$RaceType <- as.factor(t5df$RaceType)
# t5df$Gender <- as.factor(t5df$Gender)
# t5df$Handicap <- as.factor(t5df$Handicap)
# t5df$FcFav_Odds_Range <- as.factor(t5df$FcFav_Odds_Range)
#
# colSums(is.na(t5df))
#
# t5df <- t5df %>%
#   select(-c(Runners_Range, BHAclassToday))
#
# colnames(t5df[colSums(is.na(t5df)) > 0])
#
# t5dfData <- na.omit(t5df)
#
# colnames(t5dfData)
#
# write_csv(t5dfData, "Top5QData_Oct18.csv")


#########################################################################

library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)

t5dfData <- read_csv("Top5QData_Oct18.csv", col_names = T)

t5dfData <- t5dfData %>% 
  select(-Gender)

summary(t5dfData)

t5dfData <- t5dfData %>% 
  select(-UKHR_RaceID.1)

t5dfData <- t5dfData %>% 
  select(-c(TrFormLastRun, TrForm2RunsAgo, TrForm3RunsAgo, TrForm4RunsAgo, TrForm5RunsAgo, TrForm6RunsAgo,
            TrForm7RunsAgo, TrForm8RunsAgo, TrForm9RunsAgo, TrForm10RunsAgo))

library(caret)
library(xgboost)

set.seed(100)

ukTrainRows <- createDataPartition(t5dfData$BFSP_PL, p = 0.6, list = FALSE)

ukTrainSet <- t5dfData[ukTrainRows,]

ukTestSet <- t5dfData[-ukTrainRows,]

colnames(ukTrainSet)

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)
#classProbs = TRUE, 
#summaryFunction = RMSE)


tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 125, 150),
                         lambda = c(0.025, 0.05, 0.075),
                         alpha = c(1.0))


set.seed(200)

xgbT5ModUK <- train(BFSP_PL ~ .,
                      data = ukTrainSet,
                      method = "xgbLinear",
                      preProc = c("center", "scale"),
                      metric = "RMSE",
                      tuneGrid = tune.grid,
                      trControl = train.control)

print(xgbT5ModUK)

varImp(xgbT5ModUK)





predMod <- predict(xgbT5ModUK, newdata = ukTestSet, type = "raw")

head(predMod)


summary(predMod)


R2(predMod, ukTestSet$BFSP_PL)
RMSE(predMod, ukTestSet$BFSP_PL)
cor(predMod, ukTestSet$BFSP_PL)

ukTestSet$ModelPL <- predMod


ukPos <- filter(ukTestSet, ModelPL > 0)
mean(ukPos$BFSP_PL)

ukNeg <- filter(ukTestSet, ModelPL <= 0)
mean(ukNeg$BFSP_PL)

mean(ukTestSet$BFSP_PL)

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(ModelPL > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))
