library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)

ukhr_master_BF <- read_csv("ukhr_17_18.csv", col_names = T)

mean(ukhr_master_BF$BF_Placed_SP_PL, na.rm = T)

unique(ukhr_master_BF$RaceType)


ukFlat <- ukhr_master_BF %>%
  filter(RaceType == "FLAT")

ukAW <- ukhr_master_BF %>%
  filter(RaceType == "AW")

codeNH <- c("CHASE", "HURDLE", "NH FLAT")

ukNH <- ukhr_master_BF %>%
  filter(RaceType %in% codeNH)

ukNumAW <- ukAW %>%
  select_if(is.numeric)

colSums(is.na(ukNumAW))

ukNumAW <- ukNumAW %>%
    select(-matches("BHA"))

ukNumAW <- ukNumAW %>%
  select(-matches("LengthsWonLost"))

naCols <- names(which(colSums(is.na(ukNumAW)) > 0))

impData <- ukNumAW %>%
  select(naCols, BFSP_PL, RatingAdvantage)

library(caret)

impTrain <- preProcess(impData, method = "knnImpute")
imputedData <- predict(impTrain, impData)
View(imputedData)

ukNumAW$Prize <- imputedData$Prize
ukNumAW$TJPL <- imputedData$TJPL
ukNumAW$WeightDiffTotal <- imputedData$WeightDiffTotal
ukNumAW$WeightDiffTotal1Year <- imputedData$WeightDiffTotal1Year
ukNumAW$TrainerLast10RunsSR <- imputedData$TrainerLast10RunsSR
ukNumAW$S.P. <- imputedData$S.P.
ukNumAW$Place_Expected <- imputedData$Place_Expected


colSums(is.na(ukNumAW))

ukNumCompAW <- ukNumAW[complete.cases(ukNumAW),]

colnames(ukNumCompAW)

awTrain <- ukNumCompAW %>%
  select(-matches("UKHR"))

awTrain <- awTrain %>%
  select(-c(S.P., Actual.Runners, LengthsBehind, Duration, BFSP_PL, VSP_PL, VSP_Stake, Actual, Act_Minus_Exp, BF_Placed_SP_PL, Betfair.Placed,
            BFSP_ValOdds_Ratio, Fin_Pos, Act_Btn, Year, Month, DayOfMonth, Weekday, CardNumber, StallNumber, StallPercentage, Stall))

colnames(awTrain)

awTrain$BtnLPerF <- awTrain$LengthsBehindTotal / awTrain$Furlongs

awTrain <- awTrain %>%
  select(-LengthsBehindTotal)

awTrain <- awTrain %>%
  select(BtnLPerF, everything())

nzv <- nearZeroVar(awTrain)

zvNames <- colnames(awTrain[,nzv])

zvNames

awTrain <- awTrain[,-nzv]

highCorrs <- findCorrelation(cor(awTrain), cutoff = 0.75)

hcNames <- names(awTrain[, highCorrs])

hcNames

awTrain <- awTrain[, -highCorrs]


set.seed(123)

splitA <- createDataPartition(awTrain$BtnLPerF, p = 0.1, list = F)

awTrainData <- awTrain[splitA,]

awTestData <- awTrain[-splitA,]




# Train Control for all models


train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              verboseIter = T)

###########################################################################
# XGB Tree Algorithm to predict BF_Placed_SP_PL

# library(xgboost)

tune.grid <- expand.grid(eta = c(0.01, 0.025, 0.05),
                         nrounds = c(100, 150),
                         lambda = c(0.01, 0.025, 0.05),
                         alpha = c(1.0))


#View(tune.grid)

set.seed(123)

xgbMod <- train(BtnLPerF ~ .,
                data = awTrainData,
                method = "xgbLinear",
                preProc = c("center", "scale"),
                metric = "RMSE",
                tuneGrid = tune.grid,
                trControl = train.control)

print(xgbMod)

varImp(xgbMod)

saveRDS(xgbMod, "Lng_Btn_AW_Mod.RDS")

xgbMod$results




