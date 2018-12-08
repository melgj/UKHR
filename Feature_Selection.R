library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(doMC)
library(caret)


#registerDoMC(8)

setwd("~/git_projects/UKHR_Project")

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_11_30.csv", col_names = T)

colnames(ukhr_master_BF)

# Remove Wolverhampton Polytrack data from pre Tapeta era

# wPoly1 <- which(ukhr_master_BF$Year < 2014 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")
# 
# wPoly2 <- which(ukhr_master_BF$Year == 2014 & ukhr_master_BF$Month < 8 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")
# 
# wPolyAll <- c(wPoly1, wPoly2)
# 
# ukhr_master_BF <- ukhr_master_BF[-wPolyAll,]
# 
# unique(ukhr_master_BF$Year)
# 
# ukhr_master_BF <- ukhr_master_BF %>% 
#   group_by(UKHR_RaceID) %>% 
#   mutate(Fin_Pos = min_rank(LengthsBehindTotal),
#          Exp_Btn = Actual.Runners - Fav_Rank,
#          Act_Btn = Actual.Runners - Fin_Pos)
# 
winter <- c(12,1,2)
spring <- c(3,4,5)
summer <- c(6,7,8)
autumn <- c(9,10,11)
# 
# ukhr_master_BF <- ukhr_master_BF %>% 
#   mutate(Season = if_else(Month %in% winter, "Winter",
#                           if_else(Month %in% spring, "Spring",
#                                   if_else(Month %in% summer,"Summer",
#                                           "Autumn"))))

# Create Going Range Vars

slowGround <- c("SOFT","SFT-HVY","HEAVY", "GD-SFT", "YIELD", "GD-YLD", "YLD-SFT")

fastGround <- c("GD-FM", "FIRM", "HARD")

syntheticGround <- c("STAND", "STD-SLOW", "STANDARD", "STD-FAST", "SLOW")

softGround <- c("SOFT", "SFT-HVY", "HEAVY")

uk <- ukhr_master_BF %>% 
  filter(Year == 2017)

uk <- uk %>% 
  mutate(BFSP_FC_Win_Odds = if_else(is.na(BetFairSPForecastWinPrice), Betfair.Win.S.P., BetFairSPForecastWinPrice))

table(uk$RaceType)

nh <- c("CHASE", "HURDLE", "NH FLAT")
flat <- c("AW", "FLAT")

ukNH <- uk %>% 
  filter(RaceType %in% nh)

ukFlat <- uk %>% 
  filter(RaceType %in% flat)

# NH

nhVarsNA <- names(which(colSums(is.na(ukNH)) > 1000))
nhVarsNA  

nhNA <- ukNH %>% 
  select(nhVarsNA)

colSums(is.na(nhNA))

ukNH <- ukNH %>% 
  select(-nhVarsNA)

colSums(is.na(ukNH))



ukNH <- ukNH %>% 
  select_if(is.numeric)

(which(colSums(is.na(ukNH)) > 0))

nhRemove <- c(7, 401, 402, 404, 422, 427)

ukNH <- ukNH %>% 
  select(-nhRemove)

colnames(ukNH)

ukdataNH <- ukNH %>% 
  select(5:399, 401, 409, 411, 418, 419, 420, 424, 428) %>% 
  select(LengthsBehindTotal, everything())



#library(caret)

nzv <- nearZeroVar(ukdataNH)
nzv
nzvCols <- colnames(select(ukdataNH, nzv))
nzvCols

ukDataNH <- ukdataNH %>% 
  select(-nzvCols)

ukDataNH <- ukDataNH %>% 
  rename(BetFairSPForecastWinPrice = BFSP_FC_Win_Odds)

colnames(ukDataNH)

which(colSums(is.na(ukDataNH)) > 0) 

# impute missing data

pre.process <- preProcess(ukDataNH, method = "knnImpute")
imputed <- predict(pre.process, ukDataNH)

colSums(is.na(imputed))

ukDataNH$WeightDiffTotal <- imputed$WeightDiffTotal
ukDataNH$WeightDiffTotal1Year <- imputed$WeightDiffTotal1Year
ukDataNH$TrainerLast10RunsSR <- imputed$TrainerLast10RunsSR

write_csv(ukDataNH, "UKHR_NH_Data_2017.csv")


###############################################################################

#library(caret)

#library(doMC)

registerDoMC(8)

nhData <- read_csv("UKHR_NH_Data_2017.csv", col_names = T)

correlations <- cor(nhData)

highCorr <- findCorrelation(correlations, cutoff = .75)

colnames(nhData[, highCorr])

colnames(nhData[, -highCorr])

nhData <- nhData[, -highCorr]

colnames(nhData)

nhData <- nhData %>% 
  select(-(65:68))

colnames(nhData)

set.seed(100)

dataSplit <- createDataPartition(nhData$LengthsBehindTotal, p = 0.25, list = FALSE)

nhTrain <- nhData[dataSplit,]
nhTest <- nhData[-dataSplit,]

index <- createMultiFolds(nhTrain$LengthsBehindTotal, times = 5)

varsRF <- var_seq(100, len = 5)

ctrl <- rfeControl(method = "repeatedcv",
                   repeats = 5,
                   verbose = T,
                   functions = rfFuncs,
                   index = index)

set.seed(100)

rfRFE <- rfe(LengthsBehindTotal ~ .,
             data = nhTrain,
             sizes = varsRF,
             metric = "RMSE",
             rfeControl = ctrl,
             ntree = 1000)

rfRFE

saveRDS(rfRFE, "NH_Features_RFE.RDS")

vImp <- varImp(rfRFE)

write_csv(vImp, "NH_Feature_Scores.csv")
