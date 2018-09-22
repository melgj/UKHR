
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
#library(doMC)
# library(doParallel)
# cl <- makePSOCKcluster(4) # number of cores to use
# registerDoParallel(cl)

# library(doSNOW)
# cl <- makeCluster(4, outfile="")
# registerDoSNOW(cl)


setwd("~/git_projects/UKHR_Project")

systemsAnalysisASQ_2018 <- read_csv("All_System_Qualifiers_to_2018_08.csv", col_names = T)


library(caret)
library(earth)
library(broom)
library(xgboost)
library(kernlab)


head(systemsAnalysisASQ_2018)

#colSums(is.na(systemsAnalysisASQ_2018))

uk <- systemsAnalysisASQ_2018 %>% 
  select(Meeting:Exp_Wins, Runners, Placed_Archie, Archie, BFSP_PL ,VSP_PL, -c(Age, Rev_Weight_Rank, Alarms))

ukNum <- uk %>% 
  select_if(is.numeric)

colnames(uk)

summary(uk)

colSums(is.na(ukNum))

ukNum <- ukNum %>% 
  drop_na(Val_Ratio)

head(ukNum)

#View(head(ukNum))

# ukPreds <- select(ukNum, -c(BFSP_PL, VSP_PL))
# ukClass <- ukNum$VSP_PL

ukNum <- select(ukNum, -c(Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake, Runners, BFSP_PL))

# KNN Model
# 
# Load Model
#knnTune <- readRDS("KNN_Systems_VSP_PL_Model.RDS")

set.seed(100)

ukTrainRows <- createDataPartition(ukNum$VSP_PL, p = 0.60, list = FALSE)

ukTrainSet <- ukNum[ukTrainRows,]

ukTestSet <- ukNum[-ukTrainRows,]



knnTune <- train(VSP_PL ~ .,
                 data = ukTrainSet,
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k = 50:100),
                 metric = "RMSE",
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10,
                                          repeats = 3))

stopCluster(cl)



print(knnTune)

#saveRDS(knnTune, "KNN_Systems_VSP_PL_Model.RDS")

knnPreds <- predict(knnTune, newdata = ukTestSet, type = "raw")

head(knnPreds)
head(ukTestSet$VSP_PL)

df <- tibble(actual = ukTestSet$VSP_PL,
             preds = knnPreds)


#View(head(df, 50))

posPreds <- filter(df, preds > 0)
negPreds <- filter(df, preds <= 0)

mean(posPreds$actual)
sum(posPreds$actual)
mean(negPreds$actual)
sum(negPreds$actual)
mean(systemsAnalysisASQ_2018$VSP_PL)
sum(systemsAnalysisASQ_2018$VSP_PL)

today <- read_csv(file.choose(), col_names = T)

# Load Model

knnTune <- readRDS("KNN_Systems_VSP_PL_Model.RDS")


todayPreds <- predict(knnTune, newdata = today, type = "raw")

x = ukTrainSet[!(colnames(ukTrainSet) %in% colnames(today))]

colnames(x)

today$Preds <- todayPreds

View(today)

todayPos <- filter(today, today$Preds > 0.0)
todayNeg <- filter(today, today$Preds <= 0.0, !(Horse %in% todayPos$Horse))

View(todayPos)
View(todayNeg)
# marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:20)


# SVM Model

svmTune <- train(VSP_PL ~ .,
                 data = ukTrainSet,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 tuneLength = 14,
                 metric = "RMSE",
                 verboseIter = T,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10,
                                          repeats = 3))
stopCluster(cl)


print(svmTune)

saveRDS(svmTune, "SVM_Systems_VSP_PL_Model.csv")

############################################################################################



ukhrData <- filter(ukhr_master_BF, Year >= 2017)

rm(ukhr_master_BF)

uk



set.seed(100)

ukTrainRows <- createDataPartition(ukhrData$Betfair.Win.S.P., p = 0.60, list = FALSE)

ukTrainSet <- ukNum[ukTrainRows,]

ukTestSet <- ukNum[-ukTrainRows,]


marsGrid <- expand.grid(.degree = 1:2, n.prune = 2:20)

marsTuned <- train(Betfair.Win.S.P. ~ .,
                 data = ukTrainSet,
                 method = "earth",
                 tuneGrid = marsGrid,
                 metric = "RMSE",
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10,
                                          repeats = 3))



print(marsTuned)

