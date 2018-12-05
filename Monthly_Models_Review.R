
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(caret)
library(nnet)
library(xgboost)
library(randomForest)
library(earth)
library(kernlab)
library(e1071)

ukTestSet <- read_csv("Model_Results_Nov18.csv", col_names = T)

mean(ukTestSet$BFSP_PL)

head(ukTestSet)

colnames(ukTestSet)

summary(ukTestSet$BFSP_PL)

ukTestSet <- ukTestSet %>% 
  drop_na(BFSP_PL)


ukTestSet %>% 
  group_by() %>% 
  filter(NN_Pred < 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by() %>% 
  filter(XGB_Pred < 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(RF_Pred < 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

# ukTestSet %>% 
#   #group_by(Handicap) %>% 
#   filter(PLS_Pred > 0) %>% 
#   summarise(Runs = n(),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>% 
#   arrange(desc(Avg_PL))
# 
# 
# ukTestSet %>% 
#   group_by(Handicap) %>% 
#   filter(RIDGE_Pred > 0.0) %>% 
#   summarise(Runs = n(),
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL)) %>% 
#   arrange(desc(Avg_PL))


#ukTestSet$Model_Avg = (ukTestSet$RF_Pred + ukTestSet$XGB_Pred + ukTestSet$NN_Pred )/4

ukTestSet %>% 
  group_by() %>% 
  filter(Models_Avg > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


ukTestSet %>% 
  group_by() %>% 
  filter(V_Models_Avg > 0.0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

NukTestSet %>% 
  group_by() %>% 
  filter(Model_Preds_V30 < 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))


ukTestSet %>% 
  group_by() %>% 
  filter(Model_Preds_V20 < 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by() %>% 
  filter(Model_Preds_V20 > 0, Model_Preds_V30 > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by(Handicap) %>% 
  filter(MARS_Model_Preds > 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))

ukTestSet %>% 
  group_by() %>% 
  filter(SVM_Model_Preds < 0) %>% 
  summarise(Runs = n(),
            Avg_PL = mean(BFSP_PL),
            Total_PL = sum(BFSP_PL)) %>% 
  arrange(desc(Avg_PL))
