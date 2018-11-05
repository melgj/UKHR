#rename raw advantage and radj advantage columns to match ukhr_master

library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(doMC)
library(caret)
library(earth)

setwd("~/git_projects/UKHR_Project")

registerDoMC(4)

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_09_30.csv", col_names = T)

wPoly1 <- which(ukhr_master_BF$Year < 2014 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")

wPoly2 <- which(ukhr_master_BF$Year == 2014 & ukhr_master_BF$Month < 8 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")

wPolyAll <- c(wPoly1, wPoly2)

ukhr_master_BF <- ukhr_master_BF[-wPolyAll,]

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Fin_Pos = min_rank(LengthsBehindTotal),
         Exp_Btn = Actual.Runners - Fav_Rank,
         Act_Btn = Actual.Runners - Fin_Pos)

ukAW <- filter(ukhr_master_BF, RaceType == "AW")

unique(ukAW$Meeting)

colnames(ukAW)

awFirst <- filter(ukAW, Fin_Pos == 1)

amIndex <- which(str_detect(awFirst$Title, "AMATEUR RIDER"))

awFirst <- awFirst[-amIndex,]

byCourse <- awFirst %>% 
  group_by(Meeting) %>% 
  nest()

byCourse    

awStdTimesModel <- function(df){
  
  lm(Duration ~ RaceClass * Furlongs,
     data = df)
  
}

byCourse <- byCourse %>% 
  mutate(Std_Times_Model = map(data, awStdTimesModel))

byCourse$Std_Times_Model[[3]]

library(modelr)

byCourse <-  byCourse %>% 
  mutate( Model_Residuals = map2(data, Std_Times_Model, add_residuals))

Model_Resids <- unnest(byCourse, Model_Residuals)

Model_Resids$resid

Course_Resids <- Model_Resids %>% 
  group_by(Meeting, DayOfMonth, Month, Year) %>% 
  summarise(Races = n(), Avg_Resid = round(mean(resid),2))

Course_Resids %>% 
  group_by(Meeting) %>% 
  summarise(Avg_Meet_Resid = mean(Avg_Resid))

View(Course_Resids)

awFirst %>%
  filter(Handicap == "HANDICAP") %>% 
  group_by(Furlongs) %>% 
  summarise(Avg_Wght = mean(Weight_Pounds))

summary(awFirst$Duration)
        





# marsGrid <- expand.grid(.degree = 1:2, .nprune = 10:20)
# 
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               summaryFunction = RMSE,
#                               verboseIter = T)
# set.seed(100)                              
# 
# train(Duration ~ RaceClass + Furlongs + Weight_Pounds, 
#       data = df,
#       method = "earth",
#       tuneGrid = marsGrid,
#       metric = "RMSE",
#       trControl = train.control)
#   
  



