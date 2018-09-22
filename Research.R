
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(caret)



setwd("~/git_projects/UKHR_Project")

#registerDoMC(4)

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_08_31.csv",col_names = T)

#ukhr_master_BF <- ukhr_master_BF %>% 
#filter(Year != 2018)

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Stall = min_rank(StallNumber))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Rating_Rank = min_rank(RatingsPosition))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Class_Rank = min_rank(ClassPosition))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Class_Rank_Range = cut(Class_Rank, 3,
                                labels = c("Top_Third", "Middle_Third", "Bottom_Third"), 
                                ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Spd_Rank = min_rank(SpeedRatingRank))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Speed_Rank_Range = cut(Spd_Rank, 3,
                                labels = c("Top_Third", "Middle_Third", "Bottom_Third"), 
                                ordered_result = T))



ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Ratings_Range = cut(Rating_Rank, 3,
                             labels = c("Top_Third", "Middle_Third" ,"Bottom_Third"), 
                             ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Weight_Range = cut(desc(Weight_Pounds), 3,
                            labels = c("High", "Middle", "Low"),
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

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(FcFav_Odds = min(BetFairSPForecastWinPrice))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(FcFav_Odds_Range = cut(FcFav_Odds, breaks = c(0, 1.5 ,2, 4, 6, 11, 100),
                                labels = c("<= 1.5",">1.5 to 2",">2 to 4", ">4 to 6", ">6 to 11", ">11"),
                                ordered_result = T))



colnames(ukhr_master_BF)

numVars <- ukhr_master_BF %>% 
  select_if(is.numeric)

colnames(numVars)

numPreds <- select(numVars, -c(Betfair.Placed:BF_Placed_SP_PL), -BFSP_ValOdds_Ratio, -Place_Expected, -Fav_Rank)

numPreds$Result <- ukhr_master_BF$Actual



temp <- na.omit(numPreds)

colSums(is.na(temp))



predVars <- select(temp, -Result)

colSums(is.na(predVars))


pScore <- function(x, y) {
  out <- t.test(x ~ y)$p.value
  
  out
}


scores <- apply(X = predVars,
                MARGIN = 2,
                FUN = pScore,
                y = temp$Result)


pCorrection <- function(score, x, y){
  score <- p.adjust(score, "bonferroni")
  keepers <- score <= 0.01
  keepers
}

goodVars <- pCorrection(scores)

goodVars[goodVars != TRUE]

gv <- names(goodVars[goodVars == TRUE])

bv <- names(goodVars[goodVars != TRUE])# 

gvf <- gv[!is.na(gv)]

gvf

finalVars <- select(numVars, gvf)

finalVars$Result <- ukhr_master_BF$Actual

colnames(finalVars)

corVals <- apply(finalVars[,2:258],
                 MARGIN = 2,
                 FUN = function(x,y) cor(x,y),
                 y = finalVars[,259])


sort(desc(corVals))
# alarmsRating <- ukhr_master_BF %>%
#   group_by(Alarms, Trainer, Ratings_Range) %>%
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.12, Horses >= 5, Archie >= 4.0, Exp_Wins >= 5.0, !is.na(Alarms)) %>%
#   arrange(desc(AE_Ratio, Archie))
# 
# alarmsRating
# 
# View(alarmsRating)
# 
# tdyAlm <- alarmsRating %>% 
#   left_join(today, by = c("Alarms", "Trainer", "Ratings_Range")) %>% 
#   filter(!is.na(Time24Hour)) %>% 
#   arrange(Time24Hour, Meeting, Horse)
# 
# tdyAlarms <- tdyAlm %>% 
#   select(Time24Hour, Meeting, Horse, Trainer, ValueOdds_BetfairFormat, BetFairSPForecastWinPrice,
#          Value_Odds_Ratio, Alarms, everything())
# 
# tdyAlarms
# 
# mean(tdyAlm$BFSP_PL)  
# sum(tdyAlm$BFSP_PL)            
# mean(tdyAlm$Betfair.Placed, na.rm = T)
# 
