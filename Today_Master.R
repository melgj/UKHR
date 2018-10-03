#rename raw advantage and radj advantage columns to match ukhr_master

library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
#library(doMC)
library(doParallel)
cl <- makePSOCKcluster(4) # number of cores to use
registerDoParallel(cl)


setwd("~/git_projects/UKHR_Project")

#registerDoMC(4)

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_08_31.csv", col_names = T)

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



#colnames(ukhr_master_BF)

#head(ukhr_master_BF$VSP_Stake)

# Load today's racecard

today <- read_csv(file.choose(),col_names = T)

# Amend Odds after removing non runners from CSV file

today$BetFairSPForecastWinPrice[today$BetFairSPForecastWinPrice <= 0] <- NA

todayOdds <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Book = sum(ValueOdds_Probability), 
         Adj_Val_Prob = ValueOdds_Probability/Book,
         Adj_Val_Odds = 1/Adj_Val_Prob,
         BFSPFC_Book = sum((1/BetFairSPForecastWinPrice),na.rm = T),
         Adj_BFSPFC_Prob = (1/BetFairSPForecastWinPrice)/BFSPFC_Book,
         Adj_BFSPFC_Odds = 1/Adj_BFSPFC_Prob,
         New_Book = sum(Adj_Val_Prob),
         New_BFSPFC_Book = sum(1/Adj_BFSPFC_Odds)) %>% 
  select(UKHRCardRaceID, Book, ValueOdds_Probability, ValueOdds_BetfairFormat, Adj_Val_Prob, Adj_Val_Odds,  BFSPFC_Book, 
         BetFairSPForecastWinPrice, Adj_BFSPFC_Prob, Adj_BFSPFC_Odds, New_Book, New_BFSPFC_Book)

todayOdds


today$BetFairSPForecastWinPrice <- todayOdds$Adj_BFSPFC_Odds
today$ValueOdds_BetfairFormat <- todayOdds$Adj_Val_Odds


colnames(ukhr_master_BF)
#colnames(today)

today <- rename(today, Raw.Advantage = `Raw Advantage`, RAdj.Advantage = `RAdj Advantage`,
                LastTimePosn = LastTimePositionRaceType, LastTimeClassDrop = ClassDifferentialOneRace,
                LastTimeWeightDrop = WeightDifferentialOneRace)



ukhrCols <- colnames(ukhr_master_BF)
todayCols <- colnames(today)

setdiff(ukhrCols, todayCols)
setdiff(todayCols, ukhrCols)

today$Handicap[is.na(today$Handicap)] <- "NONHCP"

today <- mutate_if(today, is.character, toupper)

today$Date <- dmy(today$Date) 
today$DayOfMonth <- mday(today$Date)
today$Month <- month(today$Date)
today$Year <- year(today$Date)
today$Weekday <- wday(today$Date)

today$Sire <- str_replace_all(today$Sire, "&ACUTE;", "'")
today$Trainer <- str_replace_all(today$Trainer, "&ACUTE;", "'")
today$Jockey <- str_replace_all(today$Jockey, "&ACUTE;", "'")
today$Horse <- str_replace_all(today$Horse, "&ACUTE;", "'")

today <- select_if(today, (colnames(today) %in% colnames(ukhr_master_BF)))

todayNH <- filter(today, RaceType == "CHASE" | RaceType == "HURDLE" | RaceType == "NH FLAT")
todayAW <- filter(today, RaceType == "AW")
todayFlat <- filter(today, RaceType == "FLAT")

today <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Stall = min_rank(StallNumber))

today <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Rating_Rank = min_rank(RatingsPosition))

today <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Class_Rank = min_rank(ClassPosition))

today <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Class_Rank_Range = cut(Class_Rank, 3,
                                labels = c("Top_Third", "Middle_Third", "Bottom_Third"), 
                                ordered_result = T))

today <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Spd_Rank = min_rank(SpeedRatingRank))

today <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Speed_Rank_Range = cut(Spd_Rank, 3,
                                labels = c("Top_Third", "Middle_Third", "Bottom_Third"), 
                                ordered_result = T))




today <- today %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Ratings_Range = cut(Rating_Rank, 3,
                             labels = c("Top_Third", "Middle_Third" ,"Bottom_Third"), 
                             ordered_result = T))

today <- today %>% 
  mutate(LTO_Days_Range = cut(DaysSinceLastRun, breaks = c(0, 4, 7, 35, 91, 182, 365, 1000),
                              labels = c("<=4", "<=7", "<=35", "<=91", "<=182", "<=365", "<=1000"), 
                              ordered_result = T))

today <- today %>% 
  mutate(BFSPFC_Odds_Range = cut(BetFairSPForecastWinPrice, breaks = c(0, 6, 11, 21, 51, 1000),
                                 labels = c("<=6", ">6 to 11", ">11 to 21" ,">21 to 51", ">51"),
                                 ordered_result = T))

today <- today %>% 
  mutate(Value_Odds_Range = cut(ValueOdds_BetfairFormat, breaks = c(0, 6, 11, 21, 51, 1000),
                                labels = c("<=6", ">6 to 11", ">11 to 21",">21 to 51", ">51"),
                                ordered_result = T))

today <- today %>% 
  mutate(Value_Odds_Ratio = BetFairSPForecastWinPrice / ValueOdds_BetfairFormat,
         VOR_Range = cut(Value_Odds_Ratio, breaks = c(0, 0.5, 1.0, 2.5, 5.0, 10, 100),
                         labels = c("<=0.5", ">0.5 to 1.0", ">1 to 2.50", ">2.50 to 5.0", ">5 to 10", ">10"),
                         ordered_result = T)) 

today$Going_Range <- ifelse(today$Going %in% slowGround, "SLOW",
                            ifelse(today$Going %in% fastGround,"FAST", "SYNTHETIC"))

today <- today %>% 
  mutate(Weight_Range = cut(desc(Weight_Pounds), 3,
                            labels = c("High", "Middle", "Low"),
                            ordered_result = T))


today <- today %>%
  group_by(UKHRCardRaceID) %>%
  mutate(Weight_Rank = min_rank(desc(Weight_Pounds)))

#head(today$Weight_Rank, 30)   
#head(today$Weight_Pounds, 30)

today <- today %>%
  group_by(UKHRCardRaceID) %>%
  mutate(Rev_Weight_Rank = min_rank(Weight_Pounds))

today <- today %>%
  group_by(UKHRCardRaceID) %>%
  mutate(FC_Fav_Rank = min_rank(BetFairSPForecastWinPrice))

today <- today %>%
  group_by(UKHRCardRaceID) %>%
  mutate(FcFav_Odds = min(BetFairSPForecastWinPrice))

today <- today %>%
  group_by(UKHRCardRaceID) %>%
  mutate(FcFav_Odds_Range = cut(FcFav_Odds, breaks = c(0, 1.5 ,2, 4, 6, 11, 100),
                                labels = c("<= 1.5",">1.5 to 2",">2 to 4", ">4 to 6", ">6 to 11", ">11"),
                                ordered_result = T))

today <- today %>% 
  mutate(Runners_Range = cut(Runners, breaks = c(0, 8, 16, 100),
                             labels = c("<=8", "9-16", "17+"),
                             ordered_result = T)) 


#system qualifier columns for export to csv

#sysCols <- c("Time24Hour","Meeting","Horse","BetFairSPForecastWinPrice","RatingsPosition","Runs","meanPL","sumPL","AE_Ratio","WinPercent")

#######################################################

table(today$Meeting, today$Going)
table(today$Meeting, today$RaceType)
# 
# #todays qualifiers combined in one file
# 
# #join AW Trainer & Sire Qualifiers
# 
# allAWQuals <- todayAllAW_Qualifiers %>% 
#   arrange(Time24Hour,Meeting,Horse)
# 
# allAWQuals
# 
# #View(allAWQuals)
# 
# 
# #write_csv(allAWQuals, "todays_ALL_AW_Qualifiers_2017_22_12_17.csv")
# 
# #join AW & Flat Qualifiers
# 
# allQualsFlatAW <- allAWQuals%>%
#   full_join(allFlatQuals)%>%
#   arrange(Time24Hour, Meeting, Horse)
# 
# allQualsFlatAW
# 
# #join Flat, AW & NH qualifiers
# 
# allSystemQualifiers <- allQualsFlatAW%>%
#   full_join(allNHSystemQualifiers)%>%
#   full_join(todayExtra) %>% 
#   arrange(Time24Hour, Meeting, Horse)
# 
# allSystemQualifiers <- select(allSystemQualifiers, Time24Hour, Meeting, Horse, RatingsPosition, BetFairSPForecastWinPrice,
#                               RaceType, Trainer, Sire, Jockey, everything())
# 
# allSystemQualifiers
# 
# 
# 
# length(unique(allSystemQualifiers$Horse))
# 
# 
# #####################################################################################
# 
# # Archie Score Calculation  
# # Formula: (Runners * (Winners - Expected Winners) ^ 2) / Expected Winners * (Runners - Expected Winners)
# 
# allArchie <- allSystemQualifiers %>% 
#   mutate(Winners = round(Runs * WinPercent), Exp_Wins = Winners/AE_Ratio, 
#          Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0),
#          Arch_Strength = cut(Archie, breaks = c(-1, 2.5, 3.5, 4.5, 5.5, 8.0, 100), 
#                              labels = c("-", "*", "**", "***", "****", "*****")),
#          Val_Ratio = BetFairSPForecastWinPrice / ValueOdds_BetfairFormat)
# 
# allArchie <- allArchie %>% 
#   select(Time24Hour, Meeting, Horse, Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range,
#          BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, NumberOfResults,
#          Age, Runs, meanPL, totalPL, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie, Arch_Strength) %>% 
#   filter(Archie > 3.5, AE_Ratio >= 1.25) %>% 
#   arrange(Time24Hour, Meeting, Horse)
#   
# allArchie
# 
# View(allArchie)
# 
# highArchieQuals <- filter(allArchie, Archie >= 5.5, meanPL >= 0.20, WinPercent >= 0.15, Exp_Wins >= 5, ValueOdds_BetfairFormat <= 21.0)
# 
# highArchieQuals <- highArchieQuals %>% 
#   select(Time24Hour, Meeting, Horse, Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range,
#          BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, NumberOfResults,
#          Age, Runs, meanPL, totalPL, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie, Arch_Strength) %>% 
#   arrange(Time24Hour, Meeting, Horse)
# 
# highArchieQuals
# 
# View(highArchieQuals)
# 
# #valArchieQuals <- filter(archieQuals, Val_Ratio >= 1.0)
# 
# #valArchieQuals
# 
# write_csv(highArchieQuals, "High_Archie_Quals_Today_2018_07_25.csv")
# 
# write_csv(allArchie, "All_Archie_Today_2018_07_25.csv")

# write_csv(bestQualsToday, "Best_Bets_2018_07_20.csv")

#write_csv(valArchieQuals, "Value_Archie_2018_06_16.csv")


#   Archie Likelihood Of Chance
# 0.30 	58%
# 0.50 	48%
# 1.00 	32%
# 1.50 	22%
# 2.00 	16%
# 2.50 	11%
# 3.00 	8%
# 3.50 	6%
# 4.00 	5%
# 4.50 	3%
# 5.00 	3%
# 5.50 	2%
# 6.00 	1%
# 6.50 	1%
# 7.00 	1%
# 7.50 	1%
# 8.00 	1%
# 8.50 	0%
# 9.00 	0%
# 9.50 	0%
# 10.00 	0%
# 10.50 	0%
# 11.00 	0%
# 11.50 	0%
# 12.00 	0%






