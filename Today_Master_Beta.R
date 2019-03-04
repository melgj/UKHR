library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
#library(doMC)

setwd("~/git_projects/UKHR_Project")

#registerDoMC(4)

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_12_31_Header.csv", col_names = T)

# create season vars

winter <- c(12,1,2)
spring <- c(3,4,5)
summer <- c(6,7,8)
autumn <- c(9,10,11)


# Create Going Range Vars

slowGround <- c("SOFT","SFT-HVY","HEAVY", "GD-SFT", "YIELD", "GD-YLD", "YLD-SFT")

fastGround <- c("GD-FM", "FIRM", "HARD", "GOOD")

syntheticGround <- c("STAND", "STD-SLOW", "STANDARD", "STD-FAST", "SLOW")

softGround <- c("SOFT", "SFT-HVY", "HEAVY")


# Load today's racecard

today <- read_csv(file.choose(),col_names = T)

# Amend Odds after removing non runners from CSV file

today$BetFairSPForecastWinPrice[today$BetFairSPForecastWinPrice <= 0] <- NA

sum(is.na(today$BetFairSPForecastWinPrice))

if (sum(is.na(today$BetFairSPForecastWinPrice) > 0)) {

  library(caret)

  bfspToday <- today %>%
    select(RatingsPosition, RatingAdvantage, ConnRanking, ClassPosition, Runners, BetFairSPForecastWinPrice)

  bfspImpute <- preProcess(bfspToday, method = "bagImpute")
  imputedBFSP <- predict(bfspImpute, bfspToday)
  #View(imputedBFSP)

  today$BetFairSPForecastWinPrice <- imputedBFSP$BetFairSPForecastWinPrice
}

sum(is.na(today$BetFairSPForecastWinPrice))

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
  mutate(Season = if_else(Month %in% winter, "Winter",
                          if_else(Month %in% spring, "Spring",
                                  if_else(Month %in% summer,"Summer",
                                          "Autumn"))))



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

today$Going_Range <- if_else(today$Going %in% slowGround, "SLOW",
                            if_else(today$Going %in% fastGround,"FAST",
                                   "SYNTHETIC"))

today <- today %>%
  mutate(Weight_Range = cut(desc(Weight_Pounds), 3,
                            labels = c("High", "Middle", "Low"),
                            ordered_result = T))


today <- today %>%
  group_by(UKHRCardRaceID) %>%
  mutate(Weight_Rank = min_rank(desc(Weight_Pounds)))


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
  group_by(UKHRCardRaceID) %>%
  rename(Declared = Runners) %>%
  mutate(Runners = length(unique(Horse)),
         Runners_Range = cut(Runners, breaks = c(0, 8, 16, 100),
                             labels = c("<=8", "9-16", "17+"),
                             ordered_result = T))

today$Declared

#######################################################

table(today$Meeting, today$Going)
table(today$Meeting, today$RaceType)
table(today$Meeting, today$Going_Range)

# source("AW_Systems.R")
# source("Flat_Systems.R")
# source("NH_Systems.R")
# source("Extra_Qualifiers.R")


source("Today_AW_Systems.R")
source("Today_Flat_Systems.R")
source("Today_NH_Systems.R")
source("Today_Extra_Qualifiers.R")
source("Todays_Master_Qualifiers.R")
#source("Draw_Range_Analysis.R")
source("Min_Rank_Val_Bet.R")
#source("Today_Systems_Model.R")
source("Today_Systems_Model_Beta.R")
