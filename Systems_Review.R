
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

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_08_31.csv",col_names = T)

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
  mutate(BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 2, 6, 11, 21, 51, 1000),
                               labels = c("<=2","<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
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

#head(ukhr_master_BF$VSP_Stake)

# Load today's racecard

#today <- read_csv(file.choose(),col_names = T)


# ukhr_master_BF %>% 
#   filter(Year == 2018) %>% 
#   write_csv("uk2018_07.csv")

today <- ukhr_master_BF %>% 
  filter(Year >= 2018)

ukhr_master_BF <-  ukhr_master_BF %>% 
  filter(Year != 2018)



#############################################

allAWQuals <- todayAllAW_Qualifiers %>% 
  arrange(Time24Hour,Meeting,Horse)

allAWQuals

#join AW & Flat Qualifiers

allQualsFlatAW <- allAWQuals%>%
  full_join(allFlatQuals)%>%
  arrange(Time24Hour, Meeting, Horse)

allQualsFlatAW

#join Flat, AW & NH qualifiers

allSystemQualifiers <- allQualsFlatAW%>%
  full_join(allNHSystemQualifiers)%>%
  full_join(todayExtraQuals) %>% 
  arrange(Time24Hour, Meeting, Horse)

allSystemQualifiers$Val_Ratio <-allSystemQualifiers$BetFairSPForecastWinPrice / allSystemQualifiers$ValueOdds_BetfairFormat

allSystemQualifiers <- allSystemQualifiers %>%
  mutate(BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 6, 11, 21, 51, 1000),
                               labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                               ordered_result = T))


allSystemQualifiers <- select(allSystemQualifiers, Time24Hour, Meeting, Horse, System_Name, Trainer, Jockey, Sire, Dist_Range, 
                              RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range, BFSPFC_Odds_Range,
                              BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, 
                              NumberOfResults, Alarms, Age, Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake,
                              Total_VSP_Stake,VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, 
                              Winners, Exp_Wins, BFSP_PL, Actual, Expected, Betfair.Placed,Place_Expected, BF_Placed_SP_PL, 
                              Betfair.Win.S.P., BFSP_Odds_Range, VSP_PL, Runners, Month, Result, LengthsBehindTotal, Archie)

asq <- allSystemQualifiers %>% 
  mutate(Placed_Archie = ifelse(Exp_Places >= 5.0,((Places * ((Places - Exp_Places) ^ 2)) / (Exp_Places * (Runs - Exp_Places))),0),
         Place_Percent = Places/Runs) %>% 
  filter(Archie > 2.50)

#View(asq)

length(unique(asq$Horse))


#####################################################################################

# Archie Score Calculation  
# Formula: (Runners * (Winners - Expected Winners) ^ 2) / Expected Winners * (Runners - Expected Winners)

allArchie <- asq %>% 
  mutate(Winners = round(Runs * WinPercent), Exp_Wins = Winners/AE_Ratio, 
         Archie = ifelse(Exp_Wins >= 5,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0),
         Placed_Archie = ifelse(Exp_Places >= 5.0,((Places * ((Places - Exp_Places) ^ 2)) / (Exp_Places * (Runs - Exp_Places))),0),
         Arch_Strength = cut(Archie, breaks = c(-1, 2.5, 3.5, 4.5, 5.5, 8.0, 100), 
                             labels = c("-", "*", "**", "***", "****", "*****")),
         Arch_Placed_Strength = cut(Placed_Archie, breaks = c(-1, 2.5, 3.5, 4.5, 5.5, 8.0, 100), 
                                    labels = c("-", "*", "**", "***", "****", "*****")),
         Val_Ratio = BetFairSPForecastWinPrice / ValueOdds_BetfairFormat)

allArchie <- allArchie %>% 
  select(Time24Hour, Meeting, Horse, System_Name, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie,
         Arch_Strength, Placed_AE_Ratio, Placed_Archie, Arch_Placed_Strength, BFSP_PL, VSP_PL, Betfair.Win.S.P., BFSP_Odds_Range,
         Actual, Expected, Betfair.Placed, Place_Expected, BF_Placed_SP_PL, Runs, Winners, Exp_Wins, WinPercent, meanPL, totalPL, 
         VSP_ROI, Places, Exp_Places, Place_Percent, BF_Place_ROI, Value_Odds_Range, VOR_Range, BFSPFC_Odds_Range, Trainer, Jockey, 
         Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, Rev_Weight_Rank, NumberOfResults, Alarms, 
         Runners, Month, Result, LengthsBehindTotal, Age) %>% 
  filter(Archie > 3.5, Exp_Wins >= 5, AE_Ratio >= 1.20, Placed_AE_Ratio > 1.05) %>% 
  arrange(Time24Hour, Meeting, Horse)

allArchie

length(unique(allArchie$Horse))

#View(allArchie)

highArchieQuals <- filter(allArchie, Archie >= 5.5, meanPL >= 0.20, WinPercent >= 0.12, AE_Ratio >= 1.30, Placed_AE_Ratio >= 1.10)

highArchieQuals <- highArchieQuals %>% 
  # select(Time24Hour, Meeting, Horse, Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range,
  #        BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, NumberOfResults, Alarms,
  #        Age, Runs, meanPL, totalPL, VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, Placed_Archie, Place_Percent, BF_Place_ROI ,AE_Ratio, WinPercent, Winners, Exp_Wins, Archie, 
  #        Arch_Strength, Arch_Placed_Strength) %>% 
  arrange(Time24Hour, Meeting, Horse)

highArchieQuals

#View(highArchieQuals)


eliteArchieQuals <- filter(allArchie, Archie >= 8.0, AE_Ratio >= 1.40, meanPL >= 0.20, WinPercent >= 0.12, Placed_AE_Ratio >= 1.15)

eliteArchieQuals <- eliteArchieQuals %>% 
  # select(Time24Hour, Meeting, Horse, Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range,
  #        BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, NumberOfResults, Alarms, 
  #        Age, Runs, meanPL, totalPL, VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, Placed_Archie, Place_Percent, BF_Place_ROI ,AE_Ratio, WinPercent, Winners, Exp_Wins, Archie, 
  #        Arch_Strength, Arch_Placed_Strength) %>% 
  arrange(Time24Hour, Meeting, Horse)

eliteArchieQuals

#View(eliteArchieQuals)

highAERQuals <- filter(allArchie, AE_Ratio >= 1.60, Placed_AE_Ratio >= 1.20) %>% 
  arrange(Time24Hour, Meeting, Horse)

highAERQuals
# 
# write_csv(asq, "All_System_Qualifiers_to_2018_08.csv")
# #
# write_csv(allArchie, "All_Archie_Quals_to_2018_08.csv")
# #
# write_csv(highArchieQuals, "All_High_Archie_Quals_to_2018_08.csv")
# #
# write_csv(eliteArchieQuals, "All_Elite_Archie_Quals_to_2018_08.csv")
# #
# write_csv(highAERQuals, "All_High_AER_Archie_Quals_to_2018_08.csv")

systemsAnalysisASQ_2018 <- read_csv("All_System_Qualifiers_to_2018_08.csv", col_names = T)

systemsAnalysis2018 <- read_csv("All_Archie_Quals_to_2018_08.csv", col_names = T)

systemsAnalysis2018High <- read_csv("All_High_Archie_Quals_to_2018_08.csv", col_names = T)

systemsAnalysis2018Elite <- read_csv("All_Elite_Archie_Quals_to_2018_08.csv", col_names = T)

systemsAnalysis2018AER <- read_csv("All_High_AER_Archie_Quals_to_2018_08.csv", col_names = T)

colnames(systemsAnalysis2018)

#sa <- systemsAnalysis2018 %>% 
 # filter(ValueOdds_BetfairFormat < BetFairSPForecastWinPrice)

systemsSummary <- systemsAnalysis2018 %>%
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummary

View(systemsSummary)

#colSums(systemsSummary[,2:13])

#write_csv(systemsSummary, "Summary_System_Results_Tables_July_2018.csv")

poorPerforming <- systemsSummary %>% 
  filter(Runs >= 30, AE_Ratio <= 0.9, meanPL < 0, Placed_AE_Ratio <= 1.0, Exp_Wins >= 5) %>% 
  arrange(AE_Ratio, desc(Archie))

poorPerforming

View(poorPerforming)

#write_csv(poorPerforming, "Poor_Performing_Systems_July_2018.csv")

goodPerforming <- systemsSummary %>% 
  filter(Runs >= 30, AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
  arrange(desc(AE_Ratio, Archie))

goodPerforming

View(goodPerforming)

systemsSummaryHigh <- systemsAnalysis2018High %>%
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummaryHigh

View(systemsSummaryHigh)

write_csv(systemsSummaryHigh, "Summary_System_Tables_High_Archie_2018_07.csv")


poorPerformingHigh <- systemsSummaryHigh %>% 
  filter(Runs >= 30, AE_Ratio <= 0.9, meanPL < 0, Placed_AE_Ratio <= 1.0, Exp_Wins >= 5) %>% 
  arrange(AE_Ratio, desc(Archie))

poorPerformingHigh

View(poorPerformingHigh)

goodPerformingHigh <- systemsSummaryHigh %>% 
  filter(Runs >= 30, AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
  arrange(desc(AE_Ratio, Archie))

goodPerformingHigh

View(goodPerformingHigh)

#write_csv(poorPerforming, "Poor_Performing_Systems_July_2018.csv")

systemsSummaryElite <- systemsAnalysis2018Elite %>%
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummaryElite

write_csv(systemsSummaryElite, "Summary_System_Tables_Elite_Archie_2018_07.csv")

View(systemsSummaryElite)

poorPerformingElite <- systemsSummaryElite %>% 
  filter(Runs >= 30, AE_Ratio <= 0.9, meanPL < 0, Placed_AE_Ratio <= 1.0, Exp_Wins >= 5) %>% 
  arrange(AE_Ratio, desc(Archie))

poorPerformingElite

View(poorPerformingElite)


goodPerformingElite <- systemsSummaryElite %>% 
  filter(Runs >= 30, AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
  arrange(desc(AE_Ratio, Archie))

goodPerformingElite

View(goodPerformingElite)

##############################################################

systemsAnalysisASQ_2018 <- systemsAnalysisASQ_2018 %>% 
  mutate(Archie_Range = cut(Archie, breaks = c(0, 4.0, 6.0, 8.0, 100),
                         labels = c("<=4", ">4 to 6", ">6 to 8",">8"), 
                         ordered_result = T),
         Placed_AER_Range = cut(Placed_AE_Ratio, breaks = c(0, 1.0, 1.1, 1.2, 100),
                                labels = c("<=1.0", ">1 to 1.1", ">1.1 to 1.2",">1.2"), 
                                ordered_result = T),
         AER_Range = cut(AE_Ratio, breaks = c(0, 1.3, 1.4, 1.6, 1.8, 100),
                         labels = c("<=1.3", ">1.3 to 1.4", ">1.4 to 1.6",">1.6 to 1.8", ">1.8"), 
                         ordered_result = T),
         
         Value_Odds_Ratio = BetFairSPForecastWinPrice / ValueOdds_BetfairFormat,
         
         VOR_Range_2 = cut(Value_Odds_Ratio, breaks = c(0, 0.5, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 5.0, 10.0, 1000),
                                labels = c("<=0.5", ">0.5 to 1.0", ">1.0 to 1.25",">1.25 to 1.50", ">1.5 to 1.75", ">1.75 to 2.0",
                                           ">2.0 to 2.5", ">2.5 to 5.0", ">5.0 to 10.0", ">10.0"), 
                                ordered_result = T))

         

SystemsSummaryASQ <- systemsAnalysisASQ_2018 %>%
  filter(AE_Ratio >= 1.40, Archie >= 4.0, Ratings_Range != "Bottom_Third") %>% 
  group_by(VOR_Range_2) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

SystemsSummaryASQ

View(SystemsSummaryASQ)

poorPerformingASQ <- SystemsSummaryASQ %>% 
  filter(Runs >= 30, AE_Ratio < 1.0, Placed_AE_Ratio < 1.0, Exp_Wins >= 5) %>% 
  arrange(AE_Ratio, desc(Archie))

poorPerformingASQ

View(poorPerformingASQ)


goodPerformingASQ <- SystemsSummaryASQ %>% 
  filter(Runs >= 30, AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
  arrange(desc(AE_Ratio, Archie))

goodPerformingASQ

View(goodPerformingASQ)

SystemsSummaryASQ_Filtered <- SystemsSummaryASQ %>% 
  filter(!(System_Name %in% poorPerformingASQ$System_Name)) %>% 
  arrange(desc(AE_Ratio, Archie))

SystemsSummaryASQ_Filtered
  

View(SystemsSummaryASQ_Filtered)






write_csv(goodPerformingASQ, "Good_Performing_Systems_July_2018.csv" )
# 
write_csv(poorPerformingASQ, "Poor_Performing_Systems_July_2018.csv")

#remove(dupes)

allAWQuals <- todayAllAW_Qualifiers %>% 
  filter(Archie > 3.5, Exp_Wins >= 5, AE_Ratio >= 1.20, Placed_AE_Ratio > 1.05) 

awTemp <- allAWQuals %>% 
  select(DayOfMonth, Month, Year, Meeting, Horse)

x <- (which(duplicated(awTemp)))

awq <- allAWQuals[-x,]

summary(awq$BFSP_PL)
summary(awq$Actual)
summary(awq$BF_Placed_SP_PL)
sum(awq$BFSP_PL)
sum(awq$VSP_PL)


allFQuals <- allFlatQuals %>% 
  filter(Archie > 3.5, Exp_Wins >= 5, AE_Ratio >= 1.20, Placed_AE_Ratio > 1.05) 

fTemp <- allFQuals %>% 
  select(DayOfMonth, Month, Year, Meeting, Horse)

x <- (which(duplicated(fTemp)))

afq <- allFQuals[-x,]
  

summary(afq$BFSP_PL)
summary(afq$Actual)
summary(afq$BF_Placed_SP_PL)
sum(afq$BFSP_PL)
sum(afq$VSP_PL)


allNHQuals <- allNHSystemQualifiers %>% 
  filter(Archie > 3.5, Exp_Wins >= 5, AE_Ratio >= 1.20, Placed_AE_Ratio > 1.05) 

nhTemp <- allNHQuals %>% 
  select(DayOfMonth, Month, Year, Meeting, Horse)

x <- (which(duplicated(nhTemp)))

nhq <- allNHQuals[-x,]


summary(nhq$BFSP_PL)
summary(nhq$Actual)
summary(nhq$BF_Placed_SP_PL)
sum(nhq$BFSP_PL)
sum(nhq$VSP_PL)


allExtraQuals <- todayExtraQuals %>% 
  filter(Archie > 3.5, Exp_Wins >= 5, AE_Ratio >= 1.20, Placed_AE_Ratio > 1.05) 

eqTemp <- allExtraQuals %>% 
  select(DayOfMonth, Month, Year, Meeting, Horse)

x <- (which(duplicated(eqTemp)))

aeq <- allExtraQuals[-x,]

summary(aeq$BFSP_PL)
summary(aeq$Actual)
summary(aeq$BF_Placed_SP_PL)
sum(aeq$BFSP_PL)
sum(aeq$VSP_PL)
  


aeqSummary <- aeq %>%
  group_by(Ratings_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2), Winners = sum(Actual), 
            Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

aeqSummary

View(aeqSummary)

awqSummary <- awq %>%
  group_by(Ratings_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

awqSummary

View(awqSummary)

all2018Q <- awq %>% 
  full_join(afq) %>% 
  full_join(nhq) %>% 
  full_join(aeq)

all2018Temp <- all2018Q %>% 
  select(DayOfMonth, Month, Year, Meeting, Horse)

x <- (which(duplicated(all2018Temp)))

all2018Quals <- all2018Q[-x,]

summary(all2018Quals$BFSP_PL)
summary(all2018Quals$Actual)
summary(all2018Quals$BF_Placed_SP_PL)
sum(all2018Quals$BFSP_PL)
sum(all2018Quals$VSP_PL)

allSummary <- all2018Quals %>%
  group_by(Ratings_Range, VOR_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

allSummary

View(allSummary)

#Write all System Qualifiers to file, including multi-system qualifiers (duplicates)

allSystem2018Qualifiers <- allAWQuals %>% 
  full_join(allFQuals) %>% 
  full_join(allNHQuals) %>% 
  full_join(allExtraQuals)

write_csv(allSystem2018Qualifiers, "AllSystemsQualifiers_2018_07.csv")


# write all code qulifiers, including multi-system qualifiers (duplicates) to separate code files

write_csv(allAWQuals, "All_AW_System_Quals_2018_07.csv")
write_csv(allFQuals, "All_Flat_System_Quals_2018_07.csv")
write_csv(allNHQuals, "All_NH_System_Quals_2018_07.csv")
write_csv(allExtraQuals, "All_Extra_System_Quals_2018_07.csv")


############################################################################


top5Q <- today %>%
  drop_na(Rating_Rank, ValueOdds_BetfairFormat, BetFairSPForecastWinPrice, VOR_Range, Value_Odds_Range,
          Speed_Rank_Range, BFSP_PL, Betfair.Win.S.P., BFSP_ValOdds_Ratio, BFSP_VOR_Range, BFSP_Odds_Range) %>% 
  filter(Rating_Rank <= 5, ValueOdds_BetfairFormat <= 21, Runners >= 5) %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(HiRnkValBet = if_else(Value_Odds_Ratio > 1.0, 1, 0)) %>% 
  filter(HiRnkValBet == 1) %>% 
  select(UKHRCardRaceID, Time24Hour, Meeting, Horse, Ratings_Range, Speed_Rank_Range, RaceType, Handicap,
         Rating_Rank, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Value_Odds_Ratio, VOR_Range, 
         BFSP_ValOdds_Ratio, BFSP_VOR_Range, BFSP_Odds_Range,
         Value_Odds_Range, HiRnkValBet, Actual,  Betfair.Placed, BF_Placed_SP_PL, Betfair.Win.S.P., BFSP_PL, everything()) %>% 
  mutate(Min_Rnk_Val_Bet = min_rank(Rating_Rank)) %>% 
  filter(Min_Rnk_Val_Bet == 1, Value_Odds_Ratio <= 5.0) %>% 
  arrange(Time24Hour, Meeting, Horse)

top5Q

#write_csv(top5Q, "ValueTop5_Quals_to_July_2018_.csv")

top5QSummary <- top5Q %>%
  group_by() %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0) %>%
  arrange(desc(AE_Ratio, Archie))

top5QSummary

View(top5QSummary)


# dualQuals <- allArchie %>% 
#   filter(Horse %in% top5Q$Horse)
# 
# dualQuals
# 
# write_csv(dualQuals, paste0("Dual_Quals_", today$Date[1], ".csv"))


systemsSummary <- systemsAnalysis2018 %>%
  filter(AE_Ratio >= 1.2, Archie >= 6.50) %>% 
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))


systemsSummary


sum(systemsSummary$totalPL)
mean(systemsSummary$totalPL)

View(systemsSummary)


####################################

mean(systemsAnalysisASQ_2018$BFSP_PL)
mean(systemsAnalysisASQ_2018$VSP_PL)



colnames(systemsAnalysisASQ_2018)

summary(systemsAnalysisASQ_2018$BFSP_PL)

View(head(systemsAnalysisASQ_2018))

#####################################################

systemsSummary <- systemsAnalysisASQ_2018 %>%
  filter(AE_Ratio >= 1.4, Archie >= 4.0, Ratings_Range != "Bottom_Third") %>% 
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))


systemsSummary

View(systemsSummary)

###########################################################

# Model 

library(caret)
library(earth)
library(broom)
library(xgboost)

systemsAnalysisASQ_2018 <- read_csv("All_System_Qualifiers_to_2018_08.csv", col_names = T)

head(systemsAnalysisASQ_2018)

colSums(is.na(systemsAnalysisASQ_2018))

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

View(head(ukNum))

# ukPreds <- select(ukNum, -c(BFSP_PL, VSP_PL))
# ukClass <- ukNum$VSP_PL

ukNum <- select(ukNum, -c(Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake, Runners, BFSP_PL))
  
# Build Model
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



print(knnTune)

saveRDS(knnTune, "KNN_Systems_VSP_PL_Model.RDS")

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
# 
# train.control <- trainControl(method = "repeatedcv",
#                               number = 10,
#                               repeats = 3,
#                               #summaryFunction = RMSE,
#                               verboseIter = T)
# 
# summary(ukTrainSet)
# str(ukTrainSet)
# 
# 
# set.seed(100)
# 
# marsModUK <- train(BFSP_PL ~ ValueOdds_BetfairFormat +
#                      Val_Ratio +
#                      NumberOfResults +
#                      Runs +
#                      meanPL +
#                      totalPL +
#                      Avg_BFVSP_PL +
#                      Total_BFVSP_PL +
#                      Placed_AE_Ratio +
#                      AE_Ratio +
#                      WinPercent +
#                      Exp_Wins +
#                      Exp_Places +
#                      Runners +
#                      Placed_Archie +
#                      Archie,
#                    data = ukTrainSet,
#                    method = "earth",
#                    tuneGrid = marsGrid,
#                    metric = "RMSE",
#                    trControl = train.control)
# 
# print(marsModUK)
# 
# varImp(marsModUK)

#predBFSPMARS <- predict(marsModUK, newdata = ukTestSet, type = "raw")



top5Q <- ukhr_master_BF %>% 
  mutate(#Value_Odds_Ratio = Betfair.Win.S.P. / ValueOdds_BetfairFormat,
         Value_Price = if_else(Value_Odds_Ratio > 1.0, 1, 0)) %>%
  drop_na(Time24Hour, Meeting, Horse, Rating_Rank, Speed_Rank_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Value_Odds_Ratio,BFSP_PL, VSP_PL, 
          Betfair.Win.S.P., BFSP_Odds_Range, Actual, Expected, Betfair.Placed, Place_Expected, BF_Placed_SP_PL,  
          Value_Odds_Range, VOR_Range, BFSPFC_Odds_Range, Trainer, Jockey, 
          Sire, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, Rev_Weight_Rank, NumberOfResults, Alarms, 
          Runners, Month, Result, LengthsBehindTotal, Age) %>% 
  filter(Rating_Rank <= 4, ValueOdds_BetfairFormat <= 21, Runners >= 5) %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Total_Value_Runners = sum(Value_Price),
         HiRnkValBet = if_else(Value_Odds_Ratio > 1.0, 1, 0)) %>% 
  filter(HiRnkValBet == 1) %>% 
  select(Time24Hour, Meeting, Horse, Rating_Rank, Speed_Rank_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Value_Odds_Ratio,BFSP_PL, VSP_PL, 
         Betfair.Win.S.P., BFSP_Odds_Range, Actual, Expected, Betfair.Placed, Place_Expected, BF_Placed_SP_PL,Total_Value_Runners,
         Value_Odds_Range, VOR_Range, BFSPFC_Odds_Range, Trainer, Jockey, FC_Fav_Rank, FcFav_Odds_Range, 
         Sire, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, Rev_Weight_Rank, NumberOfResults, Alarms, 
         Runners, Month, Result, LengthsBehindTotal, Age) %>% 
  mutate(Min_Rnk_Val_Bet = min_rank(Rating_Rank)) %>% 
  filter(Min_Rnk_Val_Bet == 1, Value_Odds_Ratio <= 5.0, Ratings_Range != "Bottom_Third") %>% 
  arrange(Time24Hour, Meeting, Horse)

top5Q

t5Summary <- top5Q %>% 
  filter(Value_Odds_Ratio > 1, Speed_Rank_Range != "Bottom_Third") %>% 
  group_by(VOR_Range, Rating_Rank) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.12, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

t5Summary

View(t5Summary)  
