
library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)
library(doMC)
# library(doParallel)
# cl <- makePSOCKcluster(4) # number of cores to use
# registerDoParallel(cl)


setwd("~/git_projects/UKHR_Project")

registerDoMC(4)

ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_11_30.csv",col_names = T)

#colnames(ukhr_master_BF)

#head(ukhr_master_BF$Weight_Rank, 30)   
#head(ukhr_master_BF$Weight_Pounds, 30)



#head(ukhr_master_BF$VSP_Stake)

# Load today's racecard

#today <- read_csv(file.choose(),col_names = T)


# ukhr_master_BF %>% 
#   filter(Year == 2018) %>% 
#   write_csv("uk2018_07.csv")

q1 <- which(ukhr_master_BF$Month <= 3 & ukhr_master_BF$Year == 2018)
q2 <- which(ukhr_master_BF$Month > 3 & ukhr_master_BF$Month <= 6 & ukhr_master_BF$Year == 2018)
q3 <- which(ukhr_master_BF$Month >6 & ukhr_master_BF$Month <= 9 & ukhr_master_BF$Year == 2018)
q4 <- which(ukhr_master_BF$Month >9 & ukhr_master_BF$Year == 2018)

mid <- c(q1,q2)
threeQtr <- c(q1,q2,q3)


today <- ukhr_master_BF[q4,]

ukhr_master_BF <- ukhr_master_BF[-q4,]

# uk1 <- ukhr_master_BF[q1,]
# uk2 <- ukhr_master_BF[mid,]
# 
# 
# ukhr_master_BF <-  ukhr_master_BF %>% 
#   filter(Year <= 2017) 
# 
# ukhr_master_BF <- rbind(ukhr_master_BF, uk2)

summary(today$Month)
table(today$Month, today$Year)
table(ukhr_master_BF$Month, ukhr_master_BF$Year)

if (sum(is.na(today$BetFairSPForecastWinPrice) > 0)) {
  
  library(caret)
  
  bfspToday <- today %>% 
    select(RatingsPosition, RatingAdvantage, ConnRanking, ClassPosition, Runners, BetFairSPForecastWinPrice)
  
  bfspImpute <- preProcess(bfspToday, method = "bagImpute")
  imputedBFSP <- predict(bfspImpute, bfspToday)
  #View(imputedBFSP)
  
  today$BetFairSPForecastWinPrice <- imputedBFSP$BetFairSPForecastWinPrice
}  

# str(ukhr_master_BF$BetFairSPForecastWinPrice)
# str(today$BetFairSPForecastWinPrice)

slowGround <- c("SOFT","SFT-HVY","HEAVY", "GD-SFT", "YIELD", "GD-YLD", "YLD-SFT")

fastGround <- c("GD-FM", "FIRM", "HARD")

syntheticGround <- c("STAND", "STD-SLOW", "STANDARD", "STD-FAST", "SLOW")

softGround <- c("SOFT", "SFT-HVY", "HEAVY")

# str(today$BetFairSPForecastWinPrice)
# str(ukhr_master_BF$BetFairSPForecastWinPrice)

source("AW_Systems.R")
source("Flat_Systems.R")
source("NH_Systems.R")
source("Extra_Qualifiers.R")

#############################################

allAWQuals <- todayAllAW_Qualifiers %>% 
  arrange(Time24Hour,Meeting,Horse)

allAWQuals

#View(allAWQuals)

#write_csv(allAWQuals, "todays_ALL_AW_Qualifiers_2017_22_12_17.csv")

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

#allSystemQualifiers$Val_Ratio <-allSystemQualifiers$BetFairSPForecastWinPrice / allSystemQualifiers$ValueOdds_BetfairFormat

head(allSystemQualifiers)

asq <- allSystemQualifiers %>% 
  mutate(Winners = round(Runs * WinPercent), Exp_Wins = Winners/AE_Ratio, 
         Archie = ifelse(Exp_Wins >= 5,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0),
         Placed_Archie = ifelse(Exp_Places >= 5.0, round(((Places * ((Places - Exp_Places) ^ 2)) / (Exp_Places * (Runs - Exp_Places))),2),0),
         #Btn_Archie = ifelse(Total_Exp_Btn >= 5,((Runs * ((Total_Btn - Total_Exp_Btn) ^ 2)) / (Total_Exp_Btn * (Runs - Total_Exp_Btn))),0),
         Place_Percent = Places/Runs,
         Arch_Strength = cut(Archie, breaks = c(-1, 2.5, 3.5, 4.5, 5.5, 8.0, 100), 
                             labels = c("-", "*", "**", "***", "****", "*****")),
         Arch_Placed_Strength = cut(Placed_Archie, breaks = c(-1, 2.5, 3.5, 4.5, 5.5, 8.0, 100), 
                                    labels = c("-", "*", "**", "***", "****", "*****")),
         #Arch_Btn_Strength = cut(Btn_Archie, breaks = c(-1, 2.5, 3.5, 4.5, 5.5, 8.0, 100), 
         #labels = c("-", "*", "**", "***", "****", "*****")),
         Val_Ratio = BetFairSPForecastWinPrice / ValueOdds_BetfairFormat) %>% 
  select(Time24Hour, Meeting, Horse, System_Name, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Arch_Strength, 
         Placed_AE_Ratio, Placed_Archie, Arch_Placed_Strength, Btn_AE_Ratio, Total_Exp_Btn, Total_Btn, Runs, Winners, Exp_Wins, WinPercent, meanPL, 
         totalPL, VSP_ROI, Places, Exp_Places, Place_Percent, BF_Place_ROI, Value_Odds_Range, VOR_Range, BFSPFC_Odds_Range, Trainer, Jockey, Sire, 
         Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, Rev_Weight_Rank, NumberOfResults, Alarms, Age, Month, Season,
         BFSP_PL, BF_Placed_SP_PL, VSP_PL, Actual, Expected, Betfair.Placed, Place_Expected, Betfair.Win.S.P., Betfair.Place.S.P.) %>% 
  filter(AE_Ratio >= 1.20, Exp_Wins > 5.0, Archie > 2.5) %>% 
  arrange(Time24Hour, Meeting, Horse)

asq

#####################################################################################

allArchie <- asq %>% 
  select(Time24Hour, Meeting, Horse, System_Name, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Arch_Strength, 
         Placed_AE_Ratio, Placed_Archie, Arch_Placed_Strength, Btn_AE_Ratio, Total_Exp_Btn, Total_Btn, Runs, Winners, Exp_Wins, WinPercent, meanPL, 
         totalPL, VSP_ROI, Places, Exp_Places, Place_Percent, BF_Place_ROI, Value_Odds_Range, VOR_Range, BFSPFC_Odds_Range, Trainer, Jockey, Sire, 
         Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, Rev_Weight_Rank, NumberOfResults, Alarms, Age, Month, Season,
         BFSP_PL, BF_Placed_SP_PL, VSP_PL, Actual, Expected, Betfair.Placed, Place_Expected, Betfair.Win.S.P., Betfair.Place.S.P.) %>% 
  filter(Archie >= 4.0, Exp_Wins >= 5, AE_Ratio >= 1.20) %>% 
  arrange(Time24Hour, Meeting, Horse)

allArchie

#iew(allArchie)

length(unique(allArchie$Horse))

#todayArchie <- allArchie %>% 
#left_join(systemSummaryTables, by = "System_Name")

# todayArchie
# 

# View(archieGoodSystems)
# 
# 
highArchieQuals <- filter(asq, Archie >= 8.5, AE_Ratio >= 1.20)

highArchieQuals <- highArchieQuals %>%
  select(Time24Hour, Meeting, Horse, System_Name,Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range,
         BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, NumberOfResults, Alarms,
         Age, Month, Season, Runs, meanPL, totalPL, VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, Placed_Archie, Place_Percent, BF_Place_ROI, AE_Ratio, 
         WinPercent, Winners, Exp_Wins, Archie, Total_Exp_Btn, Total_Btn, Btn_AE_Ratio, Arch_Strength, Arch_Placed_Strength,
         BFSP_PL, BF_Placed_SP_PL, VSP_PL, Actual, Expected, Betfair.Placed, Place_Expected, Betfair.Win.S.P., Betfair.Place.S.P.) %>%
  arrange(Time24Hour, Meeting, Horse)

highArchieQuals

#View(highArchieQuals)

goodStatsQuals <- filter(asq, Archie >= 4.0, AE_Ratio >= 1.40, Placed_AE_Ratio >= 1.10)

goodStatsQuals <- goodStatsQuals %>%
  select(Time24Hour, Meeting, Horse, System_Name, Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, 
         Ratings_Range, VOR_Range, BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, 
         Rev_Weight_Rank, NumberOfResults, Alarms, Age, Month, Season, Runs, meanPL, totalPL, VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, Placed_Archie,
         Place_Percent, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie, Total_Exp_Btn, Total_Btn, Btn_AE_Ratio, Arch_Strength, 
         Arch_Placed_Strength, BFSP_PL, BF_Placed_SP_PL, VSP_PL, Actual, Expected, Betfair.Placed, Place_Expected, Betfair.Win.S.P., Betfair.Place.S.P.) %>%
  arrange(Time24Hour, Meeting, Horse)

goodStatsQuals

#View(goodStatsQuals)

write_csv(asq, "All_System_Qualifiers_Q4_2018.csv")
# #
write_csv(allArchie, "All_Archie_Quals_Q4_2018.csv")
# #
write_csv(highArchieQuals, "All_High_Archie_Quals_Q4_2018.csv")
# #
write_csv(goodStatsQuals, "All_Good_Stats_Quals_Q4_2018.csv")

source("Draw_Range_Analysis.R")
source("Min_Rank_Val_Bet.R")
source("Today_Systems_Model.R")
# #
# write_csv(highAERQuals, "All_High_AER_Archie_Quals_to_2018_08.csv")

qtr1 <- read_csv("All_System_Qualifiers_Q1_2018.csv", col_names = T)
qtr2 <- read_csv("All_System_Qualifiers_Q2_2018.csv", col_names = T)
qtr3 <- read_csv("All_System_Qualifiers_Q3_2018.csv", col_names = T)
qtr4 <- read_csv("All_System_Qualifiers_Q4_2018.csv", col_names = T)

asq2018 <- rbind(qtr1, qtr2, qtr3, qtr4)

write_csv(asq2018, "All_System_Qualifiers_Yr_2018.csv")

systemsAnalysisASQ_2018 <- read_csv("All_System_Qualifiers_Yr_2018.csv", col_names = T)

systemsAnalysisArchie_2018 <- read_csv("All_Archie_Quals_to_2018_10.csv", col_names = T)

systemsAnalysisHighArch_2018 <- read_csv("All_High_Archie_Quals_to_2018_10.csv", col_names = T)

systemsAnalysisGoodStats_2018 <- read_csv("All_Good_Stats_Quals_to_2018_10.csv", col_names = T)

#systemsAnalysis2018AER <- read_csv("All_High_AER_Archie_Quals_to_2018_08.csv", col_names = T)

str(systemsAnalysisASQ_2018)

colnames(systemsAnalysisASQ_2018)

summary(systemsAnalysisASQ_2018$Month)

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
                           ordered_result = T),
         Btn_AER_Range = cut(Btn_AE_Ratio, breaks = c(0, 1.0, 50.0),
                                labels = c("<=1.0",">1.0"), 
                                ordered_result = T),
         BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 6, 11, 21, 51, 1000),
                                             labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                                             ordered_result = T))


#sa <- systemsAnalysis2018 %>% 
 # filter(ValueOdds_BetfairFormat < BetFairSPForecastWinPrice)

systemsSummaryAll <- systemsAnalysisASQ_2018 %>%
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = round((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)),2)) %>%
  #filter(Handicap == "HANDICAP") %>% 
  
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummaryAll

View(systemsSummaryAll)

systemsSummaryAll %>% 
  summarise(Avg_Profit = sum(totalPL)/sum(Runs),
            Total_Profit = sum(totalPL),
            Total_Runners = sum(Runs))

#colSums(systemsSummary[,2:13])

#write_csv(systemsSummary, "Summary_System_Results_Tables_July_2018.csv")

poorPerforming <- systemsSummaryAll %>% 
  filter(Runs >= 50, AE_Ratio <= 0.9, meanPL < 0, Placed_AE_Ratio <= 1.0, Exp_Wins >= 5) %>% 
  arrange(AE_Ratio, desc(Archie))

poorPerforming

View(poorPerforming)

write_csv(poorPerforming, "Poor_Performing_Systems.csv")

goodPerforming <- systemsSummaryAll %>% 
  filter(Runs >= 50, AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
  arrange(desc(AE_Ratio, Archie))

goodPerforming

View(goodPerforming)

write_csv(goodPerforming, "Good_Performing_Systems.csv")

systemsAnalysisHighArch_2018 <- systemsAnalysisHighArch_2018 %>% 
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
                           ordered_result = T),
         Btn_AER_Range = cut(Btn_AE_Ratio, breaks = c(0, 1.0, 50.0),
                             labels = c("<=1.0",">1.0"), 
                             ordered_result = T),
         BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 6, 11, 21, 51, 1000),
                               labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                               ordered_result = T))


systemsSummaryHigh <- systemsAnalysisHighArch_2018 %>%
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = round((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)),2)) %>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummaryHigh

View(systemsSummaryHigh)

systemsSummaryHigh %>% 
  summarise(Avg_Profit = sum(totalPL)/sum(Runs),
            Total_Profit = sum(totalPL),
            Total_Runners = sum(Runs))

#write_csv(systemsSummaryHigh, "Summary_System_Tables_High_Archie_2018_07.csv")


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

systemsAnalysisGoodStats_2018 <- systemsAnalysisGoodStats_2018 %>% 
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
                           ordered_result = T),
         Btn_AER_Range = cut(Btn_AE_Ratio, breaks = c(0, 1.0, 50.0),
                             labels = c("<=1.0",">1.0"), 
                             ordered_result = T),
         BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 6, 11, 21, 51, 1000),
                               labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                               ordered_result = T))

systemsAnalysisGoodStats_2018


systemsSummaryGoodStats <- systemsAnalysisGoodStats_2018 %>%
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = round((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)),2)) %>%
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummaryGoodStats

#write_csv(systemsSummaryElite, "Summary_System_Tables_Elite_Archie_2018_07.csv")

View(systemsSummaryGoodStats)

systemsSummaryGoodStats %>% 
  summarise(Avg_Profit = sum(totalPL)/sum(Runs),
            Total_Profit = sum(totalPL),
            Total_Runners = sum(Runs))


poorPerformingGoodStats <- systemsSummaryGoodStats %>% 
  filter(Runs >= 30, AE_Ratio <= 0.9, meanPL < 0, Placed_AE_Ratio <= 1.0, Exp_Wins >= 5) %>% 
  arrange(AE_Ratio, desc(Archie))

poorPerformingGoodStats

View(poorPerformingGoodStats)


goodPerformingGoodStats <- systemsSummaryGoodStats %>% 
  filter(Runs >= 30, AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
  arrange(desc(AE_Ratio, Archie))

goodPerformingGoodStats

View(goodPerformingGoodStats)

# systemsSummaryGoodStats %>% 
#   summarise(Avg_Profit = sum(totalPL)/sum(Runs),
#             Total_Profit = sum(totalPL),
#             Total_Runners = sum(Runs))




systemsSummaryAllHcp <- systemsAnalysisASQ_2018 %>%
  filter(Handicap != "NONHCP", Ratings_Range != "Bottom_Third") %>% 
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = round((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)),2))%>%
  #filter(Handicap == "HANDICAP") %>% 
  
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummaryAllHcp

View(systemsSummaryAllHcp)

poorPerformingHcp <- systemsSummaryAllHcp %>% 
  filter(Runs >= 30, AE_Ratio <= 0.9, meanPL < 0, Placed_AE_Ratio <= 1.0, Exp_Wins >= 5) %>% 
  arrange(AE_Ratio, desc(Archie))

poorPerformingHcp

View(poorPerformingHcp)

write_csv(poorPerformingHcp, "Poor_Performing_Hcp_Systems.csv")


goodPerformingHcp <- systemsSummaryAllHcp %>% 
  filter(Runs >= 30, AE_Ratio > 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
  arrange(desc(AE_Ratio, Archie))

goodPerformingHcp

View(goodPerformingHcp)

write_csv(goodPerformingHcp, "Good_Performing_Hcp_Systems.csv")

systemsSummaryAllHcp %>% 
  summarise(Avg_Profit = sum(totalPL)/sum(Runs),
            Total_Profit = sum(totalPL),
            Total_Runners = sum(Runs))


systemsSummaryNonHcp <- systemsAnalysisASQ_2018 %>%
  filter(Handicap == "NONHCP") %>% 
  group_by(System_Name) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = round((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)),2))%>%
  #filter(Handicap == "HANDICAP") %>% 
  
  #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

systemsSummaryNonHcp

View(systemsSummaryNonHcp)

systemsSummaryNonHcp %>% 
  summarise(Avg_Profit = sum(totalPL)/sum(Runs),
            Total_Profit = sum(totalPL),
            Total_Runners = sum(Runs))


##############################################################
# SystemsSummaryASQ <- systemsAnalysisASQ_2018 %>%
#   filter(AE_Ratio >= 1.40, Archie >= 4.0, Ratings_Range != "Bottom_Third") %>% 
#   group_by(VOR_Range_2) %>%
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             #Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             #Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
#             BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
#             Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   #filter(Runs >= 20, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
#   arrange(desc(AE_Ratio, Archie))
# 
# SystemsSummaryASQ
# 
# View(SystemsSummaryASQ)
# 
# poorPerformingASQ <- SystemsSummaryASQ %>% 
#   filter(Runs >= 30, AE_Ratio < 1.0, Placed_AE_Ratio < 1.0, Exp_Wins >= 5) %>% 
#   arrange(AE_Ratio, desc(Archie))
# 
# poorPerformingASQ

# View(poorPerformingASQ)
# 
# 
# goodPerformingASQ <- SystemsSummaryASQ %>% 
#   filter(Runs >= 30, AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.0, Exp_Wins >= 5) %>% 
#   arrange(desc(AE_Ratio, Archie))
# 
# goodPerformingASQ
# 
# View(goodPerformingASQ)
# 
# SystemsSummaryASQ_Filtered <- SystemsSummaryASQ %>% 
#   filter(!(System_Name %in% poorPerformingASQ$System_Name)) %>% 
#   arrange(desc(AE_Ratio, Archie))
# 
# SystemsSummaryASQ_Filtered
#   
# 
# View(SystemsSummaryASQ_Filtered)
# 
# 
# 
# 
# 
# 
# write_csv(goodPerformingASQ, "Good_Performing_Systems_July_2018.csv" )
# # 
# write_csv(poorPerformingASQ, "Poor_Performing_Systems_July_2018.csv")

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
  group_by(Rating_Rank) %>%
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
library(tidyverse)
library(stringi)
library(stringr)
library(caret)
library(earth)
library(broom)
library(xgboost)

systemsAnalysisASQ_2018 <- read_csv("All_System_Qualifiers_to_2018_10.csv", col_names = T)

head(systemsAnalysisASQ_2018)

colSums(is.na(systemsAnalysisASQ_2018))

uk <- systemsAnalysisASQ_2018 %>% 
  select(Meeting:Exp_Wins, Placed_Archie, Archie, BFSP_PL ,VSP_PL, -c(Alarms))

colnames(uk)

ukNum <- uk %>% 
  select_if(is.numeric)

colnames(uk)

summary(uk)

colSums(is.na(ukNum))

ukNum <- ukNum %>% 
  drop_na(Val_Ratio, BetFairSPForecastWinPrice)

head(ukNum)

View(head(ukNum))

# ukPreds <- select(ukNum, -c(BFSP_PL, VSP_PL))
# ukClass <- ukNum$VSP_PL

ukNum <- select(ukNum, -c(Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake, Runners, BFSP_PL))
  
# Build Model
# 
# Load Model
#xgbTune <- readRDS("xgb_Systems_VSP_PL_Model.RDS")

set.seed(100)

ukTrainRows <- createDataPartition(ukNum$VSP_PL, p = 0.60, list = FALSE)

ukTrainSet <- ukNum[ukTrainRows,]

ukTestSet <- ukNum[-ukTrainRows,]

set.seed(100)

tune.grid <- expand.grid(eta = c(0.1, 0.2),
                         nrounds = c(100,150),
                         lambda = c(0.1,0.2),
                         alpha = c(0.5,1.0))




xgbModUKHR <- train(VSP_PL ~ ., 
                    data = ukNum,
                    method = "xgbLinear",
                    metric = "RMSE",
                    tuneGrid = tune.grid,
                    trControl = trainControl(method = "cv",
                                             number = 10,
                                             repeats = 3,
                                             verboseIter = T))

print(xgbModUKHR)
varImp(xgbModUKHR)


saveRDS(xgbModUKHR, "xgb_Systems_VSP_PL_Model.RDS")

xgbPreds <- predict(xgbModUKHR, newdata = ukTestSet, type = "raw")

head(xgbPreds)
head(ukTestSet$VSP_PL)

df <- tibble(actual = ukTestSet$VSP_PL,
             preds = xgbPreds)


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

xgbModUKHR <- readRDS("xgb_Systems_VSP_PL_Model.RDS")


todayPreds <- predict(xgbModUKHR, newdata = today, type = "raw")

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

top5Q <- top5Q %>% 
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

t5Summary <- t5 %>% 
  #filter(Value_Odds_Ratio > 1, Speed_Rank_Range != "Bottom_Third") %>% 
  group_by(Handicap) %>%
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


#################################

ratingsSummary <- ukhr_master_BF %>%
  #filter(AE_Ratio >= 1.4, Archie >= 4.0, Ratings_Range != "Bottom_Third") %>% 
  group_by(Handicap, VOR_Range, Ratings_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 100, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))


ratingsSummary

View(ratingsSummary)

#####################################
dq <- read_csv("Dual_Quals_to_2018_10.csv", col_names = T)

colnames(dq)

str(dq)

dq2 <- dq %>% 
  mutate(Archie_Range = cut(Archie, breaks = c(0, 4.0, 6.0, 8.0, 100),
                            labels = c("<=4", ">4 to 6", ">6 to 8",">8"), 
                            ordered_result = T),
         Placed_AER_Range = cut(Placed_AE_Ratio, breaks = c(0, 1.0, 1.1, 1.2, 100),
                                labels = c("<=1.0", ">1 to 1.1", ">1.1 to 1.2",">1.2"), 
                                ordered_result = T),
         AER_Range = cut(AE_Ratio, breaks = c(0, 1.3, 1.4, 1.6, 1.8, 100),
                         labels = c("<=1.3", ">1.3 to 1.4", ">1.4 to 1.6",">1.6 to 1.8", ">1.8"), 
                         ordered_result = T),
         Value_Odds_Ratio = BetFairSPForecastWinPrice.x / ValueOdds_BetfairFormat,
         VOR_Range_2 = cut(Value_Odds_Ratio, breaks = c(0, 0.5, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 5.0, 10.0, 1000),
                           labels = c("<=0.5", ">0.5 to 1.0", ">1.0 to 1.25",">1.25 to 1.50", ">1.5 to 1.75", ">1.75 to 2.0",
                                      ">2.0 to 2.5", ">2.5 to 5.0", ">5.0 to 10.0", ">10.0"), 
                           ordered_result = T),
         Btn_AER_Range = cut(Btn_AE_Ratio, breaks = c(0, 1.0, 50.0),
                             labels = c("<=1.0",">1.0"), 
                             ordered_result = T),
         BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 6, 11, 21, 51, 1000),
                               labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                               ordered_result = T))


dqSummary <- dq2 %>%
  #filter(Ratings_Range.x == "Top_Third") %>% 
  group_by(BFSP_Odds_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 50, Archie > 2.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))


dqSummary

View(dqSummary)


t3gd <- read_csv("Top_3_Rated_Good_Draw_to_2018_10.csv", col_names = T)

colnames(t3gd)

t3gd <- t3gd %>% 
  left_join(ukhr_master_BF)

t3gdSummary <- t3gd %>%
  #filter(Ratings_Range.x == "Top_Third") %>% 
  group_by(Rating_Rank) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 50, Archie > 2.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))


t3gdSummary

################################################################################################################################

nnMod <- read_csv("Today_Sys_Quals_to_2018_10.csv")

colnames(nnMod)

nnMod2 <- nnMod %>% 
  mutate(Archie_Range = cut(Archie, breaks = c(0, 4.0, 6.0, 8.0, 100),
                            labels = c("<=4", ">4 to 6", ">6 to 8",">8"), 
                            ordered_result = T),
         NN_Mod_Range = cut(PredNNPL, breaks = c(-100, 0, 0.10, 0.20, 100),
                            labels = c("<=0", ">0 to 0.10", ">0.10 to 0.20", ">0.20"),
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
                           ordered_result = T),
         Btn_AER_Range = cut(Btn_AE_Ratio, breaks = c(0, 1.0, 50.0),
                             labels = c("<=1.0",">1.0"), 
                             ordered_result = T),
         BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 6, 11, 21, 51, 1000),
                               labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                               ordered_result = T))

colnames(nnMod)
summary(nnMod2$Month)

nnModSummary <- nnMod2 %>%
  filter(Month == 10) %>% 
  group_by(NN_Mod_Range, Handicap) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), 
            BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), 
            Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  #filter(Runs >= 50, Archie > 2.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))


nnModSummary

View(nnModSummary)

  