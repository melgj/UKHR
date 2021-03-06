#todays qualifiers combined in one file

#join AW Trainer & Sire Qualifiers

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

allSystemQualifiers$Val_Ratio <-allSystemQualifiers$BetFairSPForecastWinPrice / allSystemQualifiers$ValueOdds_BetfairFormat

head(allSystemQualifiers)

poorHcpSystems <- read_csv("Poor_Performing_Hcp_Systems.csv", col_names = T)
goodHcpSystems <- read_csv("Good_Performing_Hcp_Systems.csv", col_names = T)
goodSystems <- read_csv("Good_Performing_Systems.csv", col_names = T)
poorSystems <- read_csv("Poor_Performing_Systems.csv", col_names = T)


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
  filter(AE_Ratio >= 1.20, Exp_Wins > 5.0, Archie > 2.5, !(System_Name %in% poorSystems$System_Name)) %>%
  arrange(Time24Hour, Meeting, Horse)


length(unique(asq$Horse))

write_csv(asq, "Today_All_System_Qualifiers.csv")



#####################################################################################

allHcpQuals <- allSystemQualifiers %>%
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
         Spd_Rank, ClassDiffTotal, FCPAdvantage, RAdj.Advantage, Class_Rank, DaysSinceLastRun, ClassWeightDiffRuns1Year, ClsAdvantage, FrmAdvantage,
         HCPAdvantage, DifferentialRankingClassWeight5Years, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range,
         Rev_Weight_Rank, NumberOfResults, Alarms, Age, Runners, WeightDelta,) %>%
  filter(Handicap != "NONHCP", Ratings_Range != "Bottom_Third", !(System_Name %in% poorHcpSystems)) %>%
  arrange(Time24Hour, Meeting, Horse)

allHcpQuals

#View(allHcpQuals)

allArchie <- asq %>%
  select(Time24Hour, Meeting, Horse, System_Name, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, AE_Ratio, Archie, Arch_Strength,
         Placed_AE_Ratio, Placed_Archie, Arch_Placed_Strength, Btn_AE_Ratio, Total_Exp_Btn, Total_Btn, Runs, Winners, Exp_Wins, WinPercent, meanPL,
         totalPL, VSP_ROI, Places, Exp_Places, Place_Percent, BF_Place_ROI, Value_Odds_Range, VOR_Range, BFSPFC_Odds_Range, Trainer, Jockey, Sire,
         Spd_Rank, ClassDiffTotal, FCPAdvantage, RAdj.Advantage, Class_Rank, DaysSinceLastRun, ClassWeightDiffRuns1Year, ClsAdvantage, FrmAdvantage,
         HCPAdvantage, DifferentialRankingClassWeight5Years, Spd_Rank, ClassDiffTotal, FCPAdvantage, RAdj.Advantage, Class_Rank, DaysSinceLastRun,
         ClassWeightDiffRuns1Year, ClsAdvantage, FrmAdvantage, HCPAdvantage, DifferentialRankingClassWeight5Years, Dist_Range, RaceType, Handicap,
         Going, Going_Range, Furlongs, Ratings_Range, Rev_Weight_Rank, NumberOfResults, Alarms, Age, Runners, WeightDelta,) %>%
  filter(Archie >= 4.0, Exp_Wins >= 5, AE_Ratio >= 1.20) %>%
  arrange(Time24Hour, Meeting, Horse)

allArchie

#View(allArchie)

length(unique(allArchie$Horse))


archieGoodSystems <- allArchie %>%
 filter(System_Name %in% goodSystems$System_Name) %>%
 arrange(Time24Hour, Meeting, Horse)

archieGoodSystems

#View(archieGoodSystems)

todayGoodHcpSystems <- allHcpQuals %>%
  filter(System_Name %in% goodHcpSystems$System_Name) %>%
  arrange(Time24Hour, Meeting, Horse)
#
todayGoodHcpSystems

#View(todayGoodHcpSystems)

highArchieQuals <- filter(asq, Archie >= 8.5, AE_Ratio >= 1.20)

highArchieQuals <- highArchieQuals %>%
  select(Time24Hour, Meeting, Horse, System_Name,Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range,
        BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, NumberOfResults, Alarms,
        Age, Runs, meanPL, totalPL, VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, Placed_Archie, Place_Percent, BF_Place_ROI, AE_Ratio,
        WinPercent, Winners, Exp_Wins, Archie, Total_Exp_Btn, Total_Btn, Btn_AE_Ratio, Arch_Strength, Arch_Placed_Strength, Spd_Rank, ClassDiffTotal,
        FCPAdvantage, RAdj.Advantage, Class_Rank, DaysSinceLastRun, ClassWeightDiffRuns1Year, ClsAdvantage, FrmAdvantage, HCPAdvantage,
        DifferentialRankingClassWeight5Years, Runners, WeightDelta,) %>%
  arrange(Time24Hour, Meeting, Horse)

highArchieQuals

#View(highArchieQuals)

goodStatsQuals <- filter(asq, Archie >= 4.0, AE_Ratio >= 1.40, Placed_AE_Ratio >= 1.10)

goodStatsQuals <- goodStatsQuals %>%
  select(Time24Hour, Meeting, Horse, System_Name, Trainer, Jockey, Sire, Dist_Range, RaceType, Handicap, Going, Going_Range, Furlongs, Ratings_Range, VOR_Range,
         BFSPFC_Odds_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Val_Ratio, Value_Odds_Range, Rev_Weight_Rank, NumberOfResults, Alarms,
         Age, Runs, meanPL, totalPL, VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, Placed_Archie, Place_Percent, BF_Place_ROI, AE_Ratio, WinPercent, Winners,
         Exp_Wins, Archie, Total_Exp_Btn, Total_Btn, Btn_AE_Ratio, Arch_Strength, Arch_Placed_Strength, Spd_Rank, ClassDiffTotal, FCPAdvantage, RAdj.Advantage,
         Class_Rank, DaysSinceLastRun, ClassWeightDiffRuns1Year, ClsAdvantage, FrmAdvantage, HCPAdvantage, DifferentialRankingClassWeight5Years,
         Runners, WeightDelta,) %>%
  arrange(Time24Hour, Meeting, Horse)

goodStatsQuals


# Write qualifiers to file

#write_csv(asq, paste0("All_System_Quals_", today$Date[1], ".csv"))

#write_csv(goodStatsQuals, paste0("Good_Stats_Quals_", today$Date[1], ".csv"))

#write_csv(allHcpQuals, paste0("Today_Hcp_Quals_", today$Date[1], ".csv"))

#write_csv(todayGoodHcpSystems, paste0("Today_Good_Hcp_Quals_", today$Date[1], ".csv"))


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

