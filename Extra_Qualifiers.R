#setwd("~/git_projects/UKHR_Project")

#ukhr_master_BF$Dist_Range <- as.factor(ifelse(ukhr_master_BF$Furlongs < 8, "Sprint", 
                                              #ifelse(ukhr_master_BF$Furlongs < 14,"Middle", "Long")))

wPoly1 <- which(ukhr_master_BF$Year < 2014 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")

wPoly2 <- which(ukhr_master_BF$Year == 2014 & ukhr_master_BF$Month < 8 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")

wPolyAll <- c(wPoly1, wPoly2)

ukhr_master_BF <- ukhr_master_BF[-wPolyAll,]


#hcpTr3 <- filter(ukhr_master_BF, Age == 3, Handicap == "HANDICAP", RaceType == "AW" | RaceType == "FLAT")

hcpTr3yo <- ukhr_master_BF %>% 
  filter(Age == 3, Handicap == "HANDICAP", RaceType == "AW" | RaceType == "FLAT") %>% 
  group_by(Trainer, RaceType) %>% 
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 30, AE_Ratio >= 1.20, meanPL >= 0.10, Archie > 2.5, Exp_Wins >= 5) %>%
  arrange(desc(AE_Ratio))

hcpTr3yo


todayHcpTr3yoQ <- hcpTr3yo %>% 
  left_join(today, by = c("Trainer", "RaceType")) %>% 
  filter(!is.na(Time24Hour), Handicap == "HANDICAP", Age == 3, RaceType == "AW" | RaceType == "FLAT") %>% 
  arrange(Time24Hour, Meeting, Horse)

# todayHcpTr3yoQ <- select(todayHcpTr3yo,Time24Hour, Meeting, Horse, Trainer, RaceType, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
#                          Ratings_Range, Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL,Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI,
#                          Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, BF_Place_ROI,AE_Ratio,
#                          WinPercent, Winners, Exp_Wins, Archie)

todayHcpTr3yoQ

if(nrow(todayHcpTr3yoQ) > 0) {
  todayHcpTr3yoQ$System_Name <- "Flat_AW_3yo_Hcp_Trainers"
}


#write_csv(todayHcpTr3yoQ, "Today_Hcp_Tr_3yo_Quals.csv")

####################################################################

#ukF2y3y <- filter(ukhr_master_BF, Age <= 3, RaceType == "FLAT" | RaceType == "AW")

trAge <- ukhr_master_BF %>% 
  filter(Age <= 3, RaceType == "FLAT" | RaceType == "AW") %>% 
  group_by(Trainer, Handicap, Age, RaceType, Ratings_Range) %>% 
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 100, AE_Ratio >= 1.20, meanPL >= 0.10, Archie > 2.5, Exp_Wins >= 5) %>%
  arrange(desc(AE_Ratio))

trAge


todayTrAgeQ <- trAge %>% 
  left_join(today, by = c("Trainer", "Handicap","Age","RaceType", "Ratings_Range")) %>% 
  filter(!is.na(Time24Hour)) %>% 
  arrange(Time24Hour, Meeting, Horse)

# todayTrAgeQ <- select(todayTrAge,Time24Hour, Meeting, Horse, Trainer, Handicap, Age, RaceType, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
#                       Ratings_Range, Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL,Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI , 
#                       Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, BF_Place_ROI ,AE_Ratio, 
#                       WinPercent, Winners, Exp_Wins, Archie)

todayTrAgeQ

if(nrow(todayTrAgeQ) > 0) {
  todayTrAgeQ$System_Name <- "Flat_AW_Trainer_2-3yo"
}


#write_csv(todayTrAgeQ, "Today_Tr_Age_Quals_2018_04_28.csv")


#############################################################################################################

#sto23yo <- filter(ukhr_master_BF, Age <= 3, NumberOfResults == 1)

#sto2yo$Dist_Range <- as.factor(ifelse(sto2yo$Furlongs <= 6.5, "SPRINT", "ROUTE"))

# 
# tr23yo2to <- ukhr_master_BF %>% 
#   filter(Age <= 3, NumberOfResults == 1, RaceType == "FLAT" | RaceType == "AW") %>% 
#   group_by(Trainer, RaceType, Handicap) %>% 
#   summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
#   filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.10, Archie > 2.5, Exp_Wins >= 5) %>%
#   arrange(desc(AE_Ratio))
# 
# tr23yo2to
# 
# 
# 
# 
# #####################################################################################################################
# 
# #deb2y3y <- filter(ukhr_master_BF, Age <= 3, NumberOfResults == 0, RaceType == "FLAT" | RaceType == "AW")
# 
# #deb2y3y$Dist_Range <- as.factor(ifelse(deb2y3y$Furlongs <= 6.5, "SPRINT", "ROUTE"))
# 
# 
# trDeb23 <- ukhr_master_BF %>%
#   filter(Age <= 3, NumberOfResults == 0, RaceType == "FLAT" | RaceType == "AW") %>% 
#   mutate(Dist_Range = (ifelse(Furlongs <= 6.5, "SPRINT", "ROUTE"))) %>% 
#   group_by(Trainer, Age, RaceType, Dist_Range) %>% 
#   summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
#   filter(Runs >= 30, AE_Ratio >= 1.20, meanPL >= 0.10, Archie > 2.5, Exp_Wins >= 5) %>%
#   arrange(desc(AE_Ratio))
# 
# trDeb23

#################################################################################################################

#ltoW <- filter(ukhr_master_BF, PositionLastTime == "1")

trLtoW <- ukhr_master_BF %>% 
  filter(PositionLastTime == "1") %>% 
  group_by(Trainer, Handicap, RaceType, Ratings_Range) %>% 
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 30, AE_Ratio >= 1.20, meanPL >= 0.10, Archie > 2.5, Exp_Wins >= 5) %>%
  arrange(desc(AE_Ratio))

trLtoW


todayTrWltoQ <- trLtoW %>% 
  left_join(today, by = c("Trainer", "Handicap","RaceType", "Ratings_Range")) %>% 
  filter(!is.na(Time24Hour), PositionLastTime == "1") %>% 
  arrange(Time24Hour, Meeting, Horse)
# 
# todayTrWltoQ <- select(todayTrWlto,Time24Hour, Meeting, Horse, Trainer, Handicap, Age, RaceType, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
#                        Ratings_Range, Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL,Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI , 
#                        Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, BF_Place_ROI ,AE_Ratio, 
#                        WinPercent, Winners, Exp_Wins, Archie)

todayTrWltoQ

if(nrow(todayTrWltoQ) > 0) {
  todayTrWltoQ$System_Name <- "Trainer_LTO_Winner"
}


todaySupplement <- todayTrWltoQ %>% 
  #full_join(todayTrAgeQ) %>% 
  full_join(todayHcpTr3yoQ) %>% 
  arrange(Time24Hour, Meeting, Horse)

todaySupplement

#########################################################################################################

# filter top rated only

# topR <- filter(ukhr_master_BF, Rating_Rank == 1)
# 
# # create summary table highlighted top performing trainers of top rated horses
# 
# topAE <- topR %>% 
#   group_by(Trainer, RaceType)%>%
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   filter(Runs >= 30, AE_Ratio > 1.25, meanPL >= 0.2 & WinPercent >= 0.10, Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
#   arrange(desc(AE_Ratio, meanPL))
# 
# topAE
# 
# todayTopAEQ <- topAE %>% 
#   left_join(today, by = c("Trainer", "RaceType")) %>% 
#   filter(Rating_Rank == 1, !is.na(Time24Hour))
# 
# 
# todayTopAEQ <- select(todayTopAEQ, everything())
# 
# todayTopAEQ
# 
# #View(todayTopAEQ)
# 
# ###################################################################################
# 
# 
# topAE3 <- topR %>% 
#   group_by(Sire, RaceType)%>%
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   filter(Runs >= 30, AE_Ratio >= 1.20 & meanPL >= 0.2, WinPercent >= 0.10, Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
#   arrange(desc(AE_Ratio, meanPL))
# 
# topAE3
# 
# todayTopAEQ3 <- topAE3 %>% 
#   left_join(today, by = c("Sire", "RaceType")) %>% 
#   filter(Rating_Rank == 1, !is.na(Time24Hour))
# 
# todayTopAEQ3 <- select(todayTopAEQ3, everything())
# 
# todayTopAEQ3
# 
# #View(todayTopAEQ3)

############################################################################################

################################################################################

lowMileageTrainers <- filter(ukhr_master_BF, Age <= 4, NumberOfResults  <= 5) %>% 
  group_by(Trainer, RaceType, Handicap, NumberOfResults, Ratings_Range) %>% 
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30, AE_Ratio >= 1.20, meanPL >= 0.2, WinPercent >= 0.10, Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

lowMileageTrainers  


todayTrainerRunsQuals <- lowMileageTrainers %>% 
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "NumberOfResults", "Ratings_Range")) %>% 
  filter(!is.na(Time24Hour)) %>% 
  # select(Time24Hour, Meeting, Horse, Trainer, RaceType, Handicap, NumberOfResults, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
  #        Ratings_Range,Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Meeting, Horse) 

todayTrainerRunsQuals

if(nrow(todayTrainerRunsQuals) > 0) {
  todayTrainerRunsQuals$System_Name <- "Trainers_Low_Mileage_Runners"
}

################################################################################

Trainers_VOR_Rtng <- ukhr_master_BF %>% 
  group_by(Trainer, VOR_Range, Ratings_Range) %>% 
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainers_VOR_Rtng

Trainers_VOR <- ukhr_master_BF %>% 
  group_by(Trainer, RaceType, VOR_Range) %>% 
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainers_VOR

Trainers_Days <- ukhr_master_BF %>% 
  group_by(Trainer, RaceType, Ratings_Range, LTO_Days_Range) %>% 
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainers_Days 

Trainers_Days_Odds <- ukhr_master_BF %>% 
  group_by(Trainer, RaceType, Ratings_Range, LTO_Days_Range, Value_Odds_Range) %>% 
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainers_Days_Odds 

Trainer_Value_Odds <- ukhr_master_BF %>% 
  group_by(Trainer, RaceType, Value_Odds_Range) %>% 
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainer_Value_Odds

Trainer_Value_Odds_2 <- ukhr_master_BF %>% 
  group_by(Trainer, RaceType, Handicap, Value_Odds_Range) %>% 
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainer_Value_Odds_2


Trainer_Odds <- ukhr_master_BF %>%
  group_by(Trainer, RaceType, Handicap, BFSPFC_Odds_Range, Ratings_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainer_Odds

Trainer_Odds_VOR <- ukhr_master_BF %>%
  group_by(Trainer, RaceType, BFSPFC_Odds_Range, VOR_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, Archie))

Trainer_Odds_VOR

sireMeetingValue <- ukhr_master_BF %>%
  group_by(Sire, RaceType, Meeting, Ratings_Range, Value_Odds_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Sire))%>%
  arrange(desc(AE_Ratio, Archie))

sireMeetingValue


sireMeetingVOR <- ukhr_master_BF %>%
  group_by(Sire, RaceType, Meeting, Ratings_Range, VOR_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Sire))%>%
  arrange(desc(AE_Ratio, Archie))

sireMeetingVOR

sireGoingValue <- ukhr_master_BF %>%
  group_by(Sire, RaceType, Ratings_Range, Going_Range, Value_Odds_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Sire))%>%
  arrange(desc(AE_Ratio, Archie))

sireGoingValue

sireGoingVOR <- ukhr_master_BF %>%
  group_by(Sire, RaceType, Ratings_Range, Going_Range, VOR_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Sire))%>%
  arrange(desc(AE_Ratio, Archie))

sireGoingVOR

# 
# sireValueVOR <- ukhr_master_BF %>%
#   group_by(Sire, RaceType, Going_Range, VOR_Range, Value_Odds_Range) %>%
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Sire))%>%
#   arrange(desc(AE_Ratio, Archie))
# 
# sireValueVOR

trainerValueVOR <- ukhr_master_BF %>%
  group_by(Trainer, RaceType, Handicap, VOR_Range, Value_Odds_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

trainerValueVOR


jockeyValueVOR <- ukhr_master_BF %>%
  group_by(Jockey, RaceType, Handicap, VOR_Range, Value_Odds_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 100, AE_Ratio >= 1.20, Placed_AE_Ratio >= 1.05, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Jockey))%>%
  arrange(desc(AE_Ratio, Archie))

jockeyValueVOR

#View(jockeyValueVOR)

jockeyVOR <- ukhr_master_BF %>%
  group_by(Jockey, RaceType, Handicap, VOR_Range, Ratings_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 100, AE_Ratio >= 1.20, Placed_AE_Ratio >= 1.05, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Jockey))%>%
  arrange(desc(AE_Ratio, Archie))

jockeyVOR

#View(jockeyVOR)

# jockeyValue <- ukhr_master_BF %>%
#   group_by(Jockey, RaceType, Handicap, Value_Odds_Range, Ratings_Range) %>%
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   filter(Runs >= 100, AE_Ratio >= 1.20, Placed_AE_Ratio >= 1.05, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Jockey))%>%
#   arrange(desc(AE_Ratio, Archie))
# 
# jockeyValue

#View(jockeyValue)

# valueVOR <- ukhr_master_BF %>%
#   group_by(Value_Odds_Range, Meeting, Furlongs, Stall) %>%
#   filter(ValueOdds_BetfairFormat <= 21) %>% 
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   filter(!is.na(Value_Odds_Range), !is.na(Stall), Runs >= 100, AE_Ratio >= 1.20, Placed_AE_Ratio >= 1.10, Archie >= 3.50) %>% 
#             #filter(Runs >= 100, AE_Ratio >= 1.20, Placed_AE_Ratio >= 1.10, meanPL >= 0.1, WinPercent >= 0.10, Horses >= 10, Archie > 3.5, Exp_Wins >= 10.0)%>%
#   arrange(desc(AE_Ratio, Archie))
# 
# valueVOR
# 
# View(valueVOR)

##########################################################

jockeyValueVOR_Quals <- jockeyValueVOR %>% 
  left_join(today, by = c("Jockey", "RaceType", "Handicap", "VOR_Range", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Jockey, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

jockeyValueVOR_Quals

if(nrow(jockeyValueVOR_Quals) > 0) {
  jockeyValueVOR_Quals$System_Name <- "UKHR_Jockey_Val_VOR"
}


jockeyVOR_Quals <- jockeyVOR %>% 
  left_join(today, by = c("Jockey", "RaceType", "Handicap","VOR_Range", "Ratings_Range")) %>% 
  filter(!is.na(Time24Hour)) %>% 
  # select(Time24Hour, Meeting, Horse, Jockey, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Meeting, Horse) 

jockeyVOR_Quals

if(nrow(jockeyVOR_Quals) > 0) {
  jockeyVOR_Quals$System_Name <- "UKHR_Jockey_VOR"
}


# jockeyValue_Quals <- jockeyValue %>% 
#   left_join(today, by = c("Jockey", "RaceType", "Handicap","Value_Odds_Range", "Ratings_Range")) %>% 
#   filter(!is.na(Time24Hour)) %>% 
#   # select(Time24Hour, Meeting, Horse, Jockey, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
#   #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
#   #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
#   arrange(Time24Hour, Meeting, Horse) 
# 
# jockeyValue_Quals
# 
# if(nrow(jockeyValue_Quals) > 0) {
#   jockeyValue_Quals$System_Name <- "UKHR_Jockey_Value"
# }
# 



sireGoingValue_Quals <- sireGoingValue %>%
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Going_Range", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

sireGoingValue_Quals

if(nrow(sireGoingValue_Quals) > 0) {
  sireGoingValue_Quals$System_Name <- "UKHR_Sire_Going_Value"
}


sireGoingVOR_Quals <- sireGoingVOR %>%
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Going_Range", "VOR_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

sireGoingVOR_Quals

if(nrow(sireGoingVOR_Quals) > 0) {
  sireGoingVOR_Quals$System_Name <- "UKHR_Sire_Going_VOR"
}


sireMeetingValue_Quals <- sireMeetingValue %>% 
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Meeting", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

sireMeetingValue_Quals

if(nrow(sireMeetingValue_Quals) > 0) {
  sireMeetingValue_Quals$System_Name <- "UKHR_Sire_Meeting_Value"
}


sireMeetingVOR_Quals <- sireMeetingVOR %>% 
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Meeting", "VOR_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

sireMeetingVOR_Quals

if(nrow(sireMeetingVOR_Quals) > 0) {
  sireMeetingVOR_Quals$System_Name <- "UKHR_Sire_Meeting_VOR"
}



# sireValueVOR_Quals <- sireValueVOR %>% 
#   left_join(today, by = c("Sire", "RaceType", "Going_Range", "VOR_Range", "Value_Odds_Range")) %>%
#   filter(!is.na(Time24Hour)) %>%
#   # select(Time24Hour, Meeting, Horse, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
#   #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
#   #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
#   arrange(Time24Hour, Meeting, Horse)
# 
# sireValueVOR_Quals
# 
# if(nrow(sireValueVOR_Quals) > 0) {
#   sireValueVOR_Quals$System_Name <- "UKHR_Sire_Val_VOR"
# }


trainerValueVOR_Quals <- trainerValueVOR %>% 
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "VOR_Range", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

trainerValueVOR_Quals

if(nrow(trainerValueVOR_Quals) > 0) {
  trainerValueVOR_Quals$System_Name <- "UKHR_Trainer_Val_VOR"
}



Trainers_Days_Quals <- Trainers_Days %>%
  left_join(today, by = c("Trainer", "RaceType", "Ratings_Range", "LTO_Days_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainers_Days_Quals

if(nrow(Trainers_Days_Quals) > 0) {
  Trainers_Days_Quals$System_Name <- "UKHR_Trainers_Days"
}


Trainers_Days_Odds_Quals <- Trainers_Days_Odds %>%
  left_join(today, by = c("Trainer", "RaceType", "Ratings_Range", "LTO_Days_Range", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainers_Days_Odds_Quals

if(nrow(Trainers_Days_Odds_Quals) > 0) {
  Trainers_Days_Odds_Quals$System_Name <- "UKHR_Trainers_Days_Odds"
}

# 
# 
Trainer_Odds_Quals <- Trainer_Odds %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "BFSPFC_Odds_Range", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainer_Odds_Quals

if(nrow(Trainer_Odds_Quals) > 0) {
  Trainer_Odds_Quals$System_Name <- "UKHR_Trainer_Odds"
}


Trainer_Odds_VOR_Quals <- Trainer_Odds_VOR %>%
  left_join(today, by = c("Trainer", "RaceType", "BFSPFC_Odds_Range", "VOR_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainer_Odds_VOR_Quals

if(nrow(Trainer_Odds_VOR_Quals) > 0) {
  Trainer_Odds_VOR_Quals$System_Name <- "UKHR_Trainer_Odds_VOR"
}


Trainer_Value_Odds_Quals <- Trainer_Value_Odds %>% 
  left_join(today, by = c("Trainer", "RaceType", "Value_Odds_Range")) %>% 
  filter(!is.na(Time24Hour)) %>% 
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Meeting, Horse) 

Trainer_Value_Odds_Quals

if(nrow(Trainer_Value_Odds_Quals) > 0) {
  Trainer_Value_Odds_Quals$System_Name <- "UKHR_Trainer_Val_Odds"
}


Trainer_Value_Odds_2_Quals <- Trainer_Value_Odds_2 %>% 
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "Value_Odds_Range")) %>% 
  filter(!is.na(Time24Hour)) %>% 
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Meeting, Horse) 

Trainer_Value_Odds_2_Quals

if(nrow(Trainer_Value_Odds_2_Quals) > 0) {
  Trainer_Value_Odds_2_Quals$System_Name <- "UKHR_Trainer_Val_Odds_V2"
}


Trainers_VOR_Quals <- Trainers_VOR %>% 
  left_join(today, by = c("Trainer", "RaceType", "VOR_Range")) %>% 
  filter(!is.na(Time24Hour)) %>% 
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Meeting, Horse) 

Trainers_VOR_Quals

if(nrow(Trainers_VOR_Quals) > 0) {
  Trainers_VOR_Quals$System_Name <- "UKHR_Trainer_VOR"
}


Trainers_VOR_Rtng_Quals <- Trainers_VOR_Rtng %>% 
  left_join(today, by = c("Trainer", "VOR_Range", "Ratings_Range")) %>% 
  filter(!is.na(Time24Hour)) %>% 
  # select(Time24Hour, Meeting, Horse, Trainer, Sire, RaceType, Going_Range, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Meeting, Horse) 

Trainers_VOR_Rtng_Quals

if(nrow(Trainers_VOR_Rtng_Quals) > 0) {
  Trainers_VOR_Rtng_Quals$System_Name <- "UKHR_Trainer_VOR_Ratings"
}

##############################################

# Alarms based qualifiers

newTrainer <- filter(ukhr_master_BF, str_detect(Alarms, "T"), NumberOfResults > 0)

trainerNew <- newTrainer %>%
  group_by(Trainer, Handicap) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

trainerNew

trainerNew_Quals <- trainerNew %>% 
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "T")) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Alarms, RaceType, Going_Range, Ratings_Range, Handicap, VOR_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

trainerNew_Quals

if(nrow(trainerNew_Quals) > 0) {
  trainerNew_Quals$System_Name <- "UKHR_Trainer_Change"
}

classDrop <- filter(ukhr_master_BF, str_detect(Alarms, "\\+"), NumberOfResults > 0)

#head(classDrop$Alarms, 50)

classDropper <- classDrop %>%
  group_by(Trainer, Handicap) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

classDropper

#View(classDropper)

classDropper_Quals <- classDropper %>% 
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "//+")) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Alarms, RaceType, Going_Range, Ratings_Range, Handicap, VOR_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

classDropper_Quals

if(nrow(classDropper_Quals) > 0) {
  classDropper_Quals$System_Name <- "UKHR_Trainer_Class_Dropper"
}


classRise <- filter(ukhr_master_BF, str_detect(Alarms, "\\-"), NumberOfResults > 0)

#head(classDrop$Alarms, 50)

classRiser <- classRise %>%
  group_by(Trainer, Handicap) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

classRiser

classRiser_Quals <- classRiser %>% 
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "//-")) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Alarms, RaceType, Going_Range, Ratings_Range, Handicap, VOR_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places,
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

classRiser_Quals

if(nrow(classRiser_Quals) > 0) {
  classRiser_Quals$System_Name <- "UKHR_Trainer_Class_Riser"
}

improver <- filter(ukhr_master_BF, str_detect(Alarms, ">"), NumberOfResults > 1)

impHorse <- improver %>%
  group_by(Trainer, Handicap) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

impHorse

#View(impHorse)

impHorse_Quals <- impHorse %>% 
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, ">")) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Alarms, RaceType, Going_Range, Ratings_Range, Handicap, VOR_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, 
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

impHorse_Quals

if(nrow(impHorse_Quals) > 0) {
  impHorse_Quals$System_Name <- "UKHR_Trainer_Improving_Horse"
}

impHorseVOR <- improver %>%
  group_by(Trainer, Handicap, VOR_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

impHorseVOR

#View(impHorseVOR)

impHorseVOR_Quals <- impHorseVOR %>% 
  left_join(today, by = c("Trainer", "Handicap", "VOR_Range")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, ">")) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Alarms, RaceType, Going_Range, Ratings_Range, Handicap, VOR_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, 
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

impHorseVOR_Quals

if(nrow(impHorseVOR_Quals) > 0) {
  impHorseVOR_Quals$System_Name <- "UKHR_Trainer_Improving_VOR"
}

newTrip <- filter(ukhr_master_BF, str_detect(Alarms, "D"), NumberOfResults > 0)

#head(classDrop$Alarms, 50)

newDistance <- newTrip %>%
  group_by(Trainer, Handicap) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

newDistance

#View(newDistance)

newDistance_Quals <- newDistance %>% 
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "D")) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Alarms, RaceType, Going_Range, Ratings_Range, Handicap, VOR_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, 
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

newDistance_Quals

if(nrow(newDistance_Quals) > 0) {
  newDistance_Quals$System_Name <- "UKHR_Trainer_Distance_Change"
}


newDistanceVOR <- newTrip %>%
  group_by(Trainer, Handicap, VOR_Range) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, WinPercent >= 0.15, Horses >= 5, Archie > 3.5, Exp_Wins >= 5.0, !is.na(Trainer))%>%
  arrange(desc(AE_Ratio, Archie))

newDistanceVOR

#View(newDistanceVOR)

newDistanceVOR_Quals <- newDistanceVOR %>% 
  left_join(today, by = c("Trainer", "Handicap", "VOR_Range")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "D")) %>%
  # select(Time24Hour, Meeting, Horse, Trainer, Alarms, RaceType, Going_Range, Ratings_Range, Handicap, VOR_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, 
  #        Placed_AE_Ratio, BF_Place_ROI, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>%
  arrange(Time24Hour, Meeting, Horse)

newDistanceVOR_Quals

if(nrow(newDistanceVOR_Quals) > 0) {
  newDistanceVOR_Quals$System_Name <- "UKHR_Trainer_Distance_Change_VOR"
}






#################################################################################

todayExtra <- todaySupplement %>% 
  full_join(todayTrainerRunsQuals) %>% 
  full_join(Trainers_Days_Quals) %>% 
  full_join(Trainer_Value_Odds_Quals) %>% 
  full_join(Trainer_Value_Odds_2_Quals) %>% 
  full_join(Trainers_VOR_Quals) %>% 
  full_join(Trainers_VOR_Rtng_Quals) %>% 
  full_join(Trainer_Odds_Quals) %>% 
  full_join(Trainer_Odds_VOR_Quals) %>% 
  full_join(Trainers_Days_Odds_Quals) %>% 
  full_join(sireMeetingValue_Quals) %>% 
  full_join(sireMeetingVOR_Quals) %>% 
  full_join(sireGoingValue_Quals) %>% 
  full_join(sireGoingVOR_Quals) %>% 
  #full_join(sireValueVOR_Quals) %>% 
  full_join(trainerValueVOR_Quals) %>% 
  full_join(trainerNew_Quals) %>% 
  full_join(classDropper_Quals) %>% 
  full_join(classRiser_Quals) %>% 
  full_join(impHorse_Quals) %>% 
  full_join(impHorseVOR_Quals) %>% 
  full_join(newDistance_Quals) %>% 
  full_join(newDistanceVOR_Quals) %>% 
  full_join(jockeyValueVOR_Quals) %>% 
  #full_join(jockeyValue_Quals) %>% 
  full_join(jockeyVOR_Quals)


# todayExtraQuals <- select(todayExtra, Time24Hour, Meeting, Horse, Trainer, Jockey, Sire, RaceType, Handicap, Value_Odds_Range, VOR_Range, Going_Range,
#        BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Alarms, Runs, meanPL, totalPL, Avg_BFVSP_PL, Total_BFVSP_PL, 
#        Avg_VSP_Stake, Total_VSP_Stake,VSP_ROI, Places, Exp_Places, Placed_AE_Ratio, BF_Place_ROI ,AE_Ratio, WinPercent, Winners, Exp_Wins, Archie)
#   
  

todayExtraQuals <- arrange(todayExtra, Time24Hour, Meeting, Horse)

todayExtraQuals

#View(todayExtraQuals)
