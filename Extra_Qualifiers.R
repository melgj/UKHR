# Full database queries

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

write_csv(hcpTr3yo, "Trainer3yoHcpFlatAW.csv")


todayHcpTr3yoQ <- hcpTr3yo %>%
  left_join(today, by = c("Trainer", "RaceType")) %>%
  filter(!is.na(Time24Hour), Handicap == "HANDICAP", Age == 3, RaceType == "AW" | RaceType == "FLAT") %>%
  arrange(Time24Hour, Meeting, Horse)

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

write_csv(trAge, "TrainerFlatAW_2-3yo_.csv")


todayTrAgeQ <- trAge %>%
  left_join(today, by = c("Trainer", "Handicap","Age","RaceType", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

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

write_csv(trLtoW, "TrainerLastTimeOutWinner.csv")


todayTrWltoQ <- trLtoW %>%
  left_join(today, by = c("Trainer", "Handicap","RaceType", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour), PositionLastTime == "1") %>%
  arrange(Time24Hour, Meeting, Horse)

todayTrWltoQ

if(nrow(todayTrWltoQ) > 0) {
  todayTrWltoQ$System_Name <- "Trainer_LTO_Winner"
}

#########################################################################################################

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

write_csv(lowMileageTrainers, "TrainersLowMileageRunners.csv")


todayTrainerRunsQuals <- lowMileageTrainers %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "NumberOfResults", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
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

write_csv(Trainers_VOR_Rtng, "TrainersVORRating.csv")

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

write_csv(Trainers_VOR, "TrainersVor.csv")

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

write_csv(Trainers_Days, "TrainersDays.csv")

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

write_csv(Trainers_Days_Odds, "TrainersDaysOdds.csv")


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

write_csv(Trainer_Value_Odds, "TrainerValueOdds.csv")

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

write_csv(Trainer_Value_Odds_2, "TrainerValueOdds2.csv")


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

write_csv(Trainer_Odds, "TrainerOdds.csv")

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

write_csv(Trainer_Odds_VOR, "TrainerOddsVOR.csv")

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

write_csv(sireMeetingValue, "SireMeetingValue.csv")


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

write_csv(sireMeetingVOR, "SireMeetingVOR.csv")

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

write_csv(sireGoingValue, "SireGoingValue.csv")

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

write_csv(sireGoingVOR, "SireGoingVOR.csv")

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

write_csv(trainerValueVOR, "TrainerValueVOR.csv")


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

write_csv(jockeyValueVOR, "JockeyValueVOR.csv")

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

write_csv(jockeyVOR, "JockeyVOR.csv")


Tr3yoRouteHcpRR <- ukhr_master_BF %>%
  filter(Age == 3, Furlongs >= 10.0, Handicap == "HANDICAP") %>%
  group_by(Trainer, Ratings_Range, RaceType) %>%
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

Tr3yoRouteHcpRR

write_csv(Tr3yoRouteHcpRR, "Trainer3yoRouteHcppr.csv")

Tr3yoRouteHcp <- ukhr_master_BF %>%
  filter(Age == 3, Furlongs >= 10.0, Handicap == "HANDICAP") %>%
  group_by(Trainer, RaceType) %>%
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

Tr3yoRouteHcp

write_csv(Tr3yoRouteHcp, "Trainer3yoRouteHcppr2.csv")


trSeason <- ukhr_master_BF %>%
  group_by(Trainer, RaceType, Handicap, Season) %>%
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

trSeason

write_csv(trSeason, "TrainerSeason.csv")

#View(trSeason)

trMonth <- ukhr_master_BF %>%
  group_by(Trainer, RaceType, Handicap, Month) %>%
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

trMonth

write_csv(trMonth, "TrainerMonth.csv")

##################################################################################################

trSeason_Quals <- trSeason %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "Season")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

trSeason_Quals

if(nrow(trSeason_Quals) > 0) {
  trSeason_Quals$System_Name <- "Trainer_Season"
}

trMonth_Quals <- trMonth %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "Month")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

trMonth_Quals

if(nrow(trMonth_Quals) > 0) {
  trMonth_Quals$System_Name <- "Trainer_Month"
}


Tr3yoRouteHcp_Quals <- Tr3yoRouteHcp %>%
  left_join(today, by = c("Trainer", "RaceType")) %>%
  filter(Age == 3, Furlongs >= 10.0, Handicap == "HANDICAP", !is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Tr3yoRouteHcp_Quals

if(nrow(Tr3yoRouteHcp_Quals) > 0) {
  Tr3yoRouteHcp_Quals$System_Name <- "Trainer_3yo_Hcp"
}

Tr3yoRouteHcpRR_Quals <- Tr3yoRouteHcpRR %>%
  left_join(today, by = c("Trainer", "Ratings_Range", "RaceType")) %>%
  filter(Age == 3, Furlongs >= 10.0, Handicap == "HANDICAP", !is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Tr3yoRouteHcpRR_Quals

if(nrow(Tr3yoRouteHcpRR_Quals) > 0) {
  Tr3yoRouteHcpRR_Quals$System_Name <- "Trainer_3yo_Hcp_Ratings_Range"
}

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
  arrange(Time24Hour, Meeting, Horse)

jockeyVOR_Quals

if(nrow(jockeyVOR_Quals) > 0) {
  jockeyVOR_Quals$System_Name <- "UKHR_Jockey_VOR"
}


sireGoingValue_Quals <- sireGoingValue %>%
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Going_Range", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

sireGoingValue_Quals

if(nrow(sireGoingValue_Quals) > 0) {
  sireGoingValue_Quals$System_Name <- "UKHR_Sire_Going_Value"
}


sireGoingVOR_Quals <- sireGoingVOR %>%
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Going_Range", "VOR_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

sireGoingVOR_Quals

if(nrow(sireGoingVOR_Quals) > 0) {
  sireGoingVOR_Quals$System_Name <- "UKHR_Sire_Going_VOR"
}


sireMeetingValue_Quals <- sireMeetingValue %>%
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Meeting", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

sireMeetingValue_Quals

if(nrow(sireMeetingValue_Quals) > 0) {
  sireMeetingValue_Quals$System_Name <- "UKHR_Sire_Meeting_Value"
}


sireMeetingVOR_Quals <- sireMeetingVOR %>%
  left_join(today, by = c("Sire", "RaceType", "Ratings_Range", "Meeting", "VOR_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

sireMeetingVOR_Quals

if(nrow(sireMeetingVOR_Quals) > 0) {
  sireMeetingVOR_Quals$System_Name <- "UKHR_Sire_Meeting_VOR"
}



trainerValueVOR_Quals <- trainerValueVOR %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "VOR_Range", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

trainerValueVOR_Quals

if(nrow(trainerValueVOR_Quals) > 0) {
  trainerValueVOR_Quals$System_Name <- "UKHR_Trainer_Val_VOR"
}



Trainers_Days_Quals <- Trainers_Days %>%
  left_join(today, by = c("Trainer", "RaceType", "Ratings_Range", "LTO_Days_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainers_Days_Quals

if(nrow(Trainers_Days_Quals) > 0) {
  Trainers_Days_Quals$System_Name <- "UKHR_Trainers_Days"
}


Trainers_Days_Odds_Quals <- Trainers_Days_Odds %>%
  left_join(today, by = c("Trainer", "RaceType", "Ratings_Range", "LTO_Days_Range", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainers_Days_Odds_Quals

if(nrow(Trainers_Days_Odds_Quals) > 0) {
  Trainers_Days_Odds_Quals$System_Name <- "UKHR_Trainers_Days_Odds"
}


Trainer_Odds_Quals <- Trainer_Odds %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "BFSPFC_Odds_Range", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainer_Odds_Quals

if(nrow(Trainer_Odds_Quals) > 0) {
  Trainer_Odds_Quals$System_Name <- "UKHR_Trainer_Odds"
}


Trainer_Odds_VOR_Quals <- Trainer_Odds_VOR %>%
  left_join(today, by = c("Trainer", "RaceType", "BFSPFC_Odds_Range", "VOR_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainer_Odds_VOR_Quals

if(nrow(Trainer_Odds_VOR_Quals) > 0) {
  Trainer_Odds_VOR_Quals$System_Name <- "UKHR_Trainer_Odds_VOR"
}


Trainer_Value_Odds_Quals <- Trainer_Value_Odds %>%
  left_join(today, by = c("Trainer", "RaceType", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainer_Value_Odds_Quals

if(nrow(Trainer_Value_Odds_Quals) > 0) {
  Trainer_Value_Odds_Quals$System_Name <- "UKHR_Trainer_Val_Odds"
}


Trainer_Value_Odds_2_Quals <- Trainer_Value_Odds_2 %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "Value_Odds_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainer_Value_Odds_2_Quals

if(nrow(Trainer_Value_Odds_2_Quals) > 0) {
  Trainer_Value_Odds_2_Quals$System_Name <- "UKHR_Trainer_Val_Odds_V2"
}


Trainers_VOR_Quals <- Trainers_VOR %>%
  left_join(today, by = c("Trainer", "RaceType", "VOR_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

Trainers_VOR_Quals

if(nrow(Trainers_VOR_Quals) > 0) {
  Trainers_VOR_Quals$System_Name <- "UKHR_Trainer_VOR"
}


Trainers_VOR_Rtng_Quals <- Trainers_VOR_Rtng %>%
  left_join(today, by = c("Trainer", "VOR_Range", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
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

write_csv(trainerNew, "TrainerChange.csv")

trainerNew_Quals <- trainerNew %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "T")) %>%
  arrange(Time24Hour, Meeting, Horse)

trainerNew_Quals

if(nrow(trainerNew_Quals) > 0) {
  trainerNew_Quals$System_Name <- "UKHR_Trainer_Change"
}

classDrop <- filter(ukhr_master_BF, str_detect(Alarms, "\\+"), NumberOfResults > 0)

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

write_csv(classDropper, "TrainerClassDropper.csv")

classDropper_Quals <- classDropper %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "//+")) %>%
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

write_csv(classRiser, "TrainerClassRiser.csv")

classRiser_Quals <- classRiser %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "//-")) %>%
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

write_csv(impHorse, "TrainerImprover.csv")

impHorse_Quals <- impHorse %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, ">")) %>%
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

write_csv(impHorseVOR, "TrainerImprovingVOR.csv")

impHorseVOR_Quals <- impHorseVOR %>%
  left_join(today, by = c("Trainer", "Handicap", "VOR_Range")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, ">")) %>%
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

write_csv(newDistance, "TrainerNewTrip.csv")


newDistance_Quals <- newDistance %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "D")) %>%
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

write_csv(newDistanceVOR, "TrainerNewTripVOR.csv")

newDistanceVOR_Quals <- newDistanceVOR %>%
  left_join(today, by = c("Trainer", "Handicap", "VOR_Range")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "D")) %>%
  arrange(Time24Hour, Meeting, Horse)

newDistanceVOR_Quals

if(nrow(newDistanceVOR_Quals) > 0) {
  newDistanceVOR_Quals$System_Name <- "UKHR_Trainer_Distance_Change_VOR"
}






#################################################################################

todayExtra <- todayTrWltoQ %>%
  full_join(todayHcpTr3yoQ) %>%
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
  full_join(trainerValueVOR_Quals) %>%
  full_join(trainerNew_Quals) %>%
  full_join(classDropper_Quals) %>%
  full_join(classRiser_Quals) %>%
  full_join(impHorse_Quals) %>%
  full_join(impHorseVOR_Quals) %>%
  full_join(newDistance_Quals) %>%
  full_join(newDistanceVOR_Quals) %>%
  full_join(jockeyValueVOR_Quals) %>%
  full_join(jockeyVOR_Quals) %>%
  full_join(Tr3yoRouteHcp_Quals) %>%
  full_join(Tr3yoRouteHcpRR_Quals) %>%
  full_join(trSeason_Quals)



todayExtraQuals <- arrange(todayExtra, Time24Hour, Meeting, Horse)

todayExtraQuals


