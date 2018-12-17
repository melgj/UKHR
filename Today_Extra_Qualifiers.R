# read all extra qual system files

hcpTr3yo <- read_csv("Trainer3yoHcpFlatAW.csv", col_names = T)
trAge <- read_csv("TrainerFlatAW_2-3yo_.csv", col_names = T)
trLtoW <- read_csv("TrainerLastTimeOutWinner.csv", col_names = T)
lowMileageTrainers <- read_csv("TrainersLowMileageRunners.csv", col_names = T)
Trainers_VOR_Rtng <- read_csv("TrainersVORRating.csv", col_names = T)
Trainers_VOR <- read_csv("TrainersVor.csv", col_names = T)
Trainers_Days <- read_csv("TrainersDays.csv", col_names = T)
Trainers_Days_Odds <- read_csv("TrainersDaysOdds.csv", col_names = T)
Trainer_Value_Odds <- read_csv("TrainerValueOdds.csv", col_names = T)
Trainer_Value_Odds_2 <- read_csv("TrainerValueOdds2.csv", col_names = T)
Trainer_Odds <- read_csv("TrainerOdds.csv", col_names = T)
Trainer_Odds_VOR <- read_csv("TrainerOddsVOR.csv", col_names = T)
sireMeetingValue <- read_csv("SireMeetingValue.csv", col_names = T)
sireMeetingVOR <- read_csv("SireMeetingVOR.csv", col_names = T)
sireGoingValue <- read_csv("SireGoingValue.csv", col_names = T)
sireGoingVOR <- read_csv("SireGoingVOR.csv", col_names = T)
trainerValueVOR <- read_csv("TrainerValueVOR.csv", col_names = T)
jockeyValueVOR <- read_csv("JockeyValueVOR.csv", col_names = T)
jockeyVOR <- read_csv("JockeyVOR.csv", col_names = T)
Tr3yoRouteHcpRR <- read_csv("Trainer3yoRouteHcppr.csv", col_names = T)
Tr3yoRouteHcp<- read_csv("Trainer3yoRouteHcppr2.csv", col_names = T)
trSeason <- read_csv("TrainerSeason.csv", col_names = T)
trMonth <- read_csv("TrainerMonth.csv", col_names = T)
trainerNew <- read_csv("TrainerChange.csv", col_names = T)
classDropper <- read_csv("TrainerClassDropper.csv", col_names = T)
classRiser <- read_csv("TrainerClassRiser.csv", col_names = T)
impHorse <- read_csv("TrainerImprover.csv", col_names = T)
impHorseVOR <- read_csv("TrainerImprovingVOR.csv", col_names = T)
newDistance <- read_csv("TrainerNewTrip.csv", col_names = T)
newDistanceVOR <- read_csv("TrainerNewTripVOR.csv", col_names = T)



# get qualifiers

todayHcpTr3yoQ <- hcpTr3yo %>%
  left_join(today, by = c("Trainer", "RaceType")) %>%
  filter(!is.na(Time24Hour), Handicap == "HANDICAP", Age == 3, RaceType == "AW" | RaceType == "FLAT") %>%
  arrange(Time24Hour, Meeting, Horse)

todayHcpTr3yoQ

if(nrow(todayHcpTr3yoQ) > 0) {
  todayHcpTr3yoQ$System_Name <- "Flat_AW_3yo_Hcp_Trainers"
}

todayTrAgeQ <- trAge %>%
  left_join(today, by = c("Trainer", "Handicap","Age","RaceType", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

todayTrAgeQ

if(nrow(todayTrAgeQ) > 0) {
  todayTrAgeQ$System_Name <- "Flat_AW_Trainer_2-3yo"
}


todayTrWltoQ <- trLtoW %>%
  left_join(today, by = c("Trainer", "Handicap","RaceType", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour), PositionLastTime == "1") %>%
  arrange(Time24Hour, Meeting, Horse)

todayTrWltoQ

if(nrow(todayTrWltoQ) > 0) {
  todayTrWltoQ$System_Name <- "Trainer_LTO_Winner"
}


todayTrainerRunsQuals <- lowMileageTrainers %>%
  left_join(today, by = c("Trainer", "RaceType", "Handicap", "NumberOfResults", "Ratings_Range")) %>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

todayTrainerRunsQuals

if(nrow(todayTrainerRunsQuals) > 0) {
  todayTrainerRunsQuals$System_Name <- "Trainers_Low_Mileage_Runners"
}


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


trainerNew_Quals <- trainerNew %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "T")) %>%
  arrange(Time24Hour, Meeting, Horse)

trainerNew_Quals

if(nrow(trainerNew_Quals) > 0) {
  trainerNew_Quals$System_Name <- "UKHR_Trainer_Change"
}

classDropper_Quals <- classDropper %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "//+")) %>%
  arrange(Time24Hour, Meeting, Horse)

classDropper_Quals

if(nrow(classDropper_Quals) > 0) {
  classDropper_Quals$System_Name <- "UKHR_Trainer_Class_Dropper"
}

classRiser_Quals <- classRiser %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "//-")) %>%
  arrange(Time24Hour, Meeting, Horse)

classRiser_Quals

if(nrow(classRiser_Quals) > 0) {
  classRiser_Quals$System_Name <- "UKHR_Trainer_Class_Riser"
}


impHorse_Quals <- impHorse %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, ">")) %>%
  arrange(Time24Hour, Meeting, Horse)

impHorse_Quals

if(nrow(impHorse_Quals) > 0) {
  impHorse_Quals$System_Name <- "UKHR_Trainer_Improving_Horse"
}

impHorseVOR_Quals <- impHorseVOR %>%
  left_join(today, by = c("Trainer", "Handicap", "VOR_Range")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, ">")) %>%
  arrange(Time24Hour, Meeting, Horse)

impHorseVOR_Quals

if(nrow(impHorseVOR_Quals) > 0) {
  impHorseVOR_Quals$System_Name <- "UKHR_Trainer_Improving_VOR"
}


newDistance_Quals <- newDistance %>%
  left_join(today, by = c("Trainer", "Handicap")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "D")) %>%
  arrange(Time24Hour, Meeting, Horse)

newDistance_Quals

if(nrow(newDistance_Quals) > 0) {
  newDistance_Quals$System_Name <- "UKHR_Trainer_Distance_Change"
}


newDistanceVOR_Quals <- newDistanceVOR %>%
  left_join(today, by = c("Trainer", "Handicap", "VOR_Range")) %>%
  filter(!is.na(Time24Hour), NumberOfResults > 0, str_detect(Alarms, "D")) %>%
  arrange(Time24Hour, Meeting, Horse)

newDistanceVOR_Quals

if(nrow(newDistanceVOR_Quals) > 0) {
  newDistanceVOR_Quals$System_Name <- "UKHR_Trainer_Distance_Change_VOR"
}


# Join All Extra Qualifiers


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

