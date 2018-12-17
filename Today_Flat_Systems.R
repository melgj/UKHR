# filter Flat turf races only

ukFlat <- filter(ukhr_master_BF, RaceType == "FLAT")

ukFlat$Dist_Range <- as.factor(ifelse(ukFlat$Furlongs < 8, "Sprint",
                                      ifelse(ukFlat$Furlongs < 14,"Middle", "Long")))

ukFlat$Dist_Range_Split <- as.factor(ifelse(ukFlat$Furlongs < 8, "Sprint",
                                            ifelse(ukFlat$Furlongs < 14,"Middle", "Long")))

#remove jump specific fields

ukFlat <- select(ukFlat, -c(ChaseJumpingAbility, HunterChase, Beginner))


irishMeetings <- c("BALLINROBE ", "BELLEWSTOWN", "CLONMEL", "CORK", "CURRAGH", "DOWN ROYAL", "DOWN ROYAL", "FAIRYHOUSE ",
                   "GALWAY", "GOWRAN PARK", "KILLARNEY", "LEOPARDSTOWN", "LIMERICK", "LISTOWEL", "NAAS", "PUNCHESTOWN",
                   "ROSCOMMON", "SLIGO", "TIPPERARY", "TRAMORE", "WEXFORD")

##################################################################################

todayFlat <- filter(today, RaceType == "FLAT")
todayFlat$Dist_Range <- as.factor(ifelse(todayFlat$Furlongs < 8, "Sprint", "Route"))
today$Dist_Range_Split <- as.factor(ifelse(today$Furlongs < 8, "Sprint",
                                           ifelse(today$Furlongs < 14,"Middle", "Long")))
today$Dist_Range <- as.factor(ifelse(today$Furlongs < 8, "Sprint", "Route"))


trainersFlatUK <- read_csv("Flat_TFC.csv", col_names = T)

trQuals <- trainersFlatUK%>%
  left_join(todayFlat, by = c("Trainer","Meeting","RaceType"))%>%
  filter(!is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse, Rating_Rank)

trQuals

if(nrow(trQuals) > 0) {
  trQuals$System_Name <- "Flat_TFC"
}

softSiresFlat <- read_csv("Soft_Ground_Sires_Flat.csv", col_names = T)

todaySoftSiresFlatQ <- softSiresFlat %>%
  left_join(todayFlat, by = c("Sire", "Dist_Range")) %>%
  arrange(Time24Hour, Meeting, Horse) %>%
  filter(Going %in% softGround, NumberOfResults >= 1, !is.na(Time24Hour), RaceType == "FLAT")

todaySoftSiresFlatQ

if(nrow(todaySoftSiresFlatQ) > 0) {
  todaySoftSiresFlatQ$System_Name <- "Soft_Ground_Flat_Sires"
}


trJkComboFlat <- read_csv("TrainerJockeyFlatCombo.csv", col_names = T)

trJkComboFlatQ <- trJkComboFlat%>%
  left_join(todayFlat, by = c("Trainer","Jockey")) %>%
  filter(RaceType == "FLAT", !is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

trJkComboFlatQ

if(nrow(trJkComboFlatQ) > 0) {
  trJkComboFlatQ$System_Name <- "Flat_TJ_Combo"
}


flat.TR.BW <- read_csv("TopRatedSoftGroundLowWeights.csv", col_names = T)

todayBWHQuals <- flat.TR.BW %>%
  left_join(today, by = c("Rev_Weight_Rank")) %>%
  filter(Going %in% softGround, Handicap == "HANDICAP", RaceType == "FLAT", Rating_Rank <= 3, !is.na(Time24Hour),
         !(Meeting %in% irishMeetings)) %>%
  arrange(Time24Hour, Meeting, Horse)

todayBWHQuals

if(nrow(todayBWHQuals) > 0) {
  todayBWHQuals$System_Name <- "Flat_Soft_Ground_Hcp_Bottom_Weights"
}


flatTrDr <- read_csv("FlatTrainerDistanceRange.csv", col_names = T)

todayTrDrQuals <- flatTrDr %>%
  left_join(today, by = c("Trainer", "Dist_Range_Split", "Handicap")) %>%
  filter(Age <= 5, RaceType == "FLAT", !is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

# todayTrDrQuals <- select(todayTrDrQ ,Time24Hour, Meeting, Horse, Runs, meanPL, totalPL, AE_Ratio, WinPercent, Winners,
#                          Exp_Wins, Trainer, Handicap, Dist_Range, Ratings_Range, Archie, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
#                          Rating_Rank)

todayTrDrQuals

if(nrow(todayTrDrQuals) > 0) {
  todayTrDrQuals$System_Name <- "Flat_Trainer_Hcp_Distance_Range"
}


sireGngDist <- read_csv("SireGoingDistanceRange.csv", col_names = T)


todaySireGngDistQ <- sireGngDist %>%
  left_join(today, by = c("Sire", "Dist_Range", "Going_Range")) %>%
  filter(RaceType == "FLAT", !is.na(Time24Hour), NumberOfResults >= 1) %>%
  arrange(Time24Hour, Meeting, Horse)


todaySireGngDistQ

if(nrow(todaySireGngDistQ) > 0) {
  todaySireGngDistQ$System_Name <- "Flat_Sires_Going_Distance_Range"
}


allFlatQuals <- trQuals %>%
  full_join(todaySoftSiresFlatQ) %>%
  full_join(trJkComboFlatQ) %>%
  full_join(todayBWHQuals) %>%
  full_join(todayTrDrQuals) %>%
  full_join(todaySireGngDistQ)


allFlatQuals

