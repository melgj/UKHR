#setwd("~/git_projects/UKHR_Project")

#ukhr_master_BF <- read_csv("UKHR_Master_BF.csv",col_names = T)

# filter Flat turf races only

ukFlat <- filter(ukhr_master_BF, RaceType == "FLAT") 

ukFlat$Dist_Range <- as.factor(ifelse(ukFlat$Furlongs < 8, "Sprint", 
                                    ifelse(ukFlat$Furlongs < 14,"Middle", "Long")))

#remove jump specific fields

ukFlat <- select(ukFlat, -c(ChaseJumpingAbility, HunterChase, Beginner))


colnames(ukFlat)

# Analyse profitable trainers 

trainersFlatUK <- ukFlat%>%
  group_by(Trainer,Meeting, RaceType)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50 & AE_Ratio >= 1.2, meanPL >= 0.2, WinPercent >= 0.10, Archie >= 2.5)%>%
  arrange(Meeting,desc(AE_Ratio),desc(meanPL))

trainersFlatUK

#View(trainersFlatUK)

write_csv(trainersFlatUK, "Flat_TFC.csv")

# todays qualifiers

todayFlat <- filter(today, RaceType == "FLAT")

trQuals <- trainersFlatUK%>%
  left_join(todayFlat, by = c("Trainer","Meeting","RaceType"))%>%
  filter(!is.na(Time24Hour))

trQuals <- trQuals%>%
  arrange(Time24Hour, Meeting, Horse, Rating_Rank)

trQuals

if(nrow(trQuals) > 0) {
  trQuals$System_Name <- "Flat_TFC"
}


#write_csv(trQuals, "todays_Flat_TFC_Quals_081117")


################################################################################

#softGround <- c("SOFT","SFT-HVY","HEAVY") 

ukFlat$Dist_Range <- as.factor(ifelse(ukFlat$Furlongs < 8, "Sprint", "Route"))

levels(ukFlat$Dist_Range)
                                    

softGroundFlat <- filter(ukFlat, Going %in% softGround)

softSiresFlat <- softGroundFlat %>%
  filter(NumberOfResults >= 1) %>% 
  group_by(Sire, Dist_Range)%>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected), 
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))) %>% 
  filter(Runs >= 30, AE_Ratio >= 1.2, Horses >= 5, Exp_Wins >= 5.0, Archie > 2.5) %>% 
  arrange(desc(AE_Ratio), Dist_Range)

softSiresFlat

#write_csv(softSiresFlat, "Soft_Ground_Sires_Flat.csv")

todayFlat$Dist_Range <- as.factor(ifelse(todayFlat$Furlongs < 8, "Sprint", "Route"))

levels(todayFlat$Dist_Range)
                                                            
todaySoftSiresFlat <- softSiresFlat %>% 
  left_join(todayFlat, by = c("Sire", "Dist_Range")) %>% 
  arrange(Time24Hour, Meeting, Horse) %>% 
  filter(Going %in% softGround, NumberOfResults >= 1, !is.na(Time24Hour)) 

unique(todaySoftSiresFlat$Dist_Range)
  
todaySoftSiresFlatQ <- select(todaySoftSiresFlat, everything())

todaySoftSiresFlatQ 

if(nrow(todaySoftSiresFlatQ) > 0) {
  todaySoftSiresFlatQ$System_Name <- "Soft_Ground_Flat_Sires"
}


#write_csv(todaySoftSiresFlatQ, "Soft Ground Sires Quals.csv")

##################################################################################################

trJkComboFlat <- ukFlat%>%
  group_by(Trainer, Jockey)%>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected), 
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.2, WinPercent >= 0.10, Archie > 2.50) %>%
  arrange(desc(AE_Ratio),desc(meanPL))

trJkComboFlat


#todayFlat <- select(today, RaceType, Trainer, Jockey, Meeting, Horse, Time24Hour, BetFairSPForecastWinPrice,ValueOdds_BetfairFormat,Rating_Rank)

trJkComboFlatQ <- trJkComboFlat%>%
  left_join(todayFlat, by = c("Trainer","Jockey")) %>% 
  filter(RaceType == "FLAT", !is.na(Time24Hour))


#trJkComboFlatQ <- na.omit(trJkComboFlatQ)

trJkComboFlatQ



trJkComboFlatQ <- trJkComboFlatQ%>%
  arrange(Time24Hour, Meeting, Horse)

trJkComboFlatQ

if(nrow(trJkComboFlatQ) > 0) {
  trJkComboFlatQ$System_Name <- "Flat_TJ_Combo"
}


#View(trJkComboFlatQ)

#########################################################################################################

irishMeetings <- c("BALLINROBE ", "BELLEWSTOWN", "CLONMEL", "CORK", "CURRAGH", "DOWN ROYAL", "DOWN ROYAL", "FAIRYHOUSE ",
                   "GALWAY", "GOWRAN PARK", "KILLARNEY", "LEOPARDSTOWN", "LIMERICK", "LISTOWEL", "NAAS", "PUNCHESTOWN",
                   "ROSCOMMON", "SLIGO", "TIPPERARY", "TRAMORE", "WEXFORD")

#slowGround <- c("SOFT","SFT-HVY","HEAVY") 

ukFlat$Dist_Range <- as.factor(ifelse(ukFlat$Furlongs < 8, "Sprint", 
                                      ifelse(ukFlat$Furlongs < 14,"Middle", "Long")))

# flat.TR.TW <- ukFlat %>% 
#   filter(Handicap == "HANDICAP", Rating_Rank <= 3, Weight_Rank <= 2, Going %in% softGround) %>%
#   group_by(Weight_Rank, Going) %>% 
#   summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>% 
#   arrange(desc(AE_Ratio))
# 
# flat.TR.TW

flat.TR.BW <- ukFlat %>% 
  filter(Handicap == "HANDICAP", Going %in% softGround, Rev_Weight_Rank <= 2, Rating_Rank <= 3,
         !(Meeting %in% irishMeetings)) %>%
  group_by(Rev_Weight_Rank) %>% 
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  arrange(desc(AE_Ratio))

flat.TR.BW

#View(flat.TR.BW)


todayBWHQuals <- flat.TR.BW %>% 
  left_join(today, by = c("Rev_Weight_Rank")) %>% 
  filter(Going %in% softGround, Handicap == "HANDICAP", RaceType == "FLAT", Rating_Rank <= 3, !is.na(Time24Hour),
         !(Meeting %in% irishMeetings)) %>%
  arrange(Time24Hour, Meeting, Horse)

# todayBWHQuals <- select(todayBWHQuals,Time24Hour, Meeting, Horse, Runs, meanPL, totalPL, AE_Ratio, WinPercent, Winners,
#                          Exp_Wins, Rev_Weight_Rank, Rating_Rank, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
#                          Handicap, Archie)

todayBWHQuals

if(nrow(todayBWHQuals) > 0) {
  todayBWHQuals$System_Name <- "Flat_Soft_Ground_Hcp_Bottom_Weights"
}


#write_csv(todayBWHQuals, "Bottom Weight Hcp Soft Ground Quals.csv")

#write_csv(todayBWHQ, "Today_Bottom_Wght_Hcp_Soft_Ground_2018_04_17.csv")

################################################################################


flatTrDr <- ukFlat %>% 
  filter(Age <= 5) %>%
  group_by(Trainer, Dist_Range, Handicap) %>% 
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Exp_Wins >= 5, AE_Ratio >= 1.20, Archie > 2.5) %>% 
  arrange(desc(AE_Ratio))

flatTrDr

today$Dist_Range <- as.factor(ifelse(today$Furlongs < 8, "Sprint", 
                                     ifelse(today$Furlongs < 14,"Middle", "Long")))

todayTrDrQuals <- flatTrDr %>% 
  left_join(today, by = c("Trainer", "Dist_Range", "Handicap")) %>% 
  filter(Age <= 5, RaceType == "FLAT", !is.na(Time24Hour)) %>%
  arrange(Time24Hour, Meeting, Horse)

# todayTrDrQuals <- select(todayTrDrQ ,Time24Hour, Meeting, Horse, Runs, meanPL, totalPL, AE_Ratio, WinPercent, Winners,
#                          Exp_Wins, Trainer, Handicap, Dist_Range, Ratings_Range, Archie, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
#                          Rating_Rank)

todayTrDrQuals

if(nrow(todayTrDrQuals) > 0) {
  todayTrDrQuals$System_Name <- "Flat_Trainer_Hcp_Distance_Range"
}


################################################################################


#allSlowGround <- c("SOFT","SFT-HVY","HEAVY", "GD-SFT", "YIELD", "GD-YLD", "YLD-SFT") 

#fastGround <- c("GOOD", "GD-FM", "FIRM")

levels(ukFlat$Dist_Range)

#ukFlat$Going_Range <- ifelse(ukFlat$Going %in% slowGround, "Slow", "Fast")

sireGngDist <- ukFlat %>%
  filter(NumberOfResults >= 1) %>% 
  group_by(Sire, Dist_Range, Going_Range)%>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected), 
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))) %>% 
  filter(Runs >= 30, AE_Ratio >= 1.20, Horses >= 5, Exp_Wins >= 5.0, WinPercent >= 0.10, Archie > 2.5) %>% 
  arrange(desc(AE_Ratio))

sireGngDist


#today$Going_Range <- ifelse(today$Going %in% slowGround, "Slow", "Fast")

todaySireGngDistQ <- sireGngDist %>% 
  left_join(today, by = c("Sire", "Dist_Range", "Going_Range")) %>% 
  filter(RaceType == "FLAT", !is.na(Time24Hour), NumberOfResults >= 1) %>% 
  # select(Time24Hour, RaceType, Meeting, Horse, Rating_Rank, Ratings_Range, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat,
  #        Sire, Going_Range, Dist_Range, Runs, meanPL, totalPL, AE_Ratio, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Meeting, Horse)


todaySireGngDistQ 

if(nrow(todaySireGngDistQ) > 0) {
  todaySireGngDistQ$System_Name <- "Flat_Sires_Going_Distance_Range"
}


################################################################################

allFlatQuals <- trQuals %>% 
  full_join(todaySoftSiresFlatQ) %>% 
  full_join(trJkComboFlatQ) %>% 
  full_join(todayBWHQuals) %>% 
  full_join(todayTrDrQuals) %>% 
  full_join(todaySireGngDistQ)
  
  
allFlatQuals
