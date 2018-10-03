#setwd("~/git_projects/UKHR_Project")

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Rev_Draw = min_rank(desc(Stall))) 

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Draw_Normalised = percent_rank(Stall))

summary(ukhr_master_BF$Draw_Normalised)


ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Draw_Range = cut(Draw_Normalised, breaks = c(-1, 0.25, 0.50, 0.75, 1.05),
                          labels = c("Low", "Low_Mid", "High_Mid", "High"), ordered_result = T))


todayMeetings <- unique(today$Meeting)
todayMeetings


ukFA <- filter(ukhr_master_BF, RaceType == "FLAT" | RaceType == "AW", Runners >= 8, Meeting %in% todayMeetings)

#colnames(ukFA)

#unique(ukFA$Draw_Range)

#temp <- ukFA[!is.na(ukFA$Draw_Range),]

#temp2 <- select(temp, Runners, Meeting, Time24Hour, Furlongs, StallNumber, Draw_Normalised, StallPercentage, Draw_Range) %>% 
  #arrange(Time24Hour, Meeting, StallNumber)

#sum(is.na(temp2$Draw_Range))

ukFA <- ukFA[!is.na(ukFA$Draw_Range),]

goodDrawRange <- ukFA %>%
  group_by(Meeting, RaceType, Furlongs, Draw_Range, Going_Range) %>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), 
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, Archie > 3.5, Exp_Wins >= 5)%>%
  filter(!is.na(Draw_Range)) %>% 
  arrange(desc(AE_Ratio))

goodDrawRange
#View(goodDrawRange)

badDrawRange <- ukFA %>%
  group_by(Meeting, RaceType, Furlongs, Draw_Range, Going_Range) %>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), 
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 50, AE_Ratio <= 0.75, meanPL <= -0.20, Archie > 3.5, Exp_Wins >= 5)%>%
  filter(!is.na(Draw_Range)) %>% 
  arrange(AE_Ratio)

badDrawRange
#View(badDrawRange)

################################################################

goodDrawStall <- ukFA %>%
  group_by(Meeting, RaceType, Furlongs, Stall, Going_Range) %>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), 
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, Archie > 3.5, Exp_Wins >= 5)%>%
  filter(!is.na(Stall)) %>% 
  arrange(desc(AE_Ratio))

goodDrawStall
#View(goodDrawStall)

badDrawStall <- ukFA %>%
  group_by(Meeting, RaceType, Furlongs, Stall, Going_Range) %>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), 
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 50, AE_Ratio <= 0.75, meanPL <= -0.20, Archie > 3.5, Exp_Wins >= 5)%>%
  filter(!is.na(Stall)) %>% 
  arrange(AE_Ratio)

badDrawStall
#View(badDrawStall)

################################################################

goodRevDraw <- ukFA %>%
  group_by(Meeting, RaceType, Furlongs, Rev_Draw, Going_Range) %>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), 
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 50, AE_Ratio >= 1.20, meanPL >= 0.1, Archie > 3.5, Exp_Wins >= 5)%>%
  filter(!is.na(Rev_Draw)) %>% 
  arrange(desc(AE_Ratio))

goodRevDraw
#View(goodRevDraw)

badRevDraw <- ukFA %>%
  group_by(Meeting, RaceType, Furlongs, Rev_Draw, Going_Range) %>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), 
            Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
  filter(Runs >= 50, AE_Ratio <= 0.75, meanPL <= -0.20, Archie > 3.5, Exp_Wins >= 5)%>%
  filter(!is.na(Rev_Draw)) %>% 
  arrange(AE_Ratio)

badRevDraw
#View(badRevDraw)

#################################################################

todayFlatDraws <- today %>% 
  filter(RaceType == "FLAT" | RaceType == "AW") %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(Draw_Normalised = percent_rank(StallNumber), 
         Rev_Draw = min_rank(desc(StallNumber)), 
         Stall = min_rank(StallNumber))

head(todayFlatDraws$Draw_Normalised)

#colnames(todayFlatDraws)

todayFlatDraws <- todayFlatDraws %>% 
  #filter(RaceType == "FLAT" | RaceType == "AW") %>% 
  mutate(Draw_Range = cut(Draw_Normalised, breaks = c(-1, 0.25, 0.50, 0.75, 1.05),
                          labels = c("Low", "Low_Mid", "High_Mid", "High"), ordered_result = T))

head(todayFlatDraws$Draw_Range)

#todayFlatDraws$Going_Range <- ifelse(todayFlatDraws$Going %in% SlowGround, "SLOW", "FAST")

#View(select(today, Runners, Meeting, Time24Hour, StallNumber, Draw_Normalised, StallPercentage, Draw_Range) %>% 
       #arrange(Time24Hour, Meeting, StallNumber))

todayGoodDraw <- goodDrawRange %>% 
  left_join(todayFlatDraws, by = c("Meeting", "RaceType", "Furlongs", "Draw_Range", "Going_Range")) %>% 
  filter(!is.na(Time24Hour), Runners >= 8) %>% 
  select(Time24Hour, Meeting, RaceType, Horse, Draw_Range, Stall, Runners, Going_Range, Furlongs, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
         Rating_Rank, Runs, meanPL, totalPL, AE_Ratio, ValueOdds_BetfairFormat, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Rating_Rank)


todayGoodDraw

#View(todayGoodDraw)

#write_csv(todayGoodDraw, "Today_Good_Draws_2018_05_15.csv")


todayBadDraw <- badDrawRange %>% 
  left_join(todayFlatDraws, by = c("Meeting", "RaceType", "Furlongs", "Draw_Range", "Going_Range")) %>% 
  filter(!is.na(Time24Hour), Runners >= 8) %>% 
  select(Time24Hour, Meeting, RaceType, Horse, Draw_Range, Stall, Runners, Going_Range, Furlongs, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
         Rating_Rank, Runs, meanPL, totalPL, AE_Ratio, ValueOdds_BetfairFormat, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Rating_Rank)


todayBadDraw

#View(todayBadDraw)

#write_csv(todayBadDraw, "Today_Bad_Draws_2018_05_15.csv")

###############################################################

todayGoodDrawStall <- goodDrawStall %>% 
  left_join(todayFlatDraws, by = c("Meeting", "RaceType", "Furlongs", "Stall", "Going_Range")) %>% 
  filter(!is.na(Time24Hour), Runners >= 8) %>% 
  select(Time24Hour, Meeting, RaceType, Horse, Draw_Range, Stall, Runners, Going_Range, Furlongs, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
         Rating_Rank, Runs, meanPL, totalPL, AE_Ratio, ValueOdds_BetfairFormat, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Rating_Rank)


todayGoodDrawStall

#View(todayGoodDrawStall)

#write_csv(todayGoodDrawStall, "Today_Good_StallNumber_2018_05_15.csv")


todayBadDrawStall <- badDrawStall %>% 
  left_join(todayFlatDraws, by = c("Meeting", "RaceType", "Furlongs", "Stall", "Going_Range")) %>% 
  filter(!is.na(Time24Hour), Runners >= 8) %>% 
  select(Time24Hour, Meeting, RaceType, Horse, Draw_Range, Stall, Runners, Going_Range, Furlongs, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
         Rating_Rank, Runs, meanPL, totalPL, AE_Ratio, ValueOdds_BetfairFormat, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Rating_Rank)


todayBadDrawStall

#View(todayBadDrawStall)

#write_csv(todayBadDrawStall, "Today_Bad_StallNumber_2018_05_15.csv")

##########################################################################################


todayGoodRevDraw <- goodRevDraw %>% 
  left_join(todayFlatDraws, by = c("Meeting", "RaceType", "Furlongs", "Rev_Draw", "Going_Range")) %>% 
  filter(!is.na(Time24Hour), Runners >= 8) %>% 
  select(Time24Hour, Meeting, RaceType, Horse, Draw_Range, Rev_Draw, Stall, Runners, Going_Range, Furlongs, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
         Rating_Rank, Runs, meanPL, totalPL, AE_Ratio, ValueOdds_BetfairFormat, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Rating_Rank)


todayGoodRevDraw

#View(todayGoodRevDraw)

#write_csv(todayGoodRevDraw, "Today_Good_RevDraw_2018_05_15.csv")


todayBadRevDraw <- badRevDraw %>% 
  left_join(todayFlatDraws, by = c("Meeting", "RaceType", "Furlongs", "Rev_Draw", "Going_Range")) %>% 
  filter(!is.na(Time24Hour), Runners >= 8) %>% 
  select(Time24Hour, Meeting, RaceType, Horse, Draw_Range, Rev_Draw, Stall, Runners, Going_Range, Furlongs, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
         Rating_Rank, Runs, meanPL, totalPL, AE_Ratio, ValueOdds_BetfairFormat, WinPercent, Winners, Exp_Wins, Archie) %>% 
  arrange(Time24Hour, Rating_Rank)


todayBadRevDraw

#View(todayBadRevDraw)

#write_csv(todayBadRevDraw, "Today_Bad_RevDraw_2018_04_28.csv")

##########################################################################################

# Top 3 Rated Good Draw

t3gd <- filter(today, Rating_Rank <= 3, Horse %in% todayGoodDraw$Horse | 
                 Horse %in% todayGoodDrawStall$Horse |
                 Horse %in% todayGoodRevDraw$Horse)

t3gdAll <- select(t3gd, Time24Hour, Meeting, Horse, Trainer, Handicap, Furlongs, 
                  Stall, RaceType, Rating_Rank, BetFairSPForecastWinPrice, 
                  ValueOdds_BetfairFormat)

t3gdAll <- arrange(t3gdAll, Time24Hour, Meeting, Horse)

t3gdAll

#View(t3gdAll)

write_csv(t3gdAll, "Top_3_Rated_Good_Draw.csv")

# All Good Draws

tgd <- filter(today, Horse %in% todayGoodDraw$Horse | 
                 Horse %in% todayGoodDrawStall$Horse |
                 Horse %in% todayGoodRevDraw$Horse)

tgdAll <- select(tgd, Time24Hour, Meeting, Horse, Trainer, Handicap, Furlongs, 
                  Stall, RaceType, Rating_Rank, BetFairSPForecastWinPrice, 
                  ValueOdds_BetfairFormat)

tgdAll <- arrange(tgdAll, Time24Hour, Meeting, Horse)

tgdAll

write_csv(tgdAll, "Today_Good_Draw.csv")


# All Bad Draws

tbd <- filter(today, Horse %in% todayBadDraw$Horse | 
                 Horse %in% todayBadDrawStall$Horse |
                 Horse %in% todayBadRevDraw$Horse)

tbdAll <- select(tbd, Time24Hour, Meeting, Horse, Trainer, Handicap, Furlongs, 
                  Stall, RaceType, Rating_Rank, BetFairSPForecastWinPrice, 
                  ValueOdds_BetfairFormat)

tbdAll <- arrange(tbdAll, Time24Hour, Meeting, Horse)

tbdAll

write_csv(tbdAll, "Today_Bad_Draw.csv")

###############################################################################################

qualsGD <- filter(asq,
                  Horse %in% todayGoodDraw$Horse | 
                    Horse %in% todayGoodDrawStall$Horse |
                    Horse %in% todayGoodRevDraw$Horse)

qualsBD <- filter(asq,
                  Horse %in% todayBadDraw$Horse | 
                    Horse %in% todayBadDrawStall$Horse |
                    Horse %in% todayBadRevDraw$Horse)

qualsBD
qualsGD

write_csv(qualsBD, "Archie_Quals_Bad_Draw.csv")
write_csv(qualsGD, "Archie_Quals_Good_Draw.csv")

#View(qualsBD)
#View(qualsGD)



# All Todays Draw Range Stats

# 
# todayDraws <- ukFA %>%
#   #filter(Meeting %in% todayFlatDraws$Meeting) %>% 
#   group_by(Meeting, RaceType, Furlongs, Draw_Range, Going_Range) %>%
#   summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             WinPercent = sum(Actual)/Runs, Races = length(unique(UKHR_RaceID)),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Archie = ifelse(Exp_Wins >= 5.0,((Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))),0)) %>%
#   arrange(Meeting, Furlongs, desc(AE_Ratio))
# 
# todayDraws
# View(todayDraws)
# # 
# todayDrawsFull <- todayDraws %>% 
#   right_join(todayFlatDraws, by = c("Meeting", "RaceType", "Furlongs", "Draw_Range", "Going_Range")) %>% 
#   filter(AE_Ratio >= 1.1, Placed_AE_Ratio >= 1.1) %>% 
#   arrange(Meeting, Furlongs, desc(AE_Ratio))
# 
# colnames(todayDrawsFull)
# # 
# todayDrawsFinal <- unique(todayDrawsFull[,1:33])
# 
# todayDrawsFinal <- select(todayDrawsFinal, -c(RaceClass:MeanWeight)) %>% 
#   arrange(Time, Meeting, Horse) %>% 
#   select(Time, Meeting, Horse, everything())
# # 
# View(todayDrawsFinal)
# 
# drawHorses <- todayDrawsFinal$Horse %in% allArchie$Horse
# 
# drawHorsesQuals <- allArchie %>% 
#   filter(Horse %in% drawHorses)
#   
# 
# table(today$Meeting, today$Going)
# 
# colnames(ukFA)
