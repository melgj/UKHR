#setwd("~/git_projects/UKHR_Project")
# 
# top5 <- ukhr_master_BF %>% 
#   drop_na(Rating_Rank, BFSP_PL, ValueOdds_BetfairFormat, BetFairSPForecastWinPrice, VOR_Range, Value_Odds_Range,
#           Speed_Rank_Range) %>% 
#   filter(Rating_Rank <= 5, ValueOdds_BetfairFormat <= 21, Actual.Runners >= 5) %>% 
#   group_by(UKHR_RaceID) %>% 
#   mutate(HiRnkValBet = if_else(Value_Odds_Ratio > 1.0, 1, 0),
#          Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   filter(HiRnkValBet == 1) %>% 
#   select(UKHR_RaceID, Year, Meeting, Horse, Ratings_Range ,Class_Rank_Range, Speed_Rank_Range, Runners_Range, RaceType, Handicap,Rating_Rank, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
#          Value_Odds_Ratio, VOR_Range, Value_Odds_Range, Betfair.Win.S.P., Won, BFSP_PL, HiRnkValBet) %>% 
#   mutate(Min_Rnk_Val_Bet = min_rank(Rating_Rank),
#          BFSPvVOBF = if_else(Betfair.Win.S.P. > ValueOdds_BetfairFormat, 1, 0)) %>% 
#   filter(Min_Rnk_Val_Bet == 1, 
#          Value_Odds_Ratio <= 5.0)
# 
# head(top5)
# 
# #nrow(top5)
# sum(top5$BFSP_PL)
# mean(top5$BFSP_PL)
# mean(top5$Betfair.Win.S.P.)
# mean(top5$BetFairSPForecastWinPrice)
# mean(top5$Won)
#            
# top5 %>% 
#   group_by(Speed_Rank_Range) %>% 
#   summarise(Bets = n(), 
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL),
#             Winners = sum(Won),
#             Win_Percent = mean(Won)) %>% 
#   arrange(desc(Avg_PL)) %>% 
#   View()







# str(ukhr_master_BF$Prize)
# 
# coursePrize <- ukhr_master_BF %>% 
#   group_by(Meeting) %>% 
#   summarise(Races = n(), Avg_Prize = mean(Prize, na.rm = T)) %>% 
#   mutate(Course_Rank = min_rank(desc(Avg_Prize)),
#          Course_Grade = cut(Course_Rank, 4,
#                             labels = c("G1", "G2", "G3", "G4"),
#                             ordered_result = T)) %>% 
#   arrange(desc(Avg_Prize))
#                             
# coursePrize
# 
# View(coursePrize)
# 
# ukhr <- ukhr_master_BF %>% 
#   left_join(coursePrize, by = "Meeting")
# 

  


# courseGrade <- coursePrize %>% 
#   mutate(Course_Grade = cut(desc(Avg_Prize), 4,
#                             labels = c("G1", "G2", "G3", "G4"),
#                             ordered_result = T)) %>% 
#   arrange(desc(Avg_Prize))
# 
# courseGrade
# 
# summary(courseGrade)  




top5Q <- today %>% 
  drop_na(Rating_Rank, ValueOdds_BetfairFormat, BetFairSPForecastWinPrice, VOR_Range, Value_Odds_Range,
          Speed_Rank_Range) %>% 
  filter(Rating_Rank <= 4 , ValueOdds_BetfairFormat <= 21, Runners >= 5, (Value_Odds_Ratio > 1 & Value_Odds_Ratio <= 5.0),
         Ratings_Range != "Bottom_Third", Speed_Rank_Range != "Bottom_Third") %>% 
  group_by(UKHRCardRaceID) %>% 
  mutate(HiRnkValBet = if_else(Value_Odds_Ratio > 1.0, 1, 0)) %>% 
  filter(HiRnkValBet == 1) %>% 
  select(UKHRCardRaceID, Time24Hour, Meeting, Horse, Ratings_Range, Speed_Rank_Range, RaceType, Handicap,
         Rating_Rank, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, Value_Odds_Ratio, VOR_Range, 
         Value_Odds_Range, HiRnkValBet) %>% 
  mutate(Min_Rnk_Val_Bet = min_rank(Rating_Rank)) %>% 
  filter(Min_Rnk_Val_Bet == 1) %>% 
  arrange(Time24Hour, Meeting, Horse)

top5Q

write_csv(top5Q, paste0("ValueTop5_",today$Date[1],".csv"))


dualQuals <- top5Q %>% 
  inner_join(asq, by = "Horse")

dualQuals

write_csv(dualQuals, paste0("Dual_Quals_", today$Date[1], ".csv"))

top5QualsGD <- filter(top5Q,
                  Horse %in% todayGoodDraw$Horse | 
                    Horse %in% todayGoodDrawStall$Horse |
                    Horse %in% todayGoodRevDraw$Horse)

top5QualsBD <- filter(top5Q,
                  Horse %in% todayBadDraw$Horse | 
                    Horse %in% todayBadDrawStall$Horse |
                    Horse %in% todayBadRevDraw$Horse)

top5QualsBD
top5QualsGD



############################################################
# 
# 
# rTop5 <- ukhr_master_BF %>% 
#   drop_na(Rating_Rank, BFSP_PL, ValueOdds_BetfairFormat, BetFairSPForecastWinPrice, VOR_Range, Value_Odds_Range,
#           Speed_Rank_Range) %>% 
#   filter(Rating_Rank <= 3, ValueOdds_BetfairFormat <= 21, Actual.Runners >= 5) %>% 
#   group_by(UKHR_RaceID) %>% 
#   mutate(HiRnkValBet = if_else(Value_Odds_Ratio > 1.0, 1, 0),
#          Won = if_else(BFSP_PL > 0, 1, 0)) %>%
#   filter(HiRnkValBet == 1) %>% 
#   select(UKHR_RaceID, Year, Meeting, Horse, Ratings_Range ,Class_Rank_Range, Speed_Rank_Range, Runners_Range, RaceType, Handicap,Rating_Rank, BetFairSPForecastWinPrice, ValueOdds_BetfairFormat, 
#          Value_Odds_Ratio, VOR_Range, Value_Odds_Range, Betfair.Win.S.P., Won, BFSP_PL, HiRnkValBet) %>% 
#   mutate(Min_Rev_Rnk_Val_Bet = min_rank(desc(Rating_Rank)),
#          BFSPvVOBF = if_else(Betfair.Win.S.P. > ValueOdds_BetfairFormat, 1, 0)) %>% 
#   filter(Min_Rev_Rnk_Val_Bet == 1, 
#          Value_Odds_Ratio <= 5.0)
# 
# head(rTop5)
# 
# #nrow(top5)
# sum(rTop5$BFSP_PL)
# mean(rTop5$BFSP_PL)
# mean(rTop5$Betfair.Win.S.P.)
# mean(rTop5$BetFairSPForecastWinPrice)
# mean(rTop5$Won)
# 
# rTop5 %>% 
#   group_by(Rating_Rank) %>% 
#   summarise(Bets = n(), 
#             Avg_PL = mean(BFSP_PL),
#             Total_PL = sum(BFSP_PL),
#             Winners = sum(Won),
#             Win_Percent = mean(Won)) %>% 
#   arrange(desc(Avg_PL)) %>% 
#   View()
# 


  