
ukNov18 <- ukhr_master_BF %>% 
  filter(Year == 2018, Month == 11)

todaySQ2All <- read_csv("Today_Model_Sys_Ratings_2018-11-01.csv", col_names = T)

head(todaySQ2All)

modelsNov18 <- todaySQ2All %>% 
  left_join(ukNov18, by = c("Time24Hour", "Meeting", "Horse"))  

summary(modelsNov18$BFSP_PL)

x <- which(is.na(modelsNov18$BFSP_PL))



missing <- modelsNov18[x,]

colnames(missing)

missing <- missing %>% 
  select(DayOfMonth, Time24Hour, Meeting, Horse, Meeting, Betfair.Win.S.P., BFSP_PL, Result)

View(missing)

missingUK <- missing %>% 
  left_join(ukNov18, by = c("Time24Hour", "Meeting", "Horse", "DayOfMonth"))

missingUK2 <- missingUK %>% 
  select(DayOfMonth, Time24Hour, Meeting, Horse, Result.x, Result.y)

View(missingUK2)
  

colnames(todaySQ2All)

modelNov18df <- modelsNov18 %>% 
  select(SVM_Model_Preds:RF_Pred, Handicap, RaceType, BFSP_PL)

write_csv(modelNov18df, "Model_Results_Nov18.csv")
