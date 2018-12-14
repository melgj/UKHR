library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)


ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_10_31.csv", col_names = T)

length(which(ukhr_master_BF$BetFairSPForecastWinPrice <= 0))

ukhr_master_BF$BetFairSPForecastWinPrice[ukhr_master_BF$BetFairSPForecastWinPrice <= 0] <- NA

summary(ukhr_master_BF$BetFairSPForecastWinPrice)

ukhr_master_BF$BetFairSPForecastWinPrice <- if_else(is.na(ukhr_master_BF$BetFairSPForecastWinPrice), 
        ukhr_master_BF$Betfair.Win.S.P., ukhr_master_BF$BetFairSPForecastWinPrice)

head(ukhr_master_BF$BetFairSPForecastWinPrice[x])
  

sum(is.na(ukhr_master_BF$Betfair.Win.S.P.))
sum(is.na(ukhr_master_BF$BFSP_PL))

#summary(ukhr_master_BF$Year)

ukhrCols <- colnames(ukhr_master_BF)

# Load New Month Data

newMonth <- read_csv(file.choose(), col_names = T)

newMonth$BetFairSPForecastWinPrice[newMonth$BetFairSPForecastWinPrice <= 0] <- NA

summary(newMonth$BetFairSPForecastWinPrice)

newMonth$BetFairSPForecastWinPrice <- if_else(is.na(newMonth$BetFairSPForecastWinPrice),
                                              newMonth$`Betfair Win S.P.`, newMonth$BetFairSPForecastWinPrice)



newMonthCols <- colnames(newMonth)

setdiff(newMonthCols, ukhrCols)

colnames(newMonth) <- str_replace_all(colnames(newMonth)," ",".")

newMonthCols <- colnames(newMonth)

setdiff(newMonthCols, ukhrCols)

newMonth <- rename(newMonth, LastTimePosn = LastTimePositionRaceType)
newMonth <- rename(newMonth, LastTimeClassDrop = ClassDifferentialOneRace, LastTimeWeightDrop = WeightDifferentialOneRace)

newMonthCols <- colnames(newMonth)

setdiff(newMonthCols, ukhrCols)
setdiff(ukhrCols, newMonthCols)

# Check Duplicates

temp <- select(newMonth, Date, Time24Hour, Meeting, Horse)

duped <- which(duplicated(temp))

duped

###############################################################
newMonth$BetFairSPForecastWinPrice <- if_else(is.na(newMonth$BetFairSPForecastWinPrice),
                                              newMonth$Betfair.Win.S.P., newMonth$BetFairSPForecastWinPrice)


newMonth$Code_Change <- ifelse(newMonth$DaysSinceLastRun < 0, 1, 0)

#make days since last run variable positive (absolute number)

newMonth$DaysSinceLastRun <- abs(newMonth$DaysSinceLastRun)

#remove % char from Stall percentage Variable and convert to numeric

newMonth$StallPercentage <- str_replace(newMonth$StallPercentage,"%","0")
newMonth$StallPercentage <- as.numeric(newMonth$StallPercentage)

#check observations with 0 values in days since last run variable 

test <- filter(newMonth, DaysSinceLastRun == 0)

summary(test$Race1RunAgo)

#Are these all having first run?

test2 <- filter(test, Race1RunAgo != 0)


missDays <- test[which(test$Race1RunAgo != 0),]

table(missDays$RaceType, missDays$Age)


#Check columns with missing values

MissingValColsDF <- newMonth[,colSums(is.na(newMonth))>0]

colnames(MissingValColsDF)

colSums(is.na(MissingValColsDF))

sum(is.na(MissingValColsDF$LastTimePosn))

allMissingVals <- newMonth[, colSums(is.na(newMonth)) == nrow(newMonth)]
allMissingVals

#remove columns with all missing values

colnames(newMonth)

length(which(is.na(newMonth$Betfair.Win.S.P.)))

#################################################################

newMonthBF <- subset(newMonth, !is.na(newMonth$Betfair.Win.S.P.))

#add BFSP P&L variable at 5% commission

newMonthBF$BFSP_PL <- if_else(newMonthBF$Result == 1, ((newMonthBF$Betfair.Win.S.P. - 1)*0.95),-1)
summary(newMonthBF$BFSP_PL)
summary(newMonthBF$Betfair.Win.S.P.)


#Add variables for VSP Stake and VSP P&L

newMonthBF$VSP_Stake <- 100/((newMonthBF$Betfair.Win.S.P. - 1)*0.95)
newMonthBF$VSP_PL <- if_else(newMonthBF$Result == 1, 100.00, -newMonthBF$VSP_Stake)

#Add Actual and Expected Winning Probability Variables 

newMonthBF$Actual <- ifelse(newMonthBF$BFSP_PL > 0,1,0)
newMonthBF$Expected <- 1 / newMonthBF$Betfair.Win.S.P.

#Add Actual - Expected Variable

newMonthBF$Act_Minus_Exp <- newMonthBF$Actual - newMonthBF$Expected

head(newMonthBF$Act_Minus_Exp)

#Check Betfair SP with 0 Values

newMonthBF$Betfair.Win.S.P.[newMonthBF$Betfair.Win.S.P. <= 0] <- NA

sum(is.na(newMonthBF$Betfair.Win.S.P.))

#newMonthBFtemp <- newMonthBF[!is.na(newMonthBF$Betfair.Win.S.P.),]

#remove observations with BFSP == 0 if necessary

#enter filter code to remove missing BFSP here



#add PL for Betfair Place SP

table(newMonthBF$Betfair.Placed)

newMonthBF$Betfair.Placed[newMonthBF$Betfair.Placed > 1] <- 1

table(newMonthBF$Betfair.Placed, newMonthBF$Result)

newMonthBF$BF_Placed_SP_PL <- if_else(newMonthBF$Betfair.Placed > 0, ((newMonthBF$Betfair.Place.S.P. - 1)*0.95),-1)

#convert lower case to upper case for character vectors
newMonthBF$Handicap[is.na(newMonthBF$Handicap)] <- "NONHCP"

newMonthBF <- mutate_if(newMonthBF, is.character, toupper)

# change Date format and add separate variables for day, month, year

newMonthBF$Date <- dmy(newMonthBF$Date)

newMonthBF$DayOfMonth <- mday(newMonthBF$Date)
newMonthBF$Month <- month(newMonthBF$Date)
newMonthBF$Year <- year(newMonthBF$Date)
newMonthBF$Weekday <- wday(newMonthBF$Date)

newMonthBF <- select(newMonthBF, Year, Month, DayOfMonth, Weekday, everything())

colnames(newMonthBF)

###############################################################

newMonthBF$Sire <- str_replace_all(newMonthBF$Sire, "&ACUTE;", "'")
newMonthBF$Trainer <- str_replace_all(newMonthBF$Trainer, "&ACUTE;", "'")
newMonthBF$Jockey <- str_replace_all(newMonthBF$Jockey, "&ACUTE;", "'")
newMonthBF$Horse <- str_replace_all(newMonthBF$Horse, "&ACUTE;", "'")

# Check for and remove duplicated rows

temp <- select(newMonthBF, Year, Month, DayOfMonth, Time24Hour, Meeting, Horse)

temp2 <- temp[which(duplicated(temp)),]

table(temp2$Year, temp2$Month)

table(temp$Year)

duped <- which(duplicated(temp))

duped

#enter code to remove duplicated obs here if neccesary

#newMonthBF2 <- newMonthBF[-duped,]


################################################################
newMonthBF <- select(newMonthBF, -WRITE_FAVOURITE_RANKING, -WRITE_IN_DURATION_HERE)

newMonthBFCols <- colnames(newMonthBF)
ukhrCols <- colnames(ukhr_master_BF)

setdiff(newMonthBFCols, ukhrCols)
setdiff(ukhrCols, newMonthBFCols)

summary(newMonthBF$Month)


###################################################################

ukhr_master_BF <- select(ukhr_master_BF, newMonthBFCols)

ukhr_master_BF <- rbind(ukhr_master_BF, newMonthBF) 
  

table(ukhr_master_BF$Year, ukhr_master_BF$Month)

# Remove Wolverhampton Polytrack data from pre Tapeta era

# wPoly1 <- which(ukhr_master_BF$Year < 2014 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")
# 
# wPoly2 <- which(ukhr_master_BF$Year == 2014 & ukhr_master_BF$Month < 8 & ukhr_master_BF$Meeting == "WOLVERHAMPTON")
# 
# wPolyAll <- c(wPoly1, wPoly2)
# 
# ukhr_master_BF <- ukhr_master_BF[-wPolyAll,]


ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Stall = min_rank(StallNumber))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Rating_Rank = min_rank(RatingsPosition))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Class_Rank = min_rank(ClassPosition))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Class_Rank_Range = cut(Class_Rank, 3,
                                labels = c("Top_Third", "Middle_Third", "Bottom_Third"), 
                                ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Spd_Rank = min_rank(SpeedRatingRank))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Speed_Rank_Range = cut(Spd_Rank, 3,
                                labels = c("Top_Third", "Middle_Third", "Bottom_Third"), 
                                ordered_result = T))



ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Ratings_Range = cut(Rating_Rank, 3,
                             labels = c("Top_Third", "Middle_Third" ,"Bottom_Third"), 
                             ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Weight_Range = cut(desc(Weight_Pounds), 3,
                            labels = c("High", "Middle", "Low"),
                            ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(LTO_Days_Range = cut(DaysSinceLastRun, breaks = c(0, 4, 7, 35, 91, 182, 365, 1000),
                              labels = c("<=4", "<=7", "<=35", "<=91", "<=182", "<=365", "<=1000"), 
                              ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(BFSPFC_Odds_Range = cut(BetFairSPForecastWinPrice, breaks = c(0, 6, 11, 21, 51, 1000),
                                 labels = c("<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                                 ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(BFSP_Odds_Range = cut(Betfair.Win.S.P., breaks = c(0, 2, 6, 11, 21, 51, 1000),
                               labels = c("<=2","<=6", ">6 to 11", ">11 to 21", ">21 to 51", ">51"),
                               ordered_result = T))


ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Value_Odds_Range = cut(ValueOdds_BetfairFormat, breaks = c(0, 6, 11, 21, 51, 1000),
                                labels = c("<=6", ">6 to 11", ">11 to 21",">21 to 51", ">51"),
                                ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Value_Odds_Ratio = BetFairSPForecastWinPrice / ValueOdds_BetfairFormat,
         VOR_Range = cut(Value_Odds_Ratio, breaks = c(0, 0.5, 1.0, 2.5, 5.0, 10, 100),
                         labels = c("<=0.5", ">0.5 to 1.0", ">1 to 2.50", ">2.50 to 5.0", ">5 to 10", ">10"),
                         ordered_result = T)) 

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(BFSP_ValOdds_Ratio = Betfair.Win.S.P. / ValueOdds_BetfairFormat,
         BFSP_VOR_Range = cut(BFSP_ValOdds_Ratio, breaks = c(0, 0.5, 1.0, 2.5, 5.0, 10, 100),
                              labels = c("<=0.5", ">0.5 to 1.0", ">1 to 2.50", ">2.50 to 5.0", ">5 to 10", ">10"),
                              ordered_result = T)) 

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Runners_Range = cut(Actual.Runners, breaks = c(0, 8, 16, 100),
                             labels = c("<=8", "9-16", "17+"),
                             ordered_result = T)) 




ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Place_Expected = 1/Betfair.Place.S.P.)

slowGround <- c("SOFT","SFT-HVY","HEAVY", "GD-SFT", "YIELD", "GD-YLD", "YLD-SFT") 

fastGround <- c("GD-FM", "FIRM", "HARD")

syntheticGround <- c("STAND", "STD-SLOW", "STANDARD", "STD-FAST", "SLOW")

softGround <- c("SOFT", "SFT-HVY", "HEAVY")

unique(ukhr_master_BF$Going)

ukhr_master_BF$Going_Range <- ifelse(ukhr_master_BF$Going %in% slowGround, "SLOW", 
                                     ifelse(ukhr_master_BF$Going %in% fastGround,"FAST", "SYNTHETIC"))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(Weight_Rank = min_rank(desc(Weight_Pounds)))

#head(ukhr_master_BF$Weight_Rank, 30)   
#head(ukhr_master_BF$Weight_Pounds, 30)

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(Rev_Weight_Rank = min_rank(Weight_Pounds))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(Fav_Rank = min_rank(Betfair.Win.S.P.))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(FC_Fav_Rank = min_rank(BetFairSPForecastWinPrice))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(FcFav_Odds = min(BetFairSPForecastWinPrice))

ukhr_master_BF <- ukhr_master_BF %>%
  group_by(UKHR_RaceID) %>%
  mutate(FcFav_Odds_Range = cut(FcFav_Odds, breaks = c(0, 1.5 ,2, 4, 6, 11, 100),
                                labels = c("<= 1.5",">1.5 to 2",">2 to 4", ">4 to 6", ">6 to 11", ">11"),
                                ordered_result = T))

ukhr_master_BF <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Fin_Pos = min_rank(LengthsBehindTotal),
         Exp_Btn = Actual.Runners - Fav_Rank,
         Act_Btn = Actual.Runners - Fin_Pos)

winter <- c(12,1,2)
spring <- c(3,4,5)
summer <- c(6,7,8)
autumn <- c(9,10,11)

ukhr_master_BF <- ukhr_master_BF %>% 
  mutate(Season = if_else(Month %in% winter, "Winter",
                          if_else(Month %in% spring, "Spring",
                                  if_else(Month %in% summer,"Summer",
                                          "Autumn"))))



ukhrOdds <- ukhr_master_BF %>% 
  group_by(UKHR_RaceID) %>% 
  mutate(Book = sum(ValueOdds_Probability), 
         Adj_Val_Prob = ValueOdds_Probability/Book,
         Adj_Val_Odds = 1/Adj_Val_Prob,
         BFSPFC_Book = sum(1/BetFairSPForecastWinPrice, na.rm = T),
         Adj_BFSPFC_Prob = (1/BetFairSPForecastWinPrice)/BFSPFC_Book,
         Adj_BFSPFC_Odds = 1/Adj_BFSPFC_Prob,
         New_Book = sum(Adj_Val_Prob),
         New_BFSPFC_Book = sum(1/Adj_BFSPFC_Odds)) %>% 
  select(UKHR_RaceID, Book, ValueOdds_Probability, ValueOdds_BetfairFormat, Adj_Val_Prob, Adj_Val_Odds,  BFSPFC_Book, 
         BetFairSPForecastWinPrice, Adj_BFSPFC_Prob, Adj_BFSPFC_Odds, Betfair.Win.S.P., New_Book, New_BFSPFC_Book)


ukhrOdds

summary(ukhrOdds$Adj_BFSPFC_Odds)
summary(ukhrOdds$Adj_Val_Odds)
summary(ukhrOdds$New_Book)
summary(ukhrOdds$New_BFSPFC_Book)

ukhr_master_BF$BetFairSPForecastWinPrice <- ukhrOdds$Adj_BFSPFC_Odds
ukhr_master_BF$ValueOdds_BetfairFormat <- ukhrOdds$Adj_Val_Odds

x <- max(ukhr_master_BF$ElapsedDays) - (365.25 * 6)

ukhr_master_BF <- ukhr_master_BF %>% 
  filter(ElapsedDays > x)

ukhr_master_BF <- ukhr_master_BF %>% 
  arrange(desc(Year, Month, DayOfMonth, UKHR_RaceID, Rating_Rank))

summary(ukhr_master_BF$Year)

table(ukhr_master_BF$Year, ukhr_master_BF$Month)

write_csv(ukhr_master_BF, "UKHR_Master_BF_2018_11_30.csv")

ukhr_master_BF$Year[1]
