library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)


ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_07_31.csv", col_names = T)

length(which(ukhr_master_BF$BetFairSPForecastWinPrice <= 0))

ukhr_master_BF$BetFairSPForecastWinPrice[ukhr_master_BF$BetFairSPForecastWinPrice <= 0] <- NA

summary(ukhr_master_BF$BetFairSPForecastWinPrice)

summary(ukhr_master_BF$Year)

ukhrCols <- colnames(ukhr_master_BF)

# Load New Month Data

newMonth <- read_csv(file.choose(), col_names = T)

newMonth$BetFairSPForecastWinPrice[newMonth$BetFairSPForecastWinPrice <= 0] <- NA

summary(newMonth$BetFairSPForecastWinPrice)

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

newMonthBFtemp <- newMonthBF[!is.na(newMonthBF$Betfair.Win.S.P.),]

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


###################################################################

ukhr_master_BF2 <- rbind(ukhr_master_BF, newMonthBF)

ukhrOdds <- ukhr_master_BF2 %>% 
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

ukhr_master_BF2$BetFairSPForecastWinPrice <- ukhrOdds$Adj_BFSPFC_Odds
ukhr_master_BF2$ValueOdds_BetfairFormat <- ukhrOdds$Adj_Val_Odds

#ukhr_master_BF2 <- ukhr_master_BF2 %>% 
 # filter(Year >= 2013)

write_csv(ukhr_master_BF2, "UKHR_Master_BF_2018_08_31.csv")
