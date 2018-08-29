library(tidyverse)
library(stringr)
library(stringi)
library(lubridate)


#read master ukhr csv files for ukhr data years 12-17 inc.
ukhr17 <- read_csv(file.choose(),col_names = T)
ukhr16 <- read_csv(file.choose(),col_names = T)
ukhr15 <- read_csv(file.choose(),col_names = T)
ukhr14 <- read_csv(file.choose(),col_names = T)
ukhr13 <- read_csv(file.choose(),col_names = T)
ukhr12 <- read_csv(file.choose(),col_names = T)
#ukhr11 <- read_csv(file.choose(),col_names = T)

ukhr17 <- rename(ukhr17, LastTimePosn = LastTimePositionRaceType)

ukhr_all <- rbind(ukhr17,ukhr16,ukhr15,ukhr14,ukhr13,ukhr12)

#write combined data files for years 12-17 to csv file

write.csv(ukhr_all, "/home/mel/UKHR/UKHR_data/UKHR_Master_Data/ukhr_all_12_17_master.csv", row.names = F)


################################################################################################

temp <- select(ukhr_master_BF, Year, Month, DayOfMonth, Time24Hour, Meeting, Horse)

temp2 <- temp[which(duplicated(temp)),]

table(temp2$Year, temp2$Month)

table(temp$Year)

duped <- which(duplicated(temp))

duped

ukhr

#read ukhr_all_12_17_master.csv

ukhr_all <- read_csv("/home/mel/UKHR/UKHR_data/UKHR_Master_Data/ukhr_all_12_17_master.csv", col_names = T)

colnames(ukhr_all)

#add variable for code change from last run, designated by minus symbol.

ukhr_all$Code_Change <- ifelse(ukhr_all$DaysSinceLastRun < 0, 1, 0)

#make days since last run variable positive (absolute number)

ukhr_all$DaysSinceLastRun <- abs(ukhr_all$DaysSinceLastRun)

#remove % char from Stall percentage Variable and convert to numeric

ukhr_all$StallPercentage <- str_replace(ukhr_all$StallPercentage,"%","0")
ukhr_all$StallPercentage <- as.numeric(ukhr_all$StallPercentage)

#check observations with 0 values in days since last run variable 

test <- filter(ukhr_all, DaysSinceLastRun == 0)

summary(test$Race1RunAgo)

#Are these all having first run?

test2 <- filter(test, Race1RunAgo != 0)


missDays <- test[which(test$Race1RunAgo != 0),]

table(missDays$RaceType, missDays$Age)


#Check columns with missing values

MissingValColsDF <- ukhr_all[,colSums(is.na(ukhr_all))>0]

colnames(MissingValColsDF)

colSums(is.na(MissingValColsDF))

sum(is.na(MissingValColsDF$PositionLastTime))

allMissingVals <- ukhr_all[, colSums(is.na(ukhr_all)) == nrow(ukhr_all)]
allMissingVals

#remove columns with all missing values

ukhr_all <- select(ukhr_all, -c(WRITE_FAVOURITE_RANKING,WRITE_IN_DURATION_HERE))

unique(ukhr_all$Systems)

colnames(ukhr_all)

#Create dataframe excluding horses without Betfair S.P.

ukhr_master_BF <- ukhr_all
colnames(ukhr_master_BF)
colnames(ukhr_master_BF) <- str_replace_all(colnames(ukhr_master_BF)," ",".")
colnames(ukhr_master_BF)


ukhr_master_BF <- subset(ukhr_master_BF, !is.na(ukhr_master_BF$Betfair.Win.S.P.))

#add BFSP P&L variable at 5% commission

ukhr_master_BF$BFSP_PL <- if_else(ukhr_master_BF$Result == 1, ((ukhr_master_BF$Betfair.Win.S.P. - 1)*0.95),-1)
summary(ukhr_master_BF$BFSP_PL)
summary(ukhr_master_BF$Betfair.Win.S.P.)


#Add variables for VSP Stake and VSP P&L

ukhr_master_BF$VSP_Stake <- 100/((ukhr_master_BF$Betfair.Win.S.P. - 1)*0.95)
ukhr_master_BF$VSP_PL <- if_else(ukhr_master_BF$Result == 1, 100.00, -ukhr_master_BF$VSP_Stake)

#Add Actual and Expected Winning Probability Variables 

ukhr_master_BF$Actual <- ifelse(ukhr_master_BF$BFSP_PL > 0,1,0)
ukhr_master_BF$Expected <- 1 / ukhr_master_BF$Betfair.Win.S.P.

#Add Actual - Expected Variable

ukhr_master_BF$Act_Minus_Exp <- ukhr_master_BF$Actual - ukhr_master_BF$Expected

head(ukhr_master_BF$Act_Minus_Exp)
head(ukhr_master_BF$Betfair.Win.S.P.)

#Check Betfair SP with 0 Values

missingBFSP <-which(ukhr_master_BF$Betfair.Win.S.P. == 0)

#remove observations with BFSP == 0

ukhr_master_BF2 <- ukhr_master_BF[-c(missingBFSP),]

ukhr_master_BF <- ukhr_master_BF2

#add PL for Betfair Place SP

table(ukhr_master_BF$Betfair.Placed)

ukhr_master_BF$Betfair.Placed[ukhr_master_BF$Betfair.Placed > 1] <- 1

ukhr_master_BF$BF_Placed_SP_PL <- if_else(ukhr_master_BF$Betfair.Placed > 0, ((ukhr_master_BF$Betfair.Place.S.P. - 1)*0.95),-1)

#convert lower case to upper case for character vectors
ukhr_master_BF$Handicap[is.na(ukhr_master_BF$Handicap)] <- "NONHCP"

ukhr_master_BF <- mutate_if(ukhr_master_BF, is.character, toupper)

# change Date format and add separate variables for day, month, year

ukhr_master_BF$Date <- dmy(ukhr_master_BF$Date)

ukhr_master_BF$DayOfMonth <- mday(ukhr_master_BF$Date)
ukhr_master_BF$Month <- month(ukhr_master_BF$Date)
ukhr_master_BF$Year <- year(ukhr_master_BF$Date)
ukhr_master_BF$Weekday <- wday(ukhr_master_BF$Date)

ukhr_master_BF <- select(ukhr_master_BF, Year, Month, DayOfMonth, Weekday, everything())

colnames(ukhr_master_BF)


write.csv(ukhr_master_BF, "/home/mel/UKHR/UKHR_data/UKHR_Master_Data/UKHR_Master_BF.csv", row.names = F)

###########################################################################
# read master betfair file to add edits

ukhr_master_BF <- read_csv("/home/mel/UKHR/UKHR_data/UKHR_Master_Data/UKHR_Master_BF.csv", col_names = T)

#################################################################################
# Replace apostrophe character errors in importing csv files

ukhr_master_BF$Sire <- str_replace_all(ukhr_master_BF$Sire, "&ACUTE;", "'")
ukhr_master_BF$Trainer <- str_replace_all(ukhr_master_BF$Trainer, "&ACUTE;", "'")
ukhr_master_BF$Jockey <- str_replace_all(ukhr_master_BF$Jockey, "&ACUTE;", "'")
ukhr_master_BF$Horse <- str_replace_all(ukhr_master_BF$Horse, "&ACUTE;", "'")

# Check for and remove duplicated rows

temp <- select(ukhr_master_BF, Year, Month, DayOfMonth, Time24Hour, Meeting, Horse)

temp2 <- temp[which(duplicated(temp)),]

table(temp2$Year, temp2$Month)

table(temp$Year)

duped <- which(duplicated(temp))

duped

ukhr_master_BF <- ukhr_master_BF[-duped,]

write.csv(ukhr_master_BF, "UKHR_Master_BF.csv", row.names = F)

