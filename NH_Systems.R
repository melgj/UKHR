setwd("~/git_projects/ukhr/UKHR")

#ukhr_master_BF <- read_csv("UKHR_Master_BF_2018_03_31.csv",col_names = T)

#ukhr_master_1217 <- read_csv("ukhr_all_12_17_master.csv", col_names = T)

ukChase <- filter(ukhr_master_BF, RaceType == "CHASE")
ukHurdle <- filter(ukhr_master_BF, RaceType == "HURDLE")
ukNHflat <- filter(ukhr_master_BF, RaceType == "NH FLAT")

#Remove Stall predictors from Jumps data

ukChase <- select(ukChase, -c(StallNumber,StallPercentage))
ukHurdle <- select(ukHurdle, -c(StallNumber,StallPercentage))
ukNHflat <- select(ukNHflat, -c(StallNumber,StallPercentage))

ukNH <- rbind(ukChase, ukHurdle, ukNHflat)



# NH trainers AE by course

trainersNH <- ukNH%>%
  group_by(Trainer,Meeting, RaceType)%>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))) %>% 
  filter(Runs >= 30, AE_Ratio >= 1.20, Horses >= 5, Exp_Wins >= 5.0, Archie > 2.5) %>% 
  arrange(desc(AE_Ratio))

trainersNH

write_csv(trainersNH, "TrainersNH.csv")

# todays qualifiers

head(today$Trainer)

#today2$Trainer <- stri_trans_toupper(today2$Trainer)

trTFCQuals <- trainersNH%>%
  left_join(today, by = c("Trainer","Meeting","RaceType"))%>%
  filter(RaceType == "CHASE" | RaceType == "HURDLE" | RaceType == "NH FLAT", !is.na(Time24Hour))

trTFCQuals

#write_csv(trTFCQuals, "NH_TFC_Quals_041117")

####################################################################

#NH Hurdle Staying Sires by Course

hdlStaySires <- ukHurdle%>%
  filter(Furlongs > 20) %>% 
  group_by(Sire,RaceType)%>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected), 
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))) %>% 
  filter(Runs >= 30, AE_Ratio >= 1.20, Horses >= 5, Exp_Wins >= 5.0, Archie > 2.5) %>% 
  arrange(desc(AE_Ratio))

hdlStaySires

write_csv(hdlStaySires, "HdlStaySires.csv")

#View(hdlStaySires)

#write_csv(hdlStaySires, "Hurdle_Staying_Sires.csv")

# todays qualifiers

NHSireQuals <- hdlStaySires%>%
  left_join(today, by = c("Sire","RaceType"))%>%
  filter(RaceType == "HURDLE" & Furlongs > 20, !is.na(Time24Hour))


NHSireQuals

#View(NHSireQuals)

#write_csv(NHSireQuals, "NH_LDH_Sires_Quals_041117")

########################################################

hdlStayTrainers<- ukHurdle%>%
  group_by(Trainer, RaceType)%>%
  filter(Furlongs > 20)%>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected), 
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))) %>% 
  filter(Runs >= 30, AE_Ratio >= 1.20, Horses >= 5, Exp_Wins >= 5.0, Archie > 2.5) %>% 
  arrange(Trainer,desc(AE_Ratio),desc(meanPL))

hdlStayTrainers

write_csv(hdlStayTrainers, "HdlStayTrainers.csv")

# todays qualifiers



trHLDQuals <- hdlStayTrainers%>%
  left_join(today, by = c("Trainer","RaceType"))%>%
  filter(RaceType == "HURDLE" & Furlongs > 20, !is.na(Time24Hour))

#trHLDQuals <- na.omit(trHLDQuals)

trHLDQuals

#View(trHLDQuals)

#write_csv(trHLDQuals, "NH_Stay_Trnrs_Quals_041117")


#######################################################

#Combine all qualifiers and write to file

# printNHCols <- rbind(trHLDQuals,NHSireQuals,trTFCQuals)
# printNHCols <- select(printNHCols, Time24Hour, Meeting, Horse, BetFairSPForecastWinPrice ,Rating_Rank, Trainer, Sire, everything())
# 
# 
# printNHCols <- printNHCols%>%
#   arrange(Time24Hour, Meeting, Horse, Rating_Rank)
# 
# NHQuals <- printNHCols

NHQuals <- rbind(trHLDQuals,NHSireQuals,trTFCQuals)


#####################################################################################################################

# Soft/Heavy Ground Sires

#slowGround <- c("SOFT","SFT-HVY","HEAVY") 

softGroundNH <- filter(ukNH, Going %in% softGround)

softSiresNH <- softGroundNH%>%
  group_by(Sire)%>%
  summarise(Runs = n(),meanPL = mean(BFSP_PL), totalPL = sum(BFSP_PL), AE_Ratio = sum(Actual)/sum(Expected), 
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            WinPercent = sum(Actual)/Runs, Horses = length(unique(Horse)),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins))) %>% 
  filter(Runs >= 30, AE_Ratio >= 1.20, Horses >= 5, Exp_Wins >= 5.0, Archie > 2.5) %>% 
  arrange(desc(AE_Ratio),desc(meanPL))

softSiresNH

write_csv(softSiresNH, "SoftGroundSires_NH.csv")

#todayNH <- filter(today, RaceType == "CHASE" | RaceType == "HURDLE" | RaceType == "NH FLAT")

todaySoftSiresNH <- softSiresNH %>% 
  left_join(today, by = c("Sire")) %>% 
  arrange(Time24Hour, Meeting, Horse) %>% 
  filter(Going %in% softGround, RaceType == "CHASE" | RaceType == "HURDLE" | RaceType == "NH FLAT")


todaySoftSiresNHQ <- select(todaySoftSiresNH, everything())

todaySoftSiresNHQ 


allNHSystemQualifiers <- NHQuals %>% 
  full_join(todaySoftSiresNHQ) %>% 
  arrange(Time24Hour, Meeting, Horse)

#allNHSystemQualifiers <- select(allNHSystemQualifiers, Time24Hour, Meeting, Horse, BetFairSPForecastWinPrice,
                                #ValueOdds_BetfairFormat, Trainer, Sire, everything())

allNHSystemQualifiers



