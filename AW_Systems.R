# Filter AW Races

ukAW <- filter(ukhr_master_BF, RaceType == "AW")

#remove jumping predictor from AW data

ukAW <- select(ukAW, -c(ChaseJumpingAbility, HunterChase, Beginner))

ukAW$Dist_Range <- as.factor(ifelse(ukAW$Furlongs < 8, "Sprint",
                                              ifelse(ukAW$Furlongs < 14,"Middle", "Long")))

# AW data models

#filter data to exclude races before 11th August 14 (New Tapeta surface at Wolverhampton replaced Polytrack)

f1 <- filter(ukAW, Year >= 2015)
f2 <- filter(ukAW, Year == 2014 & Month >= 8)

ukAW2 <- rbind(f1,f2)

unique(ukAW2$Year)

#split into subsets by surface (tapeta, fibresand, polytrack)
tapeta <- c("Wolverhampton","Newcastle")
polytrack <- c("Dundalk","Kempton","Lingfield","Chelmsford City")
fibresand <- c("Southwell")

tapeta <- str_to_upper(tapeta)
polytrack <- str_to_upper(polytrack)
fibresand <- str_to_upper(fibresand)


tap <- filter(ukAW2, Meeting %in% tapeta)
pol <- filter(ukAW, Meeting %in% polytrack)
fib <- filter(ukAW, Meeting %in% fibresand)#

unique(pol$Meeting)

unique(ukAW2$Meeting)
unique(ukAW$Meeting)

todayAW <- filter(today, RaceType == "AW")


#tapeta sires
tapSiresT <- tap%>%
  group_by(Sire)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

tapSiresT



#View(tapSiresT)

write_csv(tapSiresT, "TapetaSires.csv")


tapSiresT_Quals <- tapSiresT %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% tapeta, RaceType == "AW")

tapSiresT_Quals
#
if(nrow(tapSiresT_Quals) > 0) {
  tapSiresT_Quals$System_Name <- "Tapeta_Sires"
}



#plot tapeta sires
#
# tapetaPlot <- ggplot(tapSiresT)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Tapeta (Newcastle and Wolverhampton) Sires AE_Ratio"))
#
# tapetaPlot

#ggsave("tapPlot.png")

#polytrack sires
polSiresT <- pol%>%
  group_by(Sire) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

polSiresT

write_csv(polSiresT, "PolytrackSires.csv")

polSiresT_Quals <- polSiresT %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% polytrack, RaceType == "AW")

polSiresT_Quals
#
if(nrow(polSiresT_Quals) > 0) {
  polSiresT_Quals$System_Name <- "Polytrack_Sires"
}



#plot polytrack sires
#
# polPlot <- ggplot(polSiresT)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Polytrack Sires AE_Ratio"))
# polPlot
#
# ggsave("polPlot.png")



#fibresand
fibSiresT <- fib%>%
  group_by(Sire)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

fibSiresT

write_csv(fibSiresT, "FibresandSires.csv")

fibSiresT_Quals <- fibSiresT %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% fibresand, RaceType == "AW")

fibSiresT_Quals
#
if(nrow(fibSiresT_Quals) > 0) {
  fibSiresT_Quals$System_Name <- "Fibresand_Sires"
}



#plot tapeta sires
#
# fibPlot <- ggplot(fibSiresT)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Fibresand Sires AE_Ratio"))
#
# fibPlot

#ggsave("fibPlot.png")


########################################################################################################

#by Course

#Wolverhampton from August 2014

wolvesSires <- ukAW2 %>%
  filter(Meeting == "WOLVERHAMPTON") %>%
  group_by(Sire) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))



write_csv(wolvesSires, "wSires.csv")

wolvesSires_Quals <- wolvesSires %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting == "WOLVERHAMPTON", RaceType == "AW")

wolvesSires_Quals
#
if(nrow(wolvesSires_Quals) > 0) {
  wolvesSires_Quals$System_Name <- "Wolves_Sires"
}



#
# wlvPlot <- ggplot(wolvesSires) +
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire)) +
#   xlab("Sire") +
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25) +
#   coord_flip() +
#   labs(title = paste("Top Wolverhampton Sires by Actual v Expected Winner Ratio"))
#
#
# wlvPlot
# ggsave("wlvPlot.png")

#Newcastle

newcastleSires <- ukAW%>%
  filter(Meeting == "NEWCASTLE")%>%
  group_by(Sire)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))


write_csv(newcastleSires, "nSires.csv")

newcastleSires_Quals <- newcastleSires %>%
  left_join(todayAW, by = c("Sire")) %>%
  filter(!is.na(Time24Hour), Meeting == "NEWCASTLE", RaceType == "AW")

newcastleSires_Quals

if(nrow(newcastleSires_Quals) > 0) {
  newcastleSires_Quals$System_Name <- "Newcastle_Sires"
}





#
# nwcPlot <- ggplot(newcastleSires)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Top Newcastle Sires by Actual v Expected Winner Ratio"))
#
# nwcPlot
# ggsave("nwcPlot.png")


#fibresand and polytrack from 2012

# southwellSires <- ukAW%>%
#   filter(Meeting == "SOUTHWELL")%>%
#   group_by(Sire)%>%
#   summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
#             Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
#             Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
#             AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
#             Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
#             Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
#             Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
#             Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
#             Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
#   filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
#   arrange(desc(AE_Ratio, meanPL))
#
# st <- southwellSires
# st
# write_csv(st, "st.csv")



# southwellSires_Quals <- southwellSires %>%
#   left_join(todayAW, by = c("Sire")) %>%
#   filter(!is.na(Time24Hour), Meeting == "SOUTHWELL", RaceType == "AW")
#
# southwellSires_Quals
# #
# if(nrow(southwellSires_Quals) > 0) {
#   southwellSires_Quals$System_Name <- "Southwell_Sires"
# }


#
# sthPlot <- ggplot(fibresandSires)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Top Southwell Sires by Actual v Expected Winner Ratio"))
#
# sthPlot
# ggsave("sthPlot.png")

polyMeetingSires <- ukAW%>%
  filter(Meeting %in% polytrack)%>%
  group_by(Meeting, Sire)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

polyMeetingSires

write_csv(polyMeetingSires, "pSires.csv")


polyMeetingSires_Quals <- polyMeetingSires %>%
  left_join(todayAW, by = c("Meeting", "Sire")) %>%
  filter(!is.na(Time24Hour), Meeting %in% polytrack, RaceType == "AW")

polyMeetingSires_Quals

if(nrow(polyMeetingSires_Quals) > 0) {
  polyMeetingSires_Quals$System_Name <- "Poly_Meeting_Sires"
}



############################################################################################################


#Trainers

newcastleTrainers <- ukAW%>%
  filter(Meeting == "NEWCASTLE")%>%
  group_by(Trainer)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

newcastleTrainers

write_csv(newcastleTrainers, "nTrainers.csv")

newcastleTrainers_Quals <- newcastleTrainers %>%
  left_join(todayAW, by = c("Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting == "NEWCASTLE", RaceType == "AW")

newcastleTrainers_Quals

if(nrow(newcastleTrainers_Quals) > 0) {
  newcastleTrainers_Quals$System_Name <- "Newcastle_Trainers"
}




wolvesTrainers <- ukAW%>%
  filter(Meeting == "WOLVERHAMPTON")%>%
  group_by(Trainer)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

wolvesTrainers

write_csv(wolvesTrainers, "wTrainers.csv")

wolvesTrainers_Quals <- wolvesTrainers %>%
  left_join(todayAW, by = c("Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting == "WOLVERHAMPTON", RaceType == "AW")

wolvesTrainers_Quals

if(nrow(wolvesTrainers_Quals) > 0) {
  wolvesTrainers_Quals$System_Name <- "Wolves_Trainers"
}








#
# nwcPlot <- ggplot(newcastleSires)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Top Newcastle Sires by Actual v Expected Winner Ratio"))
#
# nwcPlot
# ggsave("nwcPlot.png")


#fibresand and polytrack from 2012

southwellTrainers <- ukAW%>%
  filter(Meeting == "SOUTHWELL")%>%
  group_by(Trainer)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

southwellTrainers


write_csv(southwellTrainers, "sTrainers.csv")



southwellTrainers_Quals <- southwellTrainers %>%
  left_join(todayAW, by = c("Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting == "SOUTHWELL", RaceType == "AW")

southwellTrainers_Quals

if(nrow(southwellTrainers_Quals) > 0) {
  southwellTrainers_Quals$System_Name <- "Southwell_Trainers"
}

#
# sthPlot <- ggplot(fibresandSires)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Top Southwell Sires by Actual v Expected Winner Ratio"))
#
# sthPlot
# ggsave("sthPlot.png")

polyMeetingTrainers <- ukAW%>%
  filter(Meeting %in% polytrack)%>%
  group_by(Meeting, Trainer)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

polyMeetingTrainers

write_csv(polyMeetingTrainers, "pTrainers.csv")


polyMeetingTrainers_Quals <- polyMeetingTrainers %>%
  left_join(todayAW, by = c("Meeting", "Trainer")) %>%
  filter(!is.na(Time24Hour), Meeting %in% polytrack, RaceType == "AW")

polyMeetingTrainers_Quals


if(nrow(polyMeetingTrainers_Quals) > 0) {
  polyMeetingTrainers_Quals$System_Name <- "Poly_Meeting_Trainers"
}







#
# lngPlot <- ggplot(lingfieldSires)+
#   geom_col(aes(x=reorder(Sire,AE_Ratio,sum), y=AE_Ratio, fill = Sire))+
#   xlab("Sire")+
#   ylab("Actual Winners v Expected Winners Ratio")+
#   geom_label(aes(Sire, AE_Ratio,label = round(AE_Ratio,2)), nudge_y = -0.25)+
#   coord_flip()+
#   labs(title = paste("Top Lingfield Sires by Actual v Expected Winner Ratio"))
#
# lngPlot
# ggsave("lngPlot.png")
#

# ####################################################################################################
#
# # AW Trainer/Jockey Combo's
#
# # AW data models
#
# #filter data to exclude races before 11th August 14 (New Tapeta surface at Wolverhampton replaced Polytrack)

trJkComboAW <- ukAW %>%
  group_by(Trainer, Jockey) %>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 50 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10, Archie >= 2.50)%>%
  arrange(desc(AE_Ratio, meanPL))

trJkComboAW

write_csv(trJkComboAW, "TopTrJkCombos_AW.csv")

#View(trJkComboAW)

#Today's qualifiers


todayAWTJQ <- trJkComboAW %>%
  left_join(todayAW, by = c("Trainer", "Jockey")) %>%
  filter(!is.na(Time24Hour), RaceType == "AW")

todayAWTJQ

if(nrow(todayAWTJQ) > 0) {
  todayAWTJQ$System_Name <- "AW_TJ_Combo"
}


#View(todayAWTJQ)

# tdyAWSummary <- todayAWQualifierSummary %>%
#   full_join(todayAWTJQ) %>%
#   arrange(Time24Hour, Meeting, Horse)
#
# tdyAWSummary <- select(tdyAWSummary, 1:5, Runs, meanPL, totalPL, AE_Ratio, WinPercent,
#                        Sire, Trainer, Jockey, everything())
#
# tdyAWSummary

#View(tdyAWSummary)

##########################################################################################

# AW Sire/Distance Range

#table(tap$Meeting, tap$Furlongs)

tap$Dist_Range <- as.factor(ifelse(tap$Furlongs < 8, "Sprint", "Route"))

head(tap$Dist_Range, 30)

dRangeSiresTap <- tap%>%
  group_by(Sire, Meeting, Dist_Range)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

dRangeSiresTap

write_csv(dRangeSiresTap, "TapetaSires_DistanceRange.csv")

#View(dRangeSiresTap)

pol$Dist_Range <- as.factor(ifelse(pol$Furlongs < 8, "Sprint", "Route"))

dRangeSiresPol <- pol%>%
  group_by(Sire, Meeting, Dist_Range)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

dRangeSiresPol

write_csv(dRangeSiresPol, "PolySires_DistanceRange.csv")

#View(dRangeSiresPol)


fib$Dist_Range <- as.factor(ifelse(fib$Furlongs < 8, "Sprint", "Route"))

dRangeSiresFib <- fib%>%
  group_by(Sire, Meeting, Dist_Range)%>%
  summarise(Runs = n(),meanPL = round(mean(BFSP_PL),2), totalPL = round(sum(BFSP_PL),2), Horses = length(unique(Horse)),
            Avg_BFVSP_PL = round(mean(VSP_PL), 2), Total_BFVSP_PL = round(sum(VSP_PL),2),
            Avg_VSP_Stake = mean(VSP_Stake), Total_VSP_Stake = sum(VSP_Stake), VSP_ROI = Total_BFVSP_PL/Total_VSP_Stake,
            AE_Ratio = round(sum(Actual)/sum(Expected),2),WinPercent = round(sum((Actual)/Runs),2),
            Placed_AE_Ratio = round(sum(Betfair.Placed, na.rm = T)/sum(Place_Expected, na.rm = T),2), BF_Place_ROI = round(mean(BF_Placed_SP_PL, na.rm = T),2),
            Winners = sum(Actual), Exp_Wins = round(sum(Expected),2), Places = sum(Betfair.Placed, na.rm = T), Exp_Places = sum(Place_Expected, na.rm = T),
            Total_Btn = sum(Act_Btn), Total_Exp_Btn = sum(Exp_Btn),
            Btn_AE_Ratio = round(sum(Act_Btn)/sum(Exp_Btn),2),
            Archie = (Runs * ((Winners - Exp_Wins) ^ 2)) / (Exp_Wins * (Runs - Exp_Wins)))%>%
  filter(Runs >= 30 & AE_Ratio >= 1.20 & meanPL >= 0.2 & WinPercent >= 0.10 & Horses >= 5, Archie > 2.50, Exp_Wins >= 5.0)%>%
  arrange(desc(AE_Ratio, meanPL))

dRangeSiresFib

write_csv(dRangeSiresFib, "FibSires_DistanceRange.csv")

siresDistRange <- rbind(dRangeSiresTap,dRangeSiresPol,dRangeSiresFib)

siresDistRange <- siresDistRange %>%
  arrange(Meeting, Dist_Range, desc(AE_Ratio))

siresDistRange

#View(siresDistRange)

####################################################

#todayAWTracks <- filter(today, RaceType == "AW")

todayAW$Dist_Range <- as.factor(ifelse(todayAW$Furlongs < 8, "Sprint", "Route"))

levels(todayAW$Dist_Range)

siresDistRangeQuals <- siresDistRange %>%
  left_join(todayAW, by = c("Sire", "Meeting", "Dist_Range")) %>%
  filter(!is.na(Time24Hour))

siresDistRangeQuals

if(nrow(siresDistRangeQuals) > 0) {
  siresDistRangeQuals$System_Name <- "AW_Sires_Distance_Range"
}



###################################################

todayAllAW_Qualifiers <- tapSiresT_Quals %>%
  full_join(polSiresT_Quals) %>%
  full_join(fibSiresT_Quals) %>%
  full_join(wolvesSires_Quals) %>%
  full_join(newcastleSires_Quals) %>%
  #full_join(southwellSires_Quals) %>%
  full_join(polyMeetingSires_Quals) %>%
  full_join(wolvesTrainers_Quals) %>%
  full_join(newcastleTrainers_Quals) %>%
  full_join(southwellTrainers_Quals) %>%
  full_join(polyMeetingTrainers_Quals) %>%
  full_join(todayAWTJQ) %>%
  full_join(siresDistRangeQuals)

todayAllAW_Qualifiers

