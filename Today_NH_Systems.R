
ukChase <- filter(ukhr_master_BF, RaceType == "CHASE")
ukHurdle <- filter(ukhr_master_BF, RaceType == "HURDLE")
ukNHflat <- filter(ukhr_master_BF, RaceType == "NH FLAT")

#Remove Stall predictors from Jumps data

ukChase <- select(ukChase, -c(StallNumber,StallPercentage))
ukHurdle <- select(ukHurdle, -c(StallNumber,StallPercentage))
ukNHflat <- select(ukNHflat, -c(StallNumber,StallPercentage))

ukNH <- rbind(ukChase, ukHurdle, ukNHflat)

trainersNH <- read_csv("TrainersNH.csv", col_names = T)

trTFCQuals <- trainersNH%>%
  left_join(today, by = c("Trainer","Meeting","RaceType"))%>%
  filter(RaceType == "CHASE" | RaceType == "HURDLE" | RaceType == "NH FLAT", !is.na(Time24Hour))

trTFCQuals

if(nrow(trTFCQuals) > 0) {
  trTFCQuals$System_Name <- "NH_TFC"
}


hdlStaySires <- read_csv("HdlStaySires.csv", col_names = T)

NHSireQuals <- hdlStaySires%>%
  left_join(today, by = c("Sire","RaceType"))%>%
  filter(RaceType == "HURDLE" & Furlongs > 20, !is.na(Time24Hour))


NHSireQuals

if(nrow(NHSireQuals) > 0) {
  NHSireQuals$System_Name <- "NH_Staying_Hurdle_Sires"
}

hdlStayTrainers <- read_csv("HdlStayTrainers.csv", col_names = T)


trHLDQuals <- hdlStayTrainers%>%
  left_join(today, by = c("Trainer","RaceType"))%>%
  filter(RaceType == "HURDLE" & Furlongs > 20, !is.na(Time24Hour))

#trHLDQuals <- na.omit(trHLDQuals)

trHLDQuals

if(nrow(trHLDQuals) > 0) {
  trHLDQuals$System_Name <- "NH_Trainer_Staying_Hurdlers"
}

softSiresNH <- read_csv("SoftGroundSires_NH.csv", col_names = T)

todaySoftSiresNHQ <- softSiresNH %>%
  left_join(today, by = c("Sire")) %>%
  arrange(Time24Hour, Meeting, Horse) %>%
  filter(Going %in% softGround, RaceType == "CHASE" | RaceType == "HURDLE" | RaceType == "NH FLAT")


todaySoftSiresNHQ

if(nrow(todaySoftSiresNHQ) > 0) {
  todaySoftSiresNHQ$System_Name <- "NH_Soft_Ground_Sires"
}


allNHSystemQualifiers <- trTFCQuals %>%
  full_join(todaySoftSiresNHQ) %>%
  full_join(trHLDQuals) %>%
  full_join(NHSireQuals) %>%
  arrange(Time24Hour, Meeting, Horse)



allNHSystemQualifiers
